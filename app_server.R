# Breakout MODs

# Root MOD
root_mod <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    observe({
      print("root_mod")
      print(session$ns(""))
      # Extract IP address
      ip <- session$clientData$url_hostname
      # Log IP address with timestamp
      log_entry <- paste(Sys.time(), " - IP Address:", ip, "\n")
      print(log_entry)
    })

    # Set base timer interval for dashboard refresh interval
    live_timer <- reactiveTimer(10000) # 10 sec

    # MySQL connection
    conn <- dbConnect(MySQL(),
      host = config_host,
      user = config_user,
      password = config_password,
      dbname = config_dbname
    )

    ## GROUP ##
    initial_query <- function(table, initial_dt) {
      paste0(
        "SELECT energy, date_time FROM ", table, " WHERE date_time > '",
        initial_dt, "' ORDER BY date_time ASC LIMIT 1"
      )
    }

    final_query <- function(table, final_dt) {
      paste0(
        "SELECT energy, date_time FROM ", table, " WHERE date_time < '",
        lubridate::as_date(final_dt) + 1, "' ORDER BY date_time DESC LIMIT 1"
      )
    }

    max_query <- function(table, initial_dt, final_dt) {
      paste0(
        "SELECT MAX(energy) FROM ",
        table, " WHERE date_time BETWEEN '",
        initial_dt, "' AND '", lubridate::as_date(final_dt) + 1, "'"
      )
    }

    live_query <- function(table, final_dt) {
      paste0(
        "SELECT date_time, voltage, current, power, energy FROM ", table, " WHERE date_time < '",
        lubridate::as_date(final_dt) + 1, "' ORDER BY date_time DESC LIMIT 1"
      )
    }

    cost_per_kwh <- reactive({
      round(input$bill_cost / input$bill_kwh, 2)
    })
    output$cost_per_kwh <- renderText({
      paste0("$", cost_per_kwh())
    })

    meter_df <- reactive({
      # req(length(tables[input$group][[1]]) > 0)

      # Helper function to calculate measurements
      calculate_measurements <- function(table, date_range) {
        initial <- dbGetQuery(conn, initial_query(table, date_range[1]))
        max <- dbGetQuery(conn, max_query(table, date_range[1], date_range[2]))
        final <- dbGetQuery(conn, final_query(table, date_range[2]))

        if (final$energy < max$`MAX(energy)`) {
          meas <- max$`MAX(energy)` - initial$energy + final$energy
        } else {
          meas <- final$energy - initial$energy
        }

        days <- as.numeric(difftime(as.POSIXct(final$date_time), as.POSIXct(initial$date_time), units = "days"))

        list(meas = meas, days = days)
      }

      # Calculate measurements for each table
      measurements <- lapply(tables[[input$group]], calculate_measurements, date_range = input$date_range)

      # Extract measurements and days
      meas <- sapply(measurements, `[[`, "meas")
      days <- sapply(measurements, `[[`, "days")

      # Create data frame
      data.frame(
        Label = tables[[input$group]],
        kWh = meas / 1000,
        "Days Meas" = round(days, 2),
        CostEst = round(meas / 1000 * days_selected() / days * cost_per_kwh(), 2)
      )
    })

    live_df <- reactive({
      live_timer()
      req(length(tables[[input$group]]) > 0)

      # Helper function to retrieve live data for a table
      get_live_table_data <- function(table) {
        live_table_data <- dbGetQuery(conn, live_query(table, Sys.Date()))
        list(
          date_time = live_table_data$date_time,
          voltage = live_table_data$voltage,
          current = live_table_data$current,
          power = live_table_data$power,
          energy = live_table_data$energy
        )
      }

      # Retrieve live data for each table
      live_data_list <- lapply(tables[[input$group]], get_live_table_data)

      # Combine live data into a data frame
      data.frame(
        Label = tables[[input$group]],
        LastMeasurement = unlist(lapply(live_data_list, `[[`, "date_time")),
        Current = unlist(lapply(live_data_list, `[[`, "current")),
        Power = unlist(lapply(live_data_list, `[[`, "power")) / 1000,
        Energy = unlist(lapply(live_data_list, `[[`, "energy")) / 1000,
        Voltage = unlist(lapply(live_data_list, `[[`, "voltage"))
      )
    })

    total_energy <- reactive({
      sum(unlist(meter_df()["kWh"]))
    })
    days_selected <- reactive({
      1 + lubridate::as_date(input$date_range[2]) - lubridate::as_date(input$date_range[1])
    })

    output$total_energy <- renderText({
      paste0(total_energy(), "kWh")
    })
    output$group_table_label <- renderText({
      paste(
        "Estimated Cost Breakdown (based on # days selected:",
        days_selected(),
        ")"
      )
    })
    output$est_bill_total <- renderText({
      cost_est <- as.numeric(meter_df()[["CostEst"]])
      paste0("$", sum(cost_est, na.rm = TRUE))
    })

    # Group DT
    output$tbl <- renderDT(
      meter_df(),
      options = list(
        paging = FALSE, # TRUE,
        searching = FALSE,
        fixedColumns = TRUE,
        autoWidth = TRUE,
        ordering = TRUE,
        dom = "tB",
        buttons = c("copy", "csv", "excel")
      )
    )

    # Live DT
    output$live_table <- renderDT(
      live_df(),
      options = list(
        paging = FALSE, # TRUE,
        searching = FALSE,
        fixedColumns = TRUE,
        autoWidth = TRUE,
        ordering = TRUE,
        dom = "tB",
        buttons = c("copy", "csv", "excel")
      )
    )

    ## METER ##
    # Table dropdown UI
    output$table_ui <- renderUI({
      selectInput(ns("table"), "Table", tables[input$group][[1]])
    })

    # Query to retrieve timeseries data
    data_query <- reactive({
      req(input$table != "")
      query <- paste0(
        "SELECT date_time, ", input$data, " FROM ",
        input$table, " WHERE date_time BETWEEN '",
        input$date_range[1], "' AND '", lubridate::as_date(input$date_range[2]) + 1, "'"
      )
      print(query)
      dbGetQuery(conn, query)
    })

    # Plotting timeseries data
    output$plot <- renderPlotly({
      req(nrow(data_query()) > 0)
      plot_ly(data_query(),
        x = ~date_time, y = ~ get(input$data),
        type = "scatter", mode = "lines", name = input$data
      ) %>%
        layout(
          title = paste("Time Series of", input$data, "for Table:", input$table),
          xaxis = list(title = "Datetime"),
          yaxis = list(title = input$data)
        ) %>%
        layout(
          plot_bgcolor = "rgba(0, 0, 0, 0)",
          paper_bgcolor = "rgba(0, 0, 0, 0)"
        ) %>%
        layout(font = list(family = "Roboto, sans-serif", color = "#FFFFFF")) %>%
        layout(yaxis = list(tickcolor = "#FFFFF")) %>%
        layout(
          font = list(family = "Roboto, sans-serif", color = "#FFFFFF"),
          yaxis = list(tickcolor = "#FFFFF"),
          xaxis = list(
            title = list(text = "Datetime"),
            type = "date",
            # tickformat="%Y-%m-%d %H:%M:%S",
            tickcolor = "#FFFFF",
            tickangle = -90,
            font = list(family = "sans serif", size = 8, color = "white"),
            gridcolor = toRGB("gray50"), gridwidth = 1,
            linecolor = toRGB("white"),
            linewidth = 2
          )
        )
    })

    # Disconnect from database when app is closed
    session$onSessionEnded(function() {
      dbDisconnect(conn)
    })
  })
}
