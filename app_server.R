# Breakout MODs

# Root MOD
root_MOD <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    observe({
      print("root_MOD")
      print(session$ns(""))
      # Extract IP address
      ip <- session$clientData$url_hostname
      # Log IP address with timestamp
      log_entry <- paste(Sys.time(), " - IP Address:", ip, "\n")
      print(log_entry)
    })

    # Set base timer interval for dashboard refresh interval
    timer <- reactiveTimer(900000) # 900 sec = 15 min
    livetimer <- reactiveTimer(10000) # 10 sec

    # MySQL connection
    conn <- dbConnect(MySQL(),
      host = config_host,
      user = config_user,
      password = config_password,
      dbname = config_dbname
    )

    ## GROUP ##
    initialQuery <- function(table, initialdt) {
      paste0(
        "SELECT energy, date_time FROM ", table, " WHERE date_time > '",
        initialdt, "' ORDER BY date_time ASC LIMIT 1"
      )
    }
    finalQuery <- function(table, finaldt) {
      paste0(
        "SELECT energy, date_time FROM ", table, " WHERE date_time < '",
        lubridate::as_date(finaldt) + 1, "' ORDER BY date_time DESC LIMIT 1"
      )
    }
    maxQuery <- function(table, initialdt, finaldt) {
      paste0(
        "SELECT MAX(energy) FROM ",
        table, " WHERE date_time BETWEEN '",
        initialdt, "' AND '", lubridate::as_date(finaldt) + 1, "'"
      )
    }
    liveQuery <- function(table, finaldt) {
      paste0(
        "SELECT date_time, voltage, current, power, energy FROM ", table, " WHERE date_time < '",
        lubridate::as_date(finaldt) + 1, "' ORDER BY date_time DESC LIMIT 1"
      )
    }

    costPerKwh <- reactive({
      round(input$billCost / input$billKwh, 2)
    })
    output$costPerKwh <- renderText({
      paste0("$", costPerKwh())
    })

    meter_list <- reactive({
      # req(length(tables[input$group][[1]]) > 0)
      days <- c()
      meas <- c()
      cost <- c()
      for (t in tables[input$group][[1]]) {
        print(maxQuery(t, input$date_range[1], input$date_range[2]))
        print(finalQuery(t, input$date_range[2]))
        initial <- dbGetQuery(conn, initialQuery(t, input$date_range[1]))
        max <- dbGetQuery(conn, maxQuery(t, input$date_range[1], input$date_range[2]))
        final <- dbGetQuery(conn, finalQuery(t, input$date_range[2]))
        if (final$energy < max$`MAX(energy)`) {
          meas <- c(meas, max$`MAX(energy)` - initial$energy + final$energy)
        } else {
          meas <- c(meas, final$energy - initial$energy)
        }
        days <- c(days, as.numeric(difftime(as.POSIXct(final$date_time), as.POSIXct(initial$date_time), units = "days")))
      }
      Data_Frame <- data.frame(
        Label = tables[input$group],
        kWh = meas / 1000,
        "Days Meas" = round(days, 2),
        CostEst = round(meas / 1000 * days_selected() / days * costPerKwh(), 2)
      )
    })

    live_list <- reactive({
      livetimer()
      req(length(tables[input$group][[1]]) > 0)
      date_time <- c()
      voltage <- c()
      current <- c()
      power <- c()
      energy <- c()
      for (t in tables[input$group][[1]]) {
        live <- dbGetQuery(conn, liveQuery(t, Sys.Date()))
        date_time <- c(date_time, live$date_time)
        voltage <- c(voltage, live$voltage)
        current <- c(current, live$current)
        power <- c(power, live$power)
        energy <- c(energy, live$energy)
      }
      Data_Frame <- data.frame(
        Label = tables[input$group],
        LastMeasurement = date_time,
        Current = current,
        Power = power / 1000,
        Energy = energy / 1000,
        Voltage = voltage
      )
    })

    total_energy <- reactive({
      sum(unlist(meter_list()["kWh"]))
    })
    days_selected <- reactive({
      1 + lubridate::as_date(input$date_range[2]) - lubridate::as_date(input$date_range[1])
    })

    output$totalEnergy <- renderText({
      paste0(total_energy(), "kWh")
    })
    output$daysSelected <- renderText({
      days_selected()
    })

    groupTable <- function(group, table_num) {
      tables[paste0("pzem", group)][[1]][table_num]
    }

    # Group DT
    output$tbl <- renderDT(
      meter_list(),
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
    output$liveTable <- renderDT(
      live_list(),
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
      return(dbGetQuery(conn, query))
    })

    # Plotting timeseries data
    output$plot <- renderPlotly({
      # req(nrow(data_query) > 0 )
      p <- plot_ly(data_query(),
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
          paper_bgcolor = "rgba(0, 0, 0, 0)",
          fig_bgcolor = "rgba(0, 0, 0, 0)"
        ) %>%
        layout(font = list(family = "Roboto, sans-serif", color = "#FFFFFF")) %>%
        layout(yaxis = list(tickcolor = "#FFFFF")) %>%
        layout(xaxis = list(
          title = list(text = "Datetime"),
          type = "date",
          # tickformat="%Y-%m-%d %H:%M:%S",
          tickcolor = "#FFFFF",
          tickangle = -90,
          font = list(family = "sans serif", size = 8, color = "white"),
          gridcolor = toRGB("gray50"), gridwidth = 1,
          linecolor = toRGB("white"),
          linewidth = 2
        ))
    })

    # Disconnect from database when app is closed
    session$onSessionEnded(function() {
      dbDisconnect(conn)
    })
  })
}
