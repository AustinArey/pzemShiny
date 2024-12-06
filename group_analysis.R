## Analysis of group data

## Define UI module for group analysis
group_analysis_ui <- function(id) {
    ns <- NS(id)
    div(
        id = "group",
        div(
            class = "group-container",
            div(
                class = "group-label",
                "Enter bill totals to update price per kWh for cost estimating"
            ),
            div(
                class = "group-row",
                column(2, numericInput(ns("bill_cost"), label = "Bill Total $", value = 130.86)),
                column(2, numericInput(ns("bill_kwh"), label = "Bill Total kWh", value = 396))
            ),
            div(
                class = "group-row",
                column(2, h5("price per kWh:")),
                column(2, textOutput(ns("cost_per_kwh")))
            ),
        ),
        div(
            class = "group-container",
            div(class = "group-label", "Group Results:"),
            div(
                class = "group-row",
                column(2, h5("Group kWh measured:")),
                column(2, textOutput(ns("total_energy")))
            ),
            div(
                class = "group-row",
                column(2, h5("Estimated total bill cost:")),
                column(2, textOutput(ns("est_bill_total")))
            ),
        ),
        div(
            class = "group-container",
            div(class = "group-label", textOutput(ns("group_table_label"))),
            div(
                class = "group-row",
                column(6, div(
                    id = "datatable", class = "table table-container",
                    DTOutput(ns("tbl")) %>% withSpinner(color = "#0dc5c1"),
                    style = "font-size:100%; color:white; background-color:lightgrey;"
                ))
            )
        )
    )
}

# Query Functions
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

# Data Processing Functions
calc_energy_used <- function(initial_energy, max_energy, final_energy) {
    if (final_energy < max_energy) {
        energy_measured <- max_energy - initial_energy + final_energy
    } else {
        energy_measured <- final_energy - initial_energy
    }
    energy_measured
}

days_diff_num <- function(date1, date2) {
    # Convert inputs to POSIXct if they are not already
    date1 <- as.POSIXct(date1)
    date2 <- as.POSIXct(date2)

    # Check if both inputs are dates (without time)
    if (all(format(date1, "%H:%M:%S") == "00:00:00") && all(format(date2, "%H:%M:%S") == "00:00:00")) {
        # If both are dates, include the full 24 hours of the last date
        date2 <- date2 + 86400 - 1 # Add 23 hours, 59 minutes, and 59 seconds
    }

    # Calculate the difference in days
    round(as.numeric(difftime(date2, date1, units = "days")), 2)
}

calc_energy_days_measured <- function(conn, table, date_range) {
    initial <- dbGetQuery(conn, initial_query(table, date_range[1]))
    max <- dbGetQuery(conn, max_query(table, date_range[1], date_range[2]))
    final <- dbGetQuery(conn, final_query(table, date_range[2]))

    energy_measured <- calc_energy_used(initial$energy, max$`MAX(energy)`, final$energy)

    days_measured <- days_diff_num(initial$date_time, final$date_time) # exclusive

    list(table = table, energy_measured = energy_measured, days_measured = days_measured)
}

create_meter_df <- function(conn, tables, group, date_range, cost_per_kwh, days_selected) {
    measurements_list <- lapply(tables[[group]], function(table) {
        calc_energy_days_measured(conn, table, date_range)
    })
    # extract vectors from list of lists
    tables_measured <- sapply(measurements_list, `[[`, "table")
    energy_measured <- sapply(measurements_list, `[[`, "energy_measured")
    days_measured <- sapply(measurements_list, `[[`, "days_measured")

    data.frame(
        Label = tables_measured,
        kWh = energy_measured / 1000,
        DaysMeas = round(days_measured, 2),
        CostEst = round(energy_measured / 1000 * days_selected / days_measured * cost_per_kwh, 2)
    )
}

# Define corresponding server module:
group_analysis_mod <- function(id, conn, tables, group, date_range) {
    moduleServer(id, function(input, output, session) {
        observe({
            print("group_analysis_mod")
            print(session$ns(""))
            print(date_range()[1])
            max <- dbGetQuery(conn, max_query("unitA", date_range()[1], date_range()[2]))
            print(max)
        })

        cost_per_kwh <- reactive({
            round(input$bill_cost / input$bill_kwh, 2)
        })

        days_selected <- reactive({
            1 + lubridate::as_date(date_range()[2]) - lubridate::as_date(date_range()[1])
        })

        meter_df <- reactive({
            create_meter_df(conn, tables, group(), date_range(), cost_per_kwh(), days_selected())
        })

        cost_per_kwh <- reactive({
            round(input$bill_cost / input$bill_kwh, 2)
        })
        output$cost_per_kwh <- renderText({
            paste0("$", cost_per_kwh())
        })
        total_energy <- reactive({
            sum(unlist(meter_df()["kWh"]), na.rm = TRUE)
        })
        days_selected <- reactive({
            1 + lubridate::as_date(date_range()[2]) - lubridate::as_date(date_range()[1])
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
    })
}
