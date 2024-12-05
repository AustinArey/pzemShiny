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

# Define corresponding server module:
group_analysis_mod <- function(id, conn, tables, group, date_range) {
    moduleServer(id, function(input, output, session) {
        # Accessing global `group` and `date_range` reactively
        selected_group <- reactive({
            group()
        })
        selected_date_range <- reactive({
            date_range()
        })

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

        observe({
            print("group_analysis_mod")
            print(session$ns(""))
            print(selected_date_range()[1])
            max <- dbGetQuery(conn, max_query("unitA", selected_date_range()[1], selected_date_range()[2]))
            print(max)
        })

        meter_df <- reactive({
            # req(length(tables[group()][[1]]) > 0)

            # Helper function to calculate measurements
            calculate_measurements <- function(table, date_range) {
                initial <- dbGetQuery(conn, initial_query(table, date_range[1]))
                max <- dbGetQuery(conn, max_query(table, date_range[1], date_range[2]))
                final <- dbGetQuery(conn, final_query(table, date_range[2]))

                if (nrow(initial) == 0 || nrow(max) == 0 || nrow(final) == 0) {
                    return(list(meas = NA, days = NA))
                }

                if (final$energy < max$`MAX(energy)`) {
                    meas <- max$`MAX(energy)` - initial$energy + final$energy
                } else {
                    meas <- final$energy - initial$energy
                }

                days <- as.numeric(difftime(as.POSIXct(final$date_time), as.POSIXct(initial$date_time), units = "days"))

                list(meas = meas, days = days)
            }

            # Calculate measurements for each table
            measurements <- lapply(tables[[group()]], calculate_measurements, date_range = date_range())

            # Extract measurements and days
            meas <- sapply(measurements, `[[`, "meas")
            days <- sapply(measurements, `[[`, "days")

            # Create data frame
            data.frame(
                Label = tables[[group()]],
                kWh = meas / 1000,
                "Days Meas" = round(days, 2),
                CostEst = round(meas / 1000 * days_selected() / days * cost_per_kwh(), 2)
            )
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
