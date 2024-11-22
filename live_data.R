# Live Data UI #
live_data_ui <- function(id) {
    ns <- NS(id)
    div(
        id = "live",
        fluidRow(
            column(6, div(
                id = "datatable", class = "table", DTOutput(ns("live_table")),
                style = "font-size:100%;color:white;background-color:lightgrey;"
            ))
        )
    )
}

# Live Data Module #
live_data_mod <- function(id, conn, group) {
    moduleServer(id, function(input, output, session) {
        # Accessing global `group` reactively
        selected_group <- reactive({
            group()
        })
        # Set base timer interval for live data refresh interval
        live_timer <- reactiveTimer(10000) # 10 sec

        live_query <- function(table, final_dt) {
            paste0(
                "SELECT date_time, voltage, current, power, energy FROM ", table, " WHERE date_time < '",
                lubridate::as_date(final_dt) + 1, "' ORDER BY date_time DESC LIMIT 1"
            )
        }

        live_df <- reactive({
            live_timer()
            req(length(tables[[group()]]) > 0)

            # Helper function to retrieve live data for a table
            get_live_table_data <- function(table) {
                live_table_data <- dbGetQuery(conn, live_query(table, Sys.Date()))
                if (nrow(live_table_data) == 0) {
                    return(list(
                        date_time = NA,
                        voltage = NA,
                        current = NA,
                        power = NA,
                        energy = NA
                    ))
                }
                list(
                    date_time = live_table_data$date_time,
                    voltage = live_table_data$voltage,
                    current = live_table_data$current,
                    power = live_table_data$power,
                    energy = live_table_data$energy
                )
            }

            # Retrieve live data for each table
            live_data_list <- lapply(tables[[group()]], get_live_table_data)

            # Combine live data into a data frame
            data.frame(
                Label = tables[[group()]],
                LastMeasurement = unlist(lapply(live_data_list, `[[`, "date_time")),
                Current = unlist(lapply(live_data_list, `[[`, "current")),
                Power = unlist(lapply(live_data_list, `[[`, "power")) / 1000,
                Energy = unlist(lapply(live_data_list, `[[`, "energy")) / 1000,
                Voltage = unlist(lapply(live_data_list, `[[`, "voltage"))
            )
        })

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
    })
}
