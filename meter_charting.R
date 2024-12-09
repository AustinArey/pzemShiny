# Define Meter UI
meter_charting_ui <- function(id) {
    ns <- NS(id)
    fluidPage(
        div(
            class = "outer-container",
            div(
                class = "dropdowns-container",
                uiOutput(ns("table_ui")), # Reactive table dropdown
                selectInput(
                    ns("selected_column"), "Data",
                    c("power", "current", "voltage", "energy")
                )
            ),
            div(
                class = "chart-container",
                plotlyOutput(ns("plot")) %>% withSpinner(color = "#0dc5c1")
            )
        )
    )
}

meter_charting_mod <- function(id, conn, tables, group, date_range) {
    moduleServer(id, function(input, output, session) {
        ns <- NS(id)

        # Table dropdown UI
        output$table_ui <- renderUI({
            selectInput(ns("table"), "Table", tables[[group()]])
        })

        # Query to retrieve timeseries data
        time_series_query <- reactive({
            req(input$table != "")
            paste0(
                "SELECT date_time, ", input$selected_column, " FROM ",
                input$table, " WHERE date_time BETWEEN '",
                date_range()[1], "' AND '",
                lubridate::as_date(date_range()[2]) + 1, "'"
            )
        })

        # Data from query
        time_series_data <- reactive({
            print(time_series_query())
            tryCatch(
                {
                    data <- dbGetQuery(conn, time_series_query())
                    if (nrow(data) == 0) {
                        return(data.frame(date_time = NA, value = NA))
                    }
                    data
                },
                error = function(e) {
                    warning(e)
                    return(NULL)
                }
            )
        })

        # Render UI for plot or error message
        output$plot_ui <- renderUI({
            req(time_series_data())
            data <- time_series_data()
            if (is.null(data) || nrow(data) == 0 || is.na(data$date_time[1])) {
                div(
                    style = "color: red; font-size: 20px; text-align: center; margin-top: 20px;",
                    if (!is.null(data$error)) {
                        paste("Error: ", data$error)
                    } else {
                        "No data found for the selected date range"
                    }
                )
            } else {
                plotlyOutput(ns("plot")) %>% withSpinner(color = "#0dc5c1")
            }
        })

        # Plotting timeseries data
        output$plot <- renderPlotly({
            data <- time_series_data()
            req(!is.null(data) && nrow(data) > 0)
            plot_ly(data,
                x = ~date_time, y = ~ get(input$selected_column),
                type = "scatter", mode = "lines", name = input$selected_column
            ) %>%
                layout(
                    title = paste("Time Series of", input$selected_column, "for Table:", input[[ns("table")]]),
                    xaxis = list(title = "Datetime"),
                    yaxis = list(title = input$selected_column),
                    plot_bgcolor = "rgba(0, 0, 0, 0)",
                    paper_bgcolor = "rgba(0, 0, 0, 0)"
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
    })
}
