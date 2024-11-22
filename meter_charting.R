# Define Meter UI
meter_charting_ui <- function(id) {
    ns <- NS(id)
    fluidPage(
        tags$head(
            tags$style(HTML("
        .outer-container {
          max-width: 66%; /* Limit the width to 2/3 of the screen */
          margin-left: 0; /* Align the container to the left */
          margin-right: auto; /* Keep content responsive */
        }
        .dropdowns-container {
          display: flex;
          gap: 20px; /* Add spacing between dropdowns */
          margin-top: 20px;
          align-items: center;
        }
        .chart-container {
          margin-top: 30px;
        }
      "))
        ),
        # Outer container wrapping both dropdowns and the chart
        div(
            class = "outer-container",
            # Dropdowns container
            div(
                class = "dropdowns-container",
                uiOutput(ns("table_ui")), # Reactive table dropdown
                selectInput(
                    ns("data"), "Data",
                    c("power", "current", "voltage", "energy")
                )
            ),
            # Chart container
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
        # Accessing global `group` and `date_range` reactively
        selected_group <- reactive({
            group()
        })
        selected_date_range <- reactive({
            date_range()
        })

        # Table dropdown UI
        output$table_ui <- renderUI({
            selectInput(ns("table"), "Table", tables[[selected_group()]][[1]])
        })

        # Query to retrieve timeseries data
        data_query <- reactive({
            req(input$table != "")
            query <- paste0(
                "SELECT date_time, ", input$data, " FROM ",
                input$table, " WHERE date_time BETWEEN '",
                selected_date_range()[1], "' AND '", lubridate::as_date(selected_date_range()[2]) + 1, "'"
            )
            print(query)
            data <- dbGetQuery(conn, query)
            if (nrow(data) == 0) {
                return(data.frame(date_time = NA, value = NA))
            }
            data
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
