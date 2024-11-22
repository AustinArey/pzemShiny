app_theme <- bs_theme(
  version = 5,
  font_scale = 0.75 # 1 #0.75
)

# Define Group UI
group_analysis_ui <- function(id) {
  ns <- NS(id)
  div(
    id = "group",
    tags$head(
      tags$style(HTML("
        .group-row {
          display: flex;
          align-items: center;
          margin-bottom: 10px;
        }
        .group-row h5, .group-row .shiny-text-output {
          margin: 0 10px;
        }
        .group-row .shiny-input-container {
          margin: 0 10px;
        }
        .group-row .shiny-input-container input {
          width: 100px;
        }
        .group-row .shiny-text-output {
          min-width: 100px;
        }
        .group-label {
          font-weight: bold;
          margin-bottom: 10px;
          margin-top: 20px;
        }
        .table-container {
          width: 100%;
          max-width: 600px;
          margin: 0 10px;
        }
      "))
    ),
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

# Live UI #
live_stats_ui <- function(id) {
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

# Define Meter UI
meter_charting_ui <- function(id) {
  ns <- NS(id)
  fluidPage(
    div(
      class = "dropdowns-container",
      style = "margin-top: 20px;",
      fluidRow(
        column(
          4,
          uiOutput(ns("table_ui"))
        ), # tables in database is reactive
        column(
          4,
          selectInput(
            ns("data"), "Data",
            c("power", "current", "voltage", "energy")
          )
        )
      )
    ),
    div(
      class = "chart-container",
      fluidRow(
        column(
          8,
          plotlyOutput(ns("plot")) %>% withSpinner(color = "#0dc5c1")
        )
      )
    )
  )
}

# function(id) { ns <- NS(id)
root_ui <- function(id) {
  ns <- NS(id)
  div(
    id = "root", class = "root",
    fluidRow(
      column(6, h3("Electrical Meter Shiny App v0.1"))
    ),
    fluidRow(
      column(3, selectInput(ns("group"), "Group", names(tables))),
      column(3, dateRangeInput(ns("date_range"), "Date Range", start = Sys.Date() - 30, end = Sys.Date())),
    ),
    tabsetPanel(
      id = ns("tabs_main"),
      tabPanel("Group Analysis", group_analysis_ui(id)),
      tabPanel("Live Stats", live_stats_ui(id)),
      tabPanel("Meter Charting", meter_charting_ui(id)),
    ),
  ) # div root
}
