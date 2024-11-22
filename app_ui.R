app_theme <- bs_theme(
  version = 5,
  font_scale = 0.75 # 1 #0.75
)

# Define Group UI
group_ui <- function(id) {
  ns <- NS(id)
  div(
    id = "group",
    fluidRow(
      column(2, h4("Group kWh measured:")),
      column(2, textOutput(ns("total_energy"))),
    ), fluidRow(
      column(2, h4("Number of days selected:")),
      column(2, textOutput(ns("days_selected"))),
    ), fluidRow(
      column(2, numericInput(ns("bill_cost"), label = "Bill Total $", value = 130.86)),
      column(2, numericInput(ns("bill_kwh"), label = "Bill Total kWh", value = 396)),
    ), fluidRow(
      column(2, h4("= $/Kwh")),
      column(2, textOutput(ns("cost_per_kwh"))),
    ), fluidRow(
      column(6, div(
        id = "datatable", class = "table",
        DTOutput(ns("tbl")) %>% withSpinner(color = "#0dc5c1"),
        style = "font-size:100%;color:white;background-color:lightgrey;"
      ))
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
meter_ui <- function(id) {
  ns <- NS(id)
  sidebarLayout(
    sidebarPanel(
      uiOutput(ns("table_ui")),
      selectInput(ns("data"), "Data", c("power", "current", "voltage", "energy")),
      width = 3
    ),
    mainPanel(
      plotlyOutput(ns("plot")) %>% withSpinner(color = "#0dc5c1")
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
      tabPanel("Group Analysis", group_ui(id)),
      tabPanel("Live Stats", live_stats_ui(id)),
      tabPanel("Meter Charting", meter_ui(id)),
    ),
  ) # div root
}
