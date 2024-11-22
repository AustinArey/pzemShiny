apptheme <- bs_theme(
  version = 5,
  font_scale = 0.75 # 1 #0.75
)

# Define Group UI
Group_UI <- function(id) {
  ns <- NS(id)
  div(
    id = "group",
    fluidRow(
      column(2, h4("Group kWh measured:")),
      column(2, textOutput(ns("totalEnergy"))),
    ), fluidRow(
      column(2, h4("Number of days selected:")),
      column(2, textOutput(ns("daysSelected"))),
    ), fluidRow(
      column(2, numericInput(ns("billCost"), label = "Bill Total $", value = 130.86)),
      column(2, numericInput(ns("billKwh"), label = "Bill Total kWh", value = 396)),
    ), fluidRow(
      column(2, h4("= $/Kwh")),
      column(2, textOutput(ns("costPerKwh"))),
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
LiveStats_UI <- function(id) {
  ns <- NS(id)
  div(
    id = "live",
    fluidRow(
      column(6, div(
        id = "datatable", class = "table", DTOutput(ns("liveTable")),
        style = "font-size:100%;color:white;background-color:lightgrey;"
      ))
    )
  )
}

# Define Meter UI
Meter_UI <- function(id) {
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
      id = ns("tabsmain"),
      tabPanel("Group Analysis", Group_UI(id)),
      tabPanel("Live Stats", LiveStats_UI(id)),
      tabPanel("Meter Charting", Meter_UI(id)),
    ),
  ) # div root
}
