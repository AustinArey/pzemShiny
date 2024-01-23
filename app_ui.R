apptheme <- bs_theme(
  version = 5,
  font_scale = 0.75 # 1 #0.75
)

# Apply Plotly Dark Theme for Dashboards
plotly_dark <- function(p) {
  a <- list(
    tickcolor = "#FFFFF"
  )
  pal <- c("white", "yellow")
  p %>%
    layout(
      plot_bgcolor = "rgba(0, 0, 0, 0)",
      paper_bgcolor = "rgba(0, 0, 0, 0)",
      fig_bgcolor = "rgba(0, 0, 0, 0)"
    ) %>%
    layout(font = list(family = "Roboto, sans-serif", color = "#FFFFFF")) %>%
    layout(xaxis = a, yaxis = a)
}

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
      # tabPanel("Run View", runview_ui(id)), #Run View tabsetPanel
      # tabPanel("Run History", runhist_ui(id)), #Run History tabsetPanel
      # tabPanel("Terry's Meters",tags$body(a(href="/?group=terry",class="link"))),
      tabPanel("Group Analysis", Group_UI(id)),
      tabPanel("Live Stats", LiveStats_UI(id)),
      tabPanel("Meter Charting", Meter_UI(id)),
    ),
  ) # div root
}

pressure_UI <- function(id) {
  ns <- NS(id)
  uiOutput(ns("charts"))
}
