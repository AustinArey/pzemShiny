library(shiny)
library(plotly)
library(RMySQL)
library(lubridate)
library(shinyjs)
library(shinythemes)
library(shinycssloaders)
library(DT)

source("myConfig.R")

source("group_analysis.R")
source("live_data.R")
source("meter_charting.R")

# Main User Interface
ui <- fluidPage(
  theme = shinytheme("darkly"),
  # Custom CSS
  tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "dashboard.css")),
  shinyjs::useShinyjs(),
  div(
    id = "main", class = "main",
    fluidRow(
      column(6, h3("Electrical Meter Shiny App v0.1"))
    ),
    fluidRow(
      column(3, selectInput("group", "Group", names(tables))),
      column(3, dateRangeInput("date_range", "Date Range", start = Sys.Date() - 30, end = Sys.Date())),
    ),
    tabsetPanel(
      id = "tabs_main",
      tabPanel("Group Analysis", group_analysis_ui("group_analysis")),
      tabPanel("Live Data", live_data_ui("live_data")),
      tabPanel("Meter Charting", meter_charting_ui("meter_charting")),
    ),
  )
)

server <- function(input, output, session) {
  # Set session to auto reconnect to server if connection is lost
  session$allowReconnect("force")

  observe({
    # Extract IP address
    ip <- session$clientData$url_hostname
    # Log IP address with timestamp
    log_entry <- paste(Sys.time(), " - IP Address:", ip, "\n")
    print(log_entry)
  })

  # MySQL connection
  conn <- dbConnect(MySQL(),
    host = config_host,
    user = config_user,
    password = config_password,
    dbname = config_dbname
  )

  # Disconnect from database when app is closed
  session$onSessionEnded(function() {
    dbDisconnect(conn)
  })

  group_analysis_mod(
    id = "group_analysis",
    conn = conn,
    tables = tables,
    group = reactive(input$group), # Wrap as reactive
    date_range = reactive(input$date_range) # Wrap as reactive
  )
  live_data_mod(
    id = "live_data",
    conn,
    group = reactive(input$group) # Wrap as reactive
  )
  meter_charting_mod(
    id = "meter_charting",
    conn = conn,
    tables = tables,
    group = reactive(input$group), # Wrap as reactive
    date_range = reactive(input$date_range) # Wrap as reactive
  )
}

# Run the app
shinyApp(ui = ui, server = server, options = list(host = "0.0.0.0", port = 8000, launch.browser = FALSE))
