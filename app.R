library(shiny)
library(plotly)
library(RMySQL)
library(lubridate)
library(shinyjs)
library(shinythemes)
library(shinyWidgets)
library(bslib)
library(shinycssloaders)
library(DT)
# library(shinymanager)

source("app_server.R")
source("app_ui.R")
source("router.R")
source("myConfig.R")

# Main User Interface
ui <- function(req) {
  fluidPage(
    theme = shinytheme("darkly"),
    # Custom CSS
    tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "dashboard.css")),
    shinyjs::useShinyjs(),
    routerUI("router")
  )
}
# Wrap your UI with secure_app to use authentication
# ui <- secure_app(ui)

server <- function(input, output, session) {
  query <- reactive({
    parseQueryString(session$clientData$url_search)
  })

  observe({
    print("server")
    print(input)
  })

  # Set session to auto reconnect to server if connection is lost
  session$allowReconnect("force")

  routerMOD("router", query = query) # updated
}

# Run the app
shinyApp(ui = ui, server = server, options = list(host = "0.0.0.0", port = 8000))
