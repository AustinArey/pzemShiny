
  # MySQL connection
  # mysql_host = 'deafdogranch.ddns.net'; '10.0.0.50'
  # mysql_database = 'schema1'
  # mysql_user = 'terrypi'; 'pi'
  # mysql_pw = 'PaintRobot1!'; 'Repair1'

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
library(shinymanager)

source('app_server.R')
source('app_ui.R')
source('router.R')
source('myConfig.R')


#Main User Interface
ui <- function(req) { 
  fluidPage( theme = shinytheme("darkly"),
             # Script
             #tags$script(HTML()),
             #Custom CSS
             tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "dashboard.css")),
             shinyjs::useShinyjs(),
             #Page Header
             
             routerUI('router') #replacing shiny.router working
  )
}
# Wrap your UI with secure_app
ui <- secure_app(ui)

server <- function(input, output, session) {
  #check_credentials returns a function to authenticate users
  res_auth <- secure_server(
    check_credentials = check_credentials(credentials)
  )
  #in place of shiny.router
  query <- reactive({ parseQueryString(session$clientData$url_search) })
  
  observe({ print("server")
    print(input) 
    })
  
  #Set session to auto reconnect to server if connection is lost
  session$allowReconnect("force")
  
  callModule(routerMOD,'router',query=query) #working
}

# Run the app
shinyApp(ui = ui, server = server, options = list(host = '0.0.0.0', port = 8000))

