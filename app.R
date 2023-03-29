
  # MySQL connection
  # mysql_host = 'deafdogranch.ddns.net'; '10.0.0.50'
  # mysql_database = 'schema1'
  # mysql_user = 'terrypi'; 'pi'
  # mysql_pw = 'PaintRobot1!'; 'Repair1'

library(shiny)
library(plotly)
library(RMySQL)
library(lubridate)

source("myConfig.R")

# Define UI
ui <- fluidPage(
  titlePanel("MySQL Shiny App"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("group", "Group", group_list),
      dateRangeInput("date_range", "Date Range", start = Sys.Date() - 30, end = Sys.Date()),
      uiOutput("table_ui"),
      selectInput("data", "Data", c("power", "current")),
      width = 3),
    
    mainPanel(
      fluidRow(
        column(4,h4("kWh used on selected meter:")),
        column(2,verbatimTextOutput("energyUsed")),
      ),fluidRow(
        column(4,h4("Total kWh used on all meters in group:")),
        column(2,verbatimTextOutput("totalEnergy")),
      ),fluidRow(
        column(2,uiOutput("meter1_text")),
        column(2,p("kWh used")),
        column(1,uiOutput("meter1_kWh")),
        column(2,p("percent used")),
        column(1,uiOutput("meter1_percent")),
        column(2,p("Est. $ used")),
        column(1,uiOutput("meter1_cost")),
      ),fluidRow(
        column(2,uiOutput("meter2_text")),
        column(2,p("kWh used")),
        column(1,uiOutput("meter2_kWh")),
        column(2,p("percent used")),
        column(1,uiOutput("meter2_percent")),
        column(2,p("Est. $ used")),
        column(1,uiOutput("meter2_cost")),
      ),fluidRow(
        column(2,uiOutput("meter3_text")),
        column(2,p("kWh used")),
        column(1,uiOutput("meter3_kWh")),
        column(2,p("percent used")),
        column(1,uiOutput("meter3_percent")),
        column(2,p("Est. $ used")),
        column(1,uiOutput("meter3_cost")),
    ),
      plotlyOutput("plot")
    )
  )
)

# Define server
server <- function(input, output, session) {
  
  # Table dropdown UI
  output$table_ui <- renderUI({
    selectInput("table", "Table", tables[paste0('pzem',input$group)][[1]])
  })
  
  # MySQL connection
  conn <- dbConnect(MySQL(),
                    host=config_host,
                    user=config_user,
                    password=config_password,
                    dbname=config_dbname)
  
  # Query to retrieve timeseries data
  data_query <- reactive({
    query <- paste0("SELECT datetime, ", input$data, " FROM ", 
                    input$table, " WHERE datetime BETWEEN '", 
                    input$date_range[1], "' AND '", lubridate::as_date(input$date_range[2])+1, "'")
    print(query)
    return(dbGetQuery(conn, query))
  })
  
  # Plotting timeseries data
  output$plot <- renderPlotly({
    #req(nrow(data_query) > 0 )
    plot_ly(data_query(), x = ~datetime, y = ~get(input$data), type = 'scatter', mode = 'lines', name = input$data) %>%
      layout(title = paste("Time Series of", input$data, "for Table:", input$table),
             xaxis = list(title = "Datetime"),
             yaxis = list(title = input$data))
  })
  
  energy_used <- function(table, initialdt, finaldt) {
    print(paste("getting energy used for table",table))
    initialQuery <- paste0("SELECT energy FROM ", table, " WHERE datetime > '", initialdt, "' ORDER BY datetime ASC LIMIT 1")
    initialEnergy <- dbGetQuery(conn, initialQuery)
    finalQuery <- paste0("SELECT energy FROM ", table, " WHERE datetime < '", finaldt, "' ORDER BY datetime DESC LIMIT 1")
    finalEnergy <- dbGetQuery(conn, finalQuery)
    return((finalEnergy[1] - initialEnergy[1])/1000)
  }
  
  # Calculate and display total energy
  output$energyUsed <- renderText({
    energy_used(input$table, input$date_range[1], input$date_range[2])[[1]]
  })
  
  total_energy <- reactive({
    print("gettting total energy")
    #print(length(tables[paste0('pzem',input$group)][[1]]))
    req(length(tables[paste0('pzem',input$group)][[1]]) > 0)
    total <- 0
    for (t in tables[paste0('pzem',input$group)][[1]]) {
      print(t)
      total <- total + energy_used(t, input$date_range[1], input$date_range[2])[[1]]
    }
    total
  })
  output$totalEnergy <- renderText({
    total_energy()
  })
  
  groupTable <- function(group, table_num) {
    t <- tables[paste0('pzem',group)][[1]][table_num]
  }
  
  # Meter1 UI
  output$meter1_text <- renderText({ groupTable(input$group, 1)  })
  output$meter1_kWh <- renderUI({ energy_used(groupTable(input$group, 1), input$date_range[1], input$date_range[2])[[1]] })
  output$meter1_percent <- renderUI({ 
    u <- energy_used(groupTable(input$group, 1), input$date_range[1], input$date_range[2])[[1]] 
    round(u/total_energy()*100,1)
  })
  output$meter1_cost <- renderUI({ 
    u <- energy_used(groupTable(input$group, 1), input$date_range[1], input$date_range[2])[[1]] 
    round(u*0.35,2)
  })
  # Meter2 UI
  output$meter2_text <- renderText({ groupTable(input$group, 2)  })
  output$meter2_kWh <- renderUI({ energy_used(groupTable(input$group, 2), input$date_range[1], input$date_range[2])[[1]] })
  output$meter2_percent <- renderUI({ 
    u <- energy_used(groupTable(input$group, 2), input$date_range[1], input$date_range[2])[[1]] 
    round(u/total_energy()*100,1)
  })
  output$meter2_cost <- renderUI({ 
    u <- energy_used(groupTable(input$group, 2), input$date_range[1], input$date_range[2])[[1]] 
    round(u*0.35,2)
  })
  # Meter3 UI
  output$meter3_text <- renderText({ groupTable(input$group, 3)  })
  output$meter3_kWh <- renderUI({ 
    req(groupTable(input$group, 3))
    energy_used(groupTable(input$group, 3), input$date_range[1], input$date_range[2])[[1]] })
  output$meter3_percent <- renderUI({ 
    req(groupTable(input$group, 3))
    u <- energy_used(groupTable(input$group, 3), input$date_range[1], input$date_range[2])[[1]] 
    round(u/total_energy()*100,1)
  })
  output$meter3_cost <- renderUI({ 
    req(groupTable(input$group, 3))
    u <- energy_used(groupTable(input$group, 3), input$date_range[1], input$date_range[2])[[1]] 
    round(u*0.35,2)
  })
  
  # Disconnect from database when app is closed
  session$onSessionEnded(function() {
    dbDisconnect(conn)
  })
}

# Run the app
shinyApp(ui = ui, server = server)

