
  # MySQL connection
  # mysql_host = 'deafdogranch.ddns.net'; '10.0.0.50'
  # mysql_database = 'schema1'
  # mysql_user = 'terrypi'; 'pi'
  # mysql_pw = 'PaintRobot1!'; 'Repair1'

library(shiny)
library(plotly)
library(RMySQL)

# Define UI
ui <- fluidPage(
  titlePanel("MySQL Shiny App"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("group", "Group", c("Terry's Meters", "Austin's Meters")),
      uiOutput("table_ui"),
      dateRangeInput("date_range", "Date Range", start = Sys.Date() - 30, end = Sys.Date()),
      selectInput("data", "Data", c("power", "current"))
    ),
    
    mainPanel(
      plotlyOutput("plot")
    )
  )
)

# Define server
server <- function(input, output) {
  
  # Table dropdown UI
  output$table_ui <- renderUI({
    if (input$group == "Terry's Meters") {
      selectInput("table", "Table", c("unitA", "unitB", "unitC"))
    } else {
      selectInput("table", "Table", c("pzem_trailer", "pzem_box", "pzem_cottage", "pzem_hottub"))
    }
  })
  
  # MySQL connection
  conn <- dbConnect(MySQL(),
                    host='10.0.0.50',
                    user='pi',
                    password='Repair1',
                    dbname='schema1')
  
  # Query to retrieve data
  data_query <- reactive({
    query <- paste0("SELECT datetime, ", input$data, " FROM ", input$table, " WHERE datetime BETWEEN '", input$date_range[1], "' AND '", input$date_range[2], "'")
    return(dbGetQuery(conn, query))
  })
  
  # Plotting
  output$plot <- renderPlotly({
    #req(nrow(data_query) > 0 )
    plot_ly(data_query(), x = ~datetime, y = ~get(input$data), type = 'scatter', mode = 'lines', name = input$data) %>%
      layout(title = paste("Time Series of", input$data, "for Table:", input$table),
             xaxis = list(title = "Datetime"),
             yaxis = list(title = input$data))
  })
}

# Run the app
shinyApp(ui = ui, server = server)

