##Meter_MOD##
# Define Meter MOD
Group_MOD <- function(input, output, session, conn, group, date1, date2) {
  ns <- session$ns
  
  observe({ 
    print("Meter_MOD")
    print(session$ns("")) 
  })
  
  
  energy_used <- function(table, initialdt, finaldt) {
    print(paste("getting energy used for table",table))
    print("getting initial energy")
    initialQuery <- paste0("SELECT energy FROM ", table, " WHERE datetime > '", initialdt, "' ORDER BY datetime ASC LIMIT 1")
    print(initialQuery)
    initialEnergy <- dbGetQuery(conn, initialQuery)$energy
    print("getting final energy")
    finalQuery <- paste0("SELECT energy FROM ", table, " WHERE datetime < '", lubridate::as_date(finaldt)+1, "' ORDER BY datetime DESC LIMIT 1")
    print(finalQuery)
    finalEnergy <- dbGetQuery(conn, finalQuery)$energy
    print(finalEnergy)
    return((finalEnergy[1] - initialEnergy[1])/1000)
  }
  
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

# Define Meter MOD
Meter_MOD <- function(input, output, session, group) {
  ns <- session$ns
  
  observe({ 
    print("Meter_MOD")
    print(session$ns("")) 
  })
  
  # Table dropdown UI
  output$table_ui <- renderUI({
    selectInput(ns("table"), "Table", tables[paste0('pzem', input$group)][[1]])
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
    print("getting initial energy")
    initialQuery <- paste0("SELECT energy FROM ", table, " WHERE datetime > '", initialdt, "' ORDER BY datetime ASC LIMIT 1")
    print(initialQuery)
    initialEnergy <- dbGetQuery(conn, initialQuery)
    print("getting final energy")
    finalQuery <- paste0("SELECT energy FROM ", table, " WHERE datetime < '", lubridate::as_date(finaldt)+1, "' ORDER BY datetime DESC LIMIT 1")
    print(finalQuery)
    finalEnergy <- dbGetQuery(conn, finalQuery)
    print(finalEnergy)
    return((finalEnergy[1] - initialEnergy[1])/1000)
  }
  
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

root_MOD <- function(input, output, session) {
  ns <- session$ns
  
  observe({ 
    print("root_MOD")
    print(session$ns("")) 
  })
  
  #Set base timer interval for dashboard refresh interval
  timer <- reactiveTimer(900000) #900 sec = 15 min
  livetimer <- reactiveTimer(10000) #10 sec
  
  # MySQL connection
  conn <- dbConnect(MySQL(),
                    host=config_host,
                    user=config_user,
                    password=config_password,
                    dbname=config_dbname)
  
  ## GROUP ##
  initialQuery <- function(table, initialdt){
    paste0("SELECT energy, datetime FROM ", table, " WHERE datetime > '", 
           initialdt, "' ORDER BY datetime ASC LIMIT 1")
  }
  finalQuery <- function(table, finaldt){
    paste0("SELECT energy, datetime FROM ", table, " WHERE datetime < '", 
           lubridate::as_date(finaldt)+1, "' ORDER BY datetime DESC LIMIT 1")
  }
  liveQuery <- function(table, finaldt){
    paste0("SELECT power, datetime FROM ", table, " WHERE datetime < '", 
           lubridate::as_date(finaldt)+1, "' ORDER BY datetime DESC LIMIT 1")
  }
    
  meter_list <- reactive({
    req(length(tables[paste0('pzem',input$group)][[1]]) > 0)
    days <- c()
    meas <- c()
    cost <- c()
    for (t in tables[paste0('pzem',input$group)][[1]]) {
      i <- dbGetQuery(conn, initialQuery(t,input$date_range[1]))
      f <- dbGetQuery(conn, finalQuery(t,input$date_range[2]))
      meas <- c(meas, f$energy - i$energy)
      days <- c(days, as.numeric(lubridate::as_date(f$datetime)-lubridate::as_date(i$datetime)) )
    }
    Data_Frame <- data.frame (
      Label = tables[paste0('pzem',input$group)],
      kWh = meas/1000,
      Days = days,
      CostEst = round(meas/1000*days_selected()/days*0.35, 2)
    )
  })
  
  live_list <- reactive({
    livetimer()
    req(length(tables[paste0('pzem',input$group)][[1]]) > 0)
    power <- c()
    datetime <- c()
    for (t in tables[paste0('pzem',input$group)][[1]]) {
      live <- dbGetQuery(conn, liveQuery(t,Sys.Date()))
      power <- c(power, live$power)
      datetime <- c(datetime, live$datetime)
    }
    Data_Frame <- data.frame (
      Label = tables[paste0('pzem',input$group)],
      Power = power/1000,
      LastMeasurement = datetime
    )
  })
  
  total_energy <- reactive({ sum(unlist(meter_list()['kWh']))  })
  days_selected <- reactive({ lubridate::as_date(input$date_range[2])-lubridate::as_date(input$date_range[1]) })
  
  output$totalEnergy <- renderText({ total_energy() })
  output$daysSelected <- renderText({ days_selected() })
  
  groupTable <- function(group, table_num) { tables[paste0('pzem',group)][[1]][table_num] }
  
  #Group DT
  output$tbl = renderDT(
    meter_list(), options = list(
      paging = FALSE, #TRUE,
      searching = FALSE,
      fixedColumns = TRUE,
      autoWidth = TRUE,
      ordering = TRUE,
      dom = 'tB',
      buttons = c('copy', 'csv', 'excel')
    )
  )
  
  #Live DT
  output$liveTable = renderDT(
    live_list(), options = list(
      paging = FALSE, #TRUE,
      searching = FALSE,
      fixedColumns = TRUE,
      autoWidth = TRUE,
      ordering = TRUE,
      dom = 'tB',
      buttons = c('copy', 'csv', 'excel')
    )
  )
  
  ## METER ##
  # Table dropdown UI
  output$table_ui <- renderUI({
    selectInput(ns("table"), "Table", tables[paste0('pzem', input$group)][[1]])
  })
  
  # Query to retrieve timeseries data
  data_query <- reactive({
    req(input$table != '')
    query <- paste0("SELECT datetime, ", input$data, " FROM ", 
                    input$table, " WHERE datetime BETWEEN '", 
                    input$date_range[1], "' AND '", lubridate::as_date(input$date_range[2])+1, "'")
    print(query)
    return(dbGetQuery(conn, query))
  })
  
  # Plotting timeseries data
  output$plot <- renderPlotly({
    #req(nrow(data_query) > 0 )
    p <- plot_ly(data_query(), x = ~datetime, y = ~get(input$data), 
                 type = 'scatter', mode = 'lines', name = input$data) %>%
      layout(title = paste("Time Series of", input$data, "for Table:", input$table),
             xaxis = list(title = "Datetime"),
             yaxis = list(title = input$data))
    p <- p %>% plotly_dark() %>% 
      layout(xaxis=list(title=list(text='Datetime'),
                            type="date", 
                            tickformat="%Y-%m-%d %H:%M:%S",
                            tickangle = -90,
                            font = list(family = "sans serif",size = 8,color = 'white'),
                            gridcolor = toRGB("gray50"),gridwidth = 1,
                            linecolor = toRGB("white"),
                            linewidth = 2))
  })
  
  # Disconnect from database when app is closed
  session$onSessionEnded(function() {
    dbDisconnect(conn)
  })
  
}

##RunView##
root_MODold <- function(input, output, session) {
  ns <- session$ns
  
  observe({ 
    print("root_MOD")
    print(session$ns("")) 
  })
  
  #Set base timer interval for dashboard refresh interval
  timer <- reactiveTimer(900000) #900 sec = 15 min
  livetimer <- reactiveTimer(30000) #30 sec
  
  #reactive logic
  # initialize beta 1
  # inputs -> reactive lista
  
  # reactive list required in reactive functions
  inputs_rv <- reactive({
    req(input$machine_rv,input$startRun_rv)
    print("updating inputs")
    inputs <- list()
    inputs$machine <- input$machine_rv
    inputs$startRun <- input$startRun_rv
    print("inputs")
    print(inputs)
    inputs
  }) 
  inputs_rv_d <- debounce(inputs_rv,1000)
  
  #Create values_rv$url reactiveValue and 
  #update from reactives from inputs_rv
  #update filters from actionButton input$button_rv
  values_rv <- reactiveValues(url='https://sarnode1/api/insql/b1?limit=25')
  observe({
    req(inputs_rv_d()$machine != '',inputs_rv_d()$startRun != 0)
    url <- paste0('https://sarnode1/api/insql/',inputs_rv_d()$machine,'?limit=30')
    url <- paste0(url,'&run=',inputs_rv_d()$startRun)
    values_rv$url <- url
  })
  observeEvent(input$button_rv,{ #Apply Filters
    req(inputs_rv_d()$machine != '',inputs_rv_d()$startRun != 0)
    print("updating filters")
    url <- paste0('https://sarnode1/api/insql/',inputs_rv_d()$machine,'?limit=30')
    url <- paste0(url,'&run=',inputs_rv_d()$startRun)
    #filters
    if(input$pn_rv != "" ){url <- paste0(url,'&pn=',input$pn_rv)}
    if(input$so_rv != "" ){url <- paste0(url,'&so=',input$so_rv)}
    url <- paste0(url, '&startdate=', input$date_rv[1] )
    url <- paste0(url, '&enddate=', input$date_rv[2] )
    print(input$date_rv)
    values_rv$url <- url
  })
  
  insqldata_rv <- reactive({
    req(inputs_rv_d()$machine != '',inputs_rv_d()$startRun != 0)
    print("populating insqldata")
    timer() # 900s=15m
    url <- values_rv$url
    print(url)
    print(inputs_rv_d()$startRun)
    data <- fromJSON(getContent(url,token))
    req(nrow(data) > 0)
    data
  })
  
  #update starting run and filters if machine changes
  observe({
    req(input$machine_rv != '')
    machine <- input$machine_rv
    if(machine == 'b1'){min <- 25400}
    if(machine == 'b4'){min <- 18100}
    if(machine == 'b6'){min <- 2650}
    if(machine == 'b9'){min <- 1}
    print("machine changed - updating starting run")
    url <- paste0('https://sarnode1/api/insql/',input$machine_rv,'?limit=25')
    data <- fromJSON(getContent(url,token))
    run <- min(data$runNumber)
    startdate <- data$pumpTime[1]
    updateNumericInput(session,"startRun_rv",label="Starting Run", value = run, min = min)
    updateTextInput(session, "pn_rv", value = "" )
    updateDateRangeInput(session, "date_rv",start = lubridate::as_date(as_datetime(startdate,tz)))
  })
  #update RunSelect dropdown with insqldata
  observe({
    print("populating Run Select RV")
    data <- insqldata_rv()
    df <- do.call(rbind,list(paste(unlist(data['_id']))))
    data_frame <- as.data.frame(df)
    colnames(data_frame) <- paste(data$runNumber,'-',
                                  data$pn,'-',data$color,
                                  '[',
                                  # format(.POSIXct(parse_date(data$pumpTime), 
                                  #                 tz = "US/Pacific"),'%A, %B %d, %Y %H:%M:%S'),
                                  as_datetime(data$pumpTime,tz),
                                  ']')
    dataframe_list <- rev(as.list(data_frame))
    #print(dataframe_list)
    updateSelectInput(session, "selectRun_rv",
                      choices = dataframe_list,
                      selected = paste(dataframe_list[1]))
  })
  
  #Run View
  machine_rv <- reactive({ 
    req(nrow(insqldata_rv()) > 0,inputs_rv_d()$machine != '')
    print(paste("selected machine: ",inputs_rv_d()$machine))
    inputs_rv_d()$machine
  })
  id <- reactive({ 
    req(nrow(insqldata_rv()) > 0, input$selectRun_rv != '')
    print(paste("selected run: ",input$selectRun_rv))
    input$selectRun_rv
  })
  
  callModule( HUDinfoMOD, "hud", plotly_dark , machine_rv, insqldata_rv)
  #runCompare toggle "toggle_rc" in hud namespace
  runCompare <- reactive({ 
    print("toggle")
    input[['hud-toggle_rc']]
  })
  
  
  callModule( LivePumpMOD, "livepump", plotly_dark , machine_rv, id, insqldata_rv, runCompare) 
  callModule( S1PowersMOD, "s1powers", plotly_dark , machine_rv, id, insqldata_rv) 
  callModule( S2PowersMOD, "s2powers", plotly_dark , machine_rv, id, insqldata_rv) 
  callModule( PCDefrostMOD, "pcdefrost", plotly_dark , machine_rv, id, insqldata_rv) 
  callModule( PCCooldownMOD, "pccooldown", plotly_dark , machine_rv, id, insqldata_rv) 
  
  #} runView -> replaced with single rootMOD
  
  ##Run History##
  #runhistMOD <- function(input, output, session) {
  inputs_rh <- reactive({
    req(input$machine_rh,input$startRun_rh,input$numRuns_rh)
    print("updating inputs")
    inputs <- list()
    inputs$machine <- input$machine_rh
    inputs$startRun <- input$startRun_rh
    inputs$numRuns <- input$numRuns_rh
    print("inputs")
    print(inputs)
    inputs
  }) 
  inputs_rh_d <- debounce(inputs_rh,1000)
  
  #Create values_rv$url reactiveValue and 
  #update from reactives from inputs_rv
  #update filters from actionButton input$button_rv
  values_rh <- reactiveValues(url='https://sarnode1/api/insql/b1?limit=25')
  observe({
    req(inputs_rh_d()$machine != '',inputs_rh_d()$startRun != 0,,inputs_rh_d()$numRuns > 0)
    url <- paste0('https://sarnode1/api/insql/',inputs_rh_d()$machine)
    url <- paste0(url,'?limit=',inputs_rh_d()$numRuns)
    url <- paste0(url,'&run=',inputs_rh_d()$startRun)
    values_rh$url <- url
  })
  observeEvent(input$button_rh,{
    req(inputs_rh_d()$machine != '',inputs_rh_d()$startRun != 0,,inputs_rh_d()$numRuns > 0)
    print("updating filters")
    url <- paste0('https://sarnode1/api/insql/',inputs_rh_d()$machine)
    url <- paste0(url,'?limit=',inputs_rh_d()$numRuns)
    url <- paste0(url,'&run=',inputs_rh_d()$startRun)
    #filters
    if(input$pn_rh != "" ){url <- paste0(url,'&pn=',input$pn_rh)}
    if(input$so_rh != "" ){url <- paste0(url,'&so=',input$so_rh)}
    url <- paste0(url, '&startdate=', input$date_rh[1] )
    url <- paste0(url, '&enddate=', input$date_rh[2] )
    values_rh$url <- url
  })
  
  insqldata_rh <- reactive({
    #req(inputs_rh_d()$machine != '',inputs_rh_d()$startRun != 0,inputs_rh_d()$numRuns > 0)
    print("populating insqldata")
    timer() # 900s=15m
    url <- values_rh$url
    print(url)
    print(inputs_rh_d()$startRun)
    data <- fromJSON(getContent(url,token))
    req(nrow(data) > 0)
    print('filtering insql')
    if(inputs_rh_d()$machine == 'b9'){
      print('b9 insql data')
      data <- data[lubridate::as_datetime(data$pumpTime)>"2023-01-25 PST",]
    }
    data
  })
  
  df <- reactive({ df_passes_avg(insqldata_rh())  })
  
  #update filter sstarting run if insqldatachanges
  observe({
    req(input$machine_rh != '')
    machine <- input$machine_rh
    if(machine == 'b1'){min <- 25400}
    if(machine == 'b4'){min <- 18100}
    if(machine == 'b6'){min <- 2650}
    print("machine changed - updating starting run")
    url <- paste0('https://sarnode1/api/insql/',input$machine_rh,'?limit=25')
    data <- fromJSON(getContent(url,token))
    run <- min(data$runNumber)
    startdate <- data$pumpTime[1]
    updateNumericInput(session,"startRun_rh",label="Starting Run", value = run, min = min)
    updateNumericInput(session,"numRuns_rh",label="Run Limit", value = 25, min = 1)
    updateTextInput(session, "pn_rh", label="Part Number", value = "" )
    updateDateRangeInput(session, "date_rh",start = lubridate::as_date(as_datetime(startdate,tz)))
  })
  
  machine_rh <- reactive({ 
    req(nrow(insqldata_rh()) > 0,inputs_rh_d()$machine != '')
    print(paste("selected machine: ",inputs_rh_d()$machine))
    inputs_rh_d()$machine
  })
  
  callModule( RunTimesMOD, "runtimes", plotly_dark , querydata = insqldata_rh) 
  callModule( BaseVacMOD, "basevac", plotly_dark , querydata = insqldata_rh) 
  callModule( CoatPressureMOD, "coatpressure", plotly_dark , querydata = insqldata_rh, df = df) 
  callModule( PolycoldMOD, "polycold", plotly_dark , querydata = insqldata_rh, df = df) 
  callModule( PolycoldDefrostMOD, "polycolddefrost", plotly_dark , querydata = insqldata_rh)
  #filtered by pn
  insqldata_rh_pn <- reactive({
    req(inputs_rh_d()$machine != '',inputs_rh_d()$startRun != 0,inputs_rh_d()$numRuns > 0)
    timer() # 900s=15m
    pn_latest <- insqldata_rh()['pn'][nrow(insqldata_rh()),]
    url <- paste0('https://sarnode1/api/insql/',inputs_rh_d()$machine)
    url <- paste0(url,'?limit=',inputs_rh_d()$numRuns)
    url <- paste0(url,'&run=',inputs_rh_d()$startRun)
    #pn from other query NOT from filter
    url <- paste0(url,'&pn=',pn_latest) #ensure url always has pn of latest from other query
    fromJSON(getContent(url,token)) })
  # output$s1pn <- renderText({
  #   pn_latest <- querydata_pn()['pn'][nrow(querydata_pn()),]
  #   paste("PN: ",pn_latest)
  # })
  callModule( Side1ShewartMOD, "s1shewart", plotly_dark , querydata = insqldata_rh_pn) 
  callModule( Side2ShewartMOD, "s2shewart", plotly_dark , querydata = insqldata_rh_pn) 
  
  #control charts from tab
  observe({
    print("tab observer")
    print(input$tabsmain)
    path <- session$clientData$url_pathname
    print(paste("url path: ", path))
    if(path == '/'){path <- ''}
    if(!is.null(input$tabsmain)){
      if(input$tabsmain == "Beta 1 CC"){
        shinyjs::runjs(paste0('window.open("',path,'/?machine=b1", "_self");'))
      }else if(input$tabsmain == "Beta 4 CC"){
        shinyjs::runjs(paste0('window.open("',path,'/?machine=b4", "_self");'))
      }else if(input$tabsmain == "Beta 6 CC"){
        shinyjs::runjs(paste0('window.open("',path,'/?machine=b6", "_self");'))
      }else if(input$tabsmain == "Beta 9 CC"){
        shinyjs::runjs(paste0('window.open("',path,'/?machine=b9", "_self");'))
      }
      
    }
    
  })
}

pressure_MOD <- function(input, output, session, plotly_dark, querydata, df){
  livetimer <- reactiveTimer(30000) #30 sec,
  callModule( CoatPressureMOD, "b6_coatpressure", plotly_dark, querydata, df)
  callModule( BaseVacMOD, "b6_basevac", plotly_dark, querydata) 
  #print(paste("pressureMOD",session$ns("name")))
  toggle_ui <- function(){
    div(id="toggle",
        "Coat Pressure TEST",
        CoatPressureUI("b6_coatpressure"), #switch w/ base pressure on timer
        "Base Pressure",
        BaseVacUI("b6_basevac"), #switch w/ coat pressure on timer
    )
  }
  
  output$charts <- renderUI({ 
    livetimer()  # 30s
    toggle_ui()
  }) 
  
}

##Control Charts##
#B1 CC
b1_MOD <- function(input, output, session) {
  observe({ 
    print("b1_MOD")
    print(session$ns("")) 
  })
  #Set base timer interval for dashboard refresh interval
  timer <- reactiveTimer(900000) #900 sec = 15 min
  livetimer <- reactiveTimer(30000) #30 sec
  #Setup reactive counter
  livecounter <- reactiveVal(0)
  observe({
    livetimer()
    newValue <- isolate(livecounter()) + 1    
    livecounter(newValue)   
  })
  b1_machine <- reactive({ 'b1' }) #for modular live charts
  b1_insqldata <- reactive({ 
    timer() # 900s=15m
    fromJSON(getContent(paste0('https://sarnode1/api/insql/','b1'),token)) 
    #insql.b1
  })
  b1_id <- reactive({ b1_insqldata()['_id'][nrow(b1_insqldata()),] }) #latest insql run id
  b1_df <- reactive({ df_passes_avg(b1_insqldata())  }) #function from utilities.R
  
  callModule( MachineInfoMOD, "b1_info", plotly_dark , machine = b1_machine, querydata = b1_insqldata)
  callModule( TAtimesMOD, "b1TAinfo", plotly_dark) #beta 1 CC
  
  callModule( RunTimesMOD, "b1_runtimes", plotly_dark , querydata = b1_insqldata)
  callModule( CoatPressureMOD, "b1_coatpressure", plotly_dark , querydata = b1_insqldata, df = b1_df) 
  callModule( BaseVacMOD, "b1_basevac", plotly_dark , querydata = b1_insqldata) 
  # observe({
  #   print(paste("b1 livecounter: ",livecounter()))
  #   if(livecounter()%%2==0){
  #     shinyjs::hide("b1_base")
  #     shinyjs::show("b1_coat")
  #   }else{
  #     shinyjs::hide("b1_coat")
  #     shinyjs::show("b1_base")
  #   }
  # })
  runCompare <- reactive({ TRUE })
  
  callModule( LivePumpMOD, "b1_livepump", plotly_dark , b1_machine, b1_id, b1_insqldata, runCompare )
  #filtered by pn
  b1_insqldata_pn <- reactive({
    timer() # 900s=15m
    pn_latest <- b1_insqldata()['pn'][nrow(b1_insqldata()),]
    fromJSON(getContent(paste0('https://sarnode1/api/insql/','b1','?pn=',pn_latest),token)) 
    #insql.b1
  })
  callModule( Side1ShewartMOD, "b1_s1shewart", plotly_dark , querydata = b1_insqldata_pn)
  callModule( Side2ShewartMOD, "b1_s2shewart", plotly_dark , querydata = b1_insqldata_pn)
  
}


