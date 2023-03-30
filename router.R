############
# Router for ui (shiny.router would load all modules regardless of route)
#This is a work around via renderUI and callModule based on query '?machine=bx'
###########

routerUI <- function(id) {
  ns <- NS(id)
  fluidRow(
    column(12,uiOutput(ns('ui')))
  )
  
}
#function(id) {ns <- NS(id)
routerMOD <- function(input, output, session, query) { 
  observe({ 
    print("routerMOD")
    print(session$ns("")) 
  })
  
  observe({
    print("query")
    print(query())
    if(length(query())>0){
      if(query()$group == 'terry'){
        print("show terry")
        output$ui <- renderUI({  Meter_UI(session$ns('terry')) })
        callModule(Meter_MOD,'terry')
      }else if(query()$group == '580'){
        print("show 580")
        output$ui <- renderUI({  Meter_UI(session$ns('580')) })
        callModule(Meter_MOD,'582')
      }else if(query()$group == 'b6'){
        print("show 582")
        output$ui <- renderUI({  Meter_UI(session$ns('582')) })
        callModule(Meter_MOD,'582')
      }
    }else{
      print("show root")
      output$ui <- renderUI({  root_ui(session$ns('root')) })
      callModule(root_MOD,'root')
      #output$ui <- renderUI({  Group_UI(session$ns('root')) })
      #callModule(Group_MOD,'root')
    }
  })
  
  
}
