############
# Router for ui (shiny.router would load all modules regardless of route)
# This is a work around via renderUI and callModule based on query '?machine=bx'
###########

router_ui <- function(id) {
  ns <- NS(id)
  fluidRow(
    column(12, uiOutput(ns("ui")))
  )
}

router_mod <- function(id, query) {
  moduleServer(id, function(input, output, session) {
    observe({
      print("routerMOD")
      print(session$ns(""))
    })

    observe({
      print("query")
      print(query())
      # if(length(query())>0){
      #   if(query()$group == 'terry'){
      #     print("show terry")
      #     output$ui <- renderUI({  meter_charting_ui(session$ns('terry')) })
      #     callModule(Meter_MOD,'terry')
      #   }else if(query()$group == '580'){
      #     print("show 580")
      #     output$ui <- renderUI({  meter_charting_ui(session$ns('580')) })
      #     callModule(Meter_MOD,'582')
      #   }else if(query()$group == 'b6'){
      #     print("show 582")
      #     output$ui <- renderUI({  meter_charting_ui(session$ns('582')) })
      #     callModule(Meter_MOD,'582')
      #   }
      # }else{
      print("show root")
      output$ui <- renderUI({
        root_ui(session$ns("root"))
      })
      root_mod("root")
      # output$ui <- renderUI({  group_analysis_ui(session$ns('root')) })
      # callModule(Group_MOD,'root')
      # }
    })
  })
}
