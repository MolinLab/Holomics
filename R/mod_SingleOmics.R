#' Single omics UI Functions
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_SingleOmics_ui <- function(id){
  ns <- NS(id)
  tagList(
    shinybusy::add_busy_spinner(spin = "circle", position = "bottom-right", height = "60px", width = "60px"),
    fluidRow(
      bs4Dash::column(width = 12,
        uiOutput(ns("dataSelection"))
      )
    ),
    fluidRow(
      bs4Dash::column( width = 6,
                       h1("PCA"),
                       mod_PCA_ui("PCA")
      ),
      bs4Dash::column( width = 6,
                       h1("PLS-DA"),
                       mod_PLSDA_ui("PLSDA")
      )
    )
  )
}

#' PCA Server Functions
#'
#' @noRd 
mod_SingleOmics_server <- function(id, data, selection){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    observeEvent(data$data, {
      output$dataSelection <- renderUI({
        choices <- generateDatasetChoices(data$data)
        getDatasetComponent(ns("selection"), "Select dataset:", choices)
      })
    })
  
    observeEvent(input$selection, {
      selection$data <- data$data[[input$selection]]
    })
  })
}