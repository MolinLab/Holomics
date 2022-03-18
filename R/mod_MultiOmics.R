#' MultiOmics UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_MultiOmics_ui <- function(id){
  ns <- NS(id)
  tagList(
    shinybusy::add_busy_spinner(spin = "circle", position = "bottom-right", height = "60px", width = "60px"),
    fluidRow(
      selectInput(ns("dataset1"), "Select first dataset:", 
                  choices = c("Transcriptomic"= "t", "Metabolomic"= "me", "Microbiomic" = "mi"), width = "150"),
      selectInput(ns("dataset2"), "Select second dataset:", 
                  choices = c("Transcriptomic"= "t", "Metabolomic"= "me", "Microbiomic" = "mi"), 
                  selected = "me", width = "fit-content"),
      style = "display: flex; gap: 1rem"
    ),
    fluidRow(
      bs4Dash::column( width = 6,
                       h1("Sparse PLS"),
                       mod_sPLS_ui("sPLS")
      ),
      bs4Dash::column( width = 6,
                       h1("DIABLO"),
                       mod_DIABLO_ui("DIABLO")
      )
    )
  )
}

#' MultiOmics Server Functions
#'
#' @noRd 
mod_MultiOmics_server <- function(id, dataset){
  moduleServer( id, function(input, output, session){
    observeEvent(input$dataset1, {
      if(input$dataset1 == "t"){
        dataset$data1 <- Holomics::data.transcriptomic
      } else if (input$dataset1 == "me"){
        dataset$data1 <- Holomics::data.metabolites
      } else if (input$dataset1 == "mi"){
        dataset$data1 <- Holomics::data.microbiomic
      }
      
    })
    
    observeEvent(input$dataset2, {
      if(input$dataset2 == "t"){
        dataset$data2 <- Holomics::data.transcriptomic
      } else if (input$dataset2 == "me"){
        dataset$data2 <- Holomics::data.metabolites
      } else if (input$dataset2 == "mi"){
        dataset$data2 <- Holomics::data.microbiomic
      }
    })
  })
}