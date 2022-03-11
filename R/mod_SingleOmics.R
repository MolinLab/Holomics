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
    fluidRow(
      selectInput(ns("dataset"), "Select dataset:", 
                  choices = c("Transcriptomic"= "t", "Metabolomic"= "me", "Microbiomic" = "mi"))
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
mod_SingleOmics_server <- function(id, dataset){
  moduleServer(id, function(input, output, session){
    observeEvent(input$dataset, {
      if(input$dataset == "t"){
        dataset$selection <- Holomics::data.transcriptomic
      } else if (input$dataset == "me"){
        dataset$selection <- Holomics::data.metabolites
      } else if (input$dataset == "mi"){
        dataset$selection <- Holomics::data.microbiomic
      }
    })
  })
}



