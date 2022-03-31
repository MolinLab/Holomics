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
      bs4Dash::column(width = 6,
                      selectInput(ns("dataset1"), "Select first dataset:", 
                                  choices = c("Transcriptomic"= "t", "Metabolomic"= "me", "Microbiomic" = "mi"), width = "150"),
                      selectInput(ns("dataset2"), "Select second dataset:", 
                                  choices = c("Transcriptomic"= "t", "Metabolomic"= "me", "Microbiomic" = "mi"), 
                                  selected = "me", width = "fit-content"),
                      style = "display: flex; gap: 1rem"
      ), 
      bs4Dash::column(width = 6, style = "display: flex; gap: 1rem",
                      selectInput(ns("diabloDataset"), "Select the datasets: ",
                                  choices = c("Transcriptomic"= "t", "Metabolomic"= "me", "Microbiomic" = "mi"), 
                                  multiple = TRUE),
                      textOutput(ns("message"))
      )
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
mod_MultiOmics_server <- function(id, pls_dataset, diablo_dataset){
  moduleServer( id, function(input, output, session){
    observeEvent(input$dataset1, {
      pls_dataset$data1 <- get_dataset(input$dataset1)
    })
    
    observeEvent(input$dataset2, {
      pls_dataset$data2 <- get_dataset(input$dataset2)
    })
    
    observeEvent(input$diabloDataset, {
      data <- list()
      for (d in strsplit(input$diabloDataset, " ")){
        if(d == "t"){
          data[["Transcriptomic"]] <- Holomics::data.transcriptomic_small
        } else if (d == "me"){
          data[["Metabolomic"]] <- Holomics::data.metabolomic_small
        } else if (d == "mi"){
          data[["Microbiomic"]] <- Holomics::data.microbiomic_small
        }
      }
      diablo_dataset$data <- data
    })
    
    output$message <- renderText({
      if (is.null(input$diabloDataset)){
        "Please select a dataset!"
      } else{
        ""
      }
    })
    
  })
}

#' Return the dataset according to the selection
get_dataset <- function(selection){
  if(selection == "t"){
    return (Holomics::data.transcriptomic_small)
  } else if (selection == "me"){
    return (Holomics::data.metabolomic_small)
  } else if (selection == "mi"){
    return (Holomics::data.microbiomic_small)
  }
}

