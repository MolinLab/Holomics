#' upload UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_Upload_ui <- function(id){
  ns <- NS(id)
  tagList(
    shinybusy::add_busy_spinner(spin = "circle", position = "bottom-right", height = "60px", width = "60px"),
    h1("Data upload"),
    fluidRow(
      bs4Dash::column(width = 12,
                      awesomeRadio(ns("type"), "Data type", inline = TRUE,
                                   choices = c("Labels/Classes" = "labels", "Omics data" = "data")),
                      uiOutput(ns("inputFields"))
      )
    )
  )
}

#' upload Server Functions
#'
#' @noRd 
mod_Upload_server <- function(id, data, classes){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    observeEvent(input$type, {
      if (input$type == "data"){
        output$inputFields <- renderUI({
          getDataUploadUI(ns)
        })
      } else if (input$type == "labels") {
        output$inputFields <- renderUI({
          getClassUploadUI(ns)
        })
      }
    })
    
    observeEvent(input$saveData, {
      df_data <- as.data.frame(readxl::read_excel(input$dataFile$datapath, col_names = TRUE))
      rownames(df_data) <- df_data[,1]   #all rows, first column
      data$data[[input$dataName]] <- df_data[,-1]
    })
    
    observeEvent(input$deleteData, {
      data$data <- list()
    })
    
    observeEvent(input$saveClass, {
      classes$data[[input$className]] <- input$classFile
    })
    
    observeEvent(input$deleteClass, {
      classes$data <- list()
    })
    
  })
}