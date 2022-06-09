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
                                   choices = c("Omics data" = "data", "Labels/Classes" = "labels")),
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
    
    tables <- reactiveValues(data = initDataMatrix(), classes = initClassMatrix())
    output$dataTable <- DT::renderDataTable({
      tables$data
    })
    
    output$classTable <- DT::renderDataTable({
      tables$classes
    })

    #switch between data and classes form
    observeEvent(input$type, {
      if (input$type == "data"){
        output$inputFields <- renderUI({
          getDataUploadUI(ns)
        })
        
        output$datafileField <- renderUI({
          fileInput(ns("dataFile"), "Choose a xlsx file", accept = c(".xlsx"))
        })
      } else if (input$type == "labels") {
        output$inputFields <- renderUI({
          getClassUploadUI(ns)
        })
        
        output$classfileField <- renderUI({
          fileInput(ns("classFile"), "Choose a xlsx file", accept = c(".xlsx"))
        })
      }
    })
    
    #data
    observeEvent(input$saveData, {
      checkResult = checkFormInput(data$data, input$dataFile, input$dataName)
      if (checkResult$valid){
        df_data <- as.data.frame(readxl::read_excel(input$dataFile$datapath, col_names = TRUE))
        
        if (ncol(df_data) == 0 || nrow(df_data) == 0){
          shinyalert::shinyalert("Error!", "The input needs to have at least one row and one column!", type = "error")
        } else {
          rownames(df_data) <- df_data[,1]   #all rows, first column
          df_data <- df_data[,-1]
          
          if(input$inverted){ #check for inverted file format
            df_data <- t(df_data)
          }
          
          if(input$isMicrobiome){ #check for microbiome data
            df_data <- performMixMC(df_data)
          }
          
          if (ncol(df_data) > 10000){  #mixOmics recommends to use only 10.000 features
            df_data  <- filterByMAD(df_data)
          }
          
          #save data and write to table
          data$data[[input$dataName]] <- df_data
          tables$data <- rbind(tables$data, c(input$dataName, input$dataFile$name, nrow(df_data), ncol(df_data), input$isMicrobiome))
          
          #reset UI
          resetDataUI(session, output)
        }
        
      } else {
        shinyalert::shinyalert("Error!", checkResult$message, type = "error")
      }
    })
    
    observeEvent(input$deleteData, {
      data$data <- list()
      tables$data <- initDataMatrix()
    })
    
    observeEvent(input$deleteDataRows, {
      if(!is.null(input$dataTable_rows_selected)){
        tables$data <- tables$data[-as.numeric(input$dataTable_rows_selected),]
      }
    })
    
    
    #classes
    observeEvent(input$saveClass, {
      checkResult = checkFormInput(classes$data, input$classFile, input$className)
      if (checkResult$valid){
        df_classes <- as.data.frame(readxl::read_excel(input$classFile$datapath, col_names = TRUE))
        
        if(nrow(df_classes) == 0){
          shinyalert::shinyalert("Error!", "The input needs to have at least one row!", type = "error")
        } else if(input$colorCode && ncol(df_classes) != 2){
          shinyalert::shinyalert("Error!", "According to your selection, you need to provide exactly two columns! 
                                 One with the labels and the second with the color codes.", type = "error")
        } else if (!input$colorCode && ncol(df_classes) != 1){
          shinyalert::shinyalert("Error!", "According to your selection, you need to provide exactly one column with the labels!", 
                                 type = "error")
        } else {
          #save classes and update table
          classes$data[[input$className]] <- df_classes
          tables$classes <- rbind(tables$classes, c(input$className, input$classFile$name, nrow(df_classes), input$colorCode))
          
          #reset UI
          resetClassUI(session, output)
        }
      } else {
        shinyalert::shinyalert("Error!", checkResult$message, type = "error")
      }
    })
    
    observeEvent(input$deleteClass, {
      classes$data <- list()
      tables$data <- initDataMatrix()
    })
    
  })
}