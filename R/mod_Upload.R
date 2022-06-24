library(shinyvalidate)

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
      bs4Dash::column(width = 5, style = "margin-left: 1rem",
                      awesomeRadio(ns("type"), "Data type", inline = TRUE,
                                   choices = c("Omics data" = "data", "Labels/Classes" = "labels")),
                      uiOutput(ns("inputFields"))
      ),
      bs4Dash::column(width = 6,
                      uiOutput(ns("table"))
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
    
    #Validators
    ivData <- initDataValidator(session, data)
    ivClass <- initClassValidator(session, classes)
    
    #tables showing the uploaded data
    tables <- reactiveValues(data = initDataMatrix(), classes = initClassMatrix())
    output$dataTable <- DT::renderDataTable({
      DT::datatable(tables$data, options = list(dom = "tp", pageLength = 5, 
                                                columnDefs = list(list(className = 'dt-body-right', targets = 2:4),
                                                                  list(className = 'dt-body-center', targets = 0:1),
                                                                  list(className = 'dt-center', targets = "_all"))
                                                ))
    })
    
    output$classTable <- DT::renderDataTable({
      DT::datatable(tables$classes, options = list(dom = "tp", pageLength = 5, 
                                                   columnDefs = list(list(className = 'dt-body-right', targets = 2:3),
                                                                     list(className = 'dt-body-center', targets = 0:1),
                                                                     list(className = 'dt-center', targets = "_all"))
                                                   ))
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
        
        output$table <- renderUI({
          getDataTable(ns)
        })
        
      } else if (input$type == "labels") {
        output$inputFields <- renderUI({
          getClassUploadUI(ns)
        })
        
        output$classfileField <- renderUI({
          fileInput(ns("classFile"), "Choose a xlsx file", accept = c(".xlsx"))
        })
        
        output$table <- renderUI({
          getClassTable(ns)
        })
      }
    })
    
    #data
    observeEvent(input$saveData, {
      ivData$enable() #check input with validator
      req(ivData$is_valid())
      
      df_data <- as.data.frame(readxl::read_excel(input$dataFile$datapath, col_names = TRUE))
      
      if (ncol(df_data) == 0 || nrow(df_data) == 0){
        shinyalert::shinyalert("Error!", "The input needs to have at least one row and one column!", type = "error")
      } else if(sum(duplicated(df_data[,1])) != 0){ #there should not be duplicates in the first column
        shinyalert::shinyalert("Error!", "The sample names cannot contain duplicates!", type = "error")
      }else{
        rownames(df_data) <- df_data[,1]   #all rows, first column
        df_data <- df_data[,-1]
        unfiltered_data <- df_data
        
        if(input$inverted){ #check for inverted file format
          df_data <- t(df_data)
        }
        
        if(input$isMicrobiome){ #check for microbiome data
          df_data <- performMixMC(df_data)
        }
        
        if (ncol(df_data) > 10000){  #mixOmics recommends to use only 10.000 features
          df_data  <- filterByMAD(df_data)
          unfiltered_data <- filterByMAD(unfiltered_data)  #also the unfiltered data can only contain 10.000 features
        }
        
        #save data and write to table
        data$data[[input$dataName]] <- list(filtered = df_data, unfiltered = unfiltered_data)
        tables$data <- rbind(tables$data, c(input$dataName, input$dataFile$name, nrow(df_data), ncol(df_data), input$isMicrobiome))
        
        #reset UI
        resetDataUI(session, output)
        ivData$disable()
        ivData <- initDataValidator(session, data)
      }
    })
    
    observeEvent(input$deleteAllData, {
      data$data <- list()
      tables$data <- initDataMatrix()
    })
    
    observeEvent(input$deleteSelectedData, {
      if(!is.null(input$dataTable_rows_selected)){
        selRows = as.numeric(input$dataTable_rows_selected)
        for (row in selRows){
          name = tables$data[row]
          data$data[[name]] = NULL
        }
        
        tables$data <- removeRowsFromMatrix(tables$data, selRows, initDataMatrix)
      }
    })
    
    
    #classes
    observeEvent(input$saveClass, {
      ivClass$enable() #check input with validator
      req(ivClass$is_valid())
      
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
        ivClass$disable()
        ivClass <- initClassValidator(session, classes)
      }
    })
    
    observeEvent(input$deleteAllClass, {
      classes$data <- list()
      tables$classes <- initClassMatrix()
    })
    
    observeEvent(input$deleteSelectedClass, {
      if(!is.null(input$classTable_rows_selected)){
        selRows = as.numeric(input$classTable_rows_selected)
        for (row in selRows){
          name = tables$classes[row]
          classes$data[[name]] = NULL
        }
        
        tables$classes <- removeRowsFromMatrix(tables$classes, selRows, initClassMatrix)
      }
    })
  })
}