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
             bs4Dash::box(title = "General information", width = 12, 
                          collapsed = FALSE, id = ns("general-information"),
                          htmlOutput(ns("infotext"))
             )
    ),
    tags$hr(),
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
mod_Upload_server <- function(id, singleData, singleClasses, multiData, multiClasses, tables){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    #Validators
    ivData <- initDataValidator(session)
    ivClass <- initClassValidator(session)
    
    # tables showing the uploaded data
    tables$data = initDataMatrix()
    tables$classes = initClassMatrix()
    output$dataTable <- DT::renderDataTable({
      DT::datatable(tables$data, rownames = F, options = list(dom = "tp", pageLength = 5, scrollX = TRUE,
                                                columnDefs = list(list(className = 'dt-body-right', targets = 2:4),
                                                                  list(className = 'dt-body-center', targets = 0:1),
                                                                  list(className = 'dt-center', targets = "_all"))
                                                )
                    )
    })

    output$classTable <- DT::renderDataTable({
      DT::datatable(tables$classes, rownames = F, options = list(dom = "tp", pageLength = 5, scrollX = TRUE,
                                                   columnDefs = list(list(className = 'dt-body-right', targets = 2:3),
                                                                     list(className = 'dt-body-center', targets = 0:1),
                                                                     list(className = 'dt-center', targets = "_all"))
                                                   ))
    })

    #switch between data and classes form
    observeEvent(input$type, {
      if (input$type == "data"){
        
        output$infotext <- renderText(
          "The dataset has to be a matrix in an excel or csv file, whereas the rows have to be the samples and the columns the measured features. <br/>
	         The first column has to contain the sample names and the first row the features names. <br/>
	         If the dataset contains more features/columns then excel allows, please transpose the matrix in the file and select the 'transposed format' checkbox.<br/>
           Also if the dataset contains more than 10.000 features, Holomics will filter the dataset to 10.000 or less features as mixOmics suggests to use
          their package with max. 10.000 features. </br>
          In addition, any wanted/necessary normalisation needs to be done before using Holomics."
        )
        
        output$inputFields <- renderUI({
          getDataUploadUI(ns)
        })
        
        output$datafileField <- renderUI({
          fileInput(ns("dataFile"), "Choose a xlsx or csv file", accept = c(".xlsx, .csv"))
        })
        
        output$table <- renderUI({
          getDataTable(ns)
        })
        
        resetDataUI(session, output)
        
      } else if (input$type == "labels") {
        
        output$infotext <- renderText(
          "The file has to be an excel or csv file with at least two columns. <br/>
           The first column contains the sample names, which have to be exactly the same as in the omics datasets. <br/>
           The second columns contains the classes of the samples (e.g. bad, good, ...). The first entry of the second column has 
           to be the name of the attribute the classes describe (e.g. Storability, Quality, ...).<br/>
	         Additionally, in a third column, a color name / hex code per class can be added, which will be used in the plots.
	         The first entry of this column has to contain the value 'Color' or 'Colorcodes'."
        )
        
        output$inputFields <- renderUI({
          getClassUploadUI(ns)
        })
        
        output$classfileField <- renderUI({
          fileInput(ns("classFile"), "Choose a xlsx or csv file", accept = c(".xlsx, .csv"))
        })
        
        output$table <- renderUI({
          getClassTable(ns)
        })
      }
    })
    
    observeEvent(input$prevFiltered, {
      if(isTRUE(input$prevFiltered)){
        shinyjs::hide("microbiomeRow")
      } else {
        shinyjs::show("microbiomeRow")
      }
    }, ignoreNULL = F)
    
    #save data
    observeEvent(input$saveData, {
      ivData$enable() #check input with validator
      req(ivData$is_valid())
      
      #disable button so the user cannot click it again while the following pipeling is running
      shinyjs::disable("saveData")
      
      #additional validation of the name
      if(!isValidName(input$dataName, c(names(singleData$data), names(multiData$data)))){
        getShinyErrorAlert("This name is already in use, please choose another one!")
        #only invalid selection case
      } else if ("multi" %in% input$omicsAnalysis && !input$prevFiltered){ 
        getShinyErrorAlert("Unfortunately, this selection is not valid. 
                            You have to either check the \"was previously filtered\" checkbox 
                            or select only the \"single\" checkbox.")
      } else {
        
        ext <- tools::file_ext(input$dataFile$name)
        
        switch(ext,
          xlsx = data <- as.data.frame(readxl::read_excel(input$dataFile$datapath, col_names = TRUE)),
          csv = data <- read.csv(input$dataFile$datapath),
          validate("Invalid file format!")
        )
        
        #Three cols because one is the row names and at least two with variable data
        if (ncol(data) < 3 || nrow(data) < 1){
          getShinyErrorAlert("The input needs to have at least one row and three columns!")
        } else if(sum(duplicated(data[,1])) != 0){ #there should not be duplicates in the first column
          getShinyErrorAlert("The sample names cannot contain duplicates!")
        } else {
          rownames(data) <- data[,1]   #all rows, first column
          data <- data[,-1]
          
          #values can only be numeric
          if(all(sapply(data, is.numeric))){

            if(input$transposed){ #check for transposed file format
              data <- as.data.frame(t(data))
            }
            
            if(input$isMicrobiome && !input$prevFiltered){ #check for microbiome data and if it was not previously filtered
              data <- performMixMC(data)
            }
            
            if (ncol(data) > 10000){  #mixOmics recommends to use only 10.000 features
              data  <- filterToTenThousand(data)
            }
            
            #save data and write to table
            analysisText = ""
            
            if("single" %in% input$omicsAnalysis){
              if (!is.null(singleData)){
                singleData$data[[input$dataName]] <- list(omicsData = data, name = input$plotName)
              }
              analysisText = "single"
              
            }
            
            if("multi" %in% input$omicsAnalysis && input$prevFiltered){
              if (!is.null(multiData)){
                multiData$data[[input$dataName]] <- list(omicsData = data, name = input$plotName)
              }
              
              analysisText = ifelse(analysisText == "", "multi", "both")
            }
            
            tables$data <- extendDataTable(tables$data, input$dataName, input$dataFile$name, nrow(data), ncol(data),
                            input$isMicrobiome && !input$prevFiltered, analysisText, input$plotName)
            
            #reset UI
            resetDataUI(session, output)
            ivData$disable()
            ivData <- initDataValidator(session)
          } else {
            getShinyErrorAlert("The data can only contain numeric values apart from the row/column names!")
          }
        }
      }

      #enable button again
      shinyjs::enable("saveData")      
    })
    
    #Delete all uploaded omics data
    observeEvent(input$deleteAllData, {
      singleData$data <- list()
      multiData$data <- list()
      tables$data <- initDataMatrix()
    })
    
    #Delete selected omics data
    observeEvent(input$deleteSelectedData, {
      if(!is.null(input$dataTable_rows_selected)){
        selRows = as.numeric(input$dataTable_rows_selected)
        for (row in selRows){
          name <- tables$data[row]
          singleData$data[[name]] <- NULL
          multiData$data[[name]] <- NULL
        }
        
        tables$data <- removeRowsFromMatrix(tables$data, selRows, initDataMatrix)
      }
    })
    
    #save classes
    observeEvent(input$saveClass, {
      ivClass$enable() #check input with validator
      req(ivClass$is_valid())
      
      #disable button so the user cannot click it again while the following pipeling is running
      shinyjs::disable("saveClass")
      
      if(!isValidName(input$className,  c(names(singleClasses$data), names(multiClasses$data)))){
        getShinyErrorAlert("This name is already in use, please choose another one!")
      } else {
        
        ext <- tools::file_ext(input$classFile$name)
        
        switch(ext,
               xlsx = df_classes <- as.data.frame(readxl::read_excel(input$classFile$datapath, col_names = TRUE)),
               csv = df_classes <- read.csv(input$classFile$datapath),
               validate("Invalid file format!")
        )
        
        #error checks
        inputCheck <- checkClassesInput(df_classes, input$colorCode)
        
        if (inputCheck$valid){
          #save classes and update table
          if(!is.null(singleClasses)){
            singleClasses$data[[input$className]] <- df_classes
          }
          
          if(!is.null(multiClasses)){
            multiClasses$data[[input$className]] <- df_classes
          }
          
          tables$classes <- extendClassTable(tables$classes, input$className, input$classFile$name, nrow(df_classes), input$colorCode)
          
          #reset UI
          resetClassUI(session, output)
          ivClass$disable()
          ivClass <- initClassValidator(session)
        } else {
          inputCheck$alert
        }
      }  
      
      #enable button again
      shinyjs::enable("saveClass")
    })
    
    #Delete all uploaded classes/labels
    observeEvent(input$deleteAllClass, {
      singleClasses$data <- list()
      multiClasses$data <- list()
      tables$classes <- initClassMatrix()
    })
    
    #Delete selected, uploaded classes/labels
    observeEvent(input$deleteSelectedClass, {
      if(!is.null(input$classTable_rows_selected)){
        selRows <- as.numeric(input$classTable_rows_selected)
        for (row in selRows){
          name <- tables$classes[row]
          singleClasses$data[[name]] <- NULL
          multiClasses$data[[name]] <- NULL
        }
        
        tables$classes <- removeRowsFromMatrix(tables$classes, selRows, initClassMatrix)
      }
    })
  })
}

