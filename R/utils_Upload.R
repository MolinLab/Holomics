#' @description A utils function that defines the UI 
#' of the data upload input fields
#'
#' @return div with the components
#'
#' @noRd
getDataUploadUI <- function(ns){
  return(
      fluidRow(
        bs4Dash::column(width = 12,
          fluidRow(style = "margin-left: 0;",
            uiOutput(ns("datafileField"))
          ),
          fluidRow(style = "margin-left: 0;",
            textInput(ns("dataName"), "Data name")
          ),
          fluidRow(style = "margin-left: 0;",
            awesomeCheckbox(ns("prevFiltered"), "Was previously filtered", width = "auto"),
            getTooltip(ns("isMicrobiome-info"), "Already ran through the single-omics step of the Holomics application")
          ), 
          fluidRow(id = ns("microbiomeRow"), style = "margin-left: 0;",
                   awesomeCheckbox(ns("isMicrobiome"), "Is microbiome data", width = "auto"),
                   getTooltip(ns("isMicrobiome-info"), "Microbiome data used for the single-omics analysis 
                       will run through the mixMc pipeline developed by mixOmics")
          ), 
          fluidRow(style = "margin-left: 0;",
                   awesomeCheckbox(ns("transposed"), "Has transposed format", width = "auto"),
                   getTooltip(ns("transposed-info"), "Uploaded data has the samples as the columns and the features as the rows")
          ),
          fluidRow(style = "margin-left: 0;",
                   bs4Dash::column(width = 12,
                                   fluidRow(
                                     tags$label("Use for ... omics analysis"),
                                     getTooltip(ns("omicsAnalysis-info"), 
                                                "Uploaded data will only be available for the selected analysis")
                                   ),
                                   fluidRow(
                                    awesomeCheckboxGroup(ns("omicsAnalysis"), "",
                                                        choices = c("single", "multi"), width = "auto")
                                   )
                   )
          ),
          fluidRow(style = "margin-left: 0;",
                   bs4Dash::column(width = 12,
                                   fluidRow(
                                     tags$label("Name for plots"),
                                     getTooltip(ns("plotName-info"), 
                                                "Name that will be used in the plots")
                                   ),
                                   fluidRow(
                                     tags$div("Delete the text of the selection to be able to insert a custom name", style = "font-size: 11px; margin-top: -0.5rem")
                                   ),
                                   fluidRow(
                                     selectizeInput(ns("plotName"), label = "", 
                                                           choices = c("Metabolomics"= "Metabolomics", "Microbiomics"= "Microbiomics",
                                                                       "Transcriptomics" = "Transcriptomics", "Proteomics" = "Proteomics",
                                                                       "Genomics" = "Genomics", "Phenomics" = "Phenomics"),
                                                    options = list(create=T))
                                   )
                   )
          ),
          fluidRow(style = "margin-left: 0;",
            actionButton(ns("saveData"), "Save")
          )
        )
      )
  )
}

#' @description A utils function that defines the UI 
#' of the classes/label upload input fields
#'
#' @return div with the components
#'
#' @noRd
getClassUploadUI <- function(ns){
  return(
    fluidRow(
      bs4Dash::column(width = 12,
        fluidRow(style = "margin-left: 0;",
                 uiOutput(ns("classfileField"))
        ),
        fluidRow(style = "margin-left: 0;",
          textInput(ns("className"), "Data name")
        ),
        fluidRow(style = "margin-left: 0;",
          awesomeCheckbox(ns("colorCode"), "Includes color code", width = "auto"),
          getTooltip(ns("colorCode-info"), "Uploaded data has a third column with color names / hex codes 
          that will be used later on in the plots")
        ),
        fluidRow(style = "margin-left: 0;",
                 actionButton(ns("saveClass"), "Save")
        )
      )
    )
  )
}

#' @description A utils function that defines the UI 
#' for the data table
#'
#' @return div with the components
#'
#' @noRd
getDataTable <- function(ns){
  return(
    fluidRow(
      bs4Dash::column(width = 12,
                      fluidRow(
                        DT::dataTableOutput(ns("dataTable"))
                      ),
                      fluidRow(style = "margin-top: .5rem",
                        actionButton(ns("deleteAllData"), "Delete all"),
                        actionButton(ns("deleteSelectedData"), "Delete selected datasets")
                      )
      )
    )
  )
}

#' @description A utils function that defines the UI 
#' for the data table
#'
#' @return div with the components
#'
#' @noRd
getClassTable <- function(ns){
  return(
    fluidRow(
      bs4Dash::column(width = 12,
                      fluidRow(
                        DT::dataTableOutput(ns("classTable"))
                      ),
                      fluidRow(style = "margin-top: .5rem",
                        actionButton(ns("deleteAllClass"), "Delete all"), 
                        actionButton(ns("deleteSelectedClass"), "Delete selected datasets")
                      )
      )
    )
  )
}

#' @description A utils function that generates the input validator 
#' for the data fields
#'
#' @return input validator
#'
#' @noRd
initDataValidator <- function(session){
  iv <- InputValidator$new(session = session)
  iv$add_rule("dataFile", sv_required(message = "You need to upload a xlsx file!"))
  iv$add_rule("dataName", sv_required(message = "You need to enter a name!"))
  iv$add_rule("dataName", sv_regex("^[a-zA-Z0-9_ ]*$", "Please only use characters, space, underscore or digits for the name."))
  iv$add_rule("omicsAnalysis", sv_required(message = "You need to select at least one type of analysis!"))
  iv$add_rule("plotName", sv_required(message = "You need to either select or enter a name!"))
  return (iv)
}

#' @description A utils function that generates the input validator 
#' for the class fields
#'
#' @return input validator
#'
#' @noRd
initClassValidator <- function(session){
  iv <- InputValidator$new(session = session)
  iv$add_rule("classFile", sv_required(message = "You need to upload a xlsx file!"))
  iv$add_rule("className", sv_required(message = "You need to enter a name!"))
  iv$add_rule("className", sv_regex("^[a-zA-Z0-9_ ]*$", "Please only use characters, space, underscore or digits for the name."))
  return (iv)
}

#' @description A utils function that checks if the 
#' given classes data is valid 
#'
#' @return list with an alert and a valid attribute
#'
#' @noRd
checkClassesInput <- function(data, colorCodeChecked){
  alert = ""
  
  if(nrow(data) == 0){
    alert = getShinyErrorAlert("The input needs to have at least one row!")
  } else if(any(is.na(data))){
    alert = getShinyErrorAlert("Every cell of the necessary columns and rows needs to have a value!")
  } else if (alert == ""){
    if(colorCodeChecked){
      if (ncol(data) != 3){
        alert = getShinyErrorAlert("According to your selection, you need to provide exactly three columns! 
                                       The first with the sample names, the second with the labels/classes 
                                       and the third with the color codes.")
      } else if(!all(areValidColors(data[,3]))){
        alert = getShinyErrorAlert("There are invalid colors in the file you want to upload!")
      } else if(length(data[,1]) != length(data[,3]) || length(data[,2]) != length(data[,3])){
        alert = getShinyErrorAlert("The number of samples, classes/labels and colors need to be the same!")
      }
    } else {
      if(ncol(data) != 2){
        alert = getShinyErrorAlert("According to your selection, you need to provide exactly two columns!
                                   The first with the sample names and the second with the labels/classes.")
      } else if(length(data[,1]) != length(data[,2])){
        alert = getShinyErrorAlert("The number of samples and classes/labels need to be the same!")
      }
    }
  }
  
  return(list(valid = ifelse(alert == "", TRUE, FALSE), alert = alert))
}

#' @description A utils function that checks if a 
#' given name is in the list of names
#'
#' @return boolean
#'
#' @noRd
isValidName <- function(name, names){
  return(!(name %in% names))
}

#' @description A utils function initialises the matrix for the table containing
#' the uploaded data
#'
#' @return initialized matrix
#'
#' @noRd
initDataMatrix <- function(){
  matrix <- matrix(nrow = 0, ncol = 7)
  colnames(matrix) <- c("Name", "Filename", "No. of samples", "No. of features", 
                        "Is microbiome data", "For ...-omics analysis", "Name for plots")
  return (matrix)
}

#' @description A utils function initialises the matrix for the table containing
#' the uploaded data
#'
#' @return initialized matrix
#'
#' @noRd
initClassMatrix <- function(){
  matrix <- matrix(nrow = 0, ncol = 4)
  colnames(matrix) <- c("Name", "Filename", "No. of samples", "Contains color code")
  return (matrix)
}

#' @description A utils function, to reset the UI components of the data form
#'
#' @noRd
resetDataUI <- function(session, output){
  ns <- session$ns
  
  output$datafileField <- renderUI({
    fileInput(ns("dataFile"), "Choose a xlsx or csv file", accept = c(".xlsx, .csv"))
  })
  updateTextInput(session, "dataName", value = "")
  
  #not nice but necessary to trigger observeEvent as only there the disable for omicsAnalysismulti works
  updateAwesomeCheckbox(session, "prevFiltered", value = TRUE)
  updateAwesomeCheckbox(session, "prevFiltered", value = FALSE)
  updateAwesomeCheckbox(session, "isMicrobiome", value = FALSE)
  updateAwesomeCheckbox(session, "transposed", value = FALSE)
  updateAwesomeCheckboxGroup(session, "omicsAnalysis", selected = "NULL")
  shinyjs::disable("omicsAnalysismulti")
  shinyjs::show("isMicrobiome")
}

#' @description A utils function, to reset the UI components of the data form
#'
#' @noRd
resetClassUI <- function(session, output){
  ns <- session$ns
  
  output$classfileField <- renderUI({
    fileInput(ns("classFile"), "Choose a xlsx csv file", accept = c(".xlsx, .csv"))
  })
  updateTextInput(session, "className", value = "")
  updateAwesomeCheckbox(session, "colorCode", value = FALSE)
}

#' @description A utils function, that perform the mixMC preprocessing
#' steps with the given data
#'
#' @return preprocessed data
#' 
#' @noRd
performMixMC <- function(data){
  data <- data + 1
  keep.asv <- which(colSums(data)*100/(sum(colSums(data)))>0.01)
  data <- data[,keep.asv]
  data <- mixOmics::logratio.transfo(as.matrix(data), logratio = "CLR")
  class(data) <- "matrix"
  data <- as.data.frame(data)
  return (data)
}

#' @description A utils function, gets the the top 10.000 features 
#' of the given data
#' 
#' @return filtered data
#' 
#' @noRd
filterToTenThousand <- function(data){
  #low count filtering
  keep <- colSums(data) > 10
  data <- data[, keep]
  
  if (ncol(data) > 10000){
    data <- filterByMAD(data)
  }
  return(data)
}

#' @description A utils function, gets the the top 10.000 features 
#' of the given data according to the Median Absolute Deviation
#' 
#' @return filtered data
#' 
#' @noRd
filterByMAD <- function(data){
  t_data = t(data)
  
  mads <- apply(t_data, 1, mad)
  feature_num <- length(mads)
  index <- sort(mads, decreasing = TRUE, index.return=TRUE)
  limit = 10000
    
  if(limit>nrow(t_data)) {
    limit=nrow(t_data)
  }

  index=index$ix[1:limit]
  filtered=t_data[index,]
  
  data_filtered <- as.data.frame(t(filtered))
  return (data_filtered)
}


#' @description A utils function, that removes the given rows
#' from the given matrix
#' 
#' @return filtered data
#' 
#' @noRd
removeRowsFromMatrix <- function(matrix, rows, initFct){
  if (length(rows) == nrow(matrix)){
    matrix <- initFct()
  } else {
    newMatrix <- as.matrix(matrix[-rows,])
    
    #if the new matrix would have only one row, the new matrix must be transposed
    if(nrow(matrix) - length(rows) == 1){
      matrix <- t(newMatrix)
    } else {
      matrix <- newMatrix
    }
  }
  
  return (matrix)
}

#' @description A utils function, that checks if the given 
#' color is actually a rgb color
#' 
#' @return boolean
#' 
#' @noRd
areValidColors <- function(color) {
  sapply(color, function(c) {
    tryCatch(is.matrix(col2rgb(c)), 
             error = function(e) FALSE)
  })
}