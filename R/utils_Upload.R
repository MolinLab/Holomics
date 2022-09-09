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
            awesomeCheckbox(ns("isMicrobiome"), "Is microbiome data")
          ), 
          fluidRow(style = "margin-left: 0;",
                   awesomeCheckbox(ns("inverted"), "Has inverted format")
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
          awesomeCheckbox(ns("colorCode"), "Includes color code")
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
  matrix <- matrix(nrow = 0, ncol = 5)
  colnames(matrix) <- c("Name", "Filename", "Number of samples", "Number of features", "Is microbiome data")
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
  colnames(matrix) <- c("Name", "Filename", "Number of samples", "Contains color code")
  return (matrix)
}

#' @description A utils function, to reset the UI components of the data form
#'
#' @noRd
resetDataUI <- function(session, output){
  ns <- session$ns
  
  output$datafileField <- renderUI({
    fileInput(ns("dataFile"), "Choose a xlsx file", accept = c(".xlsx"))
  })
  updateTextInput(session, "dataName", value = "")
  updateAwesomeCheckbox(session, "isMicrobiome", value = FALSE)
  updateAwesomeCheckbox(session, "inverted", value = FALSE)
}

#' @description A utils function, to reset the UI components of the data form
#'
#' @noRd
resetClassUI <- function(session, output){
  ns <- session$ns
  
  output$classfileField <- renderUI({
    fileInput(ns("classFile"), "Choose a xlsx file", accept = c(".xlsx"))
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
  return (data)
}

#' @description A utils function, gets the the top 10.000 features 
#' of the given data according to the Median Absolute Deviation
#' 
#' @return filtered data
#' 
#' @noRd
filterByMAD <- function(data){
  t_data = t(data)
  data_filtered <- as.data.frame(t(CancerSubtypes::FSbyMAD(t_data, cut.type = "topk", value = 10000)))
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