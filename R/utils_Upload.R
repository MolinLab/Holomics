#' @description A utils function
#'
#' @return The return value, if any, from executing the utility.
#'
#' @noRd
getDataUploadUI <- function(ns){
  return(
    tags$div(
      fluidRow(style = "margin-left: 0;",
        fileInput(ns("dataFile"), "Choose a xlsx file", accept = c(".xlsx"))
      ),
      fluidRow(style = "margin-left: 0;",
        textInput(ns("dataName"), "Data name")
      ),
      fluidRow(style = "margin-left: 0;",
        awesomeCheckbox(ns("isMicrobiome"), "Is microbiome data")
      ), 
      fluidRow(
        bs4Dash::column(width = 1,
                        actionButton(ns("saveData"), "Save")
        ),
        bs4Dash::column(width = 2,
                        actionButton(ns("deleteData"), "Delete all")
        )
      )
    )
  )
}

#' @description A utils function
#'
#' @return The return value, if any, from executing the utility.
#'
#' @noRd
getClassUploadUI <- function(ns){
  return(
    tags$div(
      fluidRow(style = "margin-left: 0;",
        fileInput(ns("classFile"), "Choose a xlsx file", accept = c(".xlsx"))
      ),
      fluidRow(style = "margin-left: 0;",
        textInput(ns("className"), "Data name")
      ),
      fluidRow(style = "margin-left: 0;",
        awesomeCheckbox(ns("colorCode"), "Includes color code")
      ),
      fluidRow(
        bs4Dash::column(width = 1,
                        actionButton(ns("saveClass"), "Save")
        ),
        bs4Dash::column(width = 2,
                        actionButton(ns("deleteClass"), "Delete all")
        )
      )
    )
  )
}