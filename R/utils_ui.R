#' utils_ui 
#'
#' @description A utils function to get the block with the analysis parameters.
#' If logratio is true a selectInput with the available logratios will be returned
#'
#' @return bootstrap box with the parameter components
#'
#' @noRd
getAnalysisParametersComponent <- function(ns, logratio = FALSE){
  if (logratio){
    return(
      bs4Dash::box(title = "Analysis parameters", width = 12, collapsed = TRUE,
                   fluidRow(style = "column-gap: 1rem", 
                            numericInput(ns("ncomp"), "Number of components", value = 3, 
                                         min = 1, max = 15, step = 1, width = "45%"),
                            selectInput(ns("logratio"), "Logratio:",
                                        c("None" = "none","centered" = "CLR"), width = "30%"),
                            awesomeCheckbox(ns("scale"), "Scaling", value = TRUE, width = "15%")
                   )
      ) 
    )
  } else {
    return(
      bs4Dash::box(title = "Analysis parameters", width = 12, collapsed = TRUE,
                   fluidRow(style = "column-gap: 1rem", 
                            numericInput(ns("ncomp"), "Number of components", value = 3, 
                                         min = 1, max = 15, step = 1, width = "45%"),
                            selectInput(ns("logratio"), "Logratio:",
                                        c("None" = "none","centered" = "CLR"), width = "30%"),
                            awesomeCheckbox(ns("scale"), "Scaling", value = TRUE, width = "15%")
                   )
      )  
    )
  }
}


#' @description A utils function to get the block with the
#' tune button and tune swith
#'
#' @return bootstrap box with the tune components
#'
#' @noRd
getTuneBox <- function(ns){
  return(
    bs4Dash::box(id = ns("tuneBox"), width = 12,
                 fluidRow(style = "flex-direction: column",
                          actionButton(ns("tune"), "Tune parameters"),
                 ),
                 fluidRow(id = ns("switchRow"),
                          uiOutput(ns("tune.switch"))
                 )
    )
  )
}

#' @description A utils function to get the block with the tuned analysis parameters.
#' If keepY is true a output field for the keepY will be returned
#'
#' @return bootstrap box with the tune components
#'
#' @noRd
getTunedParametersComponent <- function(ns, keepY = FALSE){
  if (keepY){
    return(
       bs4Dash::box(title = "Tuned analysis parameters", width = 12, collapsed = TRUE,
                    fluidRow(style = "column-gap: 1rem",
                             textOutput(ns("ncomp.tuned")),
                             textOutput(ns("keepX.tuned")),
                             textOutput(ns("keepY.tuned"))
                    )
       )
    )
  } else {
    return(
      bs4Dash::box(title = "Tuned analysis parameters", width = 12, collapsed = TRUE,
                   fluidRow(style = "column-gap: 1rem",
                            textOutput(ns("ncomp.tuned")),
                            textOutput(ns("keepX.tuned"))
                   )
      )
    )
  }
}