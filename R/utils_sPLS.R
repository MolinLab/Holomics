#' sPLS 
#' 
#' @description A utils function to get the tabPanel for
#' the sample plot for the sPLS analysis
#'
#' @return tabpanel
#'
#' @noRd
getsPLSSamplePlot <- function(ns, postfix = ""){
  return (
    tabPanel("Sample plot",
             fluidRow(style = "display: flex; column-gap: 1rem",
                      uiOutput(paste0(ns("indiv.x.comp"), postfix)),
                      uiOutput(paste0(ns("indiv.y.comp"), postfix)),
                      awesomeCheckbox(paste0(ns("indiv.names"), postfix), "Sample names", value = FALSE),
                      selectInput(paste0(ns("rep.space"), postfix), "Replication space:",
                                  c("Separated"= "NULL", "X-variate"= "X-variate",
                                    "Y-variate" = "Y-variate", "XY-variate" = "XY-variate"))
             ),
             fluidRow(
               bs4Dash::column(width = 12,
                               plotOutput(paste0(ns("Indiv"), postfix)),
                               downloadButton(paste0(ns("Indiv.download"), postfix), "Save plot"))
             )
    )
  )
}

#' @description A utils function to get the tabPanel for
#' the selected variable tables of the sPLS analysis
#'
#' @return tabpanel
#'
#' @noRd
getsPLSSelectedVarsPlot <- function(ns, postfix = ""){
  return(
    tabPanel("Selected variables",
             fluidRow(
               uiOutput(paste0(ns("sel.var.comp"), postfix))
             ),
             fluidRow(
               bs4Dash::tabBox(width = 12,
                               tabPanel("Dataset 1",
                                        DT::dataTableOutput(paste0(ns("X.Sel.Var"), postfix)),
                                        downloadButton(paste0(ns("SelVarX.download"), postfix), "Save table")
                               ),
                               tabPanel("Dataset 2",
                                        DT::dataTableOutput(paste0(ns("Y.Sel.Var"), postfix)),
                                        downloadButton(paste0(ns("SelVarY.download"), postfix), "Save table")
                               )
               )
             )
    )
  )
}

#' @description A utils function to get the tabPanel for
#' the plots with the showing the tuning results
#'
#' @return tabpanel
#'
#' @noRd
getsPLSTuningPlots <- function(ns){
  return(
    tabPanel("Tuned parameters",
             fluidRow(
               bs4Dash::tabBox(width = 12,
                               tabPanel("Components",
                                        plotOutput(ns("Tuned.ncomp")),
                                        downloadButton(ns("Tuned.ncomp.download"), "Save plot")
                               ),
                               tabPanel("Variables dataset 1",
                                        plotOutput(ns("Tuned.keepX")),
                                        downloadButton(ns("Tuned.keepX.download"), "Save plot")
                               ),
                               tabPanel("Variables dataset 2",
                                        plotOutput(ns("Tuned.keepY")),
                                        downloadButton(ns("Tuned.keepY.download"), "Save plot")
                               )
               )
             )
    )
  )
}

#' @description A utils function, which returns the number of components.
#' If the tune switch is on or off the number is taken from
#' the tuned parameters or the numeric input
#'
#' @return number of components
#'
#' @noRd
splsGetNcomp <- function(input, tuned = FALSE, tunedVals){
  return (ifelse(tuned, tunedVals$ncomp, input$ncomp))
}

#' @description A utils function, which checks if at least two components 
#' are in the model
#'
#' @return error message if less than two components
#'  else empty string
#'  
#' @noRd
splsCheckNcomp <- function(input, tuned = FALSE, tunedVals = NULL){
  if(splsGetNcomp(input, tuned, tunedVals) < 2){
    return("There need to be at least two components to render this plot!")
  } else {
    return("")
  }
}
