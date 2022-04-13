#' sPLS 
#' 
#' @description A utils function, which generates the UI code for 
#' the spls page. The postfix is added always on the end of the 
#' id values of the components
#'
#' @return number of components
#'
#' @noRd
splsGetUi <- function(ns, postfix = ""){
  bs4Dash::tabBox(width = 12, collapsible = FALSE,
                  tabPanel("Sample plot",
                           fluidRow(style = "display: flex; gap: 1rem",
                                    uiOutput(paste0(ns("indiv.x.comp"), postfix)),
                                    uiOutput(paste0(ns("indiv.y.comp"), postfix)),
                                    checkboxInput(paste0(ns("indiv.names"), postfix), "Sample names", value = FALSE),
                                    selectInput(paste0(ns("spls.rep.space"), postfix), "Replication space:",
                                                c("Separated"= "NULL", "X-variate"= "X-variate",
                                                  "Y-variate" = "Y-variate", "XY-variate" = "XY-variate"))
                           ),
                           fluidRow(
                             bs4Dash::column(width = 12,
                                             plotOutput(paste0(ns("sPLS.Indiv"), postfix)),
                                             downloadButton(paste0(ns("Indiv.download"), postfix), "Save plot"))
                           )
                  ),
                  tabPanel("Variable plot",
                           fluidRow(style = "display: flex; gap: 1rem",
                                    uiOutput(paste0(ns("var.x.comp"), postfix)),
                                    uiOutput(paste0(ns("var.y.comp"), postfix)),
                                    checkboxInput(paste0(ns("var.names"), postfix), "Variable names", value = FALSE)
                           ),
                           fluidRow(
                             bs4Dash::column(width = 12,
                                             plotOutput(paste0(ns("sPLS.Var"), postfix)),
                                             downloadButton(paste0(ns("Var.download"), postfix), "Save plot"))
                           )
                  ),
                  tabPanel("Loading plots",
                           fluidRow(
                             uiOutput(paste0(ns("load.comp"), postfix))
                           ),
                           fluidRow(
                             bs4Dash::column(width = 12,
                                             plotOutput(paste0(ns("sPLS.Load"), postfix)),
                                             downloadButton(paste0(ns("Load.download"), postfix), "Save plot"))
                           )
                  ),
                  tabPanel("Selected variables",
                           fluidRow(
                             uiOutput(paste0(ns("sel.var.comp"), postfix))
                           ),
                           fluidRow(
                             bs4Dash::tabBox(width = 12,
                                             tabPanel("Dataset 1",
                                                      DT::dataTableOutput(paste0(ns("sPLS.X.Sel.Var"), postfix)),
                                                      downloadButton(paste0(ns("SelVarX.download"), postfix), "Save table")
                                             ),
                                             tabPanel("Dataset 2",
                                                      DT::dataTableOutput(paste0(ns("sPLS.Y.Sel.Var"), postfix)),
                                                      downloadButton(paste0(ns("SelVarY.download"), postfix), "Save table")
                                             )
                             )
                           )
                  ),
                  tabPanel("CIM",
                           fluidRow(
                             uiOutput(paste0(ns("img.comp"), postfix))
                           ),
                           fluidRow(
                             bs4Dash::column(width = 12,
                                             plotOutput(paste0(ns("sPLS.Img"), postfix)),
                                             downloadButton(paste0(ns("Img.download"), postfix), "Save plot"))
                           )
                  ),
                  tabPanel("Arrow plot",
                           fluidRow(
                             checkboxInput(paste0(ns("namesArrow"), postfix), "Sample names", value = FALSE)
                           ),
                           fluidRow(
                             bs4Dash::column(width = 12,
                                             textOutput(paste0(ns("arrow.error"), postfix)),
                                             plotOutput(paste0(ns("sPLS.Arrow"), postfix))
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
splsGetNcomp <- function(input, tuned = FALSE){
  return (ifelse(tuned, tunedsPLSVals$ncomp, input$ncomp))
}

#' @description A utils function, which checks if at least two components 
#' are in the model
#'
#' @return error message if less than two components
#'  else empty string
#'  
#' @noRd
splsCheckNcomp <- function(input, tuned = FALSE){
  if(splsGetNcomp(input, tuned) < 2){
    return("There need to be at least two components to render this plot!")
  } else {
    return("")
  }
}
