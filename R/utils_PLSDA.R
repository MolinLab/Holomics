#' PLS-DA 
#' 
#' @description A utils function to get the tabPanel for
#' the loading plot for the PLS-DA analysis
#'
#' @return tabpanel
#'
#' @noRd
getPLSDASamplePlot <- function(ns, postfix = ""){
  tabPanel("Loading plot",
           fluidRow(
             bs4Dash::column(width = 12,
                             uiOutput(paste0(ns("load.comp"), postfix)),
                             selectInput(paste0(ns("load.cont"), postfix), "Contribution:", width = "100",
                                         choices = c("minimal" = "min", "maximal" = "max")),
                             selectInput(paste0(ns("load.method"), postfix), "Method:", width = "100",
                                         choices = c("mean" = "mean", "median" = "median")),
                             style = "display: flex; column-gap: 1rem"             
             )
           ),
           fluidRow(
             bs4Dash::column(width = 12,
                             plotOutput(paste0(ns("Load"), postfix)),
                             downloadButton(paste0(ns("Load.download"), postfix), "Save plot"))
           )
  )
}