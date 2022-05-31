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

#' @description A utils function to get the tabPanel for
#' the sample plots
#'
#' @return tabpanel
#'
#' @noRd
getSamplePlot <- function(ns, postfix = ""){
  return(
    tabPanel("Sample plot", 
             fluidRow(style = "display: flex; column-gap: 1rem",
                      uiOutput(paste0(ns("indiv.x.comp"), postfix)),
                      uiOutput(paste0(ns("indiv.y.comp"), postfix)),
                      awesomeCheckbox(paste0(ns("indiv.names"), postfix), "Sample names", value = FALSE)
             ),
             fluidRow(
               bs4Dash::column(width = 12,
                               textOutput(paste0(ns("indiv.error"), postfix)),
                               plotOutput(paste0(ns("Indiv"), postfix)),
                               downloadButton(paste0(ns("Indiv.download"), postfix), "Save plot"))             
             )
    )
  )
}

#' @description A utils function to get the tabPanel for
#' the variable plots
#'
#' @return tabpanel
#'
#' @noRd
getVariablePlot <- function(ns, postfix = ""){
  return(
    tabPanel("Variable plot",
             fluidRow(style = "display: flex; column-gap: 1rem",
                      uiOutput(paste0(ns("var.x.comp"), postfix)),
                      uiOutput(paste0(ns("var.y.comp"), postfix)),
                      awesomeCheckbox(paste0(ns("var.names"), postfix), "Variable names", value = FALSE)
             ),
             fluidRow(
               bs4Dash::column(width = 12,
                               textOutput(paste0(ns("var.error"), postfix)),
                               plotOutput(paste0(ns("Var"), postfix)),
                               downloadButton(paste0(ns("Var.download"), postfix), "Save plot"))         
             )
    )
  )
}

#' @description A utils function to get the tabPanel for
#' the loadings plots
#'
#' @return tabpanel
#'
#' @noRd
getLoadingsPlot <- function(ns, postfix = ""){
  return(
    tabPanel("Loading plot",
             fluidRow(
               uiOutput(paste0(ns("load.comp"), postfix)),
             ),
             fluidRow(
               bs4Dash::column(width = 12,
                               textOutput(paste0(ns("load.error"), postfix)),
                               plotOutput(paste0(ns("Load"), postfix)),
                               downloadButton(paste0(ns("Load.download"), postfix), "Save plot"))
             )
    )
  )
}

#' @description A utils function to get the tabPanel for
#' the sample plots
#'
#' @return tabpanel
#'
#' @noRd
getSelectedVarsPlot <- function(ns, postfix = ""){
  return(
    tabPanel("Selected variables",
             fluidRow(
               uiOutput(paste0(ns("sel.var.comp"), postfix))
             ),
             fluidRow(
               bs4Dash::column(width = 12,
                               DT::dataTableOutput(paste0(ns("Sel.Var"), postfix)),
                               downloadButton(paste0(ns("SelVar.download"), postfix), "Save table")
               )
             )     
    )
  )
}


#' @description A utils function to get the tabPanel for
#' the cim plot
#'
#' @return tabpanel
#'
#' @noRd
getCimPlot <- function(ns, postfix = ""){
  return(
    tabPanel("CIM",
             fluidRow(
               uiOutput(paste0(ns("img.comp"), postfix))
             ),
             fluidRow(
               bs4Dash::column(width = 12,
                               textOutput(paste0(ns("img.error"), postfix)),
                               plotOutput(paste0(ns("Img"), postfix)),
                               downloadButton(paste0(ns("Img.download"), postfix), "Save plot"))
             )
    )
  )
}

#' @description A utils function to get the tabPanel for
#' the arrow plot
#'
#' @return tabpanel
#'
#' @noRd
getArrowPlot <- function(ns, postfix = ""){
  return (
    tabPanel("Arrow plot",
             fluidRow(
               awesomeCheckbox(paste0(ns("namesArrow"), postfix), "Sample names", value = FALSE)
             ),
             fluidRow(
               bs4Dash::column(width = 12,
                               textOutput(paste0(ns("arrow.error"), postfix)),
                               plotOutput(paste0(ns("Arrow"), postfix)))
             )
    )
  )
}


