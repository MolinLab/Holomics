#' @description A utils function, which gets the dataset according to the selection
#'
#' @return dataframe with the data
#'
#' @noRd
getSelectionComponent <- function(id, label, choices, selected = NULL, multiple = FALSE, width = NULL){
  return(
    selectInput(id, label,
                choices = choices, multiple = multiple, 
                width = width, selected = selected)
  )
}

#' @description A utils function that creates the
#' popover with its action button to show a information text
#'
#' @return bootstrap popover
#'
#' @noRd
getTooltip <- function(id, title){
  return(
      tags$div(
        actionButton(id, label = "", icon = icon("question"), style = "info", size = "xs"),
        tippy::tippy_this(id, title)
      )
  )
}

#' @description A utils function to get the block with the analysis parameters.
#'
#' @return bootstrap box with the parameter components
#'
#' @noRd
getAnalysisParametersComponent <- function(ns, uiExtension = NULL, collapsed = TRUE){
  return(
    bs4Dash::box(title = "Analysis parameters", width = 12, collapsed = collapsed,
                 fluidRow(style = "column-gap: 1rem",
                          bs4Dash::column(width = 3,
                            fluidRow(
                              tags$label("Number of components")
                            ),
                            fluidRow(
                              numericInput(ns("ncomp"), label = "", value = 9, 
                                           min = 2, max = 15, step = 1)
                            )
                          ),
                          bs4Dash::column(width = 3, style = "display: flex; column-gap: inherit",
                            awesomeCheckbox(ns("scale"), "Scaling", value = TRUE, width = "fit-content"),
                            getTooltip(ns("scale-info"), "Features will be standardized to have a mean of zero 
                                       and a unit variance before the analysis takes place")
                          ),
                          uiExtension,
                 ),
                 fluidRow(
                   textOutput(ns("parameters.error"))
                 )
    )  
  )
}

#' @description A utils function to get the block with the
#' tune button and tune switch
#'
#' @return bootstrap box with the tune components
#'
#' @noRd
getTuneBox <- function(ns, name, infotext){
  return(
    bs4Dash::box(id = ns("tuneBox"), width = 12,
                 getTuneButtonRow(ns, name, infotext)
    )
  )
}

#' @description A utils function to get the block with the
#' download button
#'
#' @return bootstrap box with the download button
#'
#' @noRd
getFilterBox <- function(ns, name, infotext){
  return(
    bs4Dash::box(id = ns("filterBox"), width = 12,
                 getTuneButtonRow(ns, name, infotext),
                 fluidRow(style = "flex-direction: row",
                          downloadButton(ns("Filter.download"), "Save reduced data", 
                                         style = "margin-top: 10px; width: 100%")
                 )
    )
  )
}

#' @description A utils function to get the row with the
#' tune button and infotext
#'
#' @return bootstrap box with the tune components
#'
#' @noRd
getTuneButtonRow <- function(ns, name, infotext){
  fluidRow(style = "flex-direction: row",
           bs4Dash::column(width = 11, style = "padding: 0",
                           actionButton(ns("tune"), name, width="inherit")
           ),
           bs4Dash::column(width = 1, style = "padding-left: 0",
                           getTooltip(ns("tune-info"), infotext)
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
                             textOutput(ns("keepY.tuned")),
                             textOutput(ns("scale.tuned")),
                             textOutput(ns("mode.tuned"))
                    )
       )
    )
  } else {
    return(
      bs4Dash::box(title = "Tuned analysis parameters", width = 12, collapsed = TRUE,
                   fluidRow(style = "column-gap: 1rem",
                            textOutput(ns("ncomp.tuned")),
                            textOutput(ns("keepX.tuned")),
                            textOutput(ns("scale.tuned")),
                            textOutput(ns("matrix.tuned"))
                   )
      )
    )
  }
}

#' @description A utils function to get the tabPanel for
#' the scree plots
#'
#' @return tabpanel
#'
#' @noRd
getScreePlot <- function(ns, postfix = ""){
  return(
    tabPanel("Scree plot",       
             bs4Dash::column(width = 12,
                             plotOutput(paste0(ns("Scree"), postfix)), 
                             downloadButton(paste0(ns("Scree.download"), postfix), "Save plot"))
    )
  )
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
#' the Correlation Circle plots
#'
#' @return tabpanel
#'
#' @noRd
getVariablePlot <- function(ns, postfix = ""){
  return(
    tabPanel("Correlation circle plot",
             fluidRow(style = "display: flex; column-gap: 1rem",
                      uiOutput(paste0(ns("var.x.comp"), postfix)),
                      uiOutput(paste0(ns("var.y.comp"), postfix)),
                      awesomeCheckbox(paste0(ns("var.names"), postfix), "Feature names", value = FALSE)
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
#' the selected variable tables
#'
#' @return tabpanel
#'
#' @noRd
getSelectedVarsPlot <- function(ns, postfix = ""){
  return(
    tabPanel("Selected features",
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
getCimPlot <- function(ns, postfix = "", yMargin = 5){
  return(
    tabPanel("CIM",
             fluidRow(style = "display: flex; column-gap: 1rem",
               uiOutput(paste0(ns("img.comp"), postfix)),
               numericInput(paste0(ns("xmargin"), postfix), label = "X-Margin:", value = 5, 
                            min = 1, max = 20, step = 1),
               numericInput(paste0(ns("ymargin"), postfix), label = "Y-Margin:", value = yMargin, 
                            min = 1, max = 20, step = 1)
             ),
             fluidRow(
               bs4Dash::column(width = 12,
                               textOutput(paste0(ns("img.error"), postfix)),
                               plotOutput(paste0(ns("Img"), postfix)),
                               downloadButton(paste0(ns("Img.download"), postfix), "Save plot"),
                               downloadButton(paste0(ns("Img.table.download"), postfix), "Save as table")
              )
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
                               plotOutput(paste0(ns("Arrow"), postfix)),
                               downloadButton(paste0(ns("Arrow.download"), postfix), "Save plot"))
    
               )
    )
  )
}

#' @description A utils function to get the tabPanel for
#' the diablo plot
#'
#' @return tabpanel
#'
#' @noRd
getDiabloPlot <- function(ns, postfix = ""){
  return (
    tabPanel("Diablo plot",
             fluidRow(
               uiOutput(paste0(ns("diablo.comp"), postfix))
             ),
             fluidRow(
               bs4Dash::column(width = 12,
                               textOutput(paste0(ns("diablo.error"), postfix)),
                               plotOutput(paste0(ns("Diablo"), postfix)),
                               downloadButton(paste0(ns("Diablo.download"), postfix), "Save plot"))
             )
    )
  )
}

#' @description A utils function to get the tabPanel for
#' the circos plot
#'
#' @return tabpanel
#'
#' @noRd
getCircosPlot <- function(ns, postfix = ""){
  return (
    tabPanel("Circos plot",
             fluidRow(
               bs4Dash::column(width = 3,
                numericInput(paste0(ns("cutoffCircos"), postfix), "Cutoff value",
                            min = 0, max = 1, step = 0.1, value = 0.7)
               ),
               bs4Dash::column(width = 4,
                numericInput(paste0(ns("datasetSizeCircos"), postfix), "Dataset label size",
                            min = 0.1, max = 2, step = 0.1, value = 1.2)
               ),
               bs4Dash::column(width = 4,
                numericInput(paste0(ns("featureSizeCircos"), postfix), "Feature label size",
                            min = 0.1, max = 1, step = 0.1, value = 0.7)
               )
             ),
             fluidRow(
               awesomeCheckbox(paste0(ns("showLine"), postfix), "Show expression line", value = TRUE)
             ),
             fluidRow(
               bs4Dash::column(width = 12,
                               textOutput(paste0(ns("circos.error"), postfix)),
                               plotOutput(paste0(ns("Circos"), postfix)),
                               downloadButton(paste0(ns("Circos.download"), postfix), "Save plot"),
                               downloadButton(paste0(ns("Circos.table.download"), postfix), "Save as table"))
             )
    )
  )
}

#' @description A utils function to get the tabPanel for
#' the network plot
#'
#' @return tabpanel
#'
#' @noRd
getNetworkPlot <- function(ns, postfix = ""){
  return (
    tabPanel("Network",
             fluidRow(style = "column-gap: 1rem",
                      numericInput(paste0(ns("cutoffNetwork"), postfix), "Cutoff value",
                                   min = 0, max = 1, step = 0.1, value = 0.5),
                      uiOutput(paste0(ns("nodes"), postfix)),
                      awesomeCheckbox(paste0(ns("fullName"), postfix), "Show full names")
             ),
             fluidRow(
               bs4Dash::column(width = 12,
                               textOutput(paste0(ns("network.error"), postfix)),
                               visNetworkOutput(paste0(ns("Network"), postfix)),
                               fluidRow(
                                 tags$div(
                                   tags$span(class="line line-blue"),
                                   tags$text("Negative correlation", style="margin-right: .5rem"),
                                   tags$span(class="line line-red"),
                                   tags$text("Positive correlation")
                                 )
                               ),
                               downloadButton(paste0(ns("NetworkGml.download"), postfix), "Save as gml"),
                               downloadButton(paste0(ns("NetworkHtml.download"), postfix), "Save as html")
               )
             )
    )
  )
}

#' @description A utils function to get the tabPanel for
#' the plot with the error rate
#'
#' @return tabpanel
#'
#' @noRd
getErrorRatePlot <- function(ns){
  return(
    tabPanel("Error rates", 
             fluidRow(
               bs4Dash::column(width = 12,
                               plotOutput(ns("ErrorRate")),
                               downloadButton(ns("ErrorRate.download"), "Save plot"))             
             )
    )
  )
}

#' @description A utils function to render the 
#' components for the sample plot
#'
#' @noRd
renderIndivComps <- function(ns, input, output, tuned = FALSE, tunedInput = NULL){
  output$indiv.x.comp <- renderUI({
    selectInput(ns("indiv.x"), "X-axis component:", seq(1, input$ncomp, 1))
  })
  
  output$indiv.y.comp <- renderUI({
    selectInput(ns("indiv.y"), "Y-axis component:", seq(1, input$ncomp, 1), selected = 2)
  })

  if (tuned){
    output$indiv.x.comp.tuned <- renderUI({
      selectInput(ns("indiv.x.tuned"), "X-axis component:", seq(1, tunedInput$ncomp, 1))
    })
    
    output$indiv.y.comp.tuned <- renderUI({
      selectInput(ns("indiv.y.tuned"), "Y-axis component:", seq(1, tunedInput$ncomp, 1), selected = 2)
    })
  }
}

#' @description A utils function to render the 
#' components for the Correlation Circle plot
#'
#' @noRd
renderVarComps <- function(ns, input, output, tuned = FALSE, tunedInput = NULL){
  output$var.x.comp <- renderUI({
    selectInput(ns("var.x"), "X-axis component:", seq(1, input$ncomp, 1))
  })
  
  output$var.y.comp <- renderUI({
    selectInput(ns("var.y"), "Y-axis component:", seq(1, input$ncomp, 1), selected = 2)
  })
  
  if (tuned) {
    output$var.x.comp.tuned <- renderUI({
      selectInput(ns("var.x.tuned"), "X-axis component:", seq(1, tunedInput$ncomp, 1))
    })
    
    output$var.y.comp.tuned <- renderUI({
      selectInput(ns("var.y.tuned"), "Y-axis component:", seq(1, tunedInput$ncomp, 1), selected = 2)
    })
  }
}

#' @description A utils function to render the 
#' component for the loadings plot
#'
#' @noRd
renderLoadComp <- function(ns, input, output, tuned = FALSE, tunedInput = NULL){
  output$load.comp <- renderUI({
    selectInput(ns("load.comp"), "Component:", seq(1, input$ncomp, 1))
  })
  
  if (tuned) {
    output$load.comp.tuned <- renderUI({
      selectInput(ns("load.comp.tuned"), "Component:", seq(1, tunedInput$ncomp, 1))
    })
  }
}

#' @description A utils function to render the 
#' component for the selected variables table
#'
#' @noRd
renderSelVarComp <- function(ns, input, output, tuned = FALSE, tunedInput = NULL){
  output$sel.var.comp <- renderUI({
    selectInput(ns("sel.var.comp"), "Component:", seq(1, input$ncomp, 1))
  })
  
  if (tuned) {
    output$sel.var.comp.tuned <- renderUI({
      selectInput(ns("sel.var.comp.tuned"), "Component:", seq(1, tunedInput$ncomp, 1))
    })
  }
}

#' @description A utils function to render the 
#' component for the img plot
#'
#' @noRd
renderImgComp <- function(ns, input, output, tuned = FALSE, tunedInput = NULL){
  output$img.comp <- renderUI({
    selectInput(ns("img.comp"), "Component:", seq(1, input$ncomp, 1))
  })
  
  if (tuned){
    output$img.comp.tuned <- renderUI({
      selectInput(ns("img.comp.tuned"), "Component:", seq(1, tunedInput$ncomp, 1))
    })
  } 
}

#' @description A utils function returns the
#' error message by calling the getShinyErrorAlert
#' function with a predfined text
#'
#' @noRd
getErrorMessage <- function(error, trim = TRUE){
  errorMsg = error
  if (trim && nchar(error) > 100){
    errorMsg = paste(substr(error, 0, 100), "...")
  }
  
  return (
    getShinyErrorAlert(paste0("<p>The following error appeared while trying to tune the parameters:</p>
                        <p><code>", errorMsg,"</code></p> <p>If the error keeps appearing please report it.</p>"), 
                       TRUE)
    
  )
}

#' @description A utils function that returns
#' a shiny alert of the given type with the given text
#'
#' @noRd
getShinyAlert <- function(title, text, type, html){
  shinyalert::shinyalert(title, text, type = type, html = html)
}

#' @description A utils function that returns
#' a shiny alert of the type error with a 
#' custom message
#'
#' @noRd
getShinyErrorAlert <- function(message, html = FALSE){
  getShinyAlert("Error!", message, "error", html)
}

#' @description A utils function that returns
#' a shiny alert of the type warning with a 
#' custom message
#'
#' @noRd
getShinyWarningAlert <- function(message, html = FALSE){
  getShinyAlert("Warning!", message, "warning", html)
}