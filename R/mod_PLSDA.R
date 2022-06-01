#' PLSDA UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_PLSDA_ui <- function(id){
  ns <- NS(id)
  tagList(    fluidRow(
    getAnalysisParametersComponent(ns, TRUE)
  ),
  fluidRow(
    bs4Dash::tabBox(width = 12, collapsible = FALSE,
                    getSamplePlot(ns),
                    getVariablePlot(ns),
                    tabPanel("Loading plot",
                             fluidRow(
                               bs4Dash::column(width = 12,
                                               uiOutput(ns("load.comp")),
                                               selectInput(ns("load.cont"), "Contribution:", width = "100",
                                                           choices = c("minimal" = "min", "maximal" = "max")),
                                               selectInput(ns("load.method"), "Method:", width = "100",
                                                           choices = c("mean" = "mean", "median" = "median")),
                                               style = "display: flex; column-gap: 1rem"             
                               )
                             ),
                             fluidRow(
                               bs4Dash::column(width = 12,
                                               plotOutput(ns("Load")),
                                               downloadButton(ns("Load.download"), "Save plot"))
                             )
                    ),
                    getSelectedVarsPlot(ns)
    )
  )
  )
}

#' PLSDA Server Functions
#'
#' @noRd 
mod_PLSDA_server <- function(id, dataset){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    render_plsda_ui_components(ns, input, output, dataset)
    
    generate_plsda_plots(ns, input, output, dataset)
    
  })
}

#' Render Ui functions
render_plsda_ui_components <- function(ns, input, output, dataset){
  renderIndivComps(ns, input, output)
  
  renderVarComps(ns, input, output)
  
  renderLoadComp(ns, input, output)
  
  renderSelVarComp(ns, input, output)
}

#' Business logic functions
generate_plsda_plots <- function(ns, input, output, dataset){
  #' Create reactive values
  comp.indiv <- getCompIndivReactive(input)
  comp.var <- getCompVarReactive(input)
  
  #' run analysis
  result <- reactive({
    result <- mixOmics::plsda(dataset$data, Y = sampleClasses,
                                    ncomp = input$ncomp ,logratio = input$logratio, 
                                    scale = input$scale)
  })
  
  #' plot functions
  plot.indiv <- function(){
    plotIndiv(result(), comp.indiv(), indNames = input$indiv.names)
  }
  
  plot.var <- function(){
    plotVar(result(), comp.var(), input$var.names)
  }
  
  plot.load <- function(){
    req(input$load.comp)
    plotLoadings(result(), as.numeric(input$load.comp),
                 contrib = input$load.cont, method = input$load.method)
  }
  
  table.selVar <- function(){
    req(input$sel.var.comp)
    selectVar(result(), as.numeric(input$sel.var.comp))
  }
  
  #'Sample plot
  output$Indiv <- renderPlot(
    plot.indiv()
  ) 
  
  #' Variable plot
  output$Var <- renderPlot(
    plot.var()
  )
  
  #' Loading plot
  output$Load <- renderPlot( 
    plot.load()
  )
  
  #' Selected Variables Tables
  output$Sel.Var <- DT::renderDataTable(
    table.selVar()
  )
  
  
  #' Download handler
  output$Indiv.download <- getDownloadHandler("PLS-DA_Sampleplot.png", plot.indiv)
  output$Var.download <- getDownloadHandler("PLS-DA_Variableplot.png", plot.var)
  output$Load.download <- getDownloadHandler("PLS-DA_Loadingsplot.png", plot.load, width = 2592, height = 1944)
  output$SelVar.download <- getDownloadHandler("PLS-DA_SelectedVariables.csv", table.selVar, type = "csv")
}