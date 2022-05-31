#' PCA UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_PCA_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      getAnalysisParametersComponent(ns, TRUE)
    ),
    fluidRow(
      bs4Dash::tabBox(
        width = 12, collapsible = FALSE,
        getSamplePlot(ns),
        getVariablePlot(ns),
        getLoadingsPlot(ns),
        getSelectedVarsPlot(ns),
        tabPanel("Scree plot",       
                 bs4Dash::column(width = 12,
                                 plotOutput(ns("Scree")), 
                                 downloadButton(ns("Scree.download"), "Save plot"))
        )
      )
    )
  )
}

#' PCA Server Functions
#'
#' @noRd 
mod_PCA_server <- function(id, dataset){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    render_pca_ui_components(ns, input, output, dataset)
    
    generate_pca_plots(ns, input, output, dataset)
    
  })
}

#' Render Ui functions
render_pca_ui_components <- function(ns, input, output, dataset){
  output$indiv.x.comp <- renderUI({
    selectInput(ns("indiv.x"), "X-Axis component:", seq(1, input$ncomp, 1))
  })
  
  output$indiv.y.comp <- renderUI({
    selectInput(ns("indiv.y"), "Y-Axis component:", seq(1, input$ncomp, 1), selected = 2)
  })
  
  output$var.x.comp <- renderUI({
    selectInput(ns("var.x"), "X-Axis component:", seq(1, input$ncomp, 1))
  })
  
  output$var.y.comp <- renderUI({
    selectInput(ns("var.y"), "Y-Axis component:", seq(1, input$ncomp, 1), selected = 2)
  })
  
  output$load.comp <- renderUI({
    selectInput(ns("load.comp"), "Component:", seq(1, input$ncomp, 1))
  })
  
  output$sel.var.comp <- renderUI({
    selectInput(ns("sel.var.comp"), "Component:", seq(1, input$ncomp, 1))
  })
}

#' Business logic functions
generate_pca_plots <- function(ns, input, output, dataset){
  #' Create reactive values
  comp.var <- reactive({
    req(input$var.x)
    req(input$var.y)
    comp.var <- as.numeric(c(input$var.x,input$var.y))
  })
  
  comp.indiv <- reactive({ 
    req(input$indiv.x)
    req(input$indiv.y)
    comp.indiv <- as.numeric(c(input$indiv.x,input$indiv.y))
  })
  
  #' run analysis
  result <- reactive({
    result <- mixOmics::pca(dataset$data, ncomp = input$ncomp,
                                logratio = input$logratio, scale = input$scale)
  })
  
  #' plot functions
  plot.scree <- function() {
    plot(result())
  }
  
  plot.indiv <- function(){
    mixOmics::plotIndiv(result(), comp = comp.indiv(), 
                        group = sampleClasses, ind.names = input$indiv.names,
                        legend = TRUE, legend.title = "Storability classes")
  }
  
  plot.var <- function(){
    mixOmics::plotVar(result(), comp = comp.var(),
                      var.names = input$var.names)
  }
  
  plot.load <- function(){
    req(input$load.comp)
    mixOmics::plotLoadings(result(), comp = as.numeric(input$load.comp))
  }
  
  table.selVar <- function(){
    req(input$sel.var.comp)
    selVar <- mixOmics::selectVar(result(), comp = as.numeric(input$sel.var.comp))
    listsToMatrix(selVar$name, selVar$value, c("name", "value"))
  }
  
  #'output plots
  #' Sample plot
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
  
  #'Selected Variables Tables
  output$Sel.Var <- DT::renderDataTable(
    table.selVar()
  )
  
  #' Scree plot
  output$Scree <- renderPlot(
    plot.scree()
  )
  
  #' Download handler
  output$Indiv.download <- getDownloadHandler("PCA_Sampleplot.png", plot.indiv)
  output$Var.download <- getDownloadHandler("PCA_Variableplot.png", plot.var)
  output$Load.download <- getDownloadHandler("PCA_Loadingsplot.png", plot.load, width = 2592, height = 1944)
  output$SelVar.download <- getDownloadHandler("PCA_SelectedVariables.csv", table.selVar, type = "csv")
  output$Scree.download <- getDownloadHandler("PCA_Screeplot.png", plot.scree)
}