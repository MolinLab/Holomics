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
      getAnalysisParametersComponent(ns)
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
mod_PCA_server <- function(id, dataset, classes){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    render_pca_ui_components(ns, input, output, dataset)
    
    generate_pca_plots(ns, input, output, dataset, classes)
    
  })
}

#' Render Ui functions
render_pca_ui_components <- function(ns, input, output, dataset){
  
  renderIndivComps(ns, input, output)
  
  renderVarComps(ns, input, output)
  
  renderLoadComp(ns, input, output)
  
  renderSelVarComp(ns, input, output)
}

#' Business logic functions
generate_pca_plots <- function(ns, input, output, dataset, classes){
  #' Create reactive values
  comp.indiv <- getCompIndivReactive(input)
  comp.var <- getCompVarReactive(input)
  
  #' run analysis
  result <- reactive({
    req(dataset$data$filtered)
    req(nrow(classes$data) == nrow(dataset$data$filtered))
    result <- mixOmics::pca(dataset$data$filtered, ncomp = input$ncomp,
                          scale = input$scale)
  })
  
  #' plot functions
  plot.scree <- function() {
    plot(result())
  }
  
  plot.indiv <- function(){
    req(classes$data)
    title = colnames(classes$data)[1]
    if (ncol(classes$data) == 2){
      colors = getGroupColors(classes$data)
      plotIndiv(result(), classes$data[,1], title, comp.indiv(), indNames = input$indiv.names, col.per.group = colors)
    } else {
      plotIndiv(result(), classes$data[,1], title, comp.indiv(), indNames = input$indiv.names)
    }
  }
  
  plot.var <- function(){
    plotVar(result(), comp.var(), input$var.names)
  }
  
  plot.load <- function(){
    req(input$load.comp)
    plotLoadings(result(), as.numeric(input$load.comp))
  }
  
  table.selVar <- function(){
    req(input$sel.var.comp)
    selectVar(result(), as.numeric(input$sel.var.comp))
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