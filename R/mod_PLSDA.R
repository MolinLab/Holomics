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
    getAnalysisParametersComponent(ns)
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
                                               downloadButton(ns("Filter.download"), "Filter by loadings"),
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
mod_PLSDA_server <- function(id, dataset, classes, multiDataset){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    render_plsda_ui_components(ns, input, output, dataset)
    
    generate_plsda_plots(ns, input, output, dataset, classes, multiDataset)
    
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
generate_plsda_plots <- function(ns, input, output, dataset, classes, multiDataset){
  #' Create reactive values
  comp.indiv <- getCompIndivReactive(input)
  comp.var <- getCompVarReactive(input)
  
  #' run analysis
  result <- reactive({
    req(dataset$data$filtered)
    req(classes$data)
    req(nrow(classes$data) == nrow(dataset$data$filtered))
    result <- mixOmics::plsda(dataset$data$filtered, Y = classes$data[,1],
                                    ncomp = input$ncomp , scale = input$scale)
  })
  
  #' plot functions
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
    if (ncol(classes$data) == 2){
      colors = getGroupColors(classes$data)
      plotLoadings(result(), as.numeric(input$load.comp),
                   contrib = input$load.cont, method = input$load.method, legend.color = colors)
    } else {
      plotLoadings(result(), as.numeric(input$load.comp),
                   contrib = input$load.cont, method = input$load.method)    
    }
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
  
  #' Filter function
  filterByLoadings <- function(){
    req(dataset$data$filtered)
    req(classes$data)
    
    Y <- classes$data[,1]
    
    result <- mixOmics::plsda(dataset$data$filtered, Y = Y,
                              ncomp = input$ncomp , scale = input$scale)
    grid.keepX <- getTextKeepX(ncol(dataset$data$filtered))
    set.seed(30)
    tune.splsda.result <- mixOmics::tune.splsda(dataset$data$filtered,Y = Y, ncomp = input$ncomp,
                                      test.keepX = grid.keepX, scale = input$scale,
                                      validation = c('Mfold'),
                                      folds = min(table(Y)),
                                      dist = 'max.dist',
                                      nrepeat = 50,
                                      progressBar = TRUE)
    ncomp <- tune.splsda.result$choice.ncomp$ncomp
    keepX <- tune.splsda.result$choice.keepX[1:ncomp]

    splsda.result <- mixOmics::splsda(dataset$data$filtered, Y = Y, ncomp = ncomp, keepX = keepX, scale = input$scale)

    sel_feature <- c()
    for (comp in 1:ncomp){
      loadings <- mixOmics::plotLoadings(splsda.result, comp = comp, method = 'mean', contrib = 'max')
      sel_feature <- c(sel_feature, rownames(loadings))
    }
    
    feature_cols <- (names(dataset$data$unfiltered) %in% sel_feature)
    result <- dataset$data$unfiltered[, feature_cols]
    multiDataset$data[[paste0(dataset$name, "_small")]] <- list(filtered = result, unfiltered = result)
    
    return(result)
  }
  
  #' Download handler
  output$Indiv.download <- getDownloadHandler("PLS-DA_Sampleplot.png", plot.indiv)
  output$Var.download <- getDownloadHandler("PLS-DA_Variableplot.png", plot.var)
  output$Load.download <- getDownloadHandler("PLS-DA_Loadingsplot.png", plot.load, width = 2592, height = 1944)
  output$SelVar.download <- getDownloadHandler("PLS-DA_SelectedVariables.csv", table.selVar, type = "csv")
  output$Filter.download <- getDownloadHandler(paste0(dataset$name, "_small.xlsx"), filterByLoadings, type = "xlsx")
}