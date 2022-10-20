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
        getScreePlot(ns),
        getSamplePlot(ns),
        getVariablePlot(ns),
        getLoadingsPlot(ns),
        getSelectedVarsPlot(ns),
        tabPanel("Filtering for multi-omics",
                 fluidRow(style = "display: flex; column-gap: 1rem",
                          downloadButton(ns("Filter.download"), "Filter by loadings"),
                          textOutput(ns("Var.filtered"))
                 ),
                 tags$hr(),
                 fluidRow(style = "display: flex; column-gap: 1rem",
                          uiOutput(ns("indiv.x.comp.filtered")),
                          uiOutput(ns("indiv.y.comp.filtered")),
                          uiOutput(ns("names.filtered"))
                 ),
                 fluidRow(
                   bs4Dash::column(width = 12,
                                   plotOutput(ns("Indiv.filtered")),
                                   uiOutput(ns("indiv.filtered.button"))
                   )
                 )
        )
      )
    )
  )
}

#' PCA Server Functions
#'
#' @noRd 
mod_PCA_server <- function(id, dataset, classes, multiDataset, tables){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    render_pca_ui_components(ns, input, output, dataset)
    
    generate_pca_plots(ns, input, output, dataset, classes, multiDataset, tables)
    
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
generate_pca_plots <- function(ns, input, output, dataset, classes, multiDataset, tables){
  #' Create reactive values
  comp.indiv <- getCompIndivReactive(input)
  comp.var <- getCompVarReactive(input)
  
  #' run analysis
  result <- reactive({
    req(dataset$data$filtered)
    req(nrow(classes$data) == nrow(dataset$data$filtered))
    req(identical(classes$data[,1], rownames(dataset$data$filtered)))
    
    msg <- checkDataNcompCompatibility(dataset$data$filtered, input$ncomp) 
    output$parameters.error <- renderText(msg)
    
    if(msg == ""){
      tryCatch({
        result <- mixOmics::pca(dataset$data$filtered, ncomp = input$ncomp,
                                scale = input$scale)
      }, error = function(cond){
        getErrorMessage(cond)
        result <- NULL
      })
    } else {
      result <- NULL
    }

  })
  
  #' plot functions
  plot.scree <- function() {
    if(!is.null(result())){
      plot(result())
    }
  }
  
  plot.indiv <- function(result, comp.indiv, indNames){
    if(!is.null(result)){
      req(classes$data)
      title = colnames(classes$data)[2]
      if (ncol(classes$data) == 3){
        colors = getGroupColors(classes$data)
        plotIndiv(result, classes$data[,2], title, comp.indiv, indNames = indNames, col.per.group = colors)
      } else {
        plotIndiv(result, classes$data[,2], title, comp.indiv, indNames = indNames)
      }
    }
  }
  
  plot.var <- function(){
    if(!is.null(result())){
      plotVar(result(), comp.var(), input$var.names)
    }
  }
  
  plot.load <- function(){
    if(!is.null(result())){
      req(input$load.comp)
      plotLoadings(result(), as.numeric(input$load.comp))
    }
  }
  
  table.selVar <- function(){
    if(!is.null(result())){
      req(input$sel.var.comp)
      selectVar(result(), as.numeric(input$sel.var.comp))
    }
  }
  
  #'output plots
   #' Scree plot
  output$Scree <- renderPlot(
    plot.scree()
  )
  
  #' Sample plot
  output$Indiv <- renderPlot(
    plot.indiv(result(), comp.indiv(), input$indiv.names)
  )
  
  #' Correlation Circle plot
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
  
  #' Observe dataset change
  observeEvent(dataset$data, {
    output$indiv.x.comp.filtered <- renderUI("")
    
    output$indiv.y.comp.filtered <- renderUI("")
    
    output$names.filtered <- renderUI("")
    
    output$indiv.filtered.button <- renderUI("")
    
    output$Indiv.filtered <- renderPlot(
      plot.indiv(NULL)
    )
    
    output$Var.filtered <- renderText("")
    
  })
  
  #' Filter function
  filterByLoadings <- function(output){
    req(dataset$data$filtered)

    withProgress(message = 'Filtering the dataset ... Please wait!', value = 1/3, {
      
      set.seed(30)
      
      tune <- mixOmics::tune.pca(dataset$data$filtered, scale = input$scale)
      ncomp <- min(which(tune$cum.var > 0.8))
      
      grid.keepX <- getTestKeepX(ncol(dataset$data$filtered))
      folds <- floor(nrow(dataset$data$filtered)/3)
      
      BPPARAM <- BiocParallel::SnowParam(workers = max(parallel::detectCores()-1, 2))
      tune.spca.result <- mixOmics::tune.spca(dataset$data$filtered, ncomp = ncomp,
                                             test.keepX = grid.keepX, scale = input$scale,
                                             nrepeat = 5, folds = folds)
      
      keepX <- tune.spca.result$choice.keepX[1:ncomp]
      
      incProgress(1/3)
      
      spca.result <- mixOmics::spca(dataset$data$filtered, ncomp = ncomp, keepX = tune.spca.result$choice.keepX[1:ncomp])
      
      sel_feature <- c()
      for (comp in 1:ncomp){
        loadings <- mixOmics::selectVar(spca.result, comp = comp)$name
        sel_feature <- c(sel_feature, loadings)
      }
      
      feature_cols <- (names(dataset$data$unfiltered) %in% sel_feature)
      result <- dataset$data$unfiltered[, feature_cols]
      multiDataset$data[[paste0(dataset$name, "_pca_filtered")]] <- list(filtered = result, unfiltered = result)
      
      # extend table in upload with available datasets
      tables$data <- extendDataTable(tables$data, paste0(dataset$name, "_pca_filtered"), "-", nrow(result), ncol(result),
                                     FALSE, "multi")
      
      incProgress(1/3)
    })
    
    output$Var.filtered <- renderText(sprintf("Number of resulting variables: %s", ncol(result)))
    
    output$indiv.x.comp.filtered <- renderUI({
      selectInput(ns("indiv.x.filtered"), "X-Axis component:", seq(1, ncomp, 1))
    })
    
    output$indiv.y.comp.filtered <- renderUI({
      selectInput(ns("indiv.y.filtered"), "Y-Axis component:", seq(1, ncomp, 1), selected = 2)
    })
    
    comp.indiv.filtered <- reactive({
      req(input$indiv.x.filtered)
      req(input$indiv.y.filtered)
      as.numeric(c(input$indiv.x.filtered,input$indiv.y.filtered))
    })
    
    output$names.filtered <- renderUI({
      awesomeCheckbox(ns("indiv.names.filtered"), "Sample names", value = FALSE)
    })
    
    output$Indiv.filtered <- renderPlot(
      plot.indiv(spca.result, comp.indiv.filtered(), input$indiv.names.filtered)
    )
    
    output$indiv.filtered.button <- renderUI({
      downloadButton(ns("Indiv.filtered.download"), "Save plot")             
    })  
    
    output$Indiv.filtered.download <- downloadHandler(
      filename = "PCA_filtered_Sampleplot.png",
      content = function(file){
        png(file, 1800, 1200, res = 300)
        plot.indiv(spca.result, comp.indiv.filtered(), input$indiv.names.filtered)
        dev.off()
      }
    )
    
    return(result)
  }
  
  dataName <- reactive({
    dataset$name 
  })

  #' Download handler
  output$Indiv.download <- getDownloadHandler("PCA_Sampleplot.png", plot.indiv)
  output$Var.download <- getDownloadHandler("PCA_Variableplot.png", plot.var)
  output$Load.download <- getDownloadHandler("PCA_Loadingsplot.png", plot.load, width = 2592, height = 1944)
  output$SelVar.download <- getDownloadHandler("PCA_SelectedVariables.csv", table.selVar, type = "csv")
  output$Scree.download <- getDownloadHandler("PCA_Screeplot.png", plot.scree)
  output$Filter.download <- downloadHandler(
    filename = function() {
      paste0(dataName(), "_pca_filtered.xlsx")
    },
    content = function(file){
      df <- filterByLoadings(output)
      openxlsx::write.xlsx(df, file, rowNames = TRUE, colNames = TRUE)
    }
  )
}