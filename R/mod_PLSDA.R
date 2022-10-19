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
  tagList(    
    fluidRow(
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
                                                 downloadButton(ns("Load.download"), "Save plot"))
                               )
                      ),
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

#' PLSDA Server Functions
#'
#' @noRd 
mod_PLSDA_server <- function(id, dataset, classes, multiDataset, tables){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    render_plsda_ui_components(ns, input, output, dataset)
    
    generate_plsda_plots(ns, input, output, dataset, classes, multiDataset, tables)
    
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
generate_plsda_plots <- function(ns, input, output, dataset, classes, multiDataset, tables){
  #' Create reactive values
  comp.indiv <- getCompIndivReactive(input)
  comp.var <- getCompVarReactive(input)
  
  #' run analysis
  result <- reactive({
    req(dataset$data$filtered)
    req(classes$data)
    req(nrow(classes$data) == nrow(dataset$data$filtered))
    req(identical(classes$data[,1], rownames(dataset$data$filtered)))
    
    msg <- checkDataNcompCompatibility(dataset$data$filtered, input$ncomp) 
    output$parameters.error <- renderText(msg)
    
    if(msg == ""){
      tryCatch({
        result <- mixOmics::plsda(dataset$data$filtered, Y = classes$data[,2],
                                  ncomp = input$ncomp , scale = input$scale)
      }, error = function(cond){
        getErrorMessage(cond)
        result <- NULL
      })
    } else {
      result <- NULL
    }
  })
  
  #' plot functions
  plot.indiv <- function(result, comp.indiv, indNames){
    req(classes$data)
    if (!is.null(result)){
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
    if (!is.null(result())){
      plotVar(result(), comp.var(), input$var.names)
    }
  }
  
  plot.load <- function(){
    req(classes$data)
    req(input$load.comp)
    if (!is.null(result())){
      if (ncol(classes$data) == 3){
        colors = getGroupColors(classes$data)
        plotLoadings(result(), as.numeric(input$load.comp),
                     contrib = input$load.cont, method = input$load.method, legend.color = colors)
      } else {
        plotLoadings(result(), as.numeric(input$load.comp),
                     contrib = input$load.cont, method = input$load.method)    
      }
    }
  }
  
  table.selVar <- function(){
    req(input$sel.var.comp)
    if (!is.null(result())){
      selectVar(result(), as.numeric(input$sel.var.comp))
    }
  }
  
  #'Sample plot
  output$Indiv <- renderPlot(
    plot.indiv(result(), comp.indiv(), input$indiv.names)
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
    req(classes$data)
    
    withProgress(message = 'Filtering the dataset ... Please wait!', value = 1/3, {
        
      Y <- classes$data[,2]
      result <- mixOmics::plsda(dataset$data$filtered, Y = Y,
                                ncomp = input$ncomp , scale = input$scale)
      grid.keepX <- getTestKeepX(ncol(dataset$data$filtered))
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
      
      incProgress(1/3)
  
      splsda.result <- mixOmics::splsda(dataset$data$filtered, Y = Y, ncomp = ncomp, keepX = keepX, scale = input$scale)
  
      sel_feature <- c()
      for (comp in 1:ncomp){
        loadings <- mixOmics::plotLoadings(splsda.result, comp = comp, method = 'mean', contrib = 'max')
        sel_feature <- c(sel_feature, rownames(loadings))
      }
      
      feature_cols <- (names(dataset$data$unfiltered) %in% sel_feature)
      result <- dataset$data$unfiltered[, feature_cols]
      multiDataset$data[[paste0(dataset$name, "_plsda_filtered")]] <- list(filtered = result, unfiltered = result)
      
      tables$data <- extendDataTable(tables$data, paste0(dataset$name, "_plsda_filtered"), "-", nrow(result), ncol(result),
                                     FALSE, "multi")
      
      incProgress(1/3)
    })
    
    output$Var.filtered <- renderText(sprintf("Number of resulting variables: %s", length(sel_feature)))
    
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
      plot.indiv(splsda.result, comp.indiv.filtered(), input$indiv.names.filtered)
    )
    
    output$indiv.filtered.button <- renderUI({
      downloadButton(ns("Indiv.filtered.download"), "Save plot")             
    })  
    
    output$Indiv.filtered.download <- downloadHandler(
      filename = "PLS-DA_filtered_Sampleplot.png",
      content = function(file){
        png(file, 1800, 1200, res = 300)
        plot.indiv(splsda.result, comp.indiv.filtered(), input$indiv.names.filtered)
        dev.off()
      }
    )
    
    return(result)
  }
  
  dataName <- reactive({
    dataset$name 
  })
  
  #' Download handler
  output$Indiv.download <- getDownloadHandler("PLS-DA_Sampleplot.png", plot.indiv)
  output$Var.download <- getDownloadHandler("PLS-DA_Variableplot.png", plot.var)
  output$Load.download <- getDownloadHandler("PLS-DA_Loadingsplot.png", plot.load, width = 2592, height = 1944)
  output$SelVar.download <- getDownloadHandler("PLS-DA_SelectedVariables.csv", table.selVar, type = "csv")
  output$Filter.download <- downloadHandler(
    filename = function() {
      paste0(dataName(), "_plsda_filtered.xlsx")
    },
    content = function(file){
        df <- filterByLoadings(output)
        openxlsx::write.xlsx(df, file, rowNames = TRUE, colNames = TRUE)
    }
  )
}