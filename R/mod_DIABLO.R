#' DIABLO UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_DIABLO_ui <- function(id){
  ns <- NS(id)
  tagList(
    shinybusy::add_busy_spinner(spin = "circle", position = "bottom-right", height = "60px", width = "60px"),
    fluidRow(
      bs4Dash::column(width = 12, style = "display: flex; column-gap: 1rem",
                      uiOutput(ns("dataSelComp")),
                      uiOutput(ns("classSelComp")),
                      textOutput(ns("errorMsg"))
      )
    ),
    fluidRow(
      bs4Dash::column(width = 5,
                      fluidRow(style = "padding-left: 7.5px;",
                               h1("DIABLO"),
                      ),
                      fluidRow(
                               bs4Dash::box(title = "General information", width = 12, collapsed = TRUE,
                                            htmlOutput(ns("DIABLOinfotext"))
                                            )
                      ),
                      fluidRow(width = 12,
                               getAnalysisParametersComponent(ns,
                                                              bs4Dash::column(width = 4,
                                                                              fluidRow(
                                                                                tags$label("Design matrix value"),
                                                                                getTooltip(ns("matrix-info"), "Correlation value that indicates how strong
                                                                                           the connection between the datasets is")
                                                                              ),
                                                                              fluidRow(
                                                                                uiOutput(ns("matrix.comp")),
                                                                              )
                                                              ), collapsed = F
                              )
                      ),
                      fluidRow(width = 12,
                               bs4Dash::tabBox(width = 12, collapsible = FALSE,
                                               tabPanel("Pairwise correlations", 
                                                        fluidRow(
                                                          HTML("Below are all the pairwise correlations. <br>
                                                          According to these you can set the value of the design matrix. <br>")   
                                                        ),
                                                        fluidRow(
                                                          htmlOutput(ns("pairwiseCorr"))            
                                                        )
                                               ),
                                               getSamplePlot(ns),
                                               getVariablePlot(ns),
                                               getLoadingsPlot(ns),
                                               getCimPlot(ns, yMargin = 20),
                                               getArrowPlot(ns),
                                               getDiabloPlot(ns),
                                               getCircosPlot(ns), 
                                               getNetworkPlot(ns)
                               )
                      )
      ),
      bs4Dash::column(width = 2,
                      getTuneBox(ns, "Tune parameters", "Automatically calculate the optimal number of components 
                                     and the number of features per component.")
      ),
      bs4Dash::column(id = ns("tunedCol"), width = 5,
                      fluidRow(style = "padding-left: 7.5px;",
                               h1("DIABLO tuned"),
                      ),
                      fluidRow(
                               bs4Dash::box(title = "General information", width = 12, collapsed = TRUE,
                                            htmlOutput(ns("DIABLOtunedinfotext"))
                               )
                      ),
                      fluidRow(width = 12,
                               getTunedParametersComponent(ns)
                      ),
                      fluidRow(width = 12,
                               bs4Dash::tabBox(width = 12, collapsible = FALSE,
                                               getErrorRatePlot(ns),
                                               getSamplePlot(ns, ".tuned"),
                                               getVariablePlot(ns, ".tuned"),
                                               getLoadingsPlot(ns, ".tuned"),
                                               getCimPlot(ns, ".tuned", yMargin = 20),
                                               getArrowPlot(ns, ".tuned"),
                                               getDiabloPlot(ns, ".tuned"),
                                               getCircosPlot(ns, ".tuned"), 
                                               getNetworkPlot(ns, ".tuned")
                               )
                      )
      )
    )
  )
}

#' DIABLO Server Functions
#'
#' @noRd
mod_DIABLO_server <- function(id, data, classes){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    shinyjs::hide("tunedCol")

    dataSelection <- reactiveValues()
    classSelection <- reactiveValues()
    
    nodes <- reactiveValues()
    nodesTuned <- reactiveValues()
    
    useTunedVals <- reactiveVal(FALSE)
    tunedVals <- reactiveValues(ncomp = 2, keepX = NULL, scale = F, matrix = NULL)
    
    results <- run_diablo_analysis(ns, input, output, dataSelection, classSelection, useTunedVals, tunedVals)
    
    render_diablo_ui_components(ns, input, output, dataSelection, tunedVals, nodes, nodesTuned)
    
    observe_diablo_ui_components(ns, session, input, output, data, dataSelection, classes, classSelection, results$result, useTunedVals, tunedVals)
    
    generate_diablo_plots(ns, input, output, dataSelection, classSelection, results$result, results$resultTuned, tunedVals, nodes, nodesTuned)
    
    generate_diablo_error_messages(input, output, data, classes, dataSelection, classSelection, tunedVals)
    
    render_diablo_infotexts(output)
  })
}

#' Render Ui components
#' @noRd
render_diablo_ui_components <- function(ns, input, output, dataSelection, tunedVals, nodes, nodesTuned){
  renderIndivComps(ns, input, output, TRUE, tunedVals)

  renderVarComps(ns, input, output, TRUE, tunedVals)
  
  renderLoadComp(ns, input, output, TRUE, tunedVals)
  
  renderSelVarComp(ns, input, output, TRUE, tunedVals)
  
  renderImgComp(ns, input, output, TRUE, tunedVals)
  
  output$diablo.comp <- renderUI({
    selectInput(ns("diablo.comp"), "Component:", seq(1, input$ncomp, 1))
  })
  
  output$nodes <- renderUI({
    req(nodes$data)
    selectInput(ns("nodeNames"), "Select a node", 
                choices = c("---" = "null", combineLists(nodes$data["label"], nodes$data["id"])))
  })
  
  #tuned
  output$diablo.comp.tuned <- renderUI({
    selectizeInput(ns("diablo.comp.tuned"), "Component:", seq(1, tunedVals$ncomp, 1))
  })
  
  output$nodes.tuned <- renderUI({
    req(nodesTuned$data)
    selectizeInput(ns("nodeNames.tuned"), "Select a node", 
                   choices = c("---" = "null", combineLists(nodesTuned$data["label"], nodesTuned$data["id"])))
  })
}

#' Observe different ui components
#' @noRd
observe_diablo_ui_components <- function(ns, session, input, output, data, dataSelection, classes, classSelection, result, useTunedVals, tunedVals){
  #Observe data input change
  observeEvent(data$data, {
    output$dataSelComp <- renderUI({
      choices <- generateChoices(data$data)
      getSelectionComponent(ns("dataSelection"), "Select the datasets: ", choices = choices, multiple = TRUE)
    })
  })
  
  observeEvent(input$dataSelection, {
    dataList <- list()
    token <- strsplit(input$dataSelection, "\t")
    namesList <- lapply(token, function(t) data$data[[t]]$name)
    names <- make.unique(unlist(namesList), sep = " ")
    for (i in 1:length(token)){
      orgName <- token[[i]]
      dataList[[names[[i]]]] <- data$data[[orgName]]$omicsData
    }
    dataSelection$data <- dataList
  })
  
  #Observe class input change
  observeEvent(classes$data, {
    output$classSelComp <- renderUI({
      choices <- generateChoices(classes$data)
      choice = ""
      if (length(choices) != 0L) 
        choice <- choices[[1]]
      
      selection <- isolate(input$classSelection)
      
      #same choice name as before but the data is not necessarily the same!
      #observeEvent of input$dataSelection will not be triggered
      if(!is.null(selection) && selection == choice){
        classSelection$data <- classes$data[[choice]]
      }
      
      getSelectionComponent(ns("classSelection"), "Select classes/labels:", choices = choices, width = "fit-content")
    })
  })
  
  observeEvent(input$classSelection, {
    classSelection$data <- classes$data[[input$classSelection]]
  })
  
  #Observe data change
  observeEvent(dataSelection$data, {
    useTunedVals(FALSE)
    shinyjs::hide("tunedCol")
    
    if (length(dataSelection$data) > 1){
      result <- pairwiseCorrelation(dataSelection$data)
      if (!is.null(result)){
        output$matrix.comp <- renderUI({
          value <- round(min(unlist(lapply(result, function(i) as.numeric(i[[1]])))), 1)
          numericInput(ns("matrix"), label = "", value = value, 
                       min = 0, max = 1, step = 0.1)
        })
        
        output$pairwiseCorr <- renderText({
          text = ""
          for (i in 1:length(result)){
            r = result[[i]]
            text = paste0(text , paste0("Correlation between datasets '", r[2], "' and '", r[3], "': ", round(as.numeric(r[1]), 2)), " </br> ")
          }
          HTML(text)
        })
      }
    }
  })
  
  #Observe node name selection
  observeEvent(input$nodeNames, {
    if (input$nodeNames == "null"){
      visNetworkProxy(ns("Network")) %>%
        visUnselectAll()
    }else{
      visNetworkProxy(ns("Network")) %>%
        visSetSelection(nodesId = input$nodeNames)
    }
    shinyjs::runjs(paste0("Shiny.setInputValue('", ns('clicked_node'), "', '", input$nodeNames, "')"))
  })
  
  observeEvent(input$nodeNames.tuned, {
    if (input$nodeNames.tuned == "null"){
      visNetworkProxy(ns("Network.tuned")) %>%
        visUnselectAll()
    }else{
      visNetworkProxy(ns("Network.tuned")) %>%
        visSetSelection(nodesId = input$nodeNames.tuned)
    }
    shinyjs::runjs(paste0("Shiny.setInputValue('", ns('clicked_node.tuned'), "', '", input$nodeNames.tuned, "')"))
  })
  
  # Observe node clicked
  observeEvent(input$clicked_node, {
    updateSelectizeInput(session, "nodeNames", selected = input$clicked_node)
  })
  
  observeEvent(input$clicked_node.tuned, {
    updateSelectizeInput(session, "nodeNames.tuned", selected = input$clicked_node.tuned)
  })
  
  # Observe tune button
  observeEvent(input$tune, {
    if (!is.null(input$dataSelection)){
      tryCatch({
        tune_diablo_values(dataSelection, classSelection, result, tunedVals, input, output)
        if (!is.null(tunedVals$ncomp)){
          shinyjs::show("tunedCol")
          useTunedVals(T)
        }
      }, error = function(cond){
        getErrorMessage(cond, trim = F)
      })
    }
  })
}

#' Tune the ncomp and keepX parameter for the given dataset
#' @noRd
tune_diablo_values <- function(dataSelection, classSelection, result, tunedVals, input, output){
  X <- dataSelection$data
  if (!is.null(X)){
    error <- F
    withProgress(message = 'Tuning parameters .... Please wait!', value = 1/3, {
      Y <- classSelection$data[,2]
      design <- matrix(input$matrix, ncol = length(X), nrow = length(X),
                       dimnames = list(names(X), names(X)))
      diag(design) <- 0
      
      result <- result()

      #tune ncomp
      finished <- F
      while (!finished && !error){
        dataName <- tryCatch({
          perf.diablo <- mixOmics::perf(result, validation = 'Mfold', folds = min(table(Y)), nrepeat = 50, 
                                        progressBar = TRUE, cpus = 1)
          finished = T
        }, error = function(cond){
          if (grepl("There are features with zero variance", cond$message, fixed = T)){
            dataName <- stringr::str_match(cond$message, "There are features with zero variance in block '([^']*)'")[2]
            return(dataName)
          } else {
            if (grepl("system is computationally singular", cond$message, fixed = T)){
              getShinyErrorAlert("An error appeared while trying to reduce the dataset. <br> 
                        Please reduce your number of components or change your design matrix and try again!", html = T)
            } else {
              getErrorMessage(cond)
            }
            return(NULL)
          }
        })
        
        error <- is.null(dataName)
        
        if (!finished && !error){
          data <- X[[dataName]]
          
          #get all possible value combinations
          nearZerodata <- mixOmics::nearZeroVar(data, freqCut = 1, uniqueCut = 100)
          
          if (length(nearZerodata$Position) == 0){
            getErrorMessage("Unfortunately it is not possible to tune the parameters")
            error <- T 
          } else {
            minUniqueCut <- ifelse(length(nearZerodata$Position) != 0, min(nearZerodata$Metrics$percentUnique), 100)
            
            #remove values with lowest variance
            X[[dataName]] <- data[, -mixOmics::nearZeroVar(data, freqCut = 1, uniqueCut = minUniqueCut)$Position]
            
            #calculate result with newly reduced data
            tryCatch({
              result <- mixOmics::block.splsda(X, Y, ncomp = input$ncomp , scale = input$scale,
                                             design = design)
            }, error = function(cond){
              getErrorMessage(cond)
              error <- T
            }) 
          }
        }
      }
      
      if (!error){
        ncomp = perf.diablo$choice.ncomp$WeightedVote["Overall.BER", "centroids.dist"]
        
        incProgress(1/3)
        
        #tune keepX
        test.keepX = dataSelection$data
        for (i in 1 : length(names(dataSelection$data))){
          test.keepX[[i]] = getTestKeepX(ncol(dataSelection$data[[i]]))
        }
        
        BPPARAM <- BiocParallel::SnowParam(workers = parallel::detectCores()-1)
        tune.diablo = mixOmics::tune.block.splsda(X, Y, ncomp = ncomp, scale = input$scale,
                                                  test.keepX = test.keepX, design = design,
                                                  validation = 'Mfold', folds = getFolds(Y), nrepeat = 1,
                                                  BPPARAM = BPPARAM, dist = "centroids.dist", progressBar = TRUE)
        keepX = tune.diablo$choice.keepX
        incProgress(1/3)
      }
    })
    
    if (error){
      tunedVals$ncomp <- NULL
    } else {
      tunedVals$ncomp <- ncomp
      tunedVals$keepX <- keepX
      tunedVals$scale <- input$scale
      tunedVals$matrix <- input$matrix
      
      output$ErrorRate <- renderPlot(plot(perf.diablo))
      output$ErrorRate.download <- getDownloadHandler("DIABLO_classification_error.png", function(){plot(perf.diablo)}, width = 2592, height = 1944)
    }
  }
}

#' Run analysis
#' @noRd
run_diablo_analysis <- function(ns, input, output, dataSelection, classSelection, useTunedVals, tunedVals){
  diablo.result <- reactive({
    req(dataSelection$data)
    req(classSelection$data)
    req(diabloCheckValidSelection(dataSelection$data, classSelection$data)$valid)
    req(input$matrix)
    X <- dataSelection$data
    Y <- classSelection$data[,2]  #only second column contains label information
    if (!is.null(X)){
      msg <- ""
      for (x in X){
        if(msg != ""){
          break
        }
        msg <- checkDataNcompCompatibility(x, input$ncomp) 
      }
      
      output$parameters.error <- renderText(msg)
      
      if(msg == ""){
        design <- matrix(input$matrix, ncol = length(X), nrow = length(X),
                         dimnames = list(names(X), names(X)))
        diag(design) <- 0
        tryCatch({
          result <- mixOmics::block.splsda(X, Y,
                                           ncomp = input$ncomp , scale = input$scale,
                                           design = design)
        }, error = function(cond){
          getErrorMessage(cond)
          result <- NULL
        })
      } else {
        result <- NULL
      }
    }
  })
  
  diablo.result.tuned <- reactive({
    if (useTunedVals()){
      req(dataSelection$data)
      req(classSelection$data)
      req(diabloCheckValidSelection(dataSelection$data, classSelection$data)$valid)
      X <- dataSelection$data
      Y <- classSelection$data[,2]  #only second column contains label information
      if (!is.null(X)){
        design <- matrix(tunedVals$matrix, ncol = length(X), nrow = length(X),
                         dimnames = list(names(X), names(X)))
        diag(design) <- 0
        
        tryCatch({
          result <- mixOmics::block.splsda(X, Y, ncomp = tunedVals$ncomp, scale = tunedVals$scale,
                                           keepX = tunedVals$keepX, design = design)
        }, error = function(cond){
          getErrorMessage(cond)
          result <- NULL
        })
      }
    }
  })
  
  return(list(result = diablo.result, resultTuned = diablo.result.tuned))
}

#' Business logic functions
#' @noRd
generate_diablo_plots <- function(ns, input, output, dataSelection, classSelection, result, result.tuned, tunedVals, nodes, nodesTuned){
  # Create reactive values
  comp.indiv <- getCompIndivReactive(input)
  comp.var <- getCompVarReactive(input)

  comp.diablo <- reactive({
    req(input$diablo.comp)
    comp.diablo <- as.numeric(input$diablo.comp)
  })
  
  circos.cutoff <- reactive({
    circos.cutoff <- input$cutoffCiros
  })
  
  comp.indiv.tuned <- getCompIndivReactive(input, tuned = TRUE)
  comp.var.tuned <- getCompVarReactive(input, tuned = TRUE)
  comp.img.tuned <- getCompImgReactive(input, tuned = TRUE)
  
  comp.diablo.tuned <- reactive({
    req(input$diablo.comp.tuned)
    comp.diablo.tuned <- as.numeric(input$diablo.comp.tuned)
  })
  
  circos.cutoff.tuned <- reactive({
    circos.cutoff.tuned <- input$cutoffCiros.tuned
  })
  
  diabloNetwork <- reactiveValues()
  diabloNetworkTuned <- reactiveValues()
  
  #  plot functions
  plot.indiv <- function(){
    if(!is.null(result()) & input$ncomp >= 2){
      req(classSelection$data)
      legend.title = colnames(classSelection$data)[2]
      if (ncol(classSelection$data) == 3){
        colors = getGroupColors(classSelection$data)
        plotIndiv(result(), classes=classSelection$data[,2], legend.title = legend.title, comp = comp.indiv(), 
                  indNames = input$indiv.names, legendPosition = "bottom", col.per.group = colors)
      } else {
        plotIndiv(result(), classes = classSelection$data[,2], legend.title = legend.title, comp = comp.indiv(), 
                  indNames = input$indiv.names, legendPosition = "bottom")
      }
    }
  }
  
  plot.var <- function(){
    if(!is.null(result()) & input$ncomp >= 2){
      plotVar(result(), comp.var(), input$var.names,
              pch = seq(1, length(dataSelection$data), 1), legend = TRUE)
    }
  }
  
  plot.load <- function(){
    if(!is.null(result())){
      req(input$load.comp)
      plotLoadings(result(), as.numeric(input$load.comp))
    }
  }
  
  plot.img <- function(){
    if(!is.null(result()) & length(dataSelection$data) > 1){
      if (ncol(classSelection$data) == 3){
        colors = getGroupColors(classSelection$data)
        mixOmics::cimDiablo(result(), comp = 1, margin=c(input$xmargin, input$ymargin),
                            legend.position = "right", size.legend = 1,
                            color.Y = colors, trim = F)
      } else {
        mixOmics::cimDiablo(result(), comp = 1, margin=c(input$xmargin, input$ymargin),
                            legend.position = "right",
                            size.legend = 1, trim = F)
      }
    }    
  }
  
  plot.arrow <- function(){
    if(!is.null(result()) & input$ncomp >= 2){
      req(classSelection$data)
      legend.title = colnames(classSelection$data)[2]
      
      result <- result()
      newNames <- c()
      for (name in names(result$variates)){
        if (grepl("\\s+", name)){ #check that no dataset has whitespace as their name
          name <- gsub(" ", "", name)
        }
        newNames <- cbind(newNames, name)
      }
      names(result$variates) <- newNames
      
      if (ncol(classSelection$data) == 3){
        colors = getGroupColors(classSelection$data)
        plotArrow(result, classSelection$data[,2], legend.title, input$namesArrow, col.per.group = colors)
      } else {
        plotArrow(result, classSelection$data[,2], legend.title, input$namesArrow)
      }
    }
  }
  
  plot.diablo <- function(){
    if(!is.null(result()) & length(dataSelection$data) > 1){
      comp <- checkCompNcompCombination(result()$ncomp, comp.diablo())
      if (ncol(classSelection$data) == 3){
        colors = getGroupColors(classSelection$data)
        mixOmics::plotDiablo(result(), ncomp = comp, col.per.group = colors)
      } else {
        mixOmics::plotDiablo(result(), ncomp = comp)
      }
    }
  }
  
  plot.circos <- function(){
    if(!is.null(result()) & length(dataSelection$data) > 1){
      title = colnames(classSelection$data)[2]
      if (ncol(classSelection$data) == 3){
        colors = getGroupColors(classSelection$data)
        mixOmics::circosPlot(result(), cutoff = input$cutoffCircos, line = input$showLine, legend.title = title,
                             size.labels = input$datasetSizeCircos, size.variables = input$featureSizeCircos,
                             color.Y = colors)
      } else {
        mixOmics::circosPlot(result(), cutoff = input$cutoffCircos, line = input$showLine, legend.title = title,
                             size.labels = input$datasetSizeCircos, size.variables = input$featureSizeCircos)      
      }
    }
  }
  
  #tuned
  plot.indiv.tuned <- function(){
    if(!is.null(result.tuned()) & tunedVals$ncomp >= 2){
      req(classSelection$data)
      title = colnames(classSelection$data)[2]
      if (ncol(classSelection$data) == 3){
        colors = getGroupColors(classSelection$data)
        plotIndiv(result.tuned(), classes = classSelection$data[,2], legend.title = title, 
                  comp = comp.indiv.tuned(), indNames = input$indiv.names.tuned, 
                  legendPosition = "bottom", col.per.group = colors)
      } else {
        plotIndiv(result.tuned(), classes = classSelection$data[,2], legend.title = title, 
                  comp = comp.indiv.tuned(), indNames = input$indiv.names.tuned,
                  legendPosition = "bottom")
      }
    }
  }
  
  plot.var.tuned <- function(){
    if(!is.null(result.tuned()) & tunedVals$ncomp >= 2){
      plotVar(result.tuned(), comp.var.tuned(), input$var.names.tuned,
              pch = seq(1, length(dataSelection$data), 1), legend = TRUE)
    }
  }
  
  plot.load.tuned <- function(){
    if(!is.null(result.tuned())){
      req(input$load.comp.tuned)
      plotLoadings(result.tuned(), as.numeric(input$load.comp.tuned))
    }
  }
  
  plot.img.tuned <- function(){
    if(!is.null(result.tuned()) & length(dataSelection$data) > 1){
      comp.img.tuned <- checkCompNcompCombination(result.tuned()$ncomp, comp.img.tuned())
      if (ncol(classSelection$data) == 3){
        colors = getGroupColors(classSelection$data)
        mixOmics::cimDiablo(result.tuned(), comp = comp.img.tuned, 
                            margin=c(input$xmargin.tuned, input$ymargin.tuned), 
                            legend.position = "right",
                            size.legend = 1, color.Y = colors, trim = F)
      } else {
        mixOmics::cimDiablo(result.tuned(), comp = comp.img.tuned,
                            margin=c(input$xmargin.tuned, input$ymargin.tuned), 
                            legend.position = "right",
                            size.legend = 1, trim = F)
      }
    }    
  }
  
  plot.arrow.tuned <- function(){
    if(!is.null(result.tuned()) & tunedVals$ncomp >= 2){
      req(classSelection$data)
      legend.title = colnames(classSelection$data)[2]
      
      resultTuned <- result.tuned()
      newNames <- c()
      for (name in names(resultTuned$variates)){
        if (grepl("\\s+", name)){ #check that no dataset has whitespace as their name
          name <- gsub(" ", "", name)
        }
        newNames <- cbind(newNames, name)
      }
      names(resultTuned$variates) <- newNames
      
      
      if (ncol(classSelection$data) == 3){
        colors = getGroupColors(classSelection$data)
        plotArrow(resultTuned, classSelection$data[,2], legend.title, input$namesArrow.tuned, col.per.group = colors)
      } else {
        plotArrow(resultTuned, classSelection$data[,2], legend.title, input$namesArrow.tuned)
      }
    }
  }
  
  plot.diablo.tuned <- function(){
    if(!is.null(result.tuned()) & length(dataSelection$data) > 1){
      comp <- checkCompNcompCombination(result.tuned()$ncomp, comp.diablo.tuned())
      if (ncol(classSelection$data) == 3){
        colors = getGroupColors(classSelection$data)
        mixOmics::plotDiablo(result.tuned(), ncomp = comp, col.per.group = colors)
      } else {
        mixOmics::plotDiablo(result.tuned(), ncomp = comp)
      }
    }
  }
  
  plot.circos.tuned <- function(){
    if(!is.null(result.tuned()) & length(dataSelection$data) > 1){
      title = colnames(classSelection$data)[2]
      if (ncol(classSelection$data) == 3){
        colors = getGroupColors(classSelection$data)
        mixOmics::circosPlot(result.tuned(), cutoff = input$cutoffCircos.tuned, line = input$showLine.tuned, legend.title = title,
                             size.labels = input$datasetSizeCircos.tuned, size.variables = input$featureSizeCircos.tuned,
                             color.Y = colors)
      } else {
        mixOmics::circosPlot(result.tuned(), cutoff = input$cutoffCircos.tuned, line = input$showLine.tuned, legend.title = title, 
                             size.labels = input$datasetSizeCircos.tuned, size.variables = input$featureSizeCircos.tuned)
      }
    }
  }
  
  #Set output plots
  # Sample plot
  output$Indiv <- renderPlot(
    plot.indiv()
  )
  
  # Correlation Circle plot
  output$Var <- renderPlot(
    plot.var()
  )
  
  # Loading plot
  output$Load <- renderPlot(
    plot.load()
  )
  
  # CIM plot
  output$Img <- renderPlot(
    plot.img()
  )
  
  # Arrow plot
  output$Arrow <- renderPlot(
    plot.arrow()
  )
  
  # Diablo plot
  output$Diablo <- renderPlot(
    plot.diablo()
  )
  
  # Circos plot
  output$Circos <- renderPlot(
    plot.circos()
  )
  
  # Network plot
  output$Network <- renderVisNetwork({
    if(!is.null(result()) & length(dataSelection$data) > 1){
      networkResult <- diabloGenerateNetwork(ns, "", result, dataSelection, input$cutoffNetwork, input$fullName)
      nodes$data <- networkResult$data
      diabloNetwork$mixNetwork <- networkResult$mixNetwork
      diabloNetwork$visNetwork <- networkResult$visNetwork
    }
  })
  
  # Sample plot tuned
  output$Indiv.tuned <- renderPlot(
    plot.indiv.tuned()
  )
  
  # Correlation Circle plot tuned
  output$Var.tuned <- renderPlot(
    plot.var.tuned()
  )
  
  # Loading plot tuned
  output$Load.tuned <- renderPlot(
    plot.load.tuned()
  )
  
  # CIM plot tuned
  output$Img.tuned <- renderPlot(
    plot.img.tuned()
  )
  
  # Arrow plot tuned
  output$Arrow.tuned <- renderPlot(
    plot.arrow.tuned()
  )
  
  # Diablo plot tuned
  output$Diablo.tuned <- renderPlot(
    plot.diablo.tuned()
  )
  
  # Circos plot tuned
  output$Circos.tuned <- renderPlot(
    plot.circos.tuned()
  )
  
  # Network plot tuned
  output$Network.tuned <- renderVisNetwork({
    if(!is.null(result.tuned()) & length(dataSelection$data) > 1){
      networkResult <- diabloGenerateNetwork(ns, ".tuned", result.tuned, dataSelection, input$cutoffNetwork.tuned, input$fullName.tuned)
      nodesTuned$data <- networkResult$data
      diabloNetworkTuned$mixNetwork <- networkResult$mixNetwork
      diabloNetworkTuned$visNetwork <- networkResult$visNetwork
    }
  })
  
  output$ncomp.tuned <- renderText(
    paste("Number of components: ", tunedVals$ncomp)
  )
  
  output$keepX.tuned <- renderText(
    paste("Features of dataset ", names(dataSelection$data), ": ",  tunedVals$keepX)
  )
  
  output$scale.tuned <- renderText(
    paste("scaled: ",  tunedVals$scale)
  )
  
  output$matrix.tuned <- renderText(
    paste("design matrix: ",  tunedVals$matrix)
  )
  
  # Download handler
  output$Indiv.download <- getDownloadHandler("DIABLO_Sampleplot.png", plot.indiv)
  output$Var.download <- getDownloadHandler("DIABLO_CorrelationCircleplot.png", plot.var)
  output$Load.download <- getDownloadHandler("DIABLO_Loadingsplot.png", plot.load, width = 2592, height = 1944)
  output$Img.download <- getDownloadHandler("DIABLO_Heatmap.png", plot.img, width = 2592, height = 1944)
  output$Diablo.download <- getDownloadHandler("DIABLO_Diabloplot.png", plot.diablo, width = 2592, height = 1944)
  output$Circos.download <- getDownloadHandler("DIABLO_Circosplot.png", plot.circos, width = 2592, height = 1944)
  output$Circos.table.download <- getDownloadHandler("DIABLO_Circosplot.csv", plot.circos, type = "csv")
  output$NetworkHtml.download <- diabloGetNetworkDownloadHandler("DIABLO_Network.html", diabloNetwork, "html")
  output$NetworkGml.download <- diabloGetNetworkDownloadHandler("DIABLO_Network.gml", diabloNetwork, "gml")
  output$Arrow.download <- getDownloadHandler("DIABLO_Arrowplot.png", plot.arrow, type = "ggplot")
  output$Img.table.download <- getDownloadHandler("DIABLO_Heatmap.csv", contentfct = plot.img, type = "csv", tablefct = cimToTable)
  
  
  output$Indiv.download.tuned <- getDownloadHandler("DIABLO_tuned_Sampleplot.png", plot.indiv.tuned)
  output$Var.download.tuned <- getDownloadHandler("DIABLO_tuned_CorrelationCircleplot.png", plot.var.tuned)
  output$Load.download.tuned <- getDownloadHandler("DIABLO_tuned_Loadingsplot.png", plot.load.tuned, width = 2592, height = 1944)
  output$Img.download.tuned <- getDownloadHandler("DIABLO_tuned_Heatmap.png", plot.img.tuned, width = 2592, height = 1944)
  output$Diablo.download.tuned <- getDownloadHandler("DIABLO_tuned_Diabloplot.png", plot.diablo.tuned, width = 2592, height = 1944)
  output$Circos.download.tuned <- getDownloadHandler("DIABLO_tuned_Circosplot.png", plot.circos.tuned, width = 2592, height = 1944)
  output$Circos.table.download.tuned <- getDownloadHandler("DIABLO_tuned_Circosplot.csv", plot.circos.tuned, type = "csv")
  output$NetworkHtml.download.tuned <- diabloGetNetworkDownloadHandler("DIABLO_tuned_Network.html", diabloNetworkTuned, "html")
  output$NetworkGml.download.tuned <- diabloGetNetworkDownloadHandler("DIABLO_tuned_Network.gml", diabloNetworkTuned, "gml")
  output$Arrow.download.tuned <- getDownloadHandler("DIABLO_tuned_Arrowplot.png", plot.arrow.tuned, type = "ggplot") 
  output$Img.table.download.tuned <- getDownloadHandler("DIABLO_Heatmap_tuned.csv", contentfct = plot.img.tuned, type = "csv", tablefct = cimToTable)
}

#' Generate the error messages
#' @noRd
generate_diablo_error_messages <- function(input, output, data, classes, dataSelection, classSelection, tunedVals){
  # Error message when selection is incompatible or  data or classes are missing
  inputSelChange <- reactive({
    list(dataSelection$data, classSelection$data)
  })
  
  observeEvent(inputSelChange(), {
    output$errorMsg <- renderText({
      if(length(data$data) == 0){
        "Please upload some data to be able to use the analysis!"
      } else if(length(classes$data) == 0){
        "Please upload some classes/label information to be able to use the analysis!"
      } else {
        class <- classSelection$data
        data <- dataSelection$data
  
        req(data)
        req(class)
        selectionCheck = diabloCheckValidSelection(data, class)
        if (!selectionCheck$valid){
          selectionCheck$msg
        } else {
          ""
        }
      }
    })
  })
  
  output$indiv.error <- renderText({
    return (diabloCheckOneDatasetNcomp(dataSelection, input))
  })
  
  output$var.error <- renderText({
    return (diabloCheckOneDatasetNcomp(dataSelection, input))
  })
  
  output$load.error <- renderText({
    return(diabloCheckOneDatasetNcomp(dataSelection, ncompCheck = FALSE))
  })
  
  output$img.error <- renderText({
    return(diabloCheckTwoDatasets(dataSelection))
  })
  
  output$arrow.error <- renderText({
    return (diabloCheckOneDatasetNcomp(dataSelection, input))
    
  })
  
  output$diablo.error <- renderText({
    return(diabloCheckTwoDatasets(dataSelection))
    
  })
  
  output$circos.error <- renderText({
    return(diabloCheckTwoDatasets(dataSelection))
    
  })
  
  output$network.error <- renderText({
    return(diabloCheckTwoDatasets(dataSelection))
  })
  
  #tuned
  output$indiv.error.tuned <- renderText({
    return (diabloCheckOneDatasetNcomp(dataSelection, input, tuned = TRUE, tunedVals = tunedVals))
  })
  
  output$var.error.tuned <- renderText({
    return (diabloCheckOneDatasetNcomp(dataSelection, input, tuned = TRUE, tunedVals =  tunedVals))
  })
  
  output$load.error.tuned <- renderText({
    return(diabloCheckOneDatasetNcomp(dataSelection, ncompCheck = FALSE))
  })
  
  output$img.error.tuned <- renderText({
    return(diabloCheckTwoDatasets(dataSelection))
  })
  
  output$arrow.error.tuned <- renderText({
    return (diabloCheckOneDatasetNcomp(dataSelection, input, tuned = TRUE, tunedVals = tunedVals))
  })
  
  output$diablo.error.tuned <- renderText({
    return(diabloCheckTwoDatasets(dataSelection))
    
  })
  
  output$circos.error.tuned <- renderText({
    return(diabloCheckTwoDatasets(dataSelection))
    
  })
  
  output$network.error.tuned <- renderText({
    return(diabloCheckTwoDatasets(dataSelection))
  })
}

#' Information texts
#' @noRd
render_diablo_infotexts <- function(output){
  output$DIABLOinfotext <- renderText({
    HTML("<b>D</b>ata <b>I</b>ntegration <b>A</b>nalysis for <b>B</b>iomarker discovery using <b>L</b>atent c<b>O</b>mponents is a 
    generalised version of the PLS for multiple datasets. It maximises the correlated information between the datasets and simultaneously 
    identifies the key features of the omics datasets. Generally, DIABLO can be used supervised and unsupervised, whereas, here the supervised version is used. 
    Therefore, the DIABLO algorithm works with one list containing the omics datasets and the dataset with the classes of each sample. <br/>
    Additional information can be found on the <a class='mixOmics-link' href='http://mixomics.org/mixdiablo/' target='_blank'>mixOmics website</a> and
    in several scientific papers (e.g. <a class='ref-link' href='https://academic.oup.com/bioinformatics/article/35/17/3055/5292387' target='_blank'>Singh et.al. (2019)</a>).
    More information about the plots, the feature selection and tuning methods and the meaning of the design matrix
    can be found on our <a class='mixOmics-link' onclick=\"document.getElementById('tab-help-plots').click();\">'Plots-Helppage'</a>, and
    <a class='mixOmics-link' onclick=\"document.getElementById('tab-help-tuning').click();\">'Feature selection and tuning-Helppage'</a> and
    <a class='mixOmics-link' onclick=\"document.getElementById('tab-help-designmatrix').click();\">'Design matrix helppage'</a>.</br>
    <b>Please adjust the number of components in the 'Analysis parameters' tab according to your selected dataset.</b>  
    We recommend using here a rather high number of components and then perform parameter tuning.")
  })
  
  output$DIABLOtunedinfotext <- renderText({
    HTML("The mixOmics package offers parameter tuning for the DIABLO analyses. 
    This means the number of components and features for each component on each dataset can be tuned using repeated cross-validation. <br/>
    More detailed information can be found on our <a class='mixOmics-link' onclick=\"document.getElementById('tab-help-tuning').click();\">'Feature selection and tuning-Helppage'</a>.")
  })
}