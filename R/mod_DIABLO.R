library(visNetwork)
library(shinyjs)
library(shinyWidgets)

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
                      fluidRow(width = 12,
                               getAnalysisParametersComponent(ns)
                      ),
                      fluidRow(width = 12,
                               diabloGetUi(ns)
                      )
      ),
      bs4Dash::column(width = 2,
                      getTuneBox(ns)
      ),
      bs4Dash::column(id = ns("tunedCol"), width = 5,
                      fluidRow(width = 12,
                               getTunedParametersComponent(ns)
                      ),
                      fluidRow(width = 12,
                               diabloGetUi(ns, ".tuned")
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
    
    hide("tunedCol")
    hide("switchRow")
    
    dataSelection <- reactiveValues()
    classSelection <- reactiveValues()
    
    nodes <- reactiveValues()
    nodesTuned <- reactiveValues()
    
    useTunedVals <- reactiveVal(FALSE)
    tunedVals <- reactiveValues(ncomp = 2, keepX = NULL)
    
    results <- run_diablo_analysis(ns, input, output, dataSelection, classSelection, useTunedVals, tunedVals)
    
    render_diablo_ui_components(ns, input, output, dataSelection, tunedVals, nodes, nodesTuned)
    
    observe_diablo_ui_components(ns, session, input, output, data, dataSelection, classes, classSelection, results$result, useTunedVals, tunedVals)
    
    generate_diablo_plots(ns, input, output, dataSelection, classSelection, results$result, results$resultTuned, tunedVals, nodes, nodesTuned)
    
    generate_diablo_error_messages(input, output, data, classes, dataSelection, classSelection, tunedVals)
  })
}

#' Render Ui components
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

#'Observe different ui components
observe_diablo_ui_components <- function(ns, session, input, output, data, dataSelection, classes, classSelection, result, useTunedVals, tunedVals){
  #' Observe data input change
  observeEvent(data$data, {
    output$dataSelComp <- renderUI({
      choices <- generateChoices(data$data)
      getSelectionComponent(ns("dataSelection"), "Select the datasets: ", choices = choices, multiple = TRUE)
    })
  })
  
  observeEvent(input$dataSelection, {
    list <- list()
    for (s in strsplit(input$dataSelection, " ")){
      list[[s]] <- data$data[[s]]
    }
    dataSelection$data <- list
  })
  
  #' Observe class input change
  observeEvent(classes$data, {
    output$classSelComp <- renderUI({
      choices <- generateChoices(classes$data)
      getSelectionComponent(ns("classSelection"), "Select classes/labels:", choices = choices, width = "fit-content")
    })
  })
  
  observeEvent(input$classSelection, {
    classSelection$data <- classes$data[[input$classSelection]]
  })
  
  #' Observe data change
  observeEvent(dataSelection$data, {
    output$tune.switch <- renderUI({})
    useTunedVals(FALSE)
    hide("tunedCol")
    hide("switchRow")
  })
  
  #' Observe node name selection
  observeEvent(input$nodeNames, {
    if (input$nodeNames == "null"){
      visNetworkProxy(ns("Network")) %>%
        visUnselectAll()
    }else{
      visNetworkProxy(ns("Network")) %>%
        visSetSelection(nodesId = input$nodeNames)
    }
    runjs(paste0("Shiny.setInputValue('", ns('clicked_node'), "', '", input$nodeNames, "')"))
  })
  
  observeEvent(input$nodeNames.tuned, {
    if (input$nodeNames.tuned == "null"){
      visNetworkProxy(ns("Network.tuned")) %>%
        visUnselectAll()
    }else{
      visNetworkProxy(ns("Network.tuned")) %>%
        visSetSelection(nodesId = input$nodeNames.tuned)
    }
    runjs(paste0("Shiny.setInputValue('", ns('clicked_node.tuned'), "', '", input$nodeNames.tuned, "')"))
  })
  
  #' Observe node clicked
  observeEvent(input$clicked_node, {
    updateSelectizeInput(session, "nodeNames", selected = input$clicked_node)
  })
  
  observeEvent(input$clicked_node.tuned, {
    updateSelectizeInput(session, "nodeNames.tuned", selected = input$clicked_node.tuned)
  })
  
  #' Observe tune button
  observeEvent(input$tune, {
    if (!is.null(input$dataSelection)){
      tryCatch({
        tune_diablo_values(dataSelection, classSelection, result, tunedVals)
        if (!is.null(tunedVals)){
          show("switchRow")
          output$tune.switch <- renderUI({materialSwitch(ns("tuneSwitch"), "Use tuned parameters", value = FALSE)})
        }
      }, error = function(cond){
        shinyalert::shinyalert("Error!", "There was an error while trying to tune the parameters.
                             This can be related with the chosen datasets.", type = "error")
      })
    }
  })
  
  #' Observe tune switch
  observeEvent(input$tuneSwitch,{
    useTunedVals(input$tuneSwitch)
    if(input$tuneSwitch){
      show("tunedCol")
    }else{
      hide("tunedCol")
    }
  })
}

#' Tune the ncomp and keepX parameter for the given dataset
tune_diablo_values <- function(dataSelection, classSelection, result, tunedVals){
  X <- dataSelection$data
  if (!is.null(X)){
    withProgress(message = 'Tuning parameters .... Please wait!', value = 1/3, {
      Y <- classSelection$data[,1]
      design <- matrix(0.1, ncol = length(X), nrow = length(X),
                       dimnames = list(names(X), names(X)))
      diag(design) <- 0
      
      #tune ncomp
      set.seed(30)
      perf.diablo <- mixOmics::perf(result(), validation = 'Mfold', folds = min(table(Y)), nrepeat = 50, progressBar = TRUE, cpus = 1)
      ncomp = perf.diablo$choice.ncomp$WeightedVote["Overall.BER", "centroids.dist"]
      
      incProgress(1/3)
      
      #tune keepX
      test.keepX = dataSelection$data
      for (i in 1 : length(names(dataSelection$data))){
        test.keepX[[i]] = getTextKeepX(ncol(dataSelection$data[[i]]))
      }
      
      set.seed(30)
      BPPARAM <- BiocParallel::SnowParam(workers = parallel::detectCores()-1)
      tune.diablo = mixOmics::tune.block.splsda(X, Y, ncomp = ncomp,
                                                test.keepX = test.keepX, design = design,
                                                validation = 'Mfold', folds = min(table(Y)), nrepeat = 1,
                                                BPPARAM = BPPARAM, dist = "centroids.dist", progressBar = TRUE)
      keepX = tune.diablo$choice.keepX
      
      incProgress(1/3)
    })
    
    tunedVals$ncomp <- ncomp
    tunedVals$keepX <- keepX
  }
}

#' Run analysis
run_diablo_analysis <- function(ns, input, output, dataSelection, classSelection, useTunedVals, tunedVals){
  diablo.result <- reactive({
    req(diabloCheckValidSelection(dataSelection$data, classSelection$data))
    X <- dataSelection$data
    Y <- classSelection$data[,1]  #only first column contains label information
    if (!is.null(X)){
      design <- matrix(0.1, ncol = length(X), nrow = length(X),
                       dimnames = list(names(X), names(X)))
      diag(design) <- 0
      result <- mixOmics::block.splsda(X, Y,
                                       ncomp = input$ncomp , scale = input$scale,
                                       design = design)
    }
  })
  
  diablo.result.tuned <- reactive({
    if (useTunedVals()){
      req(diabloCheckValidSelection(dataSelection$data, classSelection$data))
      X <- dataSelection$data
      Y <- classSelection$data[,1]  #only first column contains label information
      if (!is.null(X)){
        design <- matrix(0.1, ncol = length(X), nrow = length(X),
                         dimnames = list(names(X), names(X)))
        diag(design) <- 0
        result <- mixOmics::block.splsda(X, Y, ncomp = tunedVals$ncomp, keepX = tunedVals$keepX, design = design)
        
      }
    }
  })
  
  return(list(result = diablo.result, resultTuned = diablo.result.tuned))
}

#' Business logic functions
generate_diablo_plots <- function(ns, input, output, dataSelection, classSelection, result, result.tuned, tunedVals, nodes, nodesTuned){
  #' Create reactive values
  comp.indiv <- getCompIndivReactive(input)
  comp.var <- getCompVarReactive(input)
  comp.img <- getCompImgReactive(input)
  
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
  
  #'  plot functions
  plot.indiv <- function(){
    if(!is.null(result()) & input$ncomp >= 2){
      req(classSelection$data)
      title = colnames(classSelection$data)[1]
      if (ncol(classSelection$data) == 2){
        colors = getGroupColors(classSelection$data)
        plotIndiv(result(), classSelection$data[,1], title, comp.indiv(), 
                  indNames = input$indiv.names, legendPosition = "bottom", col.per.group = colors)
      } else {
        plotIndiv(result(), classSelection$data[,1], title, comp.indiv(), 
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
      if (ncol(classSelection$data) == 2){
        colors = getGroupColors(classSelection$data)
        mixOmics::cimDiablo(result(), comp = comp.img(), margin=c(8,20), legend.position = "right",
                            size.legend = 1, color.Y = colors)
      } else {
        mixOmics::cimDiablo(result(), comp = comp.img(), margin=c(8,20), legend.position = "right",
                            size.legend = 1)
      }
    }    
  }
  
  plot.arrow <- function(){
    if(!is.null(result()) & input$ncomp >= 2){
      req(classSelection$data)
      title = colnames(classSelection$data)[1]
      if (ncol(classSelection$data) == 2){
        colors = getGroupColors(classSelection$data)
        plotArrow(result(), classSelection$data[,1], title, input$namesArrow, col.per.group = colors)
      } else {
        plotArrow(result(), classSelection$data[,1], title, input$namesArrow)
      }
    }
  }
  
  plot.diablo <- function(){
    if(!is.null(result()) & length(dataSelection$data) > 1){
      if (ncol(classSelection$data) == 2){
        colors = getGroupColors(classSelection$data)
        mixOmics::plotDiablo(result(), ncomp = comp.diablo(), col.per.group = colors)
      } else {
        mixOmics::plotDiablo(result(), ncomp = comp.diablo())
      }
    }
  }
  
  plot.circos <- function(){
    if(!is.null(result()) & length(dataSelection$data) > 1){
      if (ncol(classSelection$data) == 2){
        colors = getGroupColors(classSelection$data)
        mixOmics::circosPlot(result(), cutoff = input$cutoffCircos, line = TRUE,
                             size.labels =1.5, size.variables = .85, color.Y = colors)
      } else {
        mixOmics::circosPlot(result(), cutoff = input$cutoffCircos, line = TRUE)      
      }
    }
  }
  
  #tuned
  plot.indiv.tuned <- function(){
    if(!is.null(result.tuned()) & tunedVals$ncomp >= 2){
      plotIndiv(result.tuned(), comp.indiv.tuned(), indNames = input$indiv.names.tuned, 
                repSpace = rep.space.tuned(), legendPosition = "bottom")
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
      if (ncol(classSelection$data) == 2){
        colors = getGroupColors(classSelection$data)
        mixOmics::cimDiablo(result.tuned(), comp = comp.img.tuned(), margin=c(8,20), legend.position = "right",
                            size.legend = 1, color.Y = colors)
      } else {
        mixOmics::cimDiablo(result.tuned(), comp = comp.img.tuned(), margin=c(8,20), legend.position = "right",
                            size.legend = 1)
      }
    }    
  }
  
  plot.arrow.tuned <- function(){
    if(!is.null(result.tuned()) & tunedVals$ncomp >= 2){
      req(classSelection$data)
      title = colnames(classSelection$data)[1]
      if (ncol(classSelection$data) == 2){
        colors = getGroupColors(classSelection$data)
        plotArrow(result.tuned(), classSelection$data[,1], title, input$namesArrow.tuned, col.per.group = colors)
      } else {
        plotArrow(result.tuned(), classSelection$data[,1], title, input$namesArrow.tuned)
      }
    }
  }
  
  plot.diablo.tuned <- function(){
    if(!is.null(result.tuned()) & length(dataSelection$data) > 1){
      if (ncol(classSelection$data) == 2){
        colors = getGroupColors(classSelection$data)
        mixOmics::plotDiablo(result.tuned(), ncomp = comp.diablo.tuned(), col.per.group = colors)
      } else {
        mixOmics::plotDiablo(result.tuned(), ncomp = comp.diablo.tuned())
      }
    }
  }
  
  plot.circos.tuned <- function(){
    if(!is.null(result.tuned()) & length(dataSelection$data) > 1){
      mixOmics::circosPlot(result.tuned(), cutoff = input$cutoffCircos.tuned, line = TRUE,
                           size.labels =1.5, size.variables = .85)
    }
  }
  
  #'Set output plots
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
  
  #' CIM plot
  output$Img <- renderPlot(
    plot.img()
  )
  
  #' Arrow plot
  output$Arrow <- renderPlot(
    plot.arrow()
  )
  
  #' Diablo plot
  output$Diablo <- renderPlot(
    plot.diablo()
  )
  
  #' Circos plot
  output$Circos <- renderPlot(
    plot.circos()
  )
  
  #' Network plot
  output$Network <- renderVisNetwork({
    if(!is.null(result()) & length(dataSelection$data) > 1){
      networkResult = diabloGenerateNetwork(ns, "", result, dataSelection, input$cutoffNetwork, input$fullName)
      nodes$data <- networkResult$data
      diabloNetwork$mixNetwork <- networkResult$mixNetwork
      diabloNetwork$visNetwork <- networkResult$visNetwork
    }
  })
  
  #' Sample plot tuned
  output$Indiv.tuned <- renderPlot(
    plot.indiv.tuned()
  )
  
  #' Variable plot tuned
  output$Var.tuned <- renderPlot(
    plot.var.tuned()
  )
  
  #' Loading plot tuned
  output$Load.tuned <- renderPlot(
    plot.load.tuned()
  )
  
  #' CIM plot tuned
  output$Img.tuned <- renderPlot(
    plot.img.tuned()
  )
  
  #' Arrow plot tuned
  output$Arrow.tuned <- renderPlot(
    plot.arrow.tuned()
  )
  
  #' Diablo plot tuned
  output$Diablo.tuned <- renderPlot(
    plot.diablo.tuned()
  )
  
  #' Circos plot tuned
  output$Circos.tuned <- renderPlot(
    plot.circos.tuned()
  )
  
  #' Network plot tuned
  output$Network.tuned <- renderVisNetwork({
    if(!is.null(result.tuned()) & length(dataSelection$data) > 1){
      networkResult = diabloGenerateNetwork(ns, ".tuned", result.tuned, dataSelection, input$cutoffNetwork.tuned, input$fullName.tuned)
      nodesTuned$data <- networkResult$data
      diabloNetworkTuned$mixNetwork <- networkResult$mixNetwork
      diabloNetworkTuned$visNetwork <- networkResult$visNetwork
    }
  })
  
  output$ncomp.tuned <- renderText(
    paste("Number of components: ", tunedVals$ncomp)
  )
  
  output$keepX.tuned <- renderText(
    paste("Variables of dataset ", names(dataSelection$data), ": ",  tunedVals$keepX)
  )
  
  #' Download handler
  output$Indiv.download <- getDownloadHandler("DIABLO_Sampleplot.png", plot.indiv)
  output$Var.download <- getDownloadHandler("DIABLO_Variableplot.png", plot.var)
  output$Load.download <- getDownloadHandler("DIABLO_Loadingsplot.png", plot.load, width = 2592, height = 1944)
  output$Img.download <- getDownloadHandler("DIABLO_Heatmap.png", plot.img, width = 2592, height = 1944)
  output$Diablo.download <- getDownloadHandler("DIABLO_Diabloplot.png", plot.diablo)
  output$Circos.download <- getDownloadHandler("DIABLO_Circosplot.png", plot.circos, width = 2592, height = 1944)
  output$NetworkHtml.download <- diabloGetNetworkDownloadHandler("DIABLO_Network.html", diabloNetwork, "html")
  output$NetworkGml.download <- diabloGetNetworkDownloadHandler("DIABLO_Network.gml", diabloNetwork, "gml")
  
  output$Indiv.download.tuned <- getDownloadHandler("DIABLO_tuned_Sampleplot.png", plot.indiv.tuned)
  output$Var.download.tuned <- getDownloadHandler("DIABLO_tuned_Variableplot.png", plot.var.tuned)
  output$Load.download.tuned <- getDownloadHandler("DIABLO_tuned_Loadingsplot.png", plot.load.tuned, width = 2592, height = 1944)
  output$Img.download.tuned <- getDownloadHandler("DIABLO_tuned_Heatmap.png", plot.img.tuned, width = 2592, height = 1944)
  output$Diablo.download.tuned <- getDownloadHandler("DIABLO_tuned_Diabloplot.png", plot.diablo.tuned)
  output$Circos.download.tuned <- getDownloadHandler("DIABLO_tuned_Circosplot.png", plot.circos.tuned, width = 2592, height = 1944)
  output$NetworkHtml.download.tuned <- diabloGetNetworkDownloadHandler("DIABLO_tuned_Network.html", diabloNetworkTuned, "html")
  output$NetworkGml.download.tuned <- diabloGetNetworkDownloadHandler("DIABLO_tuned_Network.gml", diabloNetworkTuned, "gml")
}

#' Generate the error messages
generate_diablo_error_messages <- function(input, output, data, classes, dataSelection, classSelection, tunedVals){
  # Error message when selection is incompatible or  data or classes are missing
  inputSelChange <- reactive({
    list(data$data, classes$data, dataSelection$data, classSelection$data)
  })
  
  observeEvent(inputSelChange(), {
    output$errorMsg <- renderText({
      dataCheck = checkMissingData(data$data, classes$data)
      class <- classSelection$data
      data <- dataSelection$data

      if (dataCheck$check){
        dataCheck$msg
      } else {
        if (!diabloCheckValidSelection(data, class)){
        "The selected data and classes are incompatible due to their different amount of samples! 
            Please change your selection!"
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