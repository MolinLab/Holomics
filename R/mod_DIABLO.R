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
      bs4Dash::column(width = 6, style = "display: flex; column-gap: 1rem",
                      getDatasetComponent(ns("dataset"), "Select the datasets: ", multiple = TRUE),
                      textOutput(ns("dataset.error"))
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
mod_DIABLO_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    hide("tunedCol")
    hide("switchRow")
    
    dataset <- reactiveValues()
    network <<- reactiveValues()
    network.tuned <<- reactiveValues()
    nodes <<- reactiveValues()
    nodes.tuned <<- reactiveValues()
    useTunedDiabloVals <<- reactiveVal(FALSE)
    tunedDiabloVals <<- reactiveValues(ncomp = 2, keepX = NULL)
    
    render_diablo_ui_components(ns, input, output, dataset)
    
    observe_diablo_ui_components(ns, session, input, output, dataset)
    
    run_diablo_analysis(ns, input, output, dataset)
    
    generate_diablo_plots(ns, input, output, dataset)
    
    generate_diablo_error_messages(ns, input, output, dataset)
    
  })
}

#' Render Ui components
render_diablo_ui_components <- function(ns, input, output, dataset){
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
  
  output$img.comp <- renderUI({
    selectInput(ns("img.comp"), "Component:", seq(1, input$ncomp, 1))
  })
  
  output$diablo.comp <- renderUI({
    selectInput(ns("diablo.comp"), "Component:", seq(1, input$ncomp, 1))
  })
  
  output$nodes <- renderUI({
    req(nodes$data)
    selectInput(ns("nodeNames"), "Select a node", 
                choices = c("---" = "null", combineLists(nodes$data["label"], nodes$data["id"])))
  })
  
  #tuned
  output$indiv.x.comp.tuned <- renderUI({
    selectInput(ns("indiv.x.tuned"), "X-Axis component:", seq(1, tunedDiabloVals$ncomp, 1))
  })
  
  output$indiv.y.comp.tuned <- renderUI({
    selectInput(ns("indiv.y.tuned"), "Y-Axis component:", seq(1, tunedDiabloVals$ncomp, 1), selected = 2)
  })
  
  output$var.x.comp.tuned <- renderUI({
    selectInput(ns("var.x.tuned"), "X-Axis component:", seq(1, tunedDiabloVals$ncomp, 1))
  })
  
  output$var.y.comp.tuned <- renderUI({
    selectInput(ns("var.y.tuned"), "Y-Axis component:", seq(1, tunedDiabloVals$ncomp, 1), selected = 2)
  })
  
  output$load.comp.tuned <- renderUI({
    selectInput(ns("load.comp.tuned"), "Component:", seq(1, tunedDiabloVals$ncomp, 1))
  })
  
  output$sel.var.comp.tuned <- renderUI({
    selectInput(ns("sel.var.comp.tuned"), "Component:", seq(1, tunedDiabloVals$ncomp, 1))
  })
  
  output$img.comp.tuned <- renderUI({
    selectInput(ns("img.comp.tuned"), "Component:", seq(1, tunedDiabloVals$ncomp, 1))
  })
  
  output$diablo.comp.tuned <- renderUI({
    selectizeInput(ns("diablo.comp.tuned"), "Component:", seq(1, tunedDiabloVals$ncomp, 1))
  })
  
  output$nodes.tuned <- renderUI({
    req(nodes.tuned$data)
    selectizeInput(ns("nodeNames.tuned"), "Select a node", 
                   choices = c("---" = "null", combineLists(nodes.tuned$data["label"], nodes.tuned$data["id"])))
  })
}

#'Observe different ui components
observe_diablo_ui_components <- function(ns, session, input, output, dataset){
  
  #' Observe data input change
  observeEvent(input$dataset, {
    data <- list()
    for (d in strsplit(input$dataset, " ")){
      if(d == "t"){
        data[["Transcriptomic"]] <- Holomics::data.transcriptomic_small
      } else if (d == "me"){
        data[["Metabolomic"]] <- Holomics::data.metabolomic_small
      } else if (d == "mi"){
        data[["Microbiomic"]] <- Holomics::data.microbiomic_small
      }
    }
    dataset$data <- data
  })
  
  #' Observe data change
  observeEvent(dataset$data, {
    output$tune.switch <- renderUI({})
    useTunedDiabloVals(FALSE)
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
    runjs(paste0("Shiny.setInputValue('", ns('clicked_node'), "'input$nodeNames"))
    
  })
  
  observeEvent(input$nodeNames.tuned, {
    if (input$nodeNames.tuned == "null"){
      visNetworkProxy(ns("Network.tuned")) %>%
        visUnselectAll()
    }else{
      visNetworkProxy(ns("Network.tuned")) %>%
        visSetSelection(nodesId = input$nodeNames.tuned)
    }
    runjs(paste0("Shiny.setInputValue('", ns('clicked_node.tuned'), "'input$nodeNames.tuned"))
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
    if (!is.null(input$dataset)){
      tryCatch({
        tune_diablo_values(dataset)
        if (!is.null(tunedDiabloVals)){
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
    useTunedDiabloVals(input$tuneSwitch)
    if(input$tuneSwitch){
      show("tunedCol")
    }else{
      hide("tunedCol")
    }
  })
}

#' Tune the ncomp and keepX parameter for the given dataset
tune_diablo_values <- function(dataset){
  X <- dataset$data
  if (!is.null(X)){
    withProgress(message = 'Tuning parameters .... Please wait!', value = 1/3, {
      Y <- sampleClasses
      design <- matrix(0.1, ncol = length(X), nrow = length(X),
                       dimnames = list(names(X), names(X)))
      diag(design) <- 0
      
      #tune ncomp
      set.seed(30)
      perf.diablo <- mixOmics::perf(diablo.result(), validation = 'Mfold', folds = 7, nrepeat = 50, progressBar = TRUE, cpus = 1)
      ncomp = perf.diablo$choice.ncomp$WeightedVote["Overall.BER", "centroids.dist"]
      
      incProgress(1/3)
      
      #tune keepX
      test.keepX = dataset$data
      for (i in 1 : length(names(dataset$data))){
        test.keepX[[i]] = c(5:9, seq(10, 18, 2), seq(20,30,5))
      }
      
      set.seed(30)
      BPPARAM <- BiocParallel::SnowParam(workers = parallel::detectCores()-1)
      tune.diablo = mixOmics::tune.block.splsda(X, Y, ncomp = ncomp,
                                                test.keepX = test.keepX, design = design,
                                                validation = 'Mfold', folds = 7, nrepeat = 1,
                                                BPPARAM = BPPARAM, dist = "centroids.dist", progressBar = TRUE)
      keepX = tune.diablo$choice.keepX
      
      incProgress(1/3)
    })
    
    tunedDiabloVals$ncomp <<- ncomp
    tunedDiabloVals$keepX <<- keepX
  }
}

#' Run analysis
run_diablo_analysis <- function(ns, input, output, dataset){
  diablo.result <<- reactive({
    X <- dataset$data
    Y <- sampleClasses
    if (!is.null(X)){
      design <- matrix(0.1, ncol = length(X), nrow = length(X),
                       dimnames = list(names(X), names(X)))
      diag(design) <- 0
      result <- mixOmics::block.splsda(X, Y,
                                       ncomp = input$ncomp , scale = input$scale,
                                       design = design)
    }
  })
  
  diablo.result.tuned <<- reactive({
    if (useTunedDiabloVals()){
      X <- dataset$data
      Y <- sampleClasses
      if (!is.null(X)){
        design <- matrix(0.1, ncol = length(X), nrow = length(X),
                         dimnames = list(names(X), names(X)))
        diag(design) <- 0
        result <- mixOmics::block.splsda(X, Y, ncomp = tunedDiabloVals$ncomp, keepX = tunedDiabloVals$keepX, design = design)
        
      }
    }
  })
}

#' Business logic functions
generate_diablo_plots <- function(ns, input, output, dataset){
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
  
  comp.img <- reactive({
    req(input$img.comp)
    comp.img <- as.numeric(input$img.comp)
  })
  
  comp.diablo <- reactive({
    req(input$diablo.comp)
    comp.diablo <- as.numeric(input$diablo.comp)
  })
  
  circos.cutoff <- reactive({
    circos.cutoff <- input$cutoffCiros
  })
  
  network.reactive <- reactive ({
    network.reactive <- network
  })
  
  comp.var.tuned <- reactive({
    req(input$var.x.tuned)
    req(input$var.y.tuned)
    comp.var.tuned <- as.numeric(c(input$var.x.tuned,input$var.y.tuned))
  })
  
  comp.indiv.tuned <- reactive({
    req(input$indiv.x.tuned)
    req(input$indiv.y.tuned)
    comp.indiv.tuned <- as.numeric(c(input$indiv.x.tuned,input$indiv.y.tuned))
  })
  
  comp.img.tuned <- reactive({
    req(input$img.comp.tuned)
    comp.img.tuned <- as.numeric(input$img.comp.tuned)
  })
  
  comp.diablo.tuned <- reactive({
    req(input$diablo.comp.tuned)
    comp.diablo.tuned <- as.numeric(input$diablo.comp.tuned)
  })
  
  circos.cutoff.tuned <- reactive({
    circos.cutoff.tuned <- input$cutoffCiros.tuned
  })
  
  network.tuned.reactive <- reactive ({
    network.tuned.reactive <- network.tuned
  })
  
  
  #'  plot functions
  plot.indiv <- function(){
    if(!is.null(diablo.result()) & input$ncomp >= 2){
      mixOmics::plotIndiv(diablo.result(), comp = comp.indiv(),
                          group = sampleClasses, ind.names = input$indiv.names,
                          legend = TRUE, legend.title = classesLabel, legend.position = "bottom")
    }
  }
  
  plot.var <- function(){
    if(!is.null(diablo.result()) & input$ncomp >= 2){
      mixOmics::plotVar(diablo.result(), comp = comp.var(),
                        var.names = input$var.names, pch = seq(1, length(dataset$data), 1),
                        legend = TRUE)
    }
  }
  
  plot.load <- function(){
    if(!is.null(diablo.result())){
      req(input$load.comp)
      mixOmics::plotLoadings(diablo.result(), comp = as.numeric(input$load.comp))
    }
  }
  
  plot.img <- function(){
    if(!is.null(diablo.result()) & length(dataset$data) > 1){
      mixOmics::cimDiablo(diablo.result(), comp = comp.img(), margin=c(8,20), legend.position = "right",
                          size.legend = 1)
    }    
  }
  
  plot.arrow <- function(){
    if(!is.null(diablo.result()) & input$ncomp >= 2){
      mixOmics::plotArrow(diablo.result(), group = sampleClasses, ind.names = input$namesArrow,
                          legend = TRUE, legend.title = classesLabel, legend.position = "bottom",
                          X.label = "Dimension 1", Y.label = "Dimension 2")
    }
  }
  
  plot.diablo <- function(){
    if(!is.null(diablo.result()) & length(dataset$data) > 1){
      mixOmics::plotDiablo(diablo.result(), ncomp = comp.diablo())
    }
  }
  
  plot.circos <- function(){
    if(!is.null(diablo.result()) & length(dataset$data) > 1){
      mixOmics::circosPlot(diablo.result(), cutoff = input$cutoffCircos, line = TRUE,
                           size.labels =1.5, size.variables = .85)
    }
  }
  
  plot.indiv.tuned <- function(){
    if(!is.null(diablo.result.tuned()) & tunedDiabloVals$ncomp >= 2){
      mixOmics::plotIndiv(diablo.result.tuned(), comp = comp.indiv.tuned(),
                          group = sampleClasses, ind.names = input$indiv.names.tuned,
                          legend = TRUE, legend.title = classesLabel, legend.position = "bottom")
    }
  }
  
  plot.var.tuned <- function(){
    if(!is.null(diablo.result.tuned()) & tunedDiabloVals$ncomp >= 2){
      mixOmics::plotVar(diablo.result.tuned(), comp = comp.var.tuned(),
                        var.names = input$var.names.tuned, pch = seq(1, length(dataset$data), 1),
                        legend = TRUE)
    }
  }
  
  plot.load.tuned <- function(){
    if(!is.null(diablo.result.tuned())){
      req(input$load.comp.tuned)
      mixOmics::plotLoadings(diablo.result.tuned(), comp = as.numeric(input$load.comp.tuned))
    }
  }
  
  plot.img.tuned <- function(){
    if(!is.null(diablo.result.tuned()) & length(dataset$data) > 1){
      mixOmics::cimDiablo(diablo.result.tuned(), comp = comp.img.tuned(), margin=c(8,20), legend.position = "right",
                          size.legend = 1)
    }    
  }
  
  plot.arrow.tuned <- function(){
    if(!is.null(diablo.result.tuned()) & tunedDiabloVals$ncomp >= 2){
      mixOmics::plotArrow(diablo.result.tuned(), group = sampleClasses, ind.names = input$namesArrow.tuned,
                          legend = TRUE, legend.title = classesLabel, legend.position = "bottom",
                          X.label = "Dimension 1", Y.label = "Dimension 2")
    }
  }
  
  plot.diablo.tuned <- function(){
    if(!is.null(diablo.result.tuned()) & length(dataset$data) > 1){
      mixOmics::plotDiablo(diablo.result.tuned(), ncomp = comp.diablo.tuned())
    }
  }
  
  plot.circos.tuned <- function(){
    if(!is.null(diablo.result.tuned()) & length(dataset$data) > 1){
      mixOmics::circosPlot(diablo.result.tuned(), cutoff = input$cutoffCircos.tuned, line = TRUE,
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
    if(!is.null(diablo.result()) & length(dataset$data) > 1){
      networkResult = diabloGenerateNetwork(ns, "", diablo.result, dataset, input$cutoffNetwork, input$fullName)
      nodes$data <- networkResult$data
      network$mixNetwork <<- networkResult$mixNetwork
      network$visNetwork <<- networkResult$visNetwork
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
    if(!is.null(diablo.result.tuned()) & length(dataset$data) > 1){
      networkResult = diabloGenerateNetwork(ns, ".tuned", diablo.result.tuned, dataset, input$cutoffNetwork.tuned, input$fullName.tuned)
      nodes.tuned$data <- networkResult$data
      network.tuned$mixNetwork <<- networkResult$mixNetwork
      network.tuned$visNetwork <<- networkResult$visNetwork
    }
  })
  
  output$ncomp.tuned <- renderText(
    paste("Number of components: ", tunedDiabloVals$ncomp)
  )
  
  output$keepX.tuned <- renderText(
    paste("Variables of dataset ", names(dataset$data), ": ",  tunedDiabloVals$keepX)
  )
  
  #' Download handler
  output$Indiv.download <- getDownloadHandler("DIABLO_Sampleplot.png", plot.indiv)
  output$Var.download <- getDownloadHandler("DIABLO_Variableplot.png", plot.var)
  output$Load.download <- getDownloadHandler("DIABLO_Loadingsplot.png", plot.load, width = 2592, height = 1944)
  output$Img.download <- getDownloadHandler("DIABLO_Heatmap.png", plot.img, width = 2592, height = 1944)
  output$Diablo.download <- getDownloadHandler("DIABLO_Diabloplot.png", plot.diablo)
  output$Circos.download <- getDownloadHandler("DIABLO_Circosplot.png", plot.circos, width = 2592, height = 1944)
  output$NetworkHtml.download <- diabloGetNetworkDownloadHandler("DIABLO_Network.html", network.reactive(), "html")
  output$NetworkGml.download <- diabloGetNetworkDownloadHandler("DIABLO_Network.gml", network.reactive(), "gml")
  
  output$Indiv.download.tuned <- getDownloadHandler("DIABLO_tuned_Sampleplot.png", plot.indiv.tuned)
  output$Var.download.tuned <- getDownloadHandler("DIABLO_tuned_Variableplot.png", plot.var.tuned)
  output$Load.download.tuned <- getDownloadHandler("DIABLO_tuned_Loadingsplot.png", plot.load.tuned, width = 2592, height = 1944)
  output$Img.download.tuned <- getDownloadHandler("DIABLO_tuned_Heatmap.png", plot.img.tuned, width = 2592, height = 1944)
  output$Diablo.download.tuned <- getDownloadHandler("DIABLO_tuned_Diabloplot.png", plot.diablo.tuned)
  output$Circos.download.tuned <- getDownloadHandler("DIABLO_tuned_Circosplot.png", plot.circos.tuned, width = 2592, height = 1944)
  output$NetworkHtml.download.tuned <- diabloGetNetworkDownloadHandler("DIABLO_tuned_Network.html", network.tuned.reactive(), "html")
  output$NetworkGml.download.tuned <- diabloGetNetworkDownloadHandler("DIABLO_tuned_Network.gml", network.tuned.reactive(), "gml")
}

#' Generate the error messages
generate_diablo_error_messages <- function(ns, input, output, dataset){
  output$dataset.error <- renderText({
    ifelse (is.null(input$dataset), "Please select a dataset!", "")
  })
  
  output$indiv.error <- renderText({
    return (diabloCheckOneDatasetNcomp(dataset, input))
  })
  
  output$var.error <- renderText({
    return (diabloCheckOneDatasetNcomp(dataset, input))
  })
  
  output$load.error <- renderText({
    return(diabloCheckOneDatasetNcomp(dataset, ncompCheck = FALSE))
  })
  
  output$img.error <- renderText({
    return(diabloCheckTwoDatasets(dataset))
  })
  
  output$arrow.error <- renderText({
    return (diabloCheckOneDatasetNcomp(dataset, input))
    
  })
  
  output$diablo.error <- renderText({
    return(diabloCheckTwoDatasets(dataset))
    
  })
  
  output$circos.error <- renderText({
    return(diabloCheckTwoDatasets(dataset))
    
  })
  
  output$network.error <- renderText({
    return(diabloCheckTwoDatasets(dataset))
  })
  
  #tuned
  output$indiv.error.tuned <- renderText({
    return (diabloCheckOneDatasetNcomp(dataset, input, tuned = TRUE))
  })
  
  output$var.error.tuned <- renderText({
    return (diabloCheckOneDatasetNcomp(dataset, input, tuned = TRUE))
  })
  
  output$load.error.tuned <- renderText({
    return(diabloCheckOneDatasetNcomp(dataset, ncompCheck = FALSE))
  })
  
  output$img.error.tuned <- renderText({
    return(diabloCheckTwoDatasets(dataset))
  })
  
  output$arrow.error.tuned <- renderText({
    return (diabloCheckOneDatasetNcomp(dataset, input, tuned = TRUE))
    
  })
  
  output$diablo.error.tuned <- renderText({
    return(diabloCheckTwoDatasets(dataset))
    
  })
  
  output$circos.error.tuned <- renderText({
    return(diabloCheckTwoDatasets(dataset))
    
  })
  
  output$network.error.tuned <- renderText({
    return(diabloCheckTwoDatasets(dataset))
  })
}