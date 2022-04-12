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
      bs4Dash::column(width = 6, style = "display: flex; gap: 1rem",
                      selectInput(ns("dataset"), "Select the datasets: ",
                                  choices = c("Transcriptomic"= "t", "Metabolomic"= "me", "Microbiomic" = "mi"), 
                                  multiple = TRUE),
                      textOutput(ns("dataset.error"))
      )
    ),
    fluidRow(
      bs4Dash::column(width = 5,
                      fluidRow(width = 12,
                               diabloGetUi(ns)
                      ),
                      fluidRow(width = 12,
                               bs4Dash::box(title = "Analysis Parameters", width = 12,
                                            fluidRow(style = "gap: 1rem",
                                                     numericInput(ns("ncomp"), "Number of components", value = 3,
                                                                  min = 1, max = 15, step = 1, width = "45%"),
                                                     checkboxInput(ns("scale"), "Scaling", value = TRUE, width = "15%")
                                            )
                               )
                      )
      ),
      bs4Dash::column(width = 2, style = "display: flex; align-items: center;",
                      bs4Dash::box(id = ns("tuneBox"), width = 12,
                                   fluidRow(style = "flex-direction: column",
                                            actionButton(ns("tune"), "Tune parameters"),
                                   ),
                                   fluidRow(
                                     uiOutput(ns("tune.switch"))
                                   )
                      )
      ),
      bs4Dash::column(id = ns("tunedCol"), width = 5,
                      fluidRow(width = 12,
                               diabloGetUi(ns, ".tuned")
                      ),
                      fluidRow(width = 12,
                               bs4Dash::box(title = "Tuned Analysis Parameters", width = 12,
                                            fluidRow(style = "gap: 1rem",
                                                     textOutput(ns("ncomp.tuned")),
                                                     textOutput(ns("keepX.tuned"))
                                            )
                               )
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
    
    dataset <- reactiveValues()
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
    selectInput(ns("diablo.indiv.x"), "X-Axis Component:", seq(1, input$ncomp, 1))
  })
  
  output$indiv.y.comp <- renderUI({
    selectInput(ns("diablo.indiv.y"), "Y-Axis Component:", seq(1, input$ncomp, 1), selected = 2)
  })
  
  output$var.x.comp <- renderUI({
    selectInput(ns("diablo.var.x"), "X-Axis Component:", seq(1, input$ncomp, 1))
  })
  
  output$var.y.comp <- renderUI({
    selectInput(ns("diablo.var.y"), "Y-Axis Component:", seq(1, input$ncomp, 1), selected = 2)
  })
  
  output$load.comp <- renderUI({
    selectInput(ns("diablo.load.comp"), "Component:", seq(1, input$ncomp, 1))
  })
  
  output$sel.var.comp <- renderUI({
    selectInput(ns("diablo.sel.var.comp"), "Component:", seq(1, input$ncomp, 1))
  })
  
  output$img.comp <- renderUI({
    selectInput(ns("diablo.img.comp"), "Component:", seq(1, input$ncomp, 1))
  })
  
  output$diablo.comp <- renderUI({
    selectInput(ns("diablo.diablo.comp"), "Component:", seq(1, input$ncomp, 1))
  })
  
  output$nodes <- renderUI({
    req(nodes$data)
    selectizeInput(ns("nodeNames"), "Select a node", 
                   choices = c("---" = "null", combineLists(nodes$data["label"], nodes$data["id"])))
  })
  
  #tuned
  output$indiv.x.comp.tuned <- renderUI({
    selectInput(ns("diablo.indiv.x.tuned"), "X-Axis Component:", seq(1, tunedDiabloVals$ncomp, 1))
  })
  
  output$indiv.y.comp.tuned <- renderUI({
    selectInput(ns("diablo.indiv.y.tuned"), "Y-Axis Component:", seq(1, tunedDiabloVals$ncomp, 1), selected = 2)
  })
  
  output$var.x.comp.tuned <- renderUI({
    selectInput(ns("diablo.var.x.tuned"), "X-Axis Component:", seq(1, tunedDiabloVals$ncomp, 1))
  })
  
  output$var.y.comp.tuned <- renderUI({
    selectInput(ns("diablo.var.y.tuned"), "Y-Axis Component:", seq(1, tunedDiabloVals$ncomp, 1), selected = 2)
  })
  
  output$load.comp.tuned <- renderUI({
    selectInput(ns("diablo.load.comp.tuned"), "Component:", seq(1, tunedDiabloVals$ncomp, 1))
  })
  
  output$sel.var.comp.tuned <- renderUI({
    selectInput(ns("diablo.sel.var.comp.tuned"), "Component:", seq(1, tunedDiabloVals$ncomp, 1))
  })
  
  output$img.comp.tuned <- renderUI({
    selectInput(ns("diablo.img.comp.tuned"), "Component:", seq(1, tunedDiabloVals$ncomp, 1))
  })
  
  output$diablo.comp.tuned <- renderUI({
    selectInput(ns("diablo.diablo.comp.tuned"), "Component:", seq(1, tunedDiabloVals$ncomp, 1))
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
  })
  
  #' Observe node name selection
  observeEvent(input$nodeNames, {
    if (input$nodeNames == "null"){
      visNetworkProxy(ns("DIABLO.Network")) %>%
        visUnselectAll()
    }else{
      visNetworkProxy(ns("DIABLO.Network")) %>%
        visSetSelection(nodesId = input$nodeNames)
    }
  })
  
  observeEvent(input$nodeNames.tuned, {
    if (input$nodeNames.tuned == "null"){
      visNetworkProxy(ns("DIABLO.Network.tuned")) %>%
        visUnselectAll()
    }else{
      visNetworkProxy(ns("DIABLO.Network.tuned")) %>%
        visSetSelection(nodesId = input$nodeNames.tuned)
    }
  })
  
  #' Observe tune button
  observeEvent(input$tune, {
    if (!is.null(input$dataset)){
      tryCatch({
        tune_diablo_values(dataset)
        if (!is.null(tunedDiabloVals)){
          output$tune.switch <- renderUI({materialSwitch(ns("tuneSwitch"), "Use parameters", value = FALSE)})
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
      Y <- storability
      design <- matrix(0.1, ncol = length(X), nrow = length(X),
                       dimnames = list(names(X), names(X)))
      diag(design) <- 0
      
      #tune ncomp
      perf.diablo <- mixOmics::perf(diablo.result(), validation = 'Mfold', folds = 7, nrepeat = 50, progressBar = TRUE, cpus = 1)
      ncomp = perf.diablo$choice.ncomp$WeightedVote["Overall.BER", "centroids.dist"]
      
      incProgress(1/3)
      
      #tune keepX
      test.keepX = dataset$data
      for (i in 1 : length(names(dataset$data))){
        test.keepX[[i]] = c(5:9, seq(10, 18, 2), seq(20,30,5))
      }
      
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
    Y <- storability
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
      Y <- storability
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
    req(input$diablo.var.x)
    req(input$diablo.var.y)
    comp.var <- as.numeric(c(input$diablo.var.x,input$diablo.var.y))
  })
  
  comp.indiv <- reactive({
    req(input$diablo.indiv.x)
    req(input$diablo.indiv.y)
    comp.indiv <- as.numeric(c(input$diablo.indiv.x,input$diablo.indiv.y))
  })
  
  comp.img <- reactive({
    req(input$diablo.img.comp)
    comp.img <- as.numeric(input$diablo.img.comp)
  })
  
  comp.diablo <- reactive({
    req(input$diablo.diablo.comp)
    comp.img <- as.numeric(input$diablo.diablo.comp)
  })
  
  circos.cutoff <- reactive({
    circos.cutoff <- input$cutoffCiros
  })
  
  comp.var.tuned <- reactive({
    req(input$diablo.var.x.tuned)
    req(input$diablo.var.y.tuned)
    comp.var.tuned <- as.numeric(c(input$diablo.var.x.tuned,input$diablo.var.y.tuned))
  })
  
  comp.indiv.tuned <- reactive({
    req(input$diablo.indiv.x.tuned)
    req(input$diablo.indiv.y.tuned)
    comp.indiv.tuned <- as.numeric(c(input$diablo.indiv.x.tuned,input$diablo.indiv.y.tuned))
  })
  
  comp.img.tuned <- reactive({
    req(input$diablo.img.comp.tuned)
    comp.img.tuned <- as.numeric(input$diablo.img.comp.tuned)
  })
  
  comp.diablo.tuned <- reactive({
    req(input$diablo.diablo.comp.tuned)
    comp.img <- as.numeric(input$diablo.diablo.comp.tuned)
  })
  
  circos.cutoff.tuned <- reactive({
    circos.cutoff.tuned <- input$cutoffCiros.tuned
  })
  
  
  #'  plot functions
  plot.indiv <- function(){
    if(!is.null(diablo.result()) & input$ncomp >= 2){
      mixOmics::plotIndiv(diablo.result(), comp = comp.indiv(),
                          group = storability, ind.names = input$indiv.names,
                          legend = TRUE, legend.title = "Storability classes", legend.position = "bottom")
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
      req(input$diablo.load.comp)
      mixOmics::plotLoadings(diablo.result(), comp = as.numeric(input$diablo.load.comp))
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
      mixOmics::plotArrow(diablo.result(), group = storability, ind.names = input$namesArrow,
                          legend = TRUE, legend.title = "Storability classes", legend.position = "bottom",
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
                          group = storability, ind.names = input$indiv.names.tuned,
                          legend = TRUE, legend.title = "Storability classes", legend.position = "bottom")
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
      req(input$diablo.load.comp.tuned)
      mixOmics::plotLoadings(diablo.result.tuned(), comp = as.numeric(input$diablo.load.comp.tuned))
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
      mixOmics::plotArrow(diablo.result.tuned(), group = storability, ind.names = input$namesArrow.tuned,
                          legend = TRUE, legend.title = "Storability classes", legend.position = "bottom",
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
  #' Sample Plot
  output$DIABLO.Indiv <- renderPlot(
    plot.indiv()
  )
  
  #' Variable Plot
  output$DIABLO.Var <- renderPlot(
    plot.var()
  )
  
  #' Loading Plot
  output$DIABLO.Load <- renderPlot(
    plot.load()
  )
  
  #' CIM Plot
  output$DIABLO.Img <- renderPlot(
    plot.img()
  )
  
  #' Arrow Plot
  output$DIABLO.Arrow <- renderPlot(
    plot.arrow()
  )
  
  #' Diablo Plot
  output$DIABLO.Diablo <- renderPlot(
    plot.diablo()
  )
  
  #' Circos Plot
  output$DIABLO.Circos <- renderPlot(
    plot.circos()
  )
  
  #' Network Plot
  output$DIABLO.Network <- renderVisNetwork({
    if(!is.null(diablo.result()) & length(dataset$data) > 1){
      networkResult = diabloGenerateNetwork(diablo.result, dataset, input$cutoffNetwork)
      nodes$data <- networkResult$data
      network.untuned <<- networkResult$network
    }
  })
  
  #' Sample Plot tuned
  output$DIABLO.Indiv.tuned <- renderPlot(
    plot.indiv.tuned()
  )
  
  #' Variable Plot tuned
  output$DIABLO.Var.tuned <- renderPlot(
    plot.var.tuned()
  )
  
  #' Loading Plot tuned
  output$DIABLO.Load.tuned <- renderPlot(
    plot.load.tuned()
  )
  
  #' CIM Plot tuned
  output$DIABLO.Img.tuned <- renderPlot(
    plot.img.tuned()
  )
  
  #' Arrow Plot tuned
  output$DIABLO.Arrow.tuned <- renderPlot(
    plot.arrow.tuned()
  )
  
  #' Diablo Plot tuned
  output$DIABLO.Diablo.tuned <- renderPlot(
    plot.diablo.tuned()
  )
  
  #' Circos Plot tuned
  output$DIABLO.Circos.tuned <- renderPlot(
    plot.circos.tuned()
  )
  
  #' Network Plot tuned
  output$DIABLO.Network.tuned <- renderVisNetwork({
    if(!is.null(diablo.result.tuned()) & length(dataset$data) > 1){
      networkResult = diabloGenerateNetwork(diablo.result.tuned, dataset, input$cutoffNetwork.tuned)
      nodes.tuned$data <- networkResult$data
      network.tuned <<- networkResult$network
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
  output$Load.download <- getDownloadHandler("DIABLO_Loadingsplot.png", plot.load)
  output$Img.download <- getDownloadHandler("DIABLO_Heatmap.png", plot.img, width = 725)
  output$Diablo.download <- getDownloadHandler("DIABLO_Diabloplot.png", plot.diablo)
  output$Circos.download <- getDownloadHandler("DIABLO_Circosplot.png", plot.circos, width = 725)
  output$NetworkHtml.download <- diabloGetNetworkDownloadHandler("DIABLO_Network.html", network.untuned)
  
  output$Indiv.download.tuned <- getDownloadHandler("DIABLO_tuned_Sampleplot.png", plot.indiv.tuned)
  output$Var.download.tuned <- getDownloadHandler("DIABLO_tuned_Variableplot.png", plot.var.tuned)
  output$Load.download.tuned <- getDownloadHandler("DIABLO_tuned_Loadingsplot.png", plot.load.tuned)
  output$Img.download.tuned <- getDownloadHandler("DIABLO_tuned_Heatmap.png", plot.img.tuned, width = 725)
  output$Diablo.download.tuned <- getDownloadHandler("DIABLO_tuned_Diabloplot.png", plot.diablo.tuned)
  output$Circos.download.tuned <- getDownloadHandler("DIABLO_tuned_Circosplot.png", plot.circos.tuned, width = 725)
  output$NetworkHtml.download.tuned <- diabloGetNetworkDownloadHandler("DIABLO_tuned_Network.html", network.tuned)
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