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
    fluidRow(
      bs4Dash::tabBox(width = 12, collapsible = FALSE,
                      tabPanel("Sample Plot",
                               fluidRow(style = "display: flex; gap: 1rem",
                                        uiOutput(ns("indiv.x.comp")),
                                        uiOutput(ns("indiv.y.comp")),
                                        checkboxInput(ns("indiv.names"), "Samplenames", value = FALSE)
                               ),
                               fluidRow(
                                 bs4Dash::column(width = 12,
                                                 textOutput(ns("indiv.error")),
                                                 plotOutput(ns("DIABLO.Indiv")))
                               )
                      ),
                      tabPanel("Variable Plot",
                               fluidRow(style = "display: flex; gap: 1rem",
                                        uiOutput(ns("var.x.comp")),
                                        uiOutput(ns("var.y.comp")),
                                        checkboxInput(ns("var.names"), "Variablenames", value = FALSE)
                               ),
                               fluidRow(
                                 bs4Dash::column(width = 12,
                                                 textOutput(ns("var.error")),
                                                 plotOutput(ns("DIABLO.Var")))
                               )
                      ),
                      tabPanel("Loading Plots",
                               fluidRow(
                                 uiOutput(ns("load.comp")),
                               ),
                               fluidRow(
                                 bs4Dash::column(width = 12,
                                                 textOutput(ns("load.error")),
                                                 plotOutput(ns("DIABLO.Load")))
                               )
                      ),
                      tabPanel("CIM",
                               fluidRow(
                                 uiOutput(ns("img.comp"))
                               ),
                               fluidRow(
                                 bs4Dash::column(width = 12,
                                                 textOutput(ns("img.error")),
                                                 plotOutput(ns("DIABLO.Img")))
                               )
                      ),
                      tabPanel("Arrow Plot",
                               fluidRow(
                                 checkboxInput(ns("namesArrow"), "Samplenames", value = FALSE)
                               ),
                               fluidRow(
                                 bs4Dash::column(width = 12,
                                                 textOutput(ns("arrow.error")),
                                                 plotOutput(ns("DIABLO.Arrow")))
                               )
                      ),
                      tabPanel("Diablo Plot",
                               fluidRow(
                                 uiOutput(ns("diablo.comp"))
                               ),
                               fluidRow(
                                 bs4Dash::column(width = 12,
                                                 textOutput(ns("diablo.error")),
                                                 plotOutput(ns("DIABLO.Diablo")))
                               )
                      ),
                      tabPanel("Circos Plot",
                               fluidRow(
                                 numericInput(ns("cutoffCircos"), "Cutoff value",
                                              min = 0, max = 1, step = 0.1, value = 0.7)
                               ),
                               fluidRow(
                                 bs4Dash::column(width = 12,
                                                 textOutput(ns("circos.error")),
                                                 plotOutput(ns("DIABLO.Circos")))
                               )
                      ), 
                      tabPanel("Network",
                               fluidRow(style = "gap: 1rem",
                                        numericInput(ns("cutoffNetwork"), "Cutoff value",
                                                     min = 0, max = 1, step = 0.1, value = 0.5),
                                        uiOutput(ns("nodes"))
                               ),
                               fluidRow(
                                 bs4Dash::column(width = 12,
                                                 textOutput(ns("network.error")),
                                                 visNetworkOutput(ns("DIABLO.Network")))
                               )
                      )
      )
    ),
    fluidRow(
      bs4Dash::box(title = "Analysis Parameters", width = 12,
                   fluidRow(style = "gap: 1rem",
                            numericInput(ns("ncomp"), "Number of components", value = 3,
                                         min = 1, max = 5, step = 1, width = "45%"),
                            checkboxInput(ns("scale"), "Scaling", value = TRUE, width = "15%")
                   ),
                   fluidRow(style = "gap: 1rem",
                            actionButton(ns("tune"), "Tune parameters"),
                            uiOutput(ns("tune.switch"))
                   )
      )
    )
  )
}

#' DIABLO Server Functions
#'
#' @noRd
mod_DIABLO_server <- function(id, dataset){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    nodes <<- reactiveValues()
    
    useTunedVal <<- reactiveVal(FALSE)
    tunedVals <<- NULL
    
    render_diablo_ui_components(ns, input, output, dataset)
    
    observe_ui_components(ns, session, input, output, dataset)
    
    run_analysis(ns, input, output, dataset)
    
    generate_error_messages(ns, input, output, dataset)
    
    generate_diablo_plots(ns, input, output, dataset)
    
  })
}

#' Render Ui functions
render_diablo_ui_components <- function(ns, input, output, dataset){
  output$indiv.x.comp <- renderUI({
    selectInput(ns("diablo.indiv.x"), "X-Axis Component:", seq(1, get_ncomp(input), 1))
  })
  
  output$indiv.y.comp <- renderUI({
    selectInput(ns("diablo.indiv.y"), "Y-Axis Component:", seq(1, get_ncomp(input), 1), selected = 2)
  })
  
  output$var.x.comp <- renderUI({
    selectInput(ns("diablo.var.x"), "X-Axis Component:", seq(1, get_ncomp(input), 1))
  })
  
  output$var.y.comp <- renderUI({
    selectInput(ns("diablo.var.y"), "Y-Axis Component:", seq(1, get_ncomp(input), 1), selected = 2)
  })
  
  output$load.comp <- renderUI({
    selectInput(ns("diablo.load.comp"), "Component:", seq(1, get_ncomp(input), 1))
  })
  
  output$sel.var.comp <- renderUI({
    selectInput(ns("diablo.sel.var.comp"), "Component:", seq(1, get_ncomp(input), 1))
  })
  
  output$img.comp <- renderUI({
    selectInput(ns("diablo.img.comp"), "Component:", seq(1, get_ncomp(input), 1))
  })
  
  output$diablo.comp <- renderUI({
    selectInput(ns("diablo.diablo.comp"), "Component:", seq(1, get_ncomp(input), 1))
  })
  
  output$nodes <- renderUI({
    req(nodes$data)
    selectizeInput(ns("nodeNames"), "Select a node", 
                   choices = c("---" = "null", CombineLists(nodes$data["label"], nodes$data["id"])))
  })
}

#'Observe different ui components
observe_ui_components <- function(ns, session, input, output, dataset){
  #' Observe dataset
  observeEvent(dataset$data, {
    output$tune.switch <- renderUI({})
    useTunedVal(FALSE)
    enable("ncomp")
    enable("scale")
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
  
  #' Observe tune button
  observeEvent(input$tune, {
    tunedVals <<- tune_values(dataset)
    if (!is.null(tunedVals)){
      output$tune.switch <- renderUI({materialSwitch(ns("tuneSwitch"), "Use tuned parameters", value = FALSE)})
    }
  })
  
  #' Observe tune switch
  observeEvent(input$tuneSwitch,{
    useTunedVal(input$tuneSwitch)
    if(input$tuneSwitch){
      disable("ncomp")
      disable("scale")
    } else {
      enable("ncomp")
      enable("scale")
    }
  })
}

#' Tune the ncomp and keepX parameter for the given dataset
tune_values <- function(dataset){
  X <- dataset$data
  if (!is.null(X)){
    Y <- storability
    design <- matrix(0.1, ncol = length(X), nrow = length(X),
                     dimnames = list(names(X), names(X)))
    diag(design) <- 0
    
    #tune ncomp
    perf.diablo <- mixOmics::perf(diablo.result(), validation = 'Mfold', folds = 7, nrepeat = 5, progressBar = TRUE, cpus = 1)
    ncomp = perf.diablo$choice.ncomp$WeightedVote["Overall.BER", "centroids.dist"]
    
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
    
    return (list("ncomp" = ncomp, "keepX" = keepX))
  }
}

#' Run analysis
run_analysis <- function(ns, input, output, dataset){
  #' run analysis
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
      
      if(useTunedVal()){
        result <- mixOmics::block.splsda(X, Y, ncomp = tunedVals$ncomp, keepX = tunedVals$keepX, design = design)
      }
      
      result
    }
  })
}

#' Generate the error messages
generate_error_messages <- function(ns, input, output, dataset){
  output$indiv.error <- renderText({
    return (check_oneDataset_ncomp(dataset, input))
  })
  
  output$var.error <- renderText({
    return (check_oneDataset_ncomp(dataset, input))
  })
  
  output$load.error <- renderText({
    return(check_oneDataset_ncomp(dataset, ncompCheck = FALSE))
  })
  
  output$img.error <- renderText({
    return(check_twoDatasets(dataset))
  })
  
  output$arrow.error <- renderText({
    return (check_oneDataset_ncomp(dataset, input))
    
  })
  
  output$diablo.error <- renderText({
    return(check_twoDatasets(dataset))
    
  })
  
  output$circos.error <- renderText({
    return(check_twoDatasets(dataset))
    
  })
  
  output$network.error <- renderText({
    return(check_twoDatasets(dataset))
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
  
  #' generate output plots
  #' Sample Plot
  output$DIABLO.Indiv <- renderPlot({
    if(!is.null(diablo.result()) & get_ncomp(input) >= 2){
      mixOmics::plotIndiv(diablo.result(), comp = comp.indiv(),
                          group = storability, ind.names = input$indiv.names,
                          legend = TRUE, legend.title = "Storability classes", legend.position = "bottom")
    }
  })
  
  #' Variable Plot
  output$DIABLO.Var <- renderPlot({
    if(!is.null(diablo.result()) & get_ncomp(input) >= 2){
      mixOmics::plotVar(diablo.result(), comp = comp.var(),
                        var.names = input$var.names, pch = seq(1, length(dataset$data), 1),
                        legend = TRUE)
    }
  })
  
  #' Loading Plot
  output$DIABLO.Load <- renderPlot({
    if(!is.null(diablo.result())){
      req(input$diablo.load.comp)
      mixOmics::plotLoadings(diablo.result(), comp = as.numeric(input$diablo.load.comp))
    }
  })
  
  #' CIM Plot
  output$DIABLO.Img <- renderPlot({
    if(!is.null(diablo.result()) & length(dataset$data) > 1){
      mixOmics::cimDiablo(diablo.result(), comp = comp.img(), margin=c(5,20), legend.position = "right",
                          size.legend = 1)
    }
  })
  
  #' Arrow Plot
  output$DIABLO.Arrow <- renderPlot({
    if(!is.null(diablo.result()) & get_ncomp(input) >= 2){
      mixOmics::plotArrow(diablo.result(), group = storability, ind.names = input$namesArrow,
                          legend = TRUE, legend.title = "Storability classes", legend.position = "bottom",
                          X.label = "Dimension 1", Y.label = "Dimension 2")
    }
  })
  
  #' Diablo Plot
  output$DIABLO.Diablo <- renderPlot({
    if(!is.null(diablo.result()) & length(dataset$data) > 1){
      mixOmics::plotDiablo(diablo.result(), ncomp = comp.diablo())
    }
  })
  
  #' Circos Plot
  output$DIABLO.Circos <- renderPlot({
    if(!is.null(diablo.result()) & length(dataset$data) > 1){
      mixOmics::circosPlot(diablo.result(), cutoff = input$cutoffCircos, line = TRUE,
                           size.labels =1.5, size.variables = .85)
    }
  })
  
  #' Network Plot
  output$DIABLO.Network <- renderVisNetwork({
    if(!is.null(diablo.result()) & length(dataset$data) > 1){
      network = mixOmics::network(diablo.result(), blocks = seq(1, length(dataset$data), 1), cutoff = input$cutoffNetwork)
      graph <- toVisNetworkData(network$gR)
      graph$nodes$label = RemovePostFix(graph$nodes$label, "_")
      
      nodes$data <- list(label = graph$nodes$label, id = graph$nodes$id)
      
      visNetwork(nodes = graph$nodes, edges = graph$edges) %>% 
        visOptions(highlightNearest = TRUE) %>%
        visPhysics(enabled = FALSE) %>%
        visInteraction(navigationButtons = TRUE)
    }
  })
}