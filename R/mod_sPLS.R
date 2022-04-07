#' PLS UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_sPLS_ui <- function(id){
  ns <- NS(id)
  tagList(
    shinybusy::add_busy_spinner(spin = "circle", position = "bottom-right", height = "60px", width = "60px"),
    fluidRow(
      bs4Dash::column(width = 6,
                      selectInput(ns("dataset1"), "Select first dataset:", 
                                  choices = c("Transcriptomic"= "t", "Metabolomic"= "me", "Microbiomic" = "mi"), width = "150"),
                      selectInput(ns("dataset2"), "Select second dataset:", 
                                  choices = c("Transcriptomic"= "t", "Metabolomic"= "me", "Microbiomic" = "mi"), 
                                  selected = "me", width = "fit-content"),
                      style = "display: flex; gap: 1rem"
      )
    ),
    fluidRow(
      bs4Dash::tabBox(width = 12, collapsible = FALSE,
                      tabPanel("Sample Plot",
                               fluidRow(style = "display: flex; gap: 1rem",
                                        uiOutput(ns("indiv.x.comp")),
                                        uiOutput(ns("indiv.y.comp")),
                                        checkboxInput(ns("indiv.names"), "Samplenames", value = FALSE),
                                        selectInput(ns("spls.rep.space"), "Replication space:",
                                                    c("Separated"= "NULL", "X-variate"= "X-variate",
                                                      "Y-variate" = "Y-variate", "XY-variate" = "XY-variate"))
                               ),
                               fluidRow(
                                 bs4Dash::column(width = 12,
                                                 plotOutput(ns("sPLS.Indiv")),
                                                 downloadButton(ns("Indiv.download"), "Save plot"))
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
                                                 plotOutput(ns("sPLS.Var")),
                                                 downloadButton(ns("Var.download"), "Save plot"))
                               )
                      ),
                      tabPanel("Loading Plots",
                               fluidRow(
                                 uiOutput(ns("load.comp"))
                               ),
                               fluidRow(
                                 bs4Dash::column(width = 12,
                                                 plotOutput(ns("sPLS.Load")),
                                                 downloadButton(ns("Load.download"), "Save plot"))
                               )
                      ),
                      tabPanel("Selected Variables",
                               fluidRow(
                                 uiOutput(ns("sel.var.comp"))
                               ),
                               fluidRow(
                                 bs4Dash::tabBox(width = 12,
                                                 tabPanel("Dataset 1",
                                                          DT::dataTableOutput(ns("sPLS.X.Sel.Var")),
                                                          downloadButton(ns("SelVarX.download"), "Save table")
                                                 ),
                                                 tabPanel("Dataset 2",
                                                          DT::dataTableOutput(ns("sPLS.Y.Sel.Var")),
                                                          downloadButton(ns("SelVarY.download"), "Save table")
                                                 )
                                 )
                               )
                      ),
                      tabPanel("CIM",
                               fluidRow(
                                 uiOutput(ns("img.comp"))
                               ),
                               fluidRow(
                                 bs4Dash::column(width = 12,
                                                 plotOutput(ns("sPLS.Img")),
                                                 downloadButton(ns("Img.download"), "Save plot"))
                               )
                      ),
                      tabPanel("Arrow Plot",
                               fluidRow(
                                 checkboxInput(ns("namesArrow"), "Samplenames", value = FALSE)
                               ),
                               fluidRow(
                                 bs4Dash::column(width = 12,
                                                 textOutput(ns("arrow.error")),
                                                 plotOutput(ns("sPLS.Arrow")),
                                                 downloadButton(ns("Arrow.download"), "Save plot"))
                               )
                      )
      )
    ),
    fluidRow(
      bs4Dash::box(title = "Analysis Parameters", width = 12,
                   fluidRow(style = "gap: 1rem",
                            numericInput(ns("ncomp"), "Number of components", value = 3,
                                         min = 1, max = 5, step = 1, width = "45%"),
                            selectInput(ns("logratio"), "Logratio",
                                        c("None" = "none",
                                          "centered" = "CLR"
                                        ), width = "30%"),
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

#' PLS Server Functions
#'
#' @noRd
mod_sPLS_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    dataset <- reactiveValues()
    useTunedsPLSVals <<- reactiveVal(FALSE)
    tunedsPLSVals <<- NULL
    
    render_spls_ui_components(ns, input, output, dataset)
    
    observe_spls_ui_components(ns, input, output, dataset)
    
    run_spls_analysis(ns, input, output, dataset)
    
    generate_spls_error_messages(ns, input, output, dataset)
    
    generate_spls_plots(ns, input, output, dataset)
    
  })
}

#' Render Ui functions
render_spls_ui_components <- function(ns, input, output, dataset){
  output$indiv.x.comp <- renderUI({
    selectInput(ns("spls.indiv.x"), "X-Axis Component:", seq(1, splsGetNcomp(input), 1))
  })
  
  output$indiv.y.comp <- renderUI({
    selectInput(ns("spls.indiv.y"), "Y-Axis Component:", seq(1, splsGetNcomp(input), 1), selected = 2)
  })
  
  output$var.x.comp <- renderUI({
    selectInput(ns("spls.var.x"), "X-Axis Component:", seq(1, splsGetNcomp(input), 1))
  })
  
  output$var.y.comp <- renderUI({
    selectInput(ns("spls.var.y"), "Y-Axis Component:", seq(1, splsGetNcomp(input), 1), selected = 2)
  })
  
  output$load.comp <- renderUI({
    selectInput(ns("spls.load.comp"), "Component:", seq(1, splsGetNcomp(input), 1))
  })
  
  output$sel.var.comp <- renderUI({
    selectInput(ns("spls.sel.var.comp"), "Component:", seq(1, splsGetNcomp(input), 1))
  })
  
  output$img.comp <- renderUI({
    selectInput(ns("spls.img.comp"), "Component:", seq(1, splsGetNcomp(input), 1))
  })
}

#'Observe different ui components
observe_spls_ui_components <- function(ns, input, output, dataset){
  #' Observe change of data selection
  observeEvent(input$dataset1, {
    dataset$data1 <- getDataset(input$dataset1)
  })
  
  observeEvent(input$dataset2, {
    dataset$data2 <- getDataset(input$dataset2)
  })
  
  #' Observe change of data
  observeDataset <- reactive({
    list(dataset$data1, dataset$data2)
  })
  
  observeEvent(observeDataset(), {
    output$tune.switch <- renderUI({})
    useTunedsPLSVals(FALSE)
    enable("ncomp")
    enable("scale")
    enable("logratio")
  })
  
  #' Observe tune button
  observeEvent(input$tune, {
    tunedsPLSVals <<- tune_values(dataset)
    
    if (!is.null(tunedsPLSVals)){
      output$tune.switch <- renderUI({materialSwitch(ns("tuneSwitch"), "Use tuned parameters", value = FALSE)})
    }
  })
  
  #' Observe tune switch
  observeEvent(input$tuneSwitch,{
    useTunedsPLSVals(input$tuneSwitch)
    if(input$tuneSwitch){
      disable("ncomp")
      disable("scale")
      disable("logratio")
    } else {
      enable("ncomp")
      enable("scale")
      enable("logratio")
    }
  })
}

#' Tune the ncomp and keepX parameter for the given dataset
tune_values <- function(dataset){
  withProgress(message = 'Tuning parameters .... Please wait!', value = 1/4, {
    #tune ncomp
    tune.spls <- mixOmics::perf(spls.result(), validation = "Mfold", folds = 7, progressBar = TRUE, nrepeat = 5)
    ncomp <- 3  #TODO change this
    
    incProgress(1/4)
    
    #tune keepX
    list_keepX <- c(2:10, 15, 20)
    BPPARAM <- BiocParallel::SnowParam(workers = parallel::detectCores()-1)
    tune.X <- tune.spls(X, Y, ncomp = ncomp,
                        validation = "Mfold",
                        test.keepX = list_keepX, 
                        measure = "cor", BPPARAM = BPPARAM,
                        folds = 5, nrepeat = 50, progressBar = TRUE)
    keepX <- tune.X$choice.keepX
    
    incProgress(1/4)
    
    #tune keepY
    list_keepY <- c(1:5, 6, 8, 10)
    tune.Y <- tune.spls(X, Y, ncomp = ncomp,
                        validation = "Mfold",
                        test.keepY = list_keepY, 
                        measure = "cor", BPPARAM = BPPARAM,
                        folds = 5, nrepeat = 50, progressBar = TRUE)
    keepY <- tune.Y$choice.keepY
    
    incProgress(1/4)
  })
  
  return (list("ncomp" = ncomp, "keepX" = keepX, "keepY" = keepY))
}

#' Run analysis
run_spls_analysis <- function(ns, input, output, dataset){
  spls.result <<- reactive({
    X <- dataset$data1
    Y <- dataset$data2
    if(useTunedsPLSVals()){  #use tuned variables
      result <- mixOmics::spls(X, Y, ncomp = tunedsPLSVals$ncomp, 
                               keepX = tunedsPLSVals$keepX, keepY = tunedsPLSVals$keepY)
    } else {
      result <- mixOmics::spls(X, Y,
                               ncomp = input$ncomp ,logratio = input$logratio,
                               scale = input$scale)
    }
    result
  })
}

#' Generate the error messages
generate_spls_error_messages <- function(ns, input, output, dataset){
  output$arrow.error <- renderText({
    return (splsCheckNcomp(input))
    
  })
}

#' Business logic functions
generate_spls_plots <- function(ns, input, output, dataset){
  #' Create reactive values
  comp.var <- reactive({
    req(input$spls.var.x)
    req(input$spls.var.y)
    comp.var <- as.numeric(c(input$spls.var.x,input$spls.var.y))
  })
  
  comp.indiv <- reactive({
    req(input$spls.indiv.x)
    req(input$spls.indiv.y)
    comp.indiv <- as.numeric(c(input$spls.indiv.x,input$spls.indiv.y))
  })
  
  comp.img <- reactive({
    req(input$spls.img.comp)
    comp.img <- as.numeric(input$spls.img.comp)
  })
  
  rep.space <- reactive({
    if (identical(input$spls.rep.space, "NULL"))  #necessary, because it's not possible to set NULL as choice value
      NULL
    else
      input$spls.rep.space
  })
  
  #' generate output plots
  #' plot functions
  plot.indiv <- function(){
    mixOmics::plotIndiv(spls.result(), comp = comp.indiv(),
                        group = storability, ind.names = input$indiv.names,
                        legend = TRUE, legend.title = "Storability classes", legend.position = "bottom",
                        rep.space = rep.space())
  }
  
  plot.var <- function(){
    mixOmics::plotVar(spls.result(), comp = comp.var(),
                      var.names = input$var.names, pch = c(1,2),
                      legend = TRUE)
  }
  
  plot.load <- function(){
    req(input$spls.load.comp)
    mixOmics::plotLoadings(spls.result(), comp = as.numeric(input$spls.load.comp))
  }
  
  plot.img <- function(){
    mixOmics::cim(spls.result(), comp = comp.img(), margin=c(8,10))
  }
  
  plot.arrow <- function(){
    if(splsGetNcomp(input) >= 2){
      mixOmics::plotArrow(spls.result(), group = storability, ind.names = input$namesArrow,
                          legend = TRUE, legend.title = "Storability classes", legend.position = "bottom",
                          X.label = "Dimension 1", Y.label = "Dimension 2")
    }
  }
  
  selVarTable <- reactive({
    req(input$spls.sel.var.comp)
    mixOmics::selectVar(spls.result(), comp = as.numeric(input$spls.sel.var.comp))
  })
  
  table.selVarX <- function(){
    listsToMatrix(selVarTable()$X$name, selVarTable()$X$value, c("name", "value"))
  }
  
  table.selVarY <- function(){
    listsToMatrix(selVarTable()$Y$name, selVarTable()$Y$value, c("name", "value"))
  }
  
  #' Sample Plot
  output$sPLS.Indiv <- renderPlot(
    plot.indiv()
  )
  
  #' Variable Plot
  output$sPLS.Var <- renderPlot(
    plot.var()
  )
  
  #' Loading Plot
  output$sPLS.Load <- renderPlot(
    plot.load()
  )
  
  #' Selected Variables Table
  output$sPLS.X.Sel.Var <- DT::renderDataTable(
    table.selVarX()
  )
  output$sPLS.Y.Sel.Var <- DT::renderDataTable(
    table.selVarY()
  )
  
  #' CIM Plot
  output$sPLS.Img <- renderPlot(
    plot.img()
  )
  
  #' Arrow Plot
  output$sPLS.Arrow <- renderPlot(
    plot.arrow()
  )
  
  #' Download handler
  output$Indiv.download <- getDownloadHandler("PLS_Sampleplot.png", plot.indiv)
  output$Var.download <- getDownloadHandler("PLS_Variableplot.png", plot.var)
  output$Load.download <- getDownloadHandler("PLS_Loadingsplot.png", plot.load)
  output$Img.download <- getDownloadHandler("PLS_Heatmap.png", plot.img, width = 725)
  output$Arrow.download <- getDownloadHandler("PLS_Arrowplot.png", plot.arrow, width = 725)
  output$SelVarX.download <- getDownloadHandler("PLS_SelectedVariable1.csv", table.selVarX, type = "csv")
  output$SelVarY.download <- getDownloadHandler("PLS_SelectedVariables2.csv", table.selVarY, type = "csv")
}