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
                      getDatasetComponent(ns("dataset1"), "Select first dataset:", width = "150"),
                      getDatasetComponent(ns("dataset2"), "Select second dataset:", 
                                  selected = "me", width = "fit-content"),
                      style = "display: flex; column-gap: 1rem"
      )
    ),
    fluidRow(
      bs4Dash::column(width = 5, 
                      fluidRow(width = 12,
                               bs4Dash::box(title = "Analysis parameters", width = 12, collapsed = TRUE,
                                            fluidRow(style = "column-gap: 1rem",
                                                     numericInput(ns("ncomp"), "Number of components", value = 3,
                                                                  min = 1, max = 15, step = 1, width = "45%"),
                                                     selectInput(ns("logratio"), "Logratio",
                                                                 c("None" = "none",
                                                                   "centered" = "CLR"
                                                                 ), width = "30%"),
                                                     awesomeCheckbox(ns("scale"), "Scaling", value = TRUE, width = "15%")
                                            )
                               )
                      ),
                      fluidRow(width = 12,
                               splsGetUi(ns)
                      )
      ),
      bs4Dash::column(width = 2,
                      bs4Dash::box(id = ns("tuneBox"), width = 12,
                                   fluidRow(style = "flex-direction: column",
                                            actionButton(ns("tune"), "Tune parameters"),
                                   ),
                                   fluidRow(id = ns("switchRow"),
                                            uiOutput(ns("tune.switch"))
                                   )
                      )
      ),
      bs4Dash::column(id = ns("tunedCol"), width = 5,
                      fluidRow(width = 12,
                               bs4Dash::box(title = "Tuned analysis parameters", width = 12, collapsed = TRUE,
                                            fluidRow(style = "column-gap: 1rem",
                                                     textOutput(ns("ncomp.tuned")),
                                                     textOutput(ns("keepX.tuned")),
                                                     textOutput(ns("keepY.tuned"))
                                            )
                               )
                      ),
                      fluidRow(width = 12,
                               splsGetUi(ns, ".tuned")
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
    
    hide("tunedCol")
    hide("switchRow")
    
    dataset <- reactiveValues()
    useTunedsPLSVals <<- reactiveVal(FALSE)
    tunedsPLSVals <<- reactiveValues(ncomp = 2, keepX = NULL)
    
    render_spls_ui_components(ns, input, output, dataset)
    
    observe_spls_ui_components(ns, input, output, dataset)
    
    run_spls_analysis(ns, input, output, dataset)
    
    generate_spls_plots(ns, input, output, dataset)
    
    generate_spls_error_messages(ns, input, output, dataset)
    
  })
}

#' Render Ui functions
render_spls_ui_components <- function(ns, input, output, dataset){
  output$indiv.x.comp <- renderUI({
    selectInput(ns("spls.indiv.x"), "X-Axis component:", seq(1, input$ncomp, 1))
  })
  
  output$indiv.y.comp <- renderUI({
    selectInput(ns("spls.indiv.y"), "Y-Axis component:", seq(1, input$ncomp, 1), selected = 2)
  })
  
  output$var.x.comp <- renderUI({
    selectInput(ns("spls.var.x"), "X-Axis component:", seq(1, input$ncomp, 1))
  })
  
  output$var.y.comp <- renderUI({
    selectInput(ns("spls.var.y"), "Y-Axis component:", seq(1, input$ncomp, 1), selected = 2)
  })
  
  output$load.comp <- renderUI({
    selectInput(ns("spls.load.comp"), "Component:", seq(1, input$ncomp, 1))
  })
  
  output$sel.var.comp <- renderUI({
    selectInput(ns("spls.sel.var.comp"), "Component:", seq(1, input$ncomp, 1))
  })
  
  output$img.comp <- renderUI({
    selectInput(ns("spls.img.comp"), "Component:", seq(1, input$ncomp, 1))
  })
  
  #tuned
  output$indiv.x.comp.tuned <- renderUI({
    selectInput(ns("spls.indiv.x.tuned"), "X-Axis component:", seq(1, tunedsPLSVals$ncomp, 1))
  })
  
  output$indiv.y.comp.tuned <- renderUI({
    selectInput(ns("spls.indiv.y.tuned"), "Y-Axis component:", seq(1, tunedsPLSVals$ncomp, 1), selected = 2)
  })
  
  output$var.x.comp.tuned <- renderUI({
    selectInput(ns("spls.var.x.tuned"), "X-Axis component:", seq(1, tunedsPLSVals$ncomp, 1))
  })
  
  output$var.y.comp.tuned <- renderUI({
    selectInput(ns("spls.var.y.tuned"), "Y-Axis component:", seq(1, tunedsPLSVals$ncomp, 1), selected = 2)
  })
  
  output$load.comp.tuned <- renderUI({
    selectInput(ns("spls.load.comp.tuned"), "Component:", seq(1, tunedsPLSVals$ncomp, 1))
  })
  
  output$sel.var.comp.tuned <- renderUI({
    selectInput(ns("spls.sel.var.comp.tuned"), "Component:", seq(1, tunedsPLSVals$ncomp, 1))
  })
  
  output$img.comp.tuned <- renderUI({
    selectInput(ns("spls.img.comp.tuned"), "Component:", seq(1, tunedsPLSVals$ncomp, 1))
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
    hide("tunedCol")
    hide("switchRow")
  })
  
  #' Observe tune button
  observeEvent(input$tune, {
    tryCatch({
      tune_values(dataset)
      
      if (!is.null(tunedsPLSVals)){
        show("switchRow")
        output$tune.switch <- renderUI({materialSwitch(ns("tuneSwitch"), "Use tuned parameters", value = FALSE)})
      }
    }, error = function(cond){
      shinyalert::shinyalert("Error!", "There was an error while trying to tune the parameters. 
                             This can be related with the chosen datasets.", type = "error")
    })
  })
  
  #' Observe tune switch
  observeEvent(input$tuneSwitch,{
    useTunedsPLSVals(input$tuneSwitch)
    if(input$tuneSwitch){
      show("tunedCol")
    } else {
      hide("tunedCol")
    }
  })
}

#' Tune the ncomp and keepX parameter for the given dataset
tune_values <- function(dataset){
  withProgress(message = 'Tuning parameters .... Please wait!', value = 1/4, {
    
    #tune ncomp
    set.seed(30)
    tune.spls <- mixOmics::perf(spls.result(), validation = "Mfold", folds = 7, progressBar = TRUE, nrepeat = 50)
    ncomp <- tune.spls$measures$Q2.total$summary[which.max(tune.spls$measures$Q2.total$summary$mean), 2]
    
    incProgress(1/4)
    
    X <- dataset$data1
    Y <- dataset$data2
    
    #tune keepX
    set.seed(30)
    list_keepX <- c(2:10, 15, 20)
    BPPARAM <- BiocParallel::SnowParam(workers = parallel::detectCores()-1)
    tune.X <- mixOmics::tune.spls(X, Y, ncomp = ncomp,
                                  validation = "Mfold",
                                  test.keepX = list_keepX, 
                                  measure = "cor", BPPARAM = BPPARAM,
                                  folds = 5, nrepeat = 50, progressBar = TRUE)
    keepX <- tune.X$choice.keepX
    
    incProgress(1/4)
    
    #tune keepY
    set.seed(30)
    list_keepY <- c(2:10, 15, 20)
    tune.Y <- mixOmics::tune.spls(X, Y, ncomp = ncomp,
                                  validation = "Mfold",
                                  test.keepY = list_keepY, 
                                  measure = "cor", BPPARAM = BPPARAM,
                                  folds = 5, nrepeat = 50, progressBar = TRUE)
    keepY <- tune.Y$choice.keepY
    
    incProgress(1/4)
  })
  
  tunedsPLSVals$ncomp = ncomp
  tunedsPLSVals$keepX = keepX
  tunedsPLSVals$keepY = keepY
}

#' Run analysis
run_spls_analysis <- function(ns, input, output, dataset){
  spls.result <<- reactive({
    X <- dataset$data1
    Y <- dataset$data2
    result <- mixOmics::spls(X, Y,
                             ncomp = input$ncomp ,logratio = input$logratio,
                             scale = input$scale)
  })
  
  spls.result.tuned <<- reactive({
    if (useTunedsPLSVals()){
      X <- dataset$data1
      Y <- dataset$data2
      result <- mixOmics::spls(X, Y, ncomp = tunedsPLSVals$ncomp, 
                               keepX = tunedsPLSVals$keepX, keepY = tunedsPLSVals$keepY)
    }
  })
}

#' Generate the error messages
generate_spls_error_messages <- function(ns, input, output, dataset){
  output$arrow.error <- renderText({
    return (splsCheckNcomp(input))
  })
  
  output$arrow.error.tuned <- renderText({
    return (splsCheckNcomp(input, tuned = TRUE))
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
    #necessary, because it's not possible to set NULL as choice value
    if (identical(input$spls.rep.space, "NULL")){
      NULL
    } else{
      input$spls.rep.space
    } 
  })
  
  comp.var.tuned <- reactive({
    req(input$spls.var.x.tuned)
    req(input$spls.var.y.tuned)
    comp.var.tuned <- as.numeric(c(input$spls.var.x.tuned,input$spls.var.y.tuned))
  })
  
  comp.indiv.tuned <- reactive({
    req(input$spls.indiv.x.tuned)
    req(input$spls.indiv.y.tuned)
    comp.indiv.tuned <- as.numeric(c(input$spls.indiv.x.tuned,input$spls.indiv.y.tuned))
  })
  
  comp.img.tuned <- reactive({
    req(input$spls.img.comp.tuned)
    comp.img.tuned <- as.numeric(input$spls.img.comp.tuned)
  })
  
  rep.space.tuned <- reactive({
    #necessary, because it's not possible to set NULL as choice value
    if(identical(input$spls.rep.space.tuned, "NULL")){
      NULL
    } else {
      input$spls.rep.space.tuned
    }
  })
  
  
  #' generate output plots
  plot.indiv <- function(){
    mixOmics::plotIndiv(spls.result(), comp = comp.indiv(),
                        group = sampleClasses, ind.names = input$indiv.names,
                        legend = TRUE, legend.title = classesLabel, legend.position = "bottom",
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
      mixOmics::plotArrow(spls.result(), group = sampleClasses, ind.names = input$namesArrow,
                          legend = TRUE, legend.title = classesLabel, legend.position = "bottom",
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
  
  #tuned
  plot.indiv.tuned <- function(){
    if (!is.null(spls.result.tuned())){
      mixOmics::plotIndiv(spls.result.tuned(), comp = comp.indiv.tuned(),
                          group = sampleClasses, ind.names = input$indiv.names.tuned,
                          legend = TRUE, legend.title = classesLabel, legend.position = "bottom",
                          rep.space = rep.space.tuned())
    }
  }
  
  plot.var.tuned <- function(){
    if (!is.null(spls.result.tuned())){
      mixOmics::plotVar(spls.result.tuned(), comp = comp.var.tuned(),
                        var.names = input$var.names.tuned, pch = c(1,2),
                        legend = TRUE)
    }
  }
  
  plot.load.tuned <- function(){
    if (!is.null(spls.result.tuned())){
      req(input$spls.load.comp.tuned)
      mixOmics::plotLoadings(spls.result.tuned(), comp = as.numeric(input$spls.load.comp.tuned))
    }
  }
  
  plot.img.tuned <- function(){
    if (!is.null(spls.result.tuned())){
      mixOmics::cim(spls.result.tuned(), comp = comp.img.tuned(), margin=c(8,10))
    }
  }
  
  plot.arrow.tuned <- function(){
    if(!is.null(spls.result.tuned()) & splsGetNcomp(input, tuned = TRUE) >= 2){
      mixOmics::plotArrow(spls.result.tuned(), group = sampleClasses, ind.names = input$namesArrow.tuned,
                          legend = TRUE, legend.title = classesLabel, legend.position = "bottom",
                          X.label = "Dimension 1", Y.label = "Dimension 2")
    }
  }
  
  selVarTable.tuned <- reactive({
    if (!is.null(spls.result.tuned())){
      req(input$spls.sel.var.comp.tuned)
      mixOmics::selectVar(spls.result.tuned(), comp = as.numeric(input$spls.sel.var.comp.tuned))
    }
  })
  
  table.selVarX.tuned <- function(){
    listsToMatrix(selVarTable.tuned()$X$name, selVarTable.tuned()$X$value, c("name", "value"))
  }
  
  table.selVarY.tuned <- function(){
    listsToMatrix(selVarTable.tuned()$Y$name, selVarTable.tuned()$Y$value, c("name", "value"))
  }
  
  #' Sample plot
  output$sPLS.Indiv <- renderPlot(
    plot.indiv()
  )
  
  #' Variable plot
  output$sPLS.Var <- renderPlot(
    plot.var()
  )
  
  #' Loading plot
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
  
  #' CIM plot
  output$sPLS.Img <- renderPlot(
    plot.img()
  )
  
  #' Arrow plot
  output$sPLS.Arrow <- renderPlot(
    plot.arrow()
  )
  
  #tuned
  #' Sample plot
  output$sPLS.Indiv.tuned <- renderPlot(
    plot.indiv.tuned()
  )
  
  #' Variable plot
  output$sPLS.Var.tuned <- renderPlot(
    plot.var.tuned()
  )
  
  #' Loading plot
  output$sPLS.Load.tuned <- renderPlot(
    plot.load.tuned()
  )
  
  #' Selected variables table
  output$sPLS.X.Sel.Var.tuned <- DT::renderDataTable(
    table.selVarX.tuned()
  )
  output$sPLS.Y.Sel.Var.tuned <- DT::renderDataTable(
    table.selVarY.tuned()
  )
  
  #' CIM plot
  output$sPLS.Img.tuned <- renderPlot(
    plot.img.tuned()
  )
  
  #' Arrow plot
  output$sPLS.Arrow.tuned <- renderPlot(
    plot.arrow.tuned()
  )
  
  output$ncomp.tuned <- renderText(
    paste("Number of components: ", tunedsPLSVals$ncomp)
  )
  
  output$keepX.tuned <- renderText(
    paste("Variables of dataset 1: ",  paste(tunedsPLSVals$keepX, collapse = ", "))
  )
  
  output$keepY.tuned <- renderText(
    paste("Variables of dataset 2: ",  paste(tunedsPLSVals$keepY, collapse = ", "))
  )
  
  #' Download handler
  output$Indiv.download <- getDownloadHandler("PLS_Sampleplot.png", plot.indiv)
  output$Var.download <- getDownloadHandler("PLS_Variableplot.png", plot.var)
  output$Load.download <- getDownloadHandler("PLS_Loadingsplot.png", plot.load, width = 2592, height = 1944)
  output$Img.download <- getDownloadHandler("PLS_Heatmap.png", plot.img, width = 2592, height = 1944)
  output$SelVarX.download <- getDownloadHandler("PLS_SelectedVariable1.csv", table.selVarX, type = "csv")
  output$SelVarY.download <- getDownloadHandler("PLS_SelectedVariables2.csv", table.selVarY, type = "csv")
  
  output$Indiv.download.tuned <- getDownloadHandler("PLS_tuned_Sampleplot.png", plot.indiv.tuned)
  output$Var.download.tuned <- getDownloadHandler("PLS_tuned_Variableplot.png", plot.var.tuned)
  output$Load.download.tuned <- getDownloadHandler("PLS_tuned_Loadingsplot.png", plot.load.tuned, width = 2592, height = 1944)
  output$Img.download.tuned <- getDownloadHandler("PLS_tuned_Heatmap.png", plot.img.tuned, width = 2592, height = 1944)
  output$SelVarX.download.tuned <- getDownloadHandler("PLS_tuned_SelectedVariable1.csv", table.selVarX.tuned, type = "csv")
  output$SelVarY.download.tuned <- getDownloadHandler("PLS_tuned_SelectedVariables2.csv", table.selVarY.tuned, type = "csv")
}