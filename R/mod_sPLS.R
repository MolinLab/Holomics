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
                               getAnalysisParametersComponent(ns)
                      ),
                      fluidRow(width = 12,
                               splsGetUi(ns)
                      )
      ),
      bs4Dash::column(width = 2,
                      getTuneBox(ns)
      ),
      bs4Dash::column(id = ns("tunedCol"), width = 5,
                      fluidRow(width = 12,
                               getTunedParametersComponent(ns, TRUE)
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
    useTunedVals <- reactiveVal(FALSE)
    tunedVals <- reactiveValues(ncomp = 2, keepX = NULL)
    
    results <- run_spls_analysis(ns, input, output, dataset, useTunedVals, tunedVals)
    
    render_spls_ui_components(ns, input, output, tunedVals)
    
    observe_spls_ui_components(ns, input, output, dataset, results$result, useTunedVals, tunedVals)
    
    generate_spls_plots(ns, input, output, dataset, results$result, results$resultTuned, tunedVals)
    
    generate_spls_error_messages(ns, input, output, dataset, tunedVals)
    
  })
}

#' Render Ui functions
render_spls_ui_components <- function(ns, input, output, tunedVals){
  
  renderIndivComps(ns, input, output, TRUE, tunedVals)
  
  renderVarComps(ns, input, output, TRUE, tunedVals)
  
  renderLoadComp(ns, input, output, TRUE, tunedVals)
  
  renderSelVarComp(ns, input, output, TRUE, tunedVals)
  
  renderImgComp(ns, input, output, TRUE, tunedVals)
}

#'Observe different ui components
observe_spls_ui_components <- function(ns, input, output, dataset, result, useTunedVals, tunedVals){
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
    useTunedVals(FALSE)
    hide("tunedCol")
    hide("switchRow")
  })
  
  #' Observe tune button
  observeEvent(input$tune, {
    tryCatch({
      tune_values(dataset, result, tunedVals)
      
      if (!is.null(tunedVals)){
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
    useTunedVals(input$tuneSwitch)
    if(input$tuneSwitch){
      show("tunedCol")
    } else {
      hide("tunedCol")
    }
  })
}

#' Tune the ncomp and keepX parameter for the given dataset
tune_values <- function(dataset, result, tunedVals){
  withProgress(message = 'Tuning parameters .... Please wait!', value = 1/4, {
    
    #tune ncomp
    set.seed(30)
    tune.spls <- mixOmics::perf(result(), validation = "Mfold", folds = 7, progressBar = TRUE, nrepeat = 50)
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
  
  tunedVals$ncomp = ncomp
  tunedVals$keepX = keepX
  tunedVals$keepY = keepY
}

#' Run analysis
run_spls_analysis <- function(ns, input, output, dataset, useTunedVals, tunedVals){
  spls.result <- reactive({
    X <- dataset$data1
    Y <- dataset$data2
    spls.result <- mixOmics::spls(X, Y,
                                  ncomp = input$ncomp, scale = input$scale)
  })
  
  spls.result.tuned <- reactive({
    if (useTunedVals()){
      X <- dataset$data1
      Y <- dataset$data2
      spls.result.tuned <- mixOmics::spls(X, Y, ncomp = tunedVals$ncomp, 
                                          keepX = tunedVals$keepX, keepY = tunedVals$keepY)
    }
  })
  
  return(list(result = spls.result, resultTuned = spls.result.tuned))
}

#' Generate the error messages
generate_spls_error_messages <- function(ns, input, output, dataset, tunedVals){
  output$arrow.error <- renderText({
    return (splsCheckNcomp(input))
  })
  
  output$arrow.error.tuned <- renderText({
    return (splsCheckNcomp(input, tuned = TRUE, tunedVals))
  })
}

#' Business logic functions
generate_spls_plots <- function(ns, input, output, dataset, result, resultTuned, tunedVals){
  #' Create reactive values
  comp.indiv <- getCompIndivReactive(input)
  comp.var <- getCompVarReactive(input)
  comp.img <- getCompImgReactive(input)
  
  rep.space <- reactive({
    #necessary, because it's not possible to set NULL as choice value
    if (identical(input$rep.space, "NULL")){
      NULL
    } else{
      input$rep.space
    } 
  })
  
  comp.indiv.tuned <- getCompIndivReactive(input, tuned = TRUE)
  comp.var.tuned <- getCompVarReactive(input, tuned = TRUE)
  comp.img.tuned <- getCompImgReactive(input, tuned = TRUE)
  
  rep.space.tuned <- reactive({
    #necessary, because it's not possible to set NULL as choice value
    if(identical(input$rep.space.tuned, "NULL")){
      NULL
    } else {
      input$rep.space.tuned
    }
  })
  
  
  #' generate output plots
  plot.indiv <- function(){
    plotIndiv(result(), comp.indiv(), indNames = input$indiv.names, 
              repSpace = rep.space(), legendPosition = "bottom")
  }
  
  plot.var <- function(){
    plotVar(result(), comp.var(), input$var.names,
            pch = c(1,2), legend = TRUE)
  }
  
  plot.load <- function(){
    req(input$load.comp)
    plotLoadings(result(), as.numeric(input$load.comp))
  }
  
  plot.img <- function(){
    mixOmics::cim(result(), comp = comp.img(), margin=c(8,10))
  }
  
  plot.arrow <- function(){
    if(splsGetNcomp(input) >= 2){
      plotArrow(result(), input$namesArrow)
    }
  }
  
  selVarTable <- reactive({
    req(input$sel.var.comp)
    selectVar(result(), as.numeric(input$sel.var.comp), XY = TRUE)
  })
  
  table.selVarX <- function(){
    selVarTable()$X
  }
  
  table.selVarY <- function(){
    selVarTable()$Y
  }
  
  #tuned
  plot.indiv.tuned <- function(){
    if (!is.null(resultTuned())){
      plotIndiv(resultTuned(), comp.indiv.tuned(), indNames = input$indiv.names.tuned, 
                repSpace = rep.space.tuned(), legendPosition = "bottom")
    }
  }
  
  plot.var.tuned <- function(){
    if (!is.null(resultTuned())){
      plotVar(resultTuned(), comp.var.tuned(), input$var.names.tuned,
              pch = c(1,2), legend = TRUE)
    }
  }
  
  plot.load.tuned <- function(){
    if (!is.null(resultTuned())){
      req(input$load.comp.tuned)
      plotLoadings(resultTuned(), as.numeric(input$load.comp.tuned))
    }
  }
  
  plot.img.tuned <- function(){
    if (!is.null(resultTuned())){
      mixOmics::cim(resultTuned(), comp = comp.img.tuned(), margin=c(8,10))
    }
  }
  
  plot.arrow.tuned <- function(){
    if(!is.null(resultTuned()) & splsGetNcomp(input, tuned = TRUE, tunedVals) >= 2){
      plotArrow(resultTuned(), input$namesArrow.tuned)
    }
  }
  
  selVarTable.tuned <- reactive({
    if (!is.null(resultTuned())){
      req(input$sel.var.comp.tuned)
      selectVar(resultTuned(), as.numeric(input$sel.var.comp.tuned), XY = TRUE)
    }
  })
  
  table.selVarX.tuned <- function(){
    selVarTable.tuned()$X
  }
  
  table.selVarY.tuned <- function(){
    selVarTable.tuned()$Y
  }
  
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
  
  #' Selected Variables Table
  output$X.Sel.Var <- DT::renderDataTable(
    table.selVarX()
  )
  output$Y.Sel.Var <- DT::renderDataTable(
    table.selVarY()
  )
  
  #' CIM plot
  output$Img <- renderPlot(
    plot.img()
  )
  
  #' Arrow plot
  output$Arrow <- renderPlot(
    plot.arrow()
  )
  
  #tuned
  #' Sample plot
  output$Indiv.tuned <- renderPlot(
    plot.indiv.tuned()
  )
  
  #' Variable plot
  output$Var.tuned <- renderPlot(
    plot.var.tuned()
  )
  
  #' Loading plot
  output$Load.tuned <- renderPlot(
    plot.load.tuned()
  )
  
  #' Selected variables table
  output$X.Sel.Var.tuned <- DT::renderDataTable(
    table.selVarX.tuned()
  )
  output$Y.Sel.Var.tuned <- DT::renderDataTable(
    table.selVarY.tuned()
  )
  
  #' CIM plot
  output$Img.tuned <- renderPlot(
    plot.img.tuned()
  )
  
  #' Arrow plot
  output$Arrow.tuned <- renderPlot(
    plot.arrow.tuned()
  )
  
  output$ncomp.tuned <- renderText(
    paste("Number of components: ", tunedVals$ncomp)
  )
  
  output$keepX.tuned <- renderText(
    paste("Variables of dataset 1: ",  paste(tunedVals$keepX, collapse = ", "))
  )
  
  output$keepY.tuned <- renderText(
    paste("Variables of dataset 2: ",  paste(tunedVals$keepY, collapse = ", "))
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