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
                                                 plotOutput(ns("sPLS.Indiv")))
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
                                                 plotOutput(ns("sPLS.Var")))
                               )
                      ),
                      tabPanel("Loading Plots",
                               fluidRow(
                                 uiOutput(ns("load.comp"))
                               ),
                               fluidRow(
                                 bs4Dash::column(width = 12,
                                                 plotOutput(ns("sPLS.Load")))
                               )
                      ),
                      tabPanel("Selected Variables",
                               fluidRow(
                                 uiOutput(ns("sel.var.comp"))
                               ),
                               fluidRow(
                                 bs4Dash::tabBox(width = 12,
                                                 tabPanel("Dataset 1",
                                                          DT::dataTableOutput(ns("sPLS.X.Sel.Var"))
                                                 ),
                                                 tabPanel("Dataset 2",
                                                          DT::dataTableOutput(ns("sPLS.Y.Sel.Var"))
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
                                                 plotOutput(ns("sPLS.Img")))
                               )
                      ),
                      tabPanel("Arrow Plot",
                               fluidRow(
                                 checkboxInput(ns("namesArrow"), "Samplenames", value = FALSE)
                               ),
                               fluidRow(
                                 bs4Dash::column(width = 12,
                                                 textOutput(ns("arrow.error")),
                                                 plotOutput(ns("sPLS.Arrow")))
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
                            uiOutput(ns("tune.switch")),
                            selectInput(ns("selection"), "Variable selection",
                                        c("Off" = "off", "On" = "on")),
                            uiOutput(ns("keep.x")),
                            uiOutput(ns("keep.y"))
                   )
      )
    )
  )
}

#' PLS Server Functions
#'
#' @noRd
mod_sPLS_server <- function(id, dataset){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    useTunedsPLSVals <<- reactiveVal(FALSE)
    tunedsPLSVals <<- NULL
    
    render_spls_ui_components(ns, input, output, dataset)
    
    observe_spls_ui_components(ns, session, input, output, dataset)
    
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
observe_spls_ui_components <- function(ns, session, input, output, dataset){
  observeDataset <- reactive({
    list(dataset$data1, dataset$data2)
  })
  
  #' Observe dataset
  observeEvent(observeDataset(), {
    output$tune.switch <- renderUI({})
    useTunedsPLSVals(FALSE)
    enable("ncomp")
    enable("scale")
    enable("logratio")
    enable("selection")
  })
  
  #'Variable selection
  observeEvent(input$selection,{
    if(input$selection == "on"){
      output$keep.x <- renderUI({
        size = ncol(dataset$data1)
        numericInput(ns("keepX"), "Variables of dataset 1",
                     min = 2, max = size, value = ifelse(size > 30, 30, round(size/2)),
                     width = "70%")
      })
      
      output$keep.y <- renderUI({
        size = ncol(dataset$data2)
        numericInput(ns("keepY"), "Variables of dataset 2",
                     min = 2, max = size, value = ifelse(size > 30, 30, round(size/2)),
                     width = "70%")
      })
      
    } else {
      output$keep.x <- renderUI({})
      output$keep.y <- renderUI({})
    }
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
      disable("selection")
    } else {
      enable("ncomp")
      enable("scale")
      enable("logratio")
      enable("selection")
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
  keepX <- reactive({
    req(input$keepX)
    keepX <- input$keepX
  })
  
  keepY <- reactive({
    req(input$keepY)
    keepY <- input$keepY
  })
  
  spls.result <<- reactive({
    X <- dataset$data1
    Y <- dataset$data2
    if(useTunedsPLSVals()){  #use tuned variables
      result <- mixOmics::spls(X, Y, ncomp = tunedsPLSVals$ncomp, 
                               keepX = tunedsPLSVals$keepX, keepY = tunedsPLSVals$keepY)
    } else if (input$selection == "on"){   #variable selection on
      result <- mixOmics::spls(X, Y,
                               ncomp = input$ncomp ,logratio = input$logratio,
                               scale = input$scale, keepX = keepX(), keepY = keepY())
    }else { #variable selection off
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
  #' Sample Plot
  output$sPLS.Indiv <- renderPlot({
    mixOmics::plotIndiv(spls.result(), comp = comp.indiv(),
                        group = storability, ind.names = input$indiv.names,
                        legend = TRUE, legend.title = "Storability classes", legend.position = "bottom",
                        rep.space = rep.space())})
  #' Variable Plot
  output$sPLS.Var <- renderPlot({
    mixOmics::plotVar(spls.result(), comp = comp.var(),
                      var.names = input$var.names, pch = c(1,2),
                      legend = TRUE)
  })
  
  #' Loading Plot
  output$sPLS.Load <- renderPlot({
    req(input$spls.load.comp)
    mixOmics::plotLoadings(spls.result(), comp = as.numeric(input$spls.load.comp))})
  
  #' Selected Variables Table
  selVarTable <- reactive({
    req(input$spls.sel.var.comp)
    mixOmics::selectVar(spls.result(), comp = as.numeric(input$spls.sel.var.comp))
  })
  output$sPLS.X.Sel.Var <- DT::renderDataTable({
    listsToMatrix(selVarTable()$X$name, selVarTable()$X$value, c("name", "value"))
  })
  output$sPLS.Y.Sel.Var <- DT::renderDataTable({
    listsToMatrix(selVarTable()$Y$name, selVarTable()$Y$value, c("name", "value"))
  })
  
  #' CIM Plot
  output$sPLS.Img <- renderPlot({
    mixOmics::cim(spls.result(), comp = comp.img())
  })
  
  #' Arrow Plot
  output$sPLS.Arrow <- renderPlot({
    if(splsGetNcomp(input) >= 2){
      mixOmics::plotArrow(spls.result(), group = storability, ind.names = input$namesArrow,
                          legend = TRUE, legend.title = "Storability classes", legend.position = "bottom",
                          X.label = "Dimension 1", Y.label = "Dimension 2")
    }
  })
}