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
                                                 plotOutput(ns("sPLS.Arrow")))         
                               )
                      )
      )
    ),
    fluidRow(
      bs4Dash::box(title = "Analysis Parameters", width = 12,
                   # bs4Dash::tabBox(width = 12, collapsible = FALSE,
                   # tabPanel("Set parameters",
                   fluidRow(style = "gap: 1rem",
                            numericInput(ns("ncomp"), "Number of components", value = 3, 
                                         min = 2, max = 5, step = 1, width = "45%"),
                            selectInput(ns("logratio"), "Logratio",
                                        c("None" = "none",
                                          "centered" = "CLR"
                                        ), width = "30%"),
                            checkboxInput(ns("scale"), "Scaling", value = TRUE, width = "15%")
                   ),
                   fluidRow(style = "gap: 1rem",
                            uiOutput(ns("keep.x")),
                            uiOutput(ns("keep.y"))
                   )
                   # ),
                   # tabPanel("Tune parameters",
                   # fluidRow(
                   # actionButton(ns("compBtn"), "Tune number of components"),
                   # textOutput(ns("compText"))
                   # )
                   # )
                   # )
                   
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
    
    render_spls_ui_components(ns, input, output, dataset)
    
    generate_spls_plots(ns, input, output, dataset)
    
  })
}

#' Render Ui functions
render_spls_ui_components <- function(ns, input, output, dataset){
  output$indiv.x.comp <- renderUI({
    selectInput(ns("spls.indiv.x"), "X-Axis Component:", seq(1, input$ncomp, 1))
  })
  
  output$indiv.y.comp <- renderUI({
    selectInput(ns("spls.indiv.y"), "Y-Axis Component:", seq(1, input$ncomp, 1), selected = 2)
  })
  
  output$var.x.comp <- renderUI({
    selectInput(ns("spls.var.x"), "X-Axis Component:", seq(1, input$ncomp, 1))
  })
  
  output$var.y.comp <- renderUI({
    selectInput(ns("spls.var.y"), "Y-Axis Component:", seq(1, input$ncomp, 1), selected = 2)
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
  
  output$keep.x <- renderUI({
    size = ncol(dataset$data1)
    numericInput(ns("keepX"), "Keep ... variables of dataset 1", width = "50%",
                 min = 2, max = size, value = round(size/2, 0))
  })
  
  output$keep.y <- renderUI({
    size = ncol(dataset$data2)
    numericInput(ns("keepY"), "Keep ... variables of dataset 2", width = "50%",
                 min = 2, max = size, value = round(size/2, 0))
  })
}

#' Business logic functions
generate_spls_plots <- function(ns, input, output, dataset){
  #' Create reactive values
  
  keepX <- reactive({
    req(input$keepX)
    keepX <- input$keepX
  })
  
  keepY <- reactive({
    req(input$keepY)
    keepY <- input$keepY
  })
  
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
  
  #' run analysis
  spls.result <- reactive({
    spls.result <- mixOmics::spls(X = dataset$data1, Y = dataset$data2,
                                  ncomp = input$ncomp ,logratio = input$logratio, 
                                  scale = input$scale, keepX = keepX(), keepY = keepY())
  })
  
  #' Tune component number
  observeEvent(input$compBtn, {
    tune.spls <- mixOmics::perf(spls.result(), validation = "Mfold", folds = 5, progressBar = TRUE, nrepeat = 100)
    plot(tune.spls$Q2.total)
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
                      var.names = input$var.names)
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
    ListsToMatrix(selVarTable()$X$name, selVarTable()$X$value, c("name", "value"))
  })
  output$sPLS.Y.Sel.Var <- DT::renderDataTable({
    ListsToMatrix(selVarTable()$Y$name, selVarTable()$Y$value, c("name", "value"))
  })
  
  #' CIM Plot
  output$sPLS.Img <- renderPlot({
    mixOmics::cim(spls.result(), comp = comp.img())
  })
  
  #' Arrow Plot
  output$sPLS.Arrow <- renderPlot({
    mixOmics::plotArrow(spls.result(), group = storability, ind.names = input$namesArrow,
                        legend = TRUE, legend.title = "Storability classes", legend.position = "bottom",
                        X.label = "Dimension 1", Y.label = "Dimension 2")
  })
}