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
                                                 plotOutput(ns("DIABLO.Var")))         
                               )
                      ),
                      tabPanel("Loading Plots",
                               fluidRow(
                                 uiOutput(ns("load.comp")),
                               ),
                               fluidRow(
                                 bs4Dash::column(width = 12,
                                                 plotOutput(ns("DIABLO.Load")))
                               )
                      ),
                      # tabPanel("Selected Variables",
                      #          fluidRow(
                      #            uiOutput(ns("sel.var.comp"))
                      #          ),
                      #          fluidRow(
                      #            bs4Dash::tabBox(width = 12,
                      #                            tabPanel("Dataset 1",
                      #                                     DT::dataTableOutput(ns("DIABLO.X.Sel.Var"))
                      #                            ),
                      #                            tabPanel("Dataset 2",
                      #                                     DT::dataTableOutput(ns("DIABLO.Y.Sel.Var"))
                      #                            )
                      #            )
                      #          )   
                      # ),
                      tabPanel("CIM",
                               fluidRow(
                                 uiOutput(ns("img.comp"))
                               ),
                               fluidRow(
                                 bs4Dash::column(width = 12,
                                                 plotOutput(ns("DIABLO.Img")))         
                               )
                      ),
                      tabPanel("Arrow Plot",
                               fluidRow(
                                 checkboxInput(ns("namesArrow"), "Samplenames", value = FALSE)
                               ),
                               fluidRow(
                                 bs4Dash::column(width = 12,
                                                 plotOutput(ns("DIABLO.Arrow")))         
                               )
                      ),
                      tabPanel("Diablo Plot",
                               fluidRow(
                                 uiOutput(ns("diablo.comp"))
                               ),
                               fluidRow(
                                 bs4Dash::column(width = 12,
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
                                                 plotOutput(ns("DIABLO.Circos")))         
                               )
                      )
      )
    ),
    fluidRow(
      bs4Dash::box(title = "Analysis Parameters", width = 12,
                   fluidRow(style = "gap: 1rem",
                            numericInput(ns("ncomp"), "Number of components", value = 3, 
                                         min = 2, max = 5, step = 1, width = "45%"),
                            checkboxInput(ns("scale"), "Scaling", value = TRUE, width = "15%")
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
    
    render_diablo_ui_components(ns, input, output, dataset)
    
    generate_diablo_plots(ns, input, output, dataset)
    
  })
}

#' Render Ui functions
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
  
  #' run analysis
  diablo.result <- reactive({
    X <- list(data1 = dataset$data1, data2 = dataset$data2)
    design <- matrix(0.1, ncol = length(X), nrow = length(X), 
                     dimnames = list(names(X), names(X)))
    diag(design) <- 0
    diablo.result <- mixOmics::block.splsda(X = X, Y = storability,
                                            ncomp = input$ncomp , scale = input$scale,
                                            design = design)
  })
  
  #' generate output plots
  #' Sample Plot
  output$DIABLO.Indiv <- renderPlot({
    mixOmics::plotIndiv(diablo.result(), comp = comp.indiv(), 
                        group = storability, ind.names = input$indiv.names,
                        legend = TRUE, legend.title = "Storability classes", legend.position = "bottom")}) 
  
  #' Variable Plot
  output$DIABLO.Var <- renderPlot({
    mixOmics::plotVar(diablo.result(), comp = comp.var(),
                      var.names = input$var.names)
  })
  
  #' Loading Plot
  output$DIABLO.Load <- renderPlot({
    req(input$diablo.load.comp)
    mixOmics::plotLoadings(diablo.result(), comp = as.numeric(input$diablo.load.comp))})
  
  #'Selected Variables Table
  selVarTable <- reactive({
    req(input$diablo.sel.var.comp)
    mixOmics::selectVar(diablo.result(), comp = as.numeric(input$diablo.sel.var.comp))
  })
  output$DIABLO.X.Sel.Var <- DT::renderDataTable({
    ListsToMatrix(selVarTable()$X$name, selVarTable()$X$value, c("name", "value"))
  })
  output$DIABLO.Y.Sel.Var <- DT::renderDataTable({
    ListsToMatrix(selVarTable()$Y$name, selVarTable()$Y$value, c("name", "value"))
  })
  
  #' CIM Plot
  output$DIABLO.Img <- renderPlot({
    mixOmics::cimDiablo(diablo.result(), comp = comp.img(), margin=c(5,15), legend.position = "right",
                        size.legend = 1)
  })
  
  #' Arrow Plot
  output$DIABLO.Arrow <- renderPlot({
    mixOmics::plotArrow(diablo.result(), group = storability, ind.names = input$namesArrow,
                        legend = TRUE, legend.title = "Storability classes", legend.position = "bottom",
                        X.label = "Dimension 1", Y.label = "Dimension 2")
  })
  
  #' Diablo Plot
  output$DIABLO.Diablo <- renderPlot({
    mixOmics::plotDiablo(diablo.result(), ncomp = comp.diablo())
  })
  
  #' Circos Plot
  output$DIABLO.Circos <- renderPlot({
    mixOmics::circosPlot(diablo.result(), cutoff = 0.7, line = TRUE,
                         size.labels =1.5, size.variables = .85)
  })
}