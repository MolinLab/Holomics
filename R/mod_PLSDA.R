#' PLSDA UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_PLSDA_ui <- function(id){
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
                                                 plotOutput(ns("PLSDA.Indiv")))             
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
                                                 plotOutput(ns("PLSDA.Var")))         
                               )
                      ),
                      tabPanel("Loading Plot",
                               fluidRow(
                                 bs4Dash::column(width = 12,
                                                 uiOutput(ns("load.comp")),
                                                 selectInput(ns("plsda.load.cont"), "Contribution:", 
                                                             choices = c("minimal" = "min", "maximal" = "max")),
                                                 selectInput(ns("plsda.load.method"), "Method:", 
                                                             choices = c("Mean" = "mean", "Median" = "median")),
                                                 style = "display: flex; gap: 1rem"             
                                 )
                               ),
                               fluidRow(
                                 bs4Dash::column(width = 12,
                                                 plotOutput(ns("PLSDA.Load")))
                               )
                      ),
                      tabPanel("Selected Variables",
                               fluidRow(
                                 uiOutput(ns("sel.var.comp"))
                               ),
                               fluidRow(
                                 bs4Dash::column(width = 12,
                                                 DT::dataTableOutput(ns("PLSDA.Sel.Var")))
                               )     
                      )
                      # tabPanel("Background Plot",
                      #          fluidRow(
                      #            selectInput(ns("plsda.background.x"), "X-Axis Component", seq(1, 5, 1)),
                      #            tags$div(style = "width: 1rem;"),
                      #            selectInput(ns("plsda.background.y"), "Y-Axis Component", seq(1, 5, 1), selected = 2)
                      #          ),
                      #          plotOutput(ns("PLSDA.Background"))
                      # )
      )
    ),
    fluidRow(
      bs4Dash::box(
        title = "Analysis Parameters", width = 12,
        fluidRow(style = "gap: 1rem",
                 numericInput(ns("ncomp"), "Number of components:", value = 3, 
                              min = 2, max = 5, step = 1, width = "45%"),
                 selectInput(ns("logratio"), "Logratio:",
                             c("None" = "none",
                               "centered" = "CLR"
                             ), width = "30%"),
                 checkboxInput(ns("scale"), "Scaling", value = TRUE, width = "15%")
        )
      )
    )
  )
}

#' PLSDA Server Functions
#'
#' @noRd 
mod_PLSDA_server <- function(id, dataset){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    render_plsda_ui_components(ns, input, output, dataset)
    
    generate_plsda_plots(ns, input, output, dataset)
    
  })
}

#' Render Ui functions
render_plsda_ui_components <- function(ns, input, output, dataset){
  output$indiv.x.comp <- renderUI({
    selectInput(ns("plsda.indiv.x"), "X-Axis Component:", seq(1, input$ncomp, 1))
  })
  
  output$indiv.y.comp <- renderUI({
    selectInput(ns("plsda.indiv.y"), "Y-Axis Component:", seq(1, input$ncomp, 1), selected = 2)
  })
  
  output$var.x.comp <- renderUI({
    selectInput(ns("plsda.var.x"), "X-Axis Component:", seq(1, input$ncomp, 1))
  })
  
  output$var.y.comp <- renderUI({
    selectInput(ns("plsda.var.y"), "Y-Axis Component:", seq(1, input$ncomp, 1), selected = 2)
  })
  
  output$load.comp <- renderUI({
    selectInput(ns("plsda.load.comp"), "Component:", seq(1, input$ncomp, 1))
  })
  
  output$sel.var.comp <- renderUI({
    selectInput(ns("plsda.sel.var.comp"), "Component:", seq(1, input$ncomp, 1))
  })
}

#' Business logic functions
generate_plsda_plots <- function(ns, input, output, dataset){
  #' Create reactive values
  comp.var <- reactive({ 
    req(input$plsda.var.x)
    req(input$plsda.var.y)
    comp.var <- as.numeric(c(input$plsda.var.x,input$plsda.var.y))
  })
  
  comp.indiv <- reactive({
    req(input$plsda.indiv.x)
    req(input$plsda.indiv.y)
    comp.indiv <- as.numeric(c(input$plsda.indiv.x,input$plsda.indiv.y))
  })
  
  # comp.background <- reactive({
  #   comp.background <- as.numeric(c(input$plsda.background.x, input$plsda.background.y))
  # })
  
  #' run analysis
  plsda.result <- reactive({
    plsda.result <- mixOmics::plsda(dataset$data, Y = Holomics::storability,
                                    ncomp = input$ncomp ,logratio = input$logratio, 
                                    scale = input$scale)
  })
  
  #'Sample Plot
  output$PLSDA.Indiv <- renderPlot({
    mixOmics::plotIndiv(plsda.result(), comp = comp.indiv(), 
                        group = storability, ind.names = input$indiv.names,
                        legend = TRUE, legend.title = "Storability classes")}) 
  
  #' Variable Plot
  output$PLSDA.Var <- renderPlot({
    mixOmics::plotVar(plsda.result(), comp = comp.var(),
                      var.names = input$var.names)
  })
  
  #' Loading Plot
  output$PLSDA.Load <- renderPlot({ 
    req(input$plsda.load.comp)
    mixOmics::plotLoadings(plsda.result(), comp = as.numeric(input$plsda.load.comp),
                           contrib = input$plsda.load.cont, method = input$plsda.load.method)
  })
  
  #' Selected Variables Tables
  output$PLSDA.Sel.Var <- DT::renderDataTable({
    req(input$plsda.sel.var.comp)
    selVar <- mixOmics::selectVar(plsda.result(), comp = as.numeric(input$plsda.sel.var.comp))
    ListsToMatrix(selVar$name, selVar$value, c("name", "value"))
  })
  
  #'Background Plot
  # output$PLSDA.Background <- renderPlot({
  #   background <- mixOmics::background.predict(plsda.result(), comp.predicted = 2, dist = "max.dist")
  #   mixOmics::plotIndiv(plsda.result(), comp = comp.background(), group = storability, 
  #                       legend = TRUE, legend.title = "Storability classes",
  #                       background = background) 
  # })    
}