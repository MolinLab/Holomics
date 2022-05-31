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
  tagList(    fluidRow(
    getAnalysisParametersComponent(ns, TRUE)
  ),
  fluidRow(
    bs4Dash::tabBox(width = 12, collapsible = FALSE,
                    tabPanel("Sample plot", 
                             fluidRow(style = "display: flex; column-gap: 1rem",
                                      uiOutput(ns("indiv.x.comp")),
                                      uiOutput(ns("indiv.y.comp")),
                                      awesomeCheckbox(ns("indiv.names"), "Sample names", value = FALSE)
                             ),
                             fluidRow(
                               bs4Dash::column(width = 12,
                                               plotOutput(ns("Indiv")),
                                               downloadButton(ns("Indiv.download"), "Save plot"))             
                             )
                    ),
                    tabPanel("Variable plot",
                             fluidRow(style = "display: flex; column-gap: 1rem",
                                      uiOutput(ns("var.x.comp")),
                                      uiOutput(ns("var.y.comp")),
                                      awesomeCheckbox(ns("var.names"), "Variable names", value = FALSE)
                             ),
                             fluidRow(
                               bs4Dash::column(width = 12,
                                               plotOutput(ns("Var")),
                                               downloadButton(ns("Var.download"), "Save plot"))         
                             )
                    ),
                    tabPanel("Loading plot",
                             fluidRow(
                               bs4Dash::column(width = 12,
                                               uiOutput(ns("load.comp")),
                                               selectInput(ns("load.cont"), "Contribution:", width = "100",
                                                           choices = c("minimal" = "min", "maximal" = "max")),
                                               selectInput(ns("load.method"), "Method:", width = "100",
                                                           choices = c("mean" = "mean", "median" = "median")),
                                               style = "display: flex; column-gap: 1rem"             
                               )
                             ),
                             fluidRow(
                               bs4Dash::column(width = 12,
                                               plotOutput(ns("Load")),
                                               downloadButton(ns("Load.download"), "Save plot"))
                             )
                    ),
                    tabPanel("Selected variables",
                             fluidRow(
                               uiOutput(ns("sel.var.comp"))
                             ),
                             fluidRow(
                               bs4Dash::column(width = 12,
                                               DT::dataTableOutput(ns("Sel.Var")),
                                               downloadButton(ns("SelVar.download"), "Save table"))
                             )     
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
}

#' Business logic functions
generate_plsda_plots <- function(ns, input, output, dataset){
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
  
  #' run analysis
  result <- reactive({
    result <- mixOmics::plsda(dataset$data, Y = sampleClasses,
                                    ncomp = input$ncomp ,logratio = input$logratio, 
                                    scale = input$scale)
  })
  
  #' plot functions
  plot.indiv <- function(){
    mixOmics::plotIndiv(result(), comp = comp.indiv(), 
                        group = sampleClasses, ind.names = input$indiv.names,
                        legend = TRUE, legend.title = classesLabel)
  }
  
  plot.var <- function(){
    mixOmics::plotVar(result(), comp = comp.var(),
                      var.names = input$var.names)
  }
  
  plot.load <- function(){
    req(input$load.comp)
    mixOmics::plotLoadings(result(), comp = as.numeric(input$load.comp),
                           contrib = input$load.cont, method = input$load.method)
  }
  
  table.selVar <- function(){
    req(input$sel.var.comp)
    selVar <- mixOmics::selectVar(result(), comp = as.numeric(input$sel.var.comp))
    listsToMatrix(selVar$name, selVar$value, c("name", "value"))
  }
  
  #'Sample plot
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
  
  #' Selected Variables Tables
  output$Sel.Var <- DT::renderDataTable(
    table.selVar()
  )
  
  
  #' Download handler
  output$Indiv.download <- getDownloadHandler("PLS-DA_Sampleplot.png", plot.indiv)
  output$Var.download <- getDownloadHandler("PLS-DA_Variableplot.png", plot.var)
  output$Load.download <- getDownloadHandler("PLS-DA_Loadingsplot.png", plot.load, width = 2592, height = 1944)
  output$SelVar.download <- getDownloadHandler("PLS-DA_SelectedVariables.csv", table.selVar, type = "csv")
}