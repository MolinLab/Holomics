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
    bs4Dash::box(
      title = "Analysis parameters", width = 12, collapsed = TRUE,
      fluidRow(style = "column-gap: 1rem",
               numericInput(ns("ncomp"), "Number of components:", value = 3, 
                            min = 1, max = 15, step = 1, width = "45%"),
               selectInput(ns("logratio"), "Logratio:",
                           c("None" = "none",
                             "centered" = "CLR"
                           ), width = "30%"),
               awesomeCheckbox(ns("scale"), "Scaling", value = TRUE, width = "15%")
      )
    )
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
                                               plotOutput(ns("PLSDA.Indiv")),
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
                                               plotOutput(ns("PLSDA.Var")),
                                               downloadButton(ns("Var.download"), "Save plot"))         
                             )
                    ),
                    tabPanel("Loading plot",
                             fluidRow(
                               bs4Dash::column(width = 12,
                                               uiOutput(ns("load.comp")),
                                               selectInput(ns("plsda.load.cont"), "Contribution:", 
                                                           choices = c("minimal" = "min", "maximal" = "max")),
                                               selectInput(ns("plsda.load.method"), "Method:", 
                                                           choices = c("Mean" = "mean", "Median" = "median")),
                                               style = "display: flex; column-gap: 1rem"             
                               )
                             ),
                             fluidRow(
                               bs4Dash::column(width = 12,
                                               plotOutput(ns("PLSDA.Load")),
                                               downloadButton(ns("Load.download"), "Save plot"))
                             )
                    ),
                    tabPanel("Selected variables",
                             fluidRow(
                               uiOutput(ns("sel.var.comp"))
                             ),
                             fluidRow(
                               bs4Dash::column(width = 12,
                                               DT::dataTableOutput(ns("PLSDA.Sel.Var")),
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
    selectInput(ns("plsda.indiv.x"), "X-Axis component:", seq(1, input$ncomp, 1))
  })
  
  output$indiv.y.comp <- renderUI({
    selectInput(ns("plsda.indiv.y"), "Y-Axis component:", seq(1, input$ncomp, 1), selected = 2)
  })
  
  output$var.x.comp <- renderUI({
    selectInput(ns("plsda.var.x"), "X-Axis component:", seq(1, input$ncomp, 1))
  })
  
  output$var.y.comp <- renderUI({
    selectInput(ns("plsda.var.y"), "Y-Axis component:", seq(1, input$ncomp, 1), selected = 2)
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
  
  #' run analysis
  plsda.result <- reactive({
    plsda.result <- mixOmics::plsda(dataset$data, Y = Holomics::storability,
                                    ncomp = input$ncomp ,logratio = input$logratio, 
                                    scale = input$scale)
  })
  
  #' plot functions
  plot.indiv <- function(){
    mixOmics::plotIndiv(plsda.result(), comp = comp.indiv(), 
                        group = storability, ind.names = input$indiv.names,
                        legend = TRUE, legend.title = "Storability classes")
  }
  
  plot.var <- function(){
    mixOmics::plotVar(plsda.result(), comp = comp.var(),
                      var.names = input$var.names)
  }
  
  plot.load <- function(){
    req(input$plsda.load.comp)
    mixOmics::plotLoadings(plsda.result(), comp = as.numeric(input$plsda.load.comp),
                           contrib = input$plsda.load.cont, method = input$plsda.load.method)
  }
  
  table.selVar <- function(){
    req(input$plsda.sel.var.comp)
    selVar <- mixOmics::selectVar(plsda.result(), comp = as.numeric(input$plsda.sel.var.comp))
    listsToMatrix(selVar$name, selVar$value, c("name", "value"))
  }
  
  #'Sample plot
  output$PLSDA.Indiv <- renderPlot(
    plot.indiv()
  ) 
  
  #' Variable plot
  output$PLSDA.Var <- renderPlot(
    plot.var()
  )
  
  #' Loading plot
  output$PLSDA.Load <- renderPlot( 
    plot.load()
  )
  
  #' Selected Variables Tables
  output$PLSDA.Sel.Var <- DT::renderDataTable(
    table.selVar()
  )
  
  
  #' Download handler
  output$Indiv.download <- getDownloadHandler("PLS-DA_Sampleplot.png", plot.indiv)
  output$Var.download <- getDownloadHandler("PLS-DA_Variableplot.png", plot.var)
  output$Load.download <- getDownloadHandler("PLS-DA_Loadingsplot.png", plot.load, width = 2592, height = 1944)
  output$SelVar.download <- getDownloadHandler("PLS-DA_SelectedVariables.csv", table.selVar, type = "csv")
}