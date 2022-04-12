#' PCA UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_PCA_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      bs4Dash::tabBox(
        width = 12, collapsible = FALSE,
        tabPanel("Sample Plot", 
                 fluidRow(style = "display: flex; gap: 1rem",
                          uiOutput(ns("indiv.x.comp")),
                          uiOutput(ns("indiv.y.comp")),
                          checkboxInput(ns("indiv.names"), "Samplenames", value = FALSE)
                 ),
                 fluidRow(
                   bs4Dash::column(width = 12,
                                   plotOutput(ns("PCA.Indiv")),
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
                                   plotOutput(ns("PCA.Var")),
                                   downloadButton(ns("Var.download"), "Save plot"))         
                 )
        ),
        tabPanel("Loading Plot",
                 fluidRow(
                   uiOutput(ns("load.comp")),
                 ),
                 fluidRow(
                   bs4Dash::column(width = 12,
                                   plotOutput(ns("PCA.Load")),
                                   downloadButton(ns("Load.download"), "Save plot"))
                 )
        ),
        tabPanel("Selected Variables",
                 fluidRow(
                   uiOutput(ns("sel.var.comp"))
                 ),
                 fluidRow(
                   bs4Dash::column(width = 12,
                                   DT::dataTableOutput(ns("PCA.Sel.Var")),
                                   downloadButton(ns("SelVar.download"), "Save table")
                   )
                 )     
        ),
        tabPanel("Scree Plot",       
                 bs4Dash::column(width = 12,
                                 plotOutput(ns("PCA.Scree")), 
                                 downloadButton(ns("Scree.download"), "Save plot"))
        )
      )
    ),
    fluidRow(
      bs4Dash::box(title = "Analysis Parameters", width = 12,
                   fluidRow(style = "gap: 1rem", 
                            numericInput(ns("ncomp"), "Number of components", value = 3, 
                                         min = 1, max = 15, step = 1, width = "45%"),
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

#' PCA Server Functions
#'
#' @noRd 
mod_PCA_server <- function(id, dataset){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    render_pca_ui_components(ns, input, output, dataset)
    
    generate_pca_plots(ns, input, output, dataset)
    
  })
}

#' Render Ui functions
render_pca_ui_components <- function(ns, input, output, dataset){
  output$indiv.x.comp <- renderUI({
    selectInput(ns("pca.indiv.x"), "X-Axis Component:", seq(1, input$ncomp, 1))
  })
  
  output$indiv.y.comp <- renderUI({
    selectInput(ns("pca.indiv.y"), "Y-Axis Component:", seq(1, input$ncomp, 1), selected = 2)
  })
  
  output$var.x.comp <- renderUI({
    selectInput(ns("pca.var.x"), "X-Axis Component:", seq(1, input$ncomp, 1))
  })
  
  output$var.y.comp <- renderUI({
    selectInput(ns("pca.var.y"), "Y-Axis Component:", seq(1, input$ncomp, 1), selected = 2)
  })
  
  output$load.comp <- renderUI({
    selectInput(ns("pca.load.comp"), "Component:", seq(1, input$ncomp, 1))
  })
  
  output$sel.var.comp <- renderUI({
    selectInput(ns("pca.sel.var.comp"), "Component:", seq(1, input$ncomp, 1))
  })
}

#' Business logic functions
generate_pca_plots <- function(ns, input, output, dataset){
  #' Create reactive values
  comp.var <- reactive({
    req(input$pca.var.x)
    req(input$pca.var.y)
    comp.var <- as.numeric(c(input$pca.var.x,input$pca.var.y))
  })
  
  comp.indiv <- reactive({ 
    req(input$pca.indiv.x)
    req(input$pca.indiv.y)
    comp.indiv <- as.numeric(c(input$pca.indiv.x,input$pca.indiv.y))
  })
  
  #' run analysis
  pca.result <- reactive({
    pca.result <- mixOmics::pca(dataset$data, ncomp = input$ncomp,
                                logratio = input$logratio, scale = input$scale)
  })
  
  #' plot functions
  plot.scree <- function() {
    plot(pca.result())
  }
  
  plot.indiv <- function(){
    mixOmics::plotIndiv(pca.result(), comp = comp.indiv(), 
                        group = storability, ind.names = input$indiv.names,
                        legend = TRUE, legend.title = "Storability classes")
  }
  
  plot.var <- function(){
    mixOmics::plotVar(pca.result(), comp = comp.var(),
                      var.names = input$var.names)
  }
  
  plot.load <- function(){
    req(input$pca.load.comp)
    mixOmics::plotLoadings(pca.result(), comp = as.numeric(input$pca.load.comp))
  }
  
  table.selVar <- function(){
    req(input$pca.sel.var.comp)
    selVar <- mixOmics::selectVar(pca.result(), comp = as.numeric(input$pca.sel.var.comp))
    listsToMatrix(selVar$name, selVar$value, c("name", "value"))
  }
  
  #'output plots
  #' Sample Plot
  output$PCA.Indiv <- renderPlot(
    plot.indiv()
  )
  
  #' Variable Plot
  output$PCA.Var <- renderPlot(
    plot.var() 
  )
  
  #' Loading Plot
  output$PCA.Load <- renderPlot(
    plot.load()
  )
  
  #'Selected Variables Tables
  output$PCA.Sel.Var <- DT::renderDataTable(
    table.selVar()
  )
  
  #' Scree Plot
  output$PCA.Scree <- renderPlot(
    plot.scree()
  )
  
  #' Download handler
  output$Indiv.download <- getDownloadHandler("PCA_Sampleplot.png", plot.indiv)
  output$Var.download <- getDownloadHandler("PCA_Variableplot.png", plot.var)
  output$Load.download <- getDownloadHandler("PCA_Loadingsplot.png", plot.load)
  output$SelVar.download <- getDownloadHandler("PCA_SelectedVariables.csv", table.selVar, type = "csv")
  output$Scree.download <- getDownloadHandler("PCA_Screeplot.png", plot.scree)
}