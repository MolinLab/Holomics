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
                          checkboxInput(ns("names"), "Samplenames", value = FALSE)
                 ),
                 fluidRow(
                   bs4Dash::column(width = 12,
                                   plotOutput(ns("PCA.Indiv")))             
                 )
        ),
        tabPanel("Variable Plot",
                 fluidRow(style = "display: flex; gap: 1rem",
                          uiOutput(ns("var.x.comp")),
                          uiOutput(ns("var.y.comp"))
                 ),
                 fluidRow(
                   bs4Dash::column(width = 12,
                                   plotOutput(ns("PCA.Var")))         
                 )
        ),
        tabPanel("Loading Plot",
                 fluidRow(
                   uiOutput(ns("load.comp")),
                 ),
                 fluidRow(
                   bs4Dash::column(width = 12,
                                   plotOutput(ns("PCA.Load")))
                 )
        ),
        tabPanel("Selected Variables",
                 fluidRow(
                   uiOutput(ns("sel.var.comp"))
                 ),
                 fluidRow(
                   bs4Dash::column(width = 12,
                                   DT::dataTableOutput(ns("PCA.Sel.Var"))
                   )
                 )     
        ),
        tabPanel("Scree Plot",       
                 bs4Dash::column(width = 12,
                                 plotOutput(ns("PCA.Scree")))
        )
      )
    ),
    fluidRow(
      bs4Dash::box(
        title = "Analysis Parameters", width = 12,
        sliderInput(ns("ncomp"), "Number of components:", value = 3, 
                    min = 2, max = 5, step = 1),
        selectInput(ns("logratio"), "Logratio:",
                    c("None" = "none",
                      "centered" = "CLR"
                    )),
        checkboxInput(ns("scale"), "Scaling", value = TRUE)
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
    
    #' Render Ui functions
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
   
     
    #' Business logic 
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
    
    pca.result <- reactive({
      pca.result <- mixOmics::pca(dataset$data, ncomp = input$ncomp ,logratio = input$logratio, scale = input$scale)
    })
    
    output$PCA.Scree <- renderPlot({
      plot(pca.result())})
    
    output$PCA.Indiv <- renderPlot({
      mixOmics::plotIndiv(pca.result(), comp = comp.indiv(), 
                          group = storability, ind.names = input$names,
                          legend = TRUE, legend.title = "Storability classes")})
    
    output$PCA.Var <- renderPlot({
      mixOmics::plotVar(pca.result(), comp = comp.var())})
    
    output$PCA.Load <- renderPlot({
      req(input$pca.load.comp)
      mixOmics::plotLoadings(pca.result(), comp = as.numeric(input$pca.load.comp))})
    
    output$PCA.Sel.Var <- DT::renderDataTable({
      req(input$pca.sel.var.comp)
      selVar <- mixOmics::selectVar(pca.result(), comp = as.numeric(input$pca.sel.var.comp))
      ListsToMatrix(selVar$name, selVar$value, c("name", "value"))
    })
  })
}