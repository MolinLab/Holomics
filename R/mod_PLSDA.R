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
      bs4Dash::tabBox(
        width = 12,
        tabPanel("Sampleplot", 
                 fluidRow(
                   selectInput(ns("plsda.indiv.x"), "X-Axis Component", seq(1, 5, 1)),
                   tags$div(style = "width: 1rem;"),
                   selectInput(ns("plsda.indiv.y"), "Y-Axis Component", seq(1, 5, 1), selected = 2)
                 ),
                 fluidRow(
                   bs4Dash::column(width = 12,
                                   plotOutput(ns("PLSDA.Indiv")),
                                   style = "margin: auto;")             
                 )
        ),
        tabPanel("Variableplot",
                 fluidRow(
                   selectInput(ns("plsda.var.x"), "X-Axis Component", seq(1, 5, 1)),
                   tags$div(style = "width: 1rem;"),
                   selectInput(ns("plsda.var.y"), "Y-Axis Component", seq(1, 5, 1), selected = 2)
                 ),
                 fluidRow(
                   bs4Dash::column(width = 12,
                                   plotOutput(ns("PLSDA.Var")),
                                   style = "margin: auto;")         
                 )
        ),
        tabPanel("Backgroundplot",
                 plotOutput(ns("PLSDA.Background"))
        )
      )
    ),
    fluidRow(
      bs4Dash::box(
        title = "Analysis Parameters", width = 12,
        sliderInput(ns("ncomp"), "Number of components:", value = 3, 
                    min = 2, max = 5, step = 1),
        selectInput(ns("logratio"), "Logratio:",
                    c("centered" = "CLR",
                      "None" = "none"
                      #"isometric" = "ILR"    error that matrix contains NA or infinite values although it does not
                    )),
        checkboxInput(ns("scale"), "Scaling", value = TRUE)
      )
    ),
  )
}

#' PLSDA Server Functions
#'
#' @noRd 
mod_PLSDA_server <- function(id, dataset){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    comp.var <- reactive({ 
      comp.var <- as.numeric(c(input$plsda.var.x,input$plsda.var.y))
    })
    
    comp.indiv <- reactive({ 
      comp.indiv <- as.numeric(c(input$plsda.indiv.x,input$plsda.indiv.y))
    })
    
    comp.load <- reactive({ 
      comp.indiv <- as.numeric(c(input$plsda.load.comp))
    })
    
    plsda.result <- reactive({
      plsda.result <- mixOmics::plsda(dataset$selection, Y = Holomics::storability,
                                      ncomp = input$ncomp ,logratio = input$logratio, 
                                      scale = input$scale)
    })
    
    output$PLSDA.Indiv <- renderPlot({
      mixOmics::plotIndiv(plsda.result(), comp = comp.indiv(), group = storability, legend = TRUE,
                          title = "Samplesplot")}) 
    
    output$PLSDA.Var <- renderPlot({
      mixOmics::plotVar(plsda.result(), comp = comp.var(), title = "Variablesplot" )})
    
    output$PLSDA.Background <- renderPlot({
      background <- mixOmics::background.predict(plsda.result(), comp.predicted = 2, dist = "max.dist")
      mixOmics::plotIndiv(plsda.result(), comp = comp.indiv(), group = storability, legend = TRUE,
                          background = background, title = "Backgroundplot") 
    })
    
  })
}



