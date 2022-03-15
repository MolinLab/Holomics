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
        width = 12,
        tabPanel("Sampleplot", 
                 fluidRow(
                   selectInput(ns("pca.indiv.x"), "X-Axis Component", seq(1, 5, 1)),
                   tags$div(style = "width: 1rem;"),
                   selectInput(ns("pca.indiv.y"), "Y-Axis Component", seq(1, 5, 1), selected = 2)
                 ),
                 fluidRow(
                   bs4Dash::column(width = 12,
                                   plotOutput(ns("PCA.Indiv")),
                                   style = "margin: auto;")             
                 )
        ),
        tabPanel("Variableplot",
                 fluidRow(
                   selectInput(ns("pca.var.x"), "X-Axis Component", seq(1, 5, 1)),
                   tags$div(style = "width: 1rem;"),
                   selectInput(ns("pca.var.y"), "Y-Axis Component", seq(1, 5, 1), selected = 2)
                 ),
                 fluidRow(
                   bs4Dash::column(width = 12,
                                   plotOutput(ns("PCA.Var")),
                                   style = "margin: auto;")         
                 )
        ),
        tabPanel("Loadingsplot",
                 fluidRow(
                   selectInput(ns("pca.load.comp"), "Select PCA Component:", 
                               choices = c("PC1"= 1,"PC2"= 2,"PC3"= 3, "PC4" = 4, "PC5" = 5))
                 ),
                 fluidRow(
                   bs4Dash::column(width = 12,
                                   tags$div(style = "width: 12.5%"),
                                   tableOutput(ns("PCA.Sel.Var")),
                                   tags$div(style = "width: 5%"),
                                   plotOutput(ns("PCA.Load"), width = "50%"),
                                   style = "margin: auto; display: flex"             
                   )
                 )
        ),
        tabPanel("Screeplot",       
                 bs4Dash::column(width = 12,
                                 plotOutput(ns("PCA.Scree")),
                                 style = "margin: auto;")
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
    
    comp.var <- reactive({ 
      comp.var <- as.numeric(c(input$pca.var.x,input$pca.var.y))
    })
    
    comp.indiv <- reactive({ 
      comp.indiv <- as.numeric(c(input$pca.indiv.x,input$pca.indiv.y))
    })
    
    comp.load <- reactive({ 
      comp.indiv <- as.numeric(c(input$pca.load.comp))
    })
    
    pca.result <- reactive({
      pca.result <- mixOmics::pca(dataset$selection, ncomp = input$ncomp ,logratio = input$logratio, scale = input$scale)
    })
    
    output$PCA.Scree <- renderPlot({
      plot(pca.result())})
    
    output$PCA.Indiv <- renderPlot({
      mixOmics::plotIndiv(pca.result(), comp = comp.indiv(), group = storability, legend = TRUE,
                          title = "Samplesplot")}) 
    
    output$PCA.Var <- renderPlot({
      mixOmics::plotVar(pca.result(), comp = comp.var(), title = "Variableplot" )})
    
    output$PCA.Load <- renderPlot({ mixOmics::plotLoadings(pca.result(), comp = as.numeric(input$pca.load.comp))})
    
    output$PCA.Sel.Var <- renderTable({ mixOmics::selectVar(pca.result(), comp = as.numeric(input$pca.load.comp) )})
  })
}



