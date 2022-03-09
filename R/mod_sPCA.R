#' PCA UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_sPCA_ui <- function(id){
  ns <- NS(id)
  tagList(
    h1("Sparse PCA"),
    fluidRow(
      bs4Dash::box(
        title = "Analysis Parameters", width = 12,
        sliderInput(ns("ncomp"), "Number of components:", value = 3, 
                    min = 2, max = 5, step = 1),
        selectInput(ns("logratio"), "Logratio:",
                    c("centered" = "CLR",
                      "None" = "none"   #,"isometric" = "ILR" isometric broken? 'X' must be a numeric matrix (possibly including NA's) with finite values
                    )),
        checkboxInput(ns("scale"), "Scaling:", value = TRUE),
        numericInput(ns("comp1.topx"), "Number top elements contributing to PC1" , value = 10 , min = 1 ,max = 50 ,step = 1),
        numericInput(ns("comp2.topx"), "Number top elements contributing to PC2" , value = 10 , min = 1 ,max = 50 ,step = 1),
        numericInput(ns("comp3.topx"), "Number top elements contributing to PC3" , value = 10 , min = 1 ,max = 50 ,step = 1)
      )
    ),
    
    fluidRow(
      bs4Dash::tabBox(
        width = 12,
        tabPanel("Screeplot",       
                 bs4Dash::column(width = 6,
                                 plotOutput(ns("sPCA.Scree")),
                                 style = "margin: auto;"
                 )
        ),
        tabPanel("Sampleplot", 
                 fluidRow(
                   selectInput(ns("spca.indiv.x"), "X-Axis Component", seq(1, 5, 1)),
                   tags$div(style = "width: 1rem;"),
                   selectInput(ns("spca.indiv.y"), "Y-Axis Component", seq(1, 5, 1), selected = 2)
                 ),
                 fluidRow(
                   bs4Dash::column(width = 6,
                                   plotOutput(ns("sPCA.Indiv")),
                                   style = "margin: auto;"           
                   )
                 )
        ),
        tabPanel("Variableplot",
                 fluidRow(
                   selectInput(ns("spca.var.x"), "X-Axis Component", seq(1, 5, 1)),
                   tags$div(style = "width: 1rem;"),
                   selectInput(ns("spca.var.y"), "Y-Axis Component", seq(1, 5, 1), selected = 2 )
                 ),
                 fluidRow(
                   bs4Dash::column(width = 6,
                                   plotOutput(ns("sPCA.Var")),
                                   style = "margin: auto;"             
                   )
                 )
        ),
        tabPanel("Loadingsplot",
                 fluidRow(
                   selectInput(ns("spca.load.comp"), "Select PCA Component:", 
                               choices = c("PC1"= 1,"PC2"= 2,"PC3"= 3, "PC4" = 4, "PC5" = 5))
                 ),
                 fluidRow(
                   bs4Dash::column(width = 12,
                                   tags$div(style = "width: 12.5%"),
                                   tableOutput(ns("sPCA.Sel.Var")),
                                   tags$div(style = "width: 5%"),
                                   plotOutput(ns("sPCA.Load"), width = "50%"),
                                   style = "margin: auto; display: flex"             
                   )
                 )
        )
      )
    )
  )
}

#' sPCA Server Functions
#'
#' @noRd 
mod_sPCA_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    ## sPCA
    comp.keepX <- reactive({ 
      comp.keepX <- c(input$comp1.topx, input$comp2.topx, input$comp3.topx)
    })
    
    comp.var <- reactive({ 
      comp.var <- as.numeric(c(input$spca.var.x,input$spca.var.y))
    })
    
    comp.indiv <- reactive({ 
      comp.indiv <- as.numeric(c(input$spca.indiv.x,input$spca.indiv.y))
    })
    
    comp.load <- reactive({ 
      comp.indiv <- as.numeric(c(input$spca.load.comp))
    })
    
    spca.result <- reactive({
      spca.result <- mixOmics::spca(Holomics::data.microbiomic, ncomp = input$ncomp, logratio = input$logratio, scale = input$scale, keepX = comp.keepX())
    })
    
    
    output$sPCA.Scree <- renderPlot({
      plot(spca.result())})
    
    output$sPCA.Indiv <- renderPlot({
      mixOmics::plotIndiv(spca.result(), comp = comp.indiv(), ind.names = F, group = storability, legend = TRUE, guide = none)}) 
    
    output$sPCA.Var <- renderPlot({
      mixOmics::plotVar(spca.result(), comp = comp.var() )})   
    
    output$sPCA.Load <- renderPlot({ mixOmics::plotLoadings(spca.result(), comp = as.numeric(input$spca.load.comp)  )})
    
    output$sPCA.Sel.Var <- renderTable({ mixOmics::selectVar(spca.result(), comp = as.numeric(input$spca.load.comp) )})
  })
}



