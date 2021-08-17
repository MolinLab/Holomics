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
      ## 1. Run method (Parameter selection)
      bs4Dash::column(width = 3,
          h2("Choose Components"),
          sliderInput(ns("ncomp"), "Select Number of components:", value = 3, 
                       min = 2, max = 5, step = 1),
          selectInput(ns("logratio"), "Logratio:",
                      c("centered" = "CLR",
                        "None" = "none"   #,"isometric" = "ILR" isometric broken? 'X' must be a numeric matrix (possibly including NA's) with finite values
                        )),
          checkboxInput(ns("scale"), "Scaling:", value = TRUE)
      ),
      
      ## Select minimal no of components with maximum variation
      bs4Dash::column(width = 6,
          plotOutput(ns("PCA"))
      )),
    br(),
    fluidRow(
      bs4Dash::column(width = 3,
          h2("Sample Plot"),
          selectInput(ns("pca.indiv.x"), "X-Axis Component", seq(1,5,1)),
          selectInput(ns("pca.indiv.y"), "Y-Axis Component", seq(1,5,1), selected = 2)
          
          
          ),
      bs4Dash::column(width = 6,
          plotOutput(ns("PCA.Indiv"))             
          )
    ),
    br(),
    fluidRow(
      bs4Dash::column(width = 3,
          h2("Variable Plot"),
          selectInput(ns("pca.var.x"), "X-Axis Component", seq(1,3,1)),
          selectInput(ns("pca.var.y"), "Y-Axis Component", seq(1,3,1), selected = 2 )
         
          ),
      bs4Dash::column(width = 6,
          h2("Plot Variables"),
          plotOutput(ns("PCA.Var")) )
    ),
    br(),
    fluidRow(
      bs4Dash::column(width = 3,
          numericInput(ns("comp1.topx"), "Number top elements contributing to PC1" , value = 10 , min = 1 ,max = 50 ,step = 1),
          numericInput(ns("comp2.topx"), "Number top elements contributing to PC2" , value = 10 , min = 1 ,max = 50 ,step = 1),
          numericInput(ns("comp3.topx"), "Number top elements contributing to PC3" , value = 10 , min = 1 ,max = 50 ,step = 1)
          ),
      bs4Dash::column(width = 6,
          plotOutput(ns("sPCA.Indiv"))
          )
    ),
    br(),
    fluidRow(
      bs4Dash::column(width = 3,
          selectInput(ns("spca.comp"), "Select sPCA Component:", choices = c("PC1"= 1,"PC2"= 2,"PC3"= 3))
          ),
      bs4Dash::column(width = 6,
          plotOutput(ns("sPCA.Var")),
          plotOutput(ns("sPCA.Load")),
          tableOutput(ns("sPCA.Sel.Var"))
          )
    )
  )
}
    
#' PCA Server Functions
#'
#' @noRd 
mod_PCA_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

## PCA    
  comp.var <- reactive({ 
    comp.var <- as.numeric(c(input$pca.var.x,input$pca.var.y))
    })
  
  comp.indiv <- reactive({ 
    comp.indiv <- as.numeric(c(input$pca.indiv.x,input$pca.indiv.y))
  })
  
    
  output$PCA <- renderPlot({
    pca.result <- mixOmics::pca(data.filter, ncomp = input$ncomp ,logratio = input$logratio, scale = input$scale)
    plot(pca.result)})
   
  output$PCA.Indiv <- renderPlot({
    pca.result <- mixOmics::pca(data.filter, ncomp = input$ncomp ,logratio = input$logratio, scale = input$scale)
    mixOmics::plotIndiv(pca.result, comp = comp.indiv(), ind.names = F, group = Y, legend = TRUE, guide = none)}) 
  
  output$PCA.Var <- renderPlot({
    pca.result <- mixOmics::pca(data.filter, ncomp = input$ncomp ,logratio = input$logratio, scale = input$scale)
    mixOmics::plotVar(pca.result, comp = comp.var() )})
## sPCA
  sPCA.results <- reactive({
    sPCA.results <- mixOmics::spca(data.filter, ncomp = input$ncomp, logratio = input$logratio, scale = input$scale, keepX = comp.keepX())
  })
  
  comp.keepX <- reactive({ 
    comp.keepX <- c(input$comp1.topx, input$comp2.topx, input$comp3.topx)
  })
  
  
  output$sPCA.Indiv <- renderPlot({ mixOmics::plotIndiv(sPCA.results()) })
  
  output$sPCA.Var <- renderPlot({ mixOmics::plotVar(sPCA.results(), cex = 2)})
  output$sPCA.Load <- renderPlot({ mixOmics::plotLoadings(sPCA.results(), comp = as.numeric(input$spca.comp)  )})
  output$sPCA.Sel.Var <- renderTable({ mixOmics::selectVar(sPCA.results(), comp = as.numeric(input$spca.comp) )})
     
  })
}
    


