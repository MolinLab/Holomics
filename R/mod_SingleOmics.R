#' Single omics UI Functions
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_SingleOmics_ui <- function(id){
  ns <- NS(id)
  tagList(
    shinybusy::add_busy_spinner(spin = "circle", position = "bottom-right", height = "60px", width = "60px"),
    fluidRow(
      bs4Dash::column(width = 12,
        uiOutput(ns("dataSelComp")), 
        uiOutput(ns("classSelComp")),
        textOutput(ns("errorMsg")),
        style = "display: flex; column-gap: 1rem"
      )
    ),
    fluidRow(
      bs4Dash::column( width = 6,
                       h1("PCA"),
                       mod_PCA_ui("PCA")
      ),
      bs4Dash::column( width = 6,
                       h1("PLS-DA"),
                       mod_PLSDA_ui("PLSDA")
      )
    )
  )
}

#' PCA Server Functions
#'
#' @noRd 
mod_SingleOmics_server <- function(id, data, dataSelection, classes, classSelection){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    observeEvent(data$data, {
      output$dataSelComp <- renderUI({
        choice <- ""
        choices <- generateChoices(data$data)
        if (length(choices) != 0L) 
          choice <- choices[[1]]
        
        selection <- isolate(input$dataSelection)
        
        #same choice name as before but the data is not necessarily the same!
        #observeEvent of input$dataSelection will not be triggered
        if(!is.null(selection) && selection == choice){
          dataSelection$data <- data$data[[choice]]
        }
        
        getSelectionComponent(ns("dataSelection"), "Select dataset:", choices, width = "150")
      })
    })
  
    observeEvent(input$dataSelection, {
      dataSelection$data <- data$data[[input$dataSelection]]
      dataSelection$name <- input$dataSelection
    })
    
    observeEvent(classes$data, {
      output$classSelComp <- renderUI({
        choices <- generateChoices(classes$data)
        choice = ""
        if (length(choices) != 0L) 
          choice <- choices[[1]]
        
        selection <- isolate(input$classSelection)
        
        #same choice name as before but the data is not necessarily the same!
        #observeEvent of input$dataSelection will not be triggered
        if(!is.null(selection) && selection == choice){
          classSelection$data <- classes$data[[choice]]
        }
        
        getSelectionComponent(ns("classSelection"), "Select classes/labels:", choices, width = "150")
      })
    })
    
    observeEvent(input$classSelection, {
      classSelection$data <- classes$data[[input$classSelection]]
    })
    

    # Error message when selection is incompatible or  data or classes are missing
    inputSelChange <- reactive({  #change of input values or selected values
      list(dataSelection$data, classSelection$data)
    })
    
    observeEvent(inputSelChange(), {
      output$errorMsg <- renderText({
        if(length(data$data) == 0){
          "Please upload some data to be able to use the analysis!"
        } else if(length(classes$data) == 0){
          "Please upload some classes/label information to be able to use the analysis!"
        } else {
          class <- classSelection$data
          data <- dataSelection$data$filtered

          req(data)
          req(class)
          if (length(data) != 0 && nrow(class) != nrow(data)){
            "The selected data and classes are incompatible due to their different amount of samples! 
            Please change your selection!"
          } else if(!identical(class[,1], rownames(data))){
            "The selected data and classes are incompatible as they do not contain the same sample(name)s! 
            Please change your selection!"
          } else {
            ""
          }
        }
      })
    })
  })
}