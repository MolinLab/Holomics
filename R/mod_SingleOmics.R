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
        choices <- generateChoices(data$data)
        getSelectionComponent(ns("dataSelection"), "Select dataset:", choices, width = "150")
      })
    })
  
    observeEvent(input$dataSelection, {
      dataSelection$data <- data$data[[input$dataSelection]]
    })
    
    observeEvent(classes$data, {
      output$classSelComp <- renderUI({
        choices <- generateChoices(classes$data)
        getSelectionComponent(ns("classSelection"), "Select classes/labels:", choices, width = "150")
      })
    })
    
    observeEvent(input$classSelection, {
      classSelection$data <- classes$data[[input$classSelection]]
    })
    

    # Error message when selection is incompatible or  data or classes are missing
    inputSelChange <- reactive({  #change of input values or selected values
      list(data$data, classes$data, dataSelection$data, classSelection$data)
    })
    
    observeEvent(inputSelChange(), {
      output$errorMsg <- renderText({
        dataCheck = checkMissingData(data$data, classes$data)
        class <- classSelection$data
        data <- dataSelection$data
        
        if (dataCheck$check){
          dataCheck$msg
        } else if (length(data) != 0 && nrow(class) != nrow(data)){
          "The selected data and classes are incompatible due to their different amount of samples! 
          Please change your selection!"
        } else {
          ""
        }
      })
    })
  })
}