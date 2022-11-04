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
                       fluidRow(style = "padding-left: 7.5px;",
                         h1("PCA"),
                       ),
                       fluidRow(
                                bs4Dash::box(title = "General information", width = 12,
                                  htmlOutput(ns("PCAinfotext"))
                                )  
                       ),
                       mod_PCA_ui("PCA")
      ),
      bs4Dash::column( width = 6,
                       fluidRow(style = "padding-left: 7.5px;",
                                h1("PLS-DA"),
                       ),
                       fluidRow(
                                bs4Dash::box(title = "General information", width = 12,
                                             htmlOutput(ns("PLSDAinfotext"))
                                )
                       ),
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
        
        getSelectionComponent(ns("dataSelection"), "Select dataset:", choices, width = "fit-content")
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
        
        getSelectionComponent(ns("classSelection"), "Select classes/labels:", choices, width = "fit-content")
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
    
    output$PCAinfotext <- renderText({
      HTML("The <b>P</b>rincipal <b>C</b>omponent <b>A</b>nalysis decreases the size of the high-dimensional dataset, 
      removes noise from the dataset and presents the similarities between the samples.
      It works unsupervised and determines uncorrelated and orthogonal principal components (PC) in the data.
      Therefore, the PCA only works with the data matrix containing the samples and features information without knowing the classification of the samples.
      It helps to identify characteristics of the data and eventual biases and artefacts by visualising the PCs with the respective features and samples. <br/>
      Additional information can be found on the <a class='mixOmics-link' href='https://mixomicsteam.github.io/Bookdown/pca.html' target='_blank'>mixOmics website</a>
      and in several scientific papers. </br>
      <b>Please adjust the number of components in the 'Analysis parameters' tab according to your selected dataset.</b> 
      We recommend to use a number of components that explains at least 80% of the dataset variance.")
    })
    
    output$PLSDAinfotext <- renderText({
      HTML("The <b>P</b>artial <b>L</b>east-<b>S</b>quares <b>D</b>iscriminant <b>A</b>nalysis is a tool used for multivariate dimension reduction of large datasets. 
      It is similar to the PCA, but with a supervised approach for reducing the input data dimension. This means that the PLS-DA knows the label of each sample when reducing the dimension. 
      The PLS-DA works with the matrix containing the sample and features information and the dataset with the classes of each sample. 
      In the context of multi-omics analyses, it is used to get a first impression of the input data and find the key features of the datasets.<br/>
      Additional information can be found on the <a class='mixOmics-link' href='https://mixomicsteam.github.io/Bookdown/plsda.html' target='_blank'>mixOmics website</a>
      and in several scientific papers. </br>
      <b>Please adjust the number of components in the 'Analysis parameters' tab according to your selected dataset.</b>")
    })
  })
}