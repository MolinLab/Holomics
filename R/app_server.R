#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function( input, output, session ) {
  # Your application server logic 
  sampleClasses <<- Holomics::storability
  classesLabel <<- "Storability classes"
  single_dataset <- reactiveValues()
  
  omicsData <- reactiveValues(data = list())
  omicsClasses <- reactiveValues(data = list())
  
  mod_Upload_server("upload", omicsData, omicsClasses )
  
  mod_SingleOmics_server("singleOmics", data = omicsData, selection = single_dataset)
  mod_PCA_server("PCA", dataset = single_dataset)
  mod_PLSDA_server("PLSDA", dataset = single_dataset)
  mod_sPLS_server("sPLS", data = omicsData)
  mod_DIABLO_server("DIABLO", data = omicsData)
}
