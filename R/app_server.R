#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function( input, output, session ) {
  # Your application server logic 
  omicsData <- reactiveValues(data = list())
  omicsClasses <- reactiveValues(data = list())
  
  mod_Upload_server("upload", omicsData, omicsClasses)
  
  single_dataset <- reactiveValues()
  single_class <- reactiveValues()
  mod_SingleOmics_server("singleOmics", data = omicsData, dataSelection = single_dataset, 
                         classes = omicsClasses, classSelection = single_class)
  mod_PCA_server("PCA", dataset = single_dataset, classes = single_class)
  mod_PLSDA_server("PLSDA", dataset = single_dataset, classes = single_class)
  
  mod_sPLS_server("sPLS", data = omicsData, classes = omicsClasses)
  mod_DIABLO_server("DIABLO", data = omicsData, classes = omicsClasses)
}
