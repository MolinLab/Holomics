#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function( input, output, session ) {
  # Your application server logic 
  singleomicsData <- reactiveValues(data = list())
  multiomicsData <- reactiveValues(data = list())
  singleomicsClasses <- reactiveValues(data = list())
  multiomicsClasses <- reactiveValues(data = list())
  
  tables <- reactiveValues(data = NULL, classes = NULL)
  
  
  mod_Upload_server("upload", singleomicsData, singleomicsClasses, multiomicsData, multiomicsClasses, tables)
  
  mod_PLSDA_server("PLSDA", singleomicsData, singleomicsClasses, multiomicsData, tables)
  mod_PCA_server("PCA", singleomicsData, singleomicsClasses, multiomicsData, tables)
  
  mod_sPLS_server("sPLS", data = multiomicsData, classes = multiomicsClasses)
  mod_DIABLO_server("DIABLO", data = multiomicsData, classes = multiomicsClasses)
}