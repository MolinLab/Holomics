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
  
  mod_Upload_server("upload", singleomicsData, singleomicsClasses, multiomicsData, multiomicsClasses)
  
  single_dataset <- reactiveValues()
  single_class <- reactiveValues()
  mod_SingleOmics_server("singleOmics", data = singleomicsData, dataSelection = single_dataset,
                         classes = singleomicsClasses, classSelection = single_class)
  mod_PCA_server("PCA", dataset = single_dataset, classes = single_class)
  mod_PLSDA_server("PLSDA", dataset = single_dataset, classes = single_class, multiDataset = multiomicsData)
  
  mod_sPLS_server("sPLS", data = multiomicsData, classes = multiomicsClasses)
  mod_DIABLO_server("DIABLO", data = multiomicsData, classes = multiomicsClasses)
}
