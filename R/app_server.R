#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function( input, output, session ) {
  # Your application server logic 
  
  dataset <- reactiveValues()
  mod_SingleOmics_server("singleOmics", dataset = dataset)
  mod_PCA_server("PCA", dataset = dataset)
  mod_PLSDA_server("PLSDA", dataset = dataset)
}
