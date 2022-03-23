#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function( input, output, session ) {
  # Your application server logic 
  
  single_dataset <- reactiveValues()
  multi_two_dataset <- reactiveValues()
  mod_SingleOmics_server("singleOmics", dataset = single_dataset)
  mod_PCA_server("PCA", dataset = single_dataset)
  mod_PLSDA_server("PLSDA", dataset = single_dataset)
  mod_MultiOmics_server("multiOmics", dataset = multi_two_dataset)
  mod_sPLS_server("sPLS", dataset = multi_two_dataset)
  mod_DIABLO_server("DIABLO", dataset = multi_two_dataset)
}
