#' DIABLO 
#' 
#' @description A utils function, which generates the UI code for 
#' the diablo page. The postfix is added always on the end of the 
#' id values of the components
#'
#' @return number of components
#'
#' @noRd
diabloGetUi <- function(ns, postfix = ""){
  bs4Dash::tabBox(width = 12, collapsible = FALSE,
                  getSamplePlot(ns, postfix),
                  getVariablePlot(ns, postfix),
                  getLoadingsPlot(ns, postfix),
                  getCimPlot(ns, postfix),
                  getArrowPlot(ns, postfix),
                  tabPanel("Diablo plot",
                           fluidRow(
                             uiOutput(paste0(ns("diablo.comp"), postfix))
                           ),
                           fluidRow(
                             bs4Dash::column(width = 12,
                                             textOutput(paste0(ns("diablo.error"), postfix)),
                                             plotOutput(paste0(ns("Diablo"), postfix)),
                                             downloadButton(paste0(ns("Diablo.download"), postfix), "Save plot"))
                           )
                  ),
                  tabPanel("Circos plot",
                           fluidRow(
                             numericInput(paste0(ns("cutoffCircos"), postfix), "Cutoff value",
                                          min = 0, max = 1, step = 0.1, value = 0.7)
                           ),
                           fluidRow(
                             bs4Dash::column(width = 12,
                                             textOutput(paste0(ns("circos.error"), postfix)),
                                             plotOutput(paste0(ns("Circos"), postfix)),
                                             downloadButton(paste0(ns("Circos.download"), postfix), "Save plot"))
                           )
                  ), 
                  tabPanel("Network",
                           fluidRow(style = "column-gap: 1rem",
                                    numericInput(paste0(ns("cutoffNetwork"), postfix), "Cutoff value",
                                                 min = 0, max = 1, step = 0.1, value = 0.5),
                                    uiOutput(paste0(ns("nodes"), postfix)),
                                    awesomeCheckbox(paste0(ns("fullName"), postfix), "Show full names")
                           ),
                           fluidRow(
                             bs4Dash::column(width = 12,
                                             textOutput(paste0(ns("network.error"), postfix)),
                                             visNetworkOutput(paste0(ns("Network"), postfix)),
                                             downloadButton(paste0(ns("NetworkGml.download"), postfix), "Save as gml"),
                                             downloadButton(paste0(ns("NetworkHtml.download"), postfix), "Save as html")
                             )
                           )
                  )
  )
}


#'
#' @description A utils function, which returns the number of components.
#' If the tune switch is on or off the number is taken from
#' the tuned parameters or the numeric input
#'
#' @return number of components
#'
#' @noRd
diabloGetNcomp <- function(input, tuned){
  return (ifelse(tuned, tunedDiabloVals$ncomp, input$ncomp))
}


#' @description A utils function, which checks if the datasets variable
#' contains at least two datasets
#'
#' @return error message if less than two datasets else empty string
#'
#' @noRd
diabloCheckTwoDatasets <- function(dataset){
  if(length(dataset$data) < 2){
    return("You have to select at least two datasets!")
  } else {
    return("")
  }
}

#' @description A utils function, which checks if the datasets variable
#' contains at least one dataset
#'
#' @return error message if less than two datasets or two components
#'  else empty string
#'  
#' @noRd
diabloCheckOneDatasetNcomp <- function(dataset, input, ncompCheck = TRUE, tuned = FALSE){
  if(length(dataset$data) < 1){
    return("You have to select at least one dataset!")
  } else if(ncompCheck){
    return(diabloCheckNcomp(input, tuned))
  } else {
    return("")
  }
}

#' @description A utils function, which checks if at least two components 
#' are in the model
#'
#' @return error message if less than two components
#'  else empty string
#'  
#' @noRd
diabloCheckNcomp <- function(input, tuned){
  if(diabloGetNcomp(input, tuned) < 2){
    return("There need to be at least two components to render this plot!")
  } else {
    return("")
  }
}

#' @description A utils function, which generates the visNetwork
#' for the given analysis result and selected dataset and cutoff
#'
#' @return list with node data, the visnetwork and the mixomics network
#'  
#' @noRd
diabloGenerateNetwork <- function(ns, postfix, result, dataset, cutoff, fullNames){
  mixNetwork <- mixOmics::network(result(), blocks = seq(1, length(dataset$data), 1), cutoff = cutoff, save = 'jpeg', name.save = "tmp")
  unlink("tmp.jpeg")
  graph <- toVisNetworkData(mixNetwork$gR, idToLabel = FALSE)
  
  data <- list(label = graph$nodes$label, id = graph$nodes$id)
  
  clickedFunc = paste0("function(ng_nodes){
                          clicked = ng_nodes.nodes[0];
                          if (clicked == undefined){
                            clicked = 'null';
                          }
                          Shiny.onInputChange('", ns('clicked_node'), postfix, "', clicked);
      }")
  
  if(fullNames){
    graph$nodes$font.size = rep(15, length(graph$nodes$size))
    visNetwork <- visNetwork(nodes = graph$nodes, edges = graph$edges) %>% 
      visOptions(highlightNearest = TRUE) %>%
      visPhysics(enabled = FALSE) %>%
      visNodes(widthConstraint = 50) %>%
      visInteraction(navigationButtons = TRUE) %>%
      visLayout(randomSeed = 2) %>%
      visExport(label = "Save as png") %>%
      visEvents(click = clickedFunc)
    
  } else {
    #hover and shorten label
    graph$nodes$title = graph$nodes$label
    graph$nodes$label = lapply(graph$nodes$label, function(x){substring(x, 1, 3)})
    
    visNetwork <- visNetwork(nodes = graph$nodes, edges = graph$edges) %>% 
      visOptions(highlightNearest = TRUE) %>%
      visPhysics(enabled = FALSE) %>%
      visNodes(widthConstraint = 50) %>%
      visInteraction(navigationButtons = TRUE) %>%
      visLayout(randomSeed = 2) %>%
      visEvents(click = clickedFunc)
  }
  
  return (list(data = data, visNetwork = visNetwork, mixNetwork = mixNetwork))
}

#'
#' @description A utils function, which generates the download handler for 
#' the visNetwork
#'
#' @return downloadHandler
#'
#' @noRd
diabloGetNetworkDownloadHandler <- function(filename, network, type){
  return(
    downloadHandler(
      filename = filename,
      content = function(file){
        if (type == "html"){
          visNetwork::visSave(network$visNetwork, file)
        } else if (type == "gml"){
          library(igraph)
          write.graph(network$mixNetwork$gR, file, format = "gml")
        }
      }
    )
  )
}