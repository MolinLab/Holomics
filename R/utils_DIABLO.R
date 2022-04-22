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
                  tabPanel("Sample plot",
                           fluidRow(style = "display: flex; column-gap: 1rem",
                                    uiOutput(paste0(ns("indiv.x.comp"), postfix)),
                                    uiOutput(paste0(ns("indiv.y.comp"), postfix)),
                                    awesomeCheckbox(paste0(ns("indiv.names"), postfix), "Sample names", value = FALSE)
                           ),
                           fluidRow(
                             bs4Dash::column(width = 12,
                                             textOutput(paste0(ns("indiv.error"), postfix)),
                                             plotOutput(paste0(ns("DIABLO.Indiv"), postfix)),
                                             downloadButton(paste0(ns("Indiv.download"), postfix), "Save plot"))
                           )
                  ),
                  tabPanel("Variable plot",
                           fluidRow(style = "display: flex; column-gap: 1rem",
                                    uiOutput(paste0(ns("var.x.comp"), postfix)),
                                    uiOutput(paste0(ns("var.y.comp"), postfix)),
                                    awesomeCheckbox(paste0(ns("var.names"), postfix), "Variable names", value = FALSE)
                           ),
                           fluidRow(
                             bs4Dash::column(width = 12,
                                             textOutput(paste0(ns("var.error"), postfix)),
                                             plotOutput(paste0(ns("DIABLO.Var"), postfix)),
                                             downloadButton(paste0(ns("Var.download"), postfix), "Save plot"))
                           )
                  ),
                  tabPanel("Loading Plots",
                           fluidRow(
                             uiOutput(paste0(ns("load.comp"), postfix)),
                           ),
                           fluidRow(
                             bs4Dash::column(width = 12,
                                             textOutput(paste0(ns("load.error"), postfix)),
                                             plotOutput(paste0(ns("DIABLO.Load"), postfix)),
                                             downloadButton(paste0(ns("Load.download"), postfix), "Save plot"))
                           )
                  ),
                  tabPanel("CIM",
                           fluidRow(
                             uiOutput(paste0(ns("img.comp"), postfix))
                           ),
                           fluidRow(
                             bs4Dash::column(width = 12,
                                             textOutput(paste0(ns("img.error"), postfix)),
                                             plotOutput(paste0(ns("DIABLO.Img"), postfix)),
                                             downloadButton(paste0(ns("Img.download"), postfix), "Save plot"))
                           )
                  ),
                  tabPanel("Arrow plot",
                           fluidRow(
                             awesomeCheckbox(paste0(ns("namesArrow"), postfix), "Sample names", value = FALSE)
                           ),
                           fluidRow(
                             bs4Dash::column(width = 12,
                                             textOutput(paste0(ns("arrow.error"), postfix)),
                                             plotOutput(paste0(ns("DIABLO.Arrow"), postfix)))
                           )
                  ),
                  tabPanel("Diablo plot",
                           fluidRow(
                             uiOutput(paste0(ns("diablo.comp"), postfix))
                           ),
                           fluidRow(
                             bs4Dash::column(width = 12,
                                             textOutput(paste0(ns("diablo.error"), postfix)),
                                             plotOutput(paste0(ns("DIABLO.Diablo"), postfix)),
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
                                             plotOutput(paste0(ns("DIABLO.Circos"), postfix)),
                                             downloadButton(paste0(ns("Circos.download"), postfix), "Save plot"))
                           )
                  ), 
                  tabPanel("Network",
                           fluidRow(style = "column-gap: 1rem",
                                    numericInput(paste0(ns("cutoffNetwork"), postfix), "Cutoff value",
                                                 min = 0, max = 1, step = 0.1, value = 0.5),
                                    uiOutput(paste0(ns("nodes"), postfix))
                           ),
                           fluidRow(
                             bs4Dash::column(width = 12,
                                             textOutput(paste0(ns("network.error"), postfix)),
                                             visNetworkOutput(paste0(ns("DIABLO.Network"), postfix)),
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
#' @return list with node data and the network
#'  
#' @noRd
diabloGenerateNetwork <- function(result, dataset, cutoff){
  mixNetwork <- mixOmics::network(result(), blocks = seq(1, length(dataset$data), 1), cutoff = cutoff, save = 'jpeg', name.save = "tmp")
  unlink("tmp.jpeg")
  graph <- toVisNetworkData(mixNetwork$gR, idToLabel = FALSE)
  
  #before changing label
  data <- list(label = graph$nodes$label, id = graph$nodes$id)
  
  #hover and shorten label
  graph$nodes$title = graph$nodes$label
  graph$nodes$label = lapply(graph$nodes$label, function(x){substring(x, 1, 3)})
  
  visNetwork <- visNetwork(nodes = graph$nodes, edges = graph$edges) %>% 
    visOptions(highlightNearest = TRUE) %>%
    visPhysics(enabled = FALSE) %>%
    visNodes(widthConstraint = 50) %>%
    visInteraction(navigationButtons = TRUE) %>%
    visExport(label = "Save as png")
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