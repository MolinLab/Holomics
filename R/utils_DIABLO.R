#'
#' @description A utils function, which returns the number of components.
#' If the tune switch is on or off the number is taken from
#' the tuned parameters or the numeric input
#'
#' @return number of components
#'
#' @noRd
diabloGetNcomp <- function(input, tuned, tunedVals){
  return (ifelse(tuned, tunedVals$ncomp, input$ncomp))
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
diabloCheckOneDatasetNcomp <- function(dataset, input, ncompCheck = TRUE, tuned = FALSE, tunedVals = NULL){
  if(length(dataset$data) < 1){
    return("You have to select at least one dataset!")
  } else if(ncompCheck){
    return(diabloCheckNcomp(input, tuned, tunedVals))
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
diabloCheckNcomp <- function(input, tuned, tunedVals){
  if(diabloGetNcomp(input, tuned, tunedVals) < 2){
    return("There need to be at least two components to render this plot!")
  } else {
    return("")
  }
}

#' @description A utils function that checks if 
#' the selected data and class are compatible
#'
#' @return boolean
#'  
#' @noRd
diabloCheckValidSelection <- function(data, class){
  valid = TRUE
  msg = ""
  for (name in names(data)) {
    if (length(data[[name]]) != 0 && nrow(class) != nrow(data[[name]])){
      msg = "The selected data and classes are incompatible due to their different amount of samples! 
            Please change your selection!"
      valid = FALSE
    } else if(!identical(class[,1], rownames(data[[name]]))){
      msg = "The selected data and classes are incompatible as they do not contain the same sample(name)s! 
            Please change your selection!"
      valid = FALSE
    }
  }
  return(list(valid = valid, msg = msg))
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
  
  #color the network connection red and blue according to correlation
  idx = 1
  for (w in graph$edges$weight){
    if (w < 0){
      graph$edges$color[idx] = "#5995fa"
    } else {
      graph$edges$color[idx] = "#de4236"
    }
    idx = idx + 1
  }
  
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
          igraph::write.graph(network$mixNetwork$gR, file, format = "gml")
        }
      }
    )
  )
}

#'
#' @description A utils function, calculates the pairwise correlation
#' of the datasets 
#' 
#' @importFrom utils combn
#'
#' @return pairwise correlation
#'
#' @noRd
pairwiseCorrelation <- function(data){
  comb <- combn(names(data),2)
  results <- lapply(1:ncol(comb), function(i) {
    nameX = comb[1, i]
    nameY = comb[2, i]
    X <- data[[nameX]]
    Y <- data[[nameY]]
    res <- mixOmics::pls(X, Y, ncomp = 1)
    cor <- cor(res$variates$X, res$variates$Y)
    c(as.numeric(cor), nameX, nameY)
  })
  return(results)
}