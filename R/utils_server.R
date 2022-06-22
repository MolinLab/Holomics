#' @description A utils function, which gets the dataset according to the selection
#'
#' @return dataframe with the data
#'
#' @noRd
getDataset <- function(selection){
  if(selection == "t"){
    return (Holomics::data.transcriptomic_small)
  } else if (selection == "me"){
    return (Holomics::data.metabolomic_small)
  } else if (selection == "mi"){
    return (Holomics::data.microbiomic_small)
  }
}

#' @description A utils function, generates the list for the
#' datase choices
#'
#' @return named list
#'
#' @noRd
generateChoices <- function(data){
  choices = list()
  if (length(data) > 0){
    for (i in 1:length(data)){
      choices[[names(data)[i]]] = names(data)[i]
    }
  }
  return (choices)
}

#' @description A utils function, which converts two list to a matrix
#'
#' @return matrix
#'
#' @noRd
listsToMatrix <- function (list1, list2, colnames){
  matrix <- unname(as.matrix(cbind(list1, list2)))
  colnames(matrix) <- colnames
  return (matrix)
}

#' @description A utils function, which removes the text after the last delimiter
#'
#' @return list of string
#'
#' @noRd
removePostFix <- function(strings, delimiter){
  new_strings = strings
  for (i in 1: length(strings)){
    string <- strings[[i]]
    new_strings[[i]] <- substr(string, 0, regexpr(paste0(delimiter, "[^", delimiter, "]*$"), string) - 1)
  }
  return(new_strings)
}

#' @description A utils function, which combines two lists to one,
#' where the value of the first list is the key and the value of the second the key
#'
#' @return list
#'
#' @noRd
combineLists <- function(a, b){
  r <- c()
  for (i in 1:length(a)){
    r[a[[i]]] = b[[i]]
  }
  return(r)
}

#' @description A utils function to get a reactive
#' object for the comp parameter of the sample plot
#'
#' @return reactive object
#'
#' @noRd
getCompIndivReactive <- function(input, tuned = FALSE){
  return (
    reactive({
      if (tuned){
        req(input$indiv.x.tuned)
        req(input$indiv.y.tuned)
        as.numeric(c(input$indiv.x.tuned,input$indiv.y.tuned))
      } else {
        req(input$indiv.x)
        req(input$indiv.y)
        as.numeric(c(input$indiv.x,input$indiv.y))
      }
    })
  )
}

#' @description A utils function to get a reactive
#' object for the comp parameter of the variable plot
#'
#' @return reactive object
#'
#' @noRd
getCompVarReactive <- function(input, tuned = FALSE){
  return (
    reactive({
      if (tuned){
        req(input$var.x.tuned)
        req(input$var.y.tuned)
        comp.var.tuned <- as.numeric(c(input$var.x.tuned,input$var.y.tuned))
      } else {
        req(input$var.x)
        req(input$var.y)
        comp.var <- as.numeric(c(input$var.x,input$var.y))
      }
    })
  )
}

#' @description A utils function to get a reactive
#' object for the comp parameter of the img plot
#'
#' @return reactive object
#'
#' @noRd
getCompImgReactive <- function(input, tuned = FALSE){
  return (
    reactive({
      if (tuned) {
        req(input$img.comp.tuned)
        comp.img.tuned <- as.numeric(input$img.comp.tuned)
      } else {
        req(input$img.comp)
        comp.img <- as.numeric(input$img.comp)
      }
    })
  )
}

#' @description adapter for the plotIndiv function of
#' the mixOmics package
#'
#' @return sample plot
#'
#' @noRd
plotIndiv <- function(result, classes, title, comp, repSpace = NULL, indNames, col.per.group, legendPosition = "right") {
  if (missing(col.per.group)){
    mixOmics::plotIndiv(result, comp = comp, rep.space = repSpace,
                        group = classes, ind.names = indNames,
                        legend = TRUE, legend.title = title,
                        legend.position = legendPosition)
  } else {
    mixOmics::plotIndiv(result, comp = comp, rep.space = repSpace,
                        group = classes, ind.names = indNames,
                        legend = TRUE, legend.title = title,
                        legend.position = legendPosition, col.per.group = col.per.group)
  }

}

#' @description adapter for the plotVar function of
#' the mixOmics package
#'
#' @return variable plot
#'
#' @noRd
plotVar <- function(result, comp, varNames, legend = FALSE, pch) {
  if (!missing(pch)){
    mixOmics::plotVar(result, comp = comp,
                      var.names = varNames, pch = pch,
                      legend = legend)
  } else {
    mixOmics::plotVar(result, comp = comp,
                      var.names = varNames, legend = legend)
  }
}

#' @description adapter for the plotLoadings function of
#' the mixOmics package
#'
#' @return loadings plot
#'
#' @noRd
plotLoadings <- function(result, comp, contrib = NULL, method = "mean") {
  mixOmics::plotLoadings(result, comp = comp,
                         contrib = contrib, method = method)
}

#' @description adapter for the selectVar function of
#' the mixOmics package
#' 
#' if XY is true then two matrices have to be generated
#'
#' @return matrix with the variables
#'
#' @noRd
selectVar <- function(result, comp, XY = FALSE) {
  selVar <- mixOmics::selectVar(result, comp = comp)
  if (XY){
    return ( 
      list (X = listsToMatrix(selVar$X$name, selVar$X$value, c("name", "value")),
            Y = listsToMatrix(selVar$Y$name, selVar$Y$value, c("name", "value"))
      )
    )
  } else {
    listsToMatrix(selVar$name, selVar$value, c("name", "value"))
  }
}

#' @description adapter for the plotArrow function of
#' the mixOmics package
#'
#' @return arrow plot
#'
#' @noRd
plotArrow <- function(result, classes, title, indNames, col.per.group) {
  if (missing(col.per.group)){
    mixOmics::plotArrow(result, group = classes, ind.names = indNames,
                        legend = TRUE, legend.title = title, legend.position = "bottom",
                        X.label = "Dimension 1", Y.label = "Dimension 2")
  } else {
    mixOmics::plotArrow(result, group = classes, ind.names = indNames,
                        legend = TRUE, legend.title = title, legend.position = "bottom",
                        X.label = "Dimension 1", Y.label = "Dimension 2", col.per.group = col.per.group)
  }
}

#' @description A utils function, generates the download handler for either png
#' or csv files calling the given content function to generate the content of the
#' file
#'
#' @return downloadHandler
#'
#' @noRd
getDownloadHandler <- function(filename, contentfct, type = "png", width = 1800, height = 1200){
  return (
    downloadHandler(
      filename = filename,
      content = function(file){
        if (type == "png"){
          png(file, width, height, res = 300)
          contentfct()
          dev.off()
        }else if (type == "csv"){
          write.csv2(contentfct(), file)
        }
      }
    )
  )
}

#' @description A utils function that returns the error
#' message according to the missing data
#'
#' @return string
#'
#' @noRd
checkMissingData <- function(data, classes){
  if(length(data) == 0){
    return(list(check = TRUE,
                msg = "Please upload some data to be able to use the analysis!"))
  } else if(length(classes) == 0){
    return(list(check = TRUE,
                msg = "Please upload some classes/label information to be able to use the analysis!"))
  } else {
    return(list(check = FALSE,
                msg = "" ))
  }
}

#' @description A utils function the set colors of the group
#'
#' @return array with color codes
#'
#' @noRd
getGroupColors <- function(data){
  match <- match(unique(data[,1]), data[,1])
  
  colors <- c()
  for (i in match){
    colors <- c(colors, data[,2][i])
  }
  return (colors)
}