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
#' object for the comp parameter of the Correlation Circle plot
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

#' @description checks if the number of components
#' is compatible with the dataset, which means that 
#' the components need to be lower or equal the number of
#' variables (cols) and samples (rows) the dataset has
#'
#' @return error message
#'
#' @noRd
checkDataNcompCompatibility <- function(data, ncomp){
  if(ncol(data) < ncomp){
    return("You cannot use more components than the dataset has features!")
  } else if(nrow(data) < ncomp){
    return("You cannot use more components than the dataset has samples!")
  } else {
    return("")
  }
}

#' @description checks if the selected ncomp is not 
#' lower than any element in the comp vector. If so
#' every value above will be set to ncomp to avoid an
#' error message when plotting
#'
#' @return numeric vector
#'
#' @noRd
checkCompNcompCombination <- function(ncomp, comp){
  newComp <- comp
  while(any(ncomp < max(newComp))){
    newComp[which.max(newComp)] = ncomp
  }
  return(newComp)
}

#' @description adapter for the plotIndiv function of
#' the mixOmics package
#'
#' @return sample plot
#'
#' @noRd
plotIndiv <- function(result, classes, title = NULL, subtitle = NULL, legend.title, comp, 
                      repSpace = NULL, indNames, col.per.group, legendPosition = "right") {
  comp <- checkCompNcompCombination(result$ncomp, comp)
  
  if (missing(col.per.group)){
    if (is.null(subtitle)){
      mixOmics::plotIndiv(result, comp = comp, rep.space = repSpace,
                          group = classes, ind.names = indNames,
                          title = title, legend = TRUE, 
                          legend.title = legend.title, legend.position = legendPosition)
    } else {
      mixOmics::plotIndiv(result, comp = comp, rep.space = repSpace,
                          group = classes, ind.names = indNames,
                          subtitle = subtitle, legend = TRUE, 
                          legend.title = legend.title, legend.position = legendPosition)
    }
    
  } else {
    if (is.null(subtitle)){
      mixOmics::plotIndiv(result, comp = comp, rep.space = repSpace,
                          group = classes, ind.names = indNames,
                          title = title, legend = TRUE, 
                          legend.title = legend.title, legend.position = legendPosition, 
                          col.per.group = col.per.group)
    } else {
      mixOmics::plotIndiv(result, comp = comp, rep.space = repSpace,
                          group = classes, ind.names = indNames,
                          subtitle = subtitle, legend = TRUE, 
                          legend.title = legend.title, legend.position = legendPosition, 
                          col.per.group = col.per.group)
    }
  }

}

#' @description adapter for the plotVar function of
#' the mixOmics package
#'
#' @return Correlation Circle plot
#'
#' @noRd
plotVar <- function(result, comp, varNames, legend = FALSE, pch) {
  comp <- checkCompNcompCombination(result$ncomp, comp)
  
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
plotLoadings <- function(result, comp, contrib = NULL, method = "mean", subtitle, legend.color = NULL) {
  comp <- checkCompNcompCombination(result$ncomp, comp)
  
  if (missing(subtitle)){
    mixOmics::plotLoadings(result, comp = comp,
                           contrib = contrib, method = method, size.title = ggplot2::rel(1), 
                           legend.color = legend.color)
  } else {
    mixOmics::plotLoadings(result, comp = comp,
                           contrib = contrib, method = method, size.title = ggplot2::rel(1), 
                           legend.color = legend.color, subtitle = subtitle)
  }
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
  comp <- checkCompNcompCombination(result$ncomp, comp)
  
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
getDownloadHandler <- function(filename, contentfct, type = "png", width = 1800, height = 1200, plot = NULL, tablefct = NULL){
  return (
    downloadHandler(
      filename = filename,
      content = function(file){
        if (type == "png"){
          png(file, width, height, res = 300)
          contentfct()
          dev.off()
        }else if (type == "csv"){
          if (is.null(tablefct)){
            write.csv2(contentfct(), file)
          } else {
            write.csv2(tablefct(contentfct()), file)
          }
        } else if (type == "xlsx"){
          df <- contentfct()
          openxlsx::write.xlsx(df, file, rowNames = TRUE, colNames = TRUE)
        } else if (type == "ggplot"){
          device <- function(..., width, height) {
            grDevices::png(..., width = 1800, height = 1200, res = 300)
          }
          if (is.null(plot)){
            ggplot2::ggsave(file, plot = contentfct(), device = device)
          } else {
            ggplot2::ggsave(file, plot = plot, device = device)
          }
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
  data = data[order(data[,2]),]
  match <- match(unique(data[,2]), data[,2])
  
  colors <- c()
  for (i in match){
    colors <- c(colors, data[,3][i])
  }
  return (colors)
}

#' @description A utils function that generates the list of
#' different keepX number used for tuning. 
#' From 10 to 20 it will be tested in steps of 2, all above 10
#' in steps of 5. The maximum value can be 100
#'
#' @return array 
#'
#' @noRd
getTestKeepX <- function(max){
  keepX <- c()
  
  if(max < 10){
    keepX <- max
  } else {
    if (max >= 100){
      max <- 100
    }
    
    if(max >= 20){
      keepX <- c(keepX, seq(20, max, 5)) 
      max <- 19
      
    }

    keepX <- c(keepX, seq(10, max, 2))
  }
  
  return (sort(keepX))
}


#' @description A utils function that returns the number of folds
#' for the cross validation according to the provided data.
#' The max. number of folds is 5.
#'
#' @return number 
#'
#' @noRd
getFolds <- function(data){
  folds <- min(table(data))
  if (folds > 5){
    folds <- 5
  }
  return (folds)
}

#' @description A utils function that extends the table 
#' with the datasets informations by one row
#'
#' @noRd
extendDataTable <- function(table, name, filename, samplesNum, variablesNum, isMicrobiome, omicsAnalysis, plotName){
  return (rbind(table, c(name, filename, samplesNum, variablesNum, isMicrobiome, omicsAnalysis, plotName)))
}

#' @description A utils function that extends the table 
#' with the classes informations by one row
#'
#' @noRd
extendClassTable <- function(table, name, filename, samplesNum, containsColor){
  return(rbind(table, c(name, filename, samplesNum, containsColor)))
}

#' @description A utils function that makes sure 
#' that the name of the dataset is unique
#'
#' @noRd
getDatasetNames <- function(name1, name2){
  if(name1 == name2){
    name2 <- paste(name1, "2")
  }
  return(list(name1 = name1, name2 = name2))
}

#'
#' @description A utils function, which generates the heatmap table
#'
#' @return matrix with the heatmap values
#'
#' @noRd
cimToTable <- function(cimPlot){
  hmp <- cimPlot$mat
  rownames(hmp) <- cimPlot$row.names
  colnames(hmp) <- cimPlot$col.names
  return(as.data.frame(hmp))
}