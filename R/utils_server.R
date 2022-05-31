#' getDataset 
#'
#' @description A utils function, which gets the dataset according to the selection
#'
#' @return dataframe with the data
#'
#' @noRd
#' Return 
getDataset <- function(selection){
  if(selection == "t"){
    return (Holomics::data.transcriptomic_small)
  } else if (selection == "me"){
    return (Holomics::data.metabolomic_small)
  } else if (selection == "mi"){
    return (Holomics::data.microbiomic_small)
  }
}


#' listsToMatrix 
#'
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

#' removePostFix 
#'
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

#' combineLists 
#'
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

#'
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