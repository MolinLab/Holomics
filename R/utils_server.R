#' ListsToMatrix 
#'
#' @description A utils function, which converts two list to a matrix
#'
#' @return matrix
#'
#' @noRd
ListsToMatrix <- function (list1, list2, colnames){
  matrix <- unname(as.matrix(cbind(list1, list2)))
  colnames(matrix) <- colnames
  return (matrix)
}

#' RemovePostFix 
#'
#' @description A utils function, which removes the text after the last delimiter
#'
#' @return list of string
#'
#' @noRd
RemovePostFix <- function(strings, delimiter){
  new_strings = strings
  for (i in 1: length(strings)){
    string <- strings[[i]]
    new_strings[[i]] <- substr(string, 0, regexpr(paste0(delimiter, "[^", delimiter, "]*$"), string) - 1)
  }
  return(new_strings)
}

#' CombineLists 
#'
#' @description A utils function, which combines two lists to one,
#' where the value of the first list is the key and the value of the second the key
#'
#' @return list
#'
#' @noRd
CombineLists <- function(a, b){
  r <- c()
  for (i in 1:length(a)){
    r[a[[i]]] = b[[i]]
  }
  return(r)
}