#' ListsToDataTable 
#'
#' @description A utils function
#'
#' @return matrix which can be then 
#'
#' @noRd

ListsToMatrix <- function (list1, list2, colnames){
  matrix <- unname(as.matrix(cbind(list1, list2)))
  colnames(matrix) <- colnames
  return (matrix)
}