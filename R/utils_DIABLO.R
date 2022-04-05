#' DIABLO 
#'
#' @description A utils function, which returns the number of components.
#' If the tune switch is on or off the number is taken from
#' the tuned parameters or the numeric input
#'
#' @return number of components
#'
#' @noRd
diabloGetNcomp <- function(input){
  return (ifelse(useTunedDiabloVals(), tunedDiabloVals$ncomp, input$ncomp))
}


#' @description A utils function, which checks if the datasets variable
#' contains at least two datasets
#'
#' @return error message if less than two datasets else empty string
#'
#' @noRd
diabloCheckTwoDatasets <- function(dataset){
  if(length(dataset$data) < 2){
    return("You have to select at least two dataset!")
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
diabloCheckOneDatasetNcomp <- function(dataset, input, ncompCheck = TRUE){
  if(length(dataset$data) < 1){
    return("You have to select at least one dataset!")
  } else if(ncompCheck){
    return(diabloCheckNcomp(input))
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
diabloCheckNcomp <- function(input){
  if(diabloGetNcomp(input) < 2){
    return("There need to be at least two components to render this plot!")
  } else {
    return("")
  }
}