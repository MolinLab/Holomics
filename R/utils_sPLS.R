#' sPLS 
#'
#' @description A utils function, which returns the number of components.
#' If the tune switch is on or off the number is taken from
#' the tuned parameters or the numeric input
#'
#' @return number of components
#'
#' @noRd
splsGetNcomp <- function(input){
  return (ifelse(useTunedsPLSVals(), tunedsPLSVals$ncomp, input$ncomp))
}

#' @description A utils function, which checks if at least two components 
#' are in the model
#'
#' @return error message if less than two components
#'  else empty string
#'  
#' @noRd
splsCheckNcomp <- function(input){
  if(splsGetNcomp(input) < 2){
    return("There need to be at least two components to render this plot!")
  } else {
    return("")
  }
}
