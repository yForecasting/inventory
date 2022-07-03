#' Inventory
#'
#' Evaluates forecast on inventory performance
#'
#' This function returns the output of an inventory simulation based on the
#' forecast input.
#'
#' @param forecast A forecast object
#'
#' @author  Yves R. Sagaert
#'
#' @return A data frame
#' @export
#'
#' @examples
#' series <- rnorm(60,100,1)
#' inventory(series)
#'
inventory <- function(forecast){
  return(data.frame(forecast))
}
