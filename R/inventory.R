#' inventory function
#'
#' Creates simulation from a given forecast
#'
#' This function creates a inventory simulation based on a forecast and a
#' given inventory policy to evaluate the average on-hand inventory and the
#' service level.
#'
#' @param forecast A data frame or matrix which represents the forecast
#' distribution
#' @param h The forecast horizon
#' @param ipolicy The inventory policy. "MTS" (Make to Stock) by default
#'
#' @author Yves R. Sagaert
#' @author Sarah Van der Auweraer
#'
#' import forecast
#'
#' @return A list of average on-hand inventory and service levels.
#' @export
#'
#' @examples
#' \dontrun{
#'    inventory(forecastdata, h=12, ipolicy="MTS")
#' }
inventory <- function(forecast, h, ipolicy="MTS"){
  return("inventory simulation here")
}
