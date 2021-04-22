#' inventory function
#'
#' Creates simulation from a given forecast
#'
#' This function creates a inventory simulation based on a forecast and a
#' given inventory policy to evaluate the average on-hand inventory and the
#' service level.
#'
#' @param forecast A forecast object, data frame or matrix which represents the forecast
#' distribution
#' @param h The forecast horizon
#' @param ipolicy The inventory policy. "MTS" (Make to Stock) by default
#'
#' @author Yves R. Sagaert
#' @author Sarah Van der Auweraer
#'
#' @import forecast
#' @import Mcomp
#'
#' @return A list of average on-hand inventory and service levels.
#' @export
#'
#' @examples
#'
#'    ets.model <- forecast::ets(Mcomp::M3[[2000]]$x)
#'    ets.forecast <- forecast::forecast(ets.model,Mcomp::M3[[2000]]$h)
#'    inventory(forecast=ets.forecast, h=12, ipolicy="MTS")
#'
inventory <- function(forecast, h, ipolicy="MTS"){
  return("inventory simulation here")
}
