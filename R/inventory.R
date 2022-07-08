#' Inventory
#'
#' Evaluates multi-step ahead model forecast distributions
#' via inventory simulations.
#'
#' This function returns the output of a sequence of inventory simulations
#' with different target alphas. This output can be used to generate a trade-off
#' curve between fill rate and average on-hand inventory.
#'
#' @param model_forecasts A list of different model forecasts. An individual
#' model forecast can be either a forecast object, a list consisting
#' of a mean and sd vector for the forecast horizon, or a point cloud of the
#' distribution of each forecast horizon.
#'
#' @param demand_test A vector containing the actual demand for the inventory
#' evaluation. The length of the test demand should be equal to the length of
#' the demand forecast.
#'
#' @param alpha A sequence of target service levels of the inventory simulation.
#' This is can be a single value, or a sequence. If alpha.seq is set on TRUE, a
#' sequence is generated (by default).
#'
#' @param plot.out Parameter to plot a graphic for every alpha in the sequence
#' , default set on FALSE.
#'
#' @param mode Shape of the distribution, default set on 'normal'.
#'
#' @param hist_demand A vector of historical demand of the series, where the demand
#' forecast model is build on. Default is set on NA.
#'
#' @param alpha.seq Should the function generate a default alpha sequence. This
#' is set by default on TRUE.
#'
#' @author  Yves R. Sagaert
#'
#' @return A numeric vector with: used service level parameter alpha, stockout
#' rate, average inventory, average on-hand inventory, fill-rate, average test
#' demand, ratio average on-hand inventory on average test demand. A plot of
#' the simulation can be shown.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' series <- rnorm(60,100,1)
#' test_series <- rnorm(12,100,5)
#' forecast1 <- series[(length(series)-12):length(series)]
#' forecast2 <- rep(mean(series),12)
#' modelfc1 <- list(forecast1,sd=rep(3,12))
#' modelfc2 <- list(forecast2,sd=rep(1,12))
#' inventory(model_forecasts=list(modelfc1, modelfc2),
#' demand_test=test_series, hist_demand=series, plot.out=FALSE)
#' }
inventory <- function(model_forecasts,
                      demand_test, alpha=0.05, plot.out=FALSE,
                      mode="normal", hist_demand=NA,
                      alpha.seq=TRUE){
  # Get input from different models
  # matrix for returning result summary
  # inventory_result <- matrix(NA,nrow=length(alpha_sequence),ncol=7)
  # colnames(inventory_result) <- c("model","alpha","stockout_ratio", "av_inventory",
  #                                 "av_onhand_inventory", "fill_rate",
  #                                 "av_test_demand",
  #                                 "scaled_av_onhand_inventory")

  if (!(class(model_forecasts[[1]])=="list")){
    # just one model not in an extra list
    return(alphas_inventory_evaluation(demand_forecast = model_forecasts,
                                       demand_test = demand_test,
                                       alpha = alpha,
                                       plot.out = plot.out,
                                       mode = mode,
                                       hist_demand = hist_demand,
                                       alpha.seq=TRUE))
  } else {
    # list of multiple models
    # m=1 # Debug
    # m=2
    for (m in 1:length(model_forecasts)){
      # Do for every individual model, one inventory simulation for every alpha
      model_res <- alphas_inventory_evaluation(demand_forecast = model_forecasts[[m]],
                                         demand_test = demand_test,
                                         alpha = alpha,
                                         plot.out = plot.out,
                                         mode = mode,
                                         hist_demand = hist_demand,
                                         alpha.seq=TRUE)
      # Handle the table with summarised results
      if (m==1){
        # make
        model_results <- cbind(m,model_res)
      } else {
        # append
        model_results <- rbind(model_results,cbind(m,model_res))
      }

    }
    return(model_results)
  }
}
