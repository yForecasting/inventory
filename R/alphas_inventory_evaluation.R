#' Inventory Evaluation for different alphas
#'
#' Evaluates a forecast for a sequence of alphas via an inventory simulation
#'
#' This function returns the output of a sequence of inventory simulations
#' with different target alphas. This output can be used to generate a trade-off
#' curve between fill rate and average on-hand inventory.
#'
#' @param demand_forecast The forecast. This can be a forecast object, a list
#' of mean and sd of the forecast or a point cloud of the distribution of the
#' forecast.
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
#' series <- rnorm(60,100,1)
#' test_series <- rnorm(12,100,1)
#' forecast <- series[(length(series)-12):length(series)]
#' alphas_inventory_evaluation(demand_forecast=list(forecast,sd=rep(3,12)),
#' demand_test=test_series, hist_demand=series, plot.out=TRUE)
#'
alphas_inventory_evaluation <- function(demand_forecast,
                                   demand_test, alpha=0.05, plot.out=FALSE,
                                   mode="normal", hist_demand=NA,
                                   alpha.seq=TRUE){
    # alpha.seq is true -> then automatically generate sequence
    # alpha value is a sequence -> detect it there
    # todo::: plot.out should be changed to the fillrate-av inventory plot curve

    if (!alpha.seq){
      # just one alpha
      return(one_inventory_evaluation(demand_forecast = demand_forecast,
                                      demand_test = demand_test,
                                      alpha = alpha,
                                      plot.out = plot.out,
                                      mode = mode,
                                      hist_demand = hist_demand))
    } else {
      # alpha sequence
      if (length(as.vector(alpha))>1){
        # take the alpha sequence given in the parameter
        alpha_sequence <- alpha
      } else {
        # automatically generate an alpha sequence
        decrease_ten <- seq(0.9,0.1,by=-0.1)
        alpha_sequence <- c(seq(0.5,0.1,by=-0.05),
                            0.1*decrease_ten,
                            0.01*decrease_ten,
                            0.001*decrease_ten)
      }
      # matrix for returning result summary
      inventory_result <- matrix(NA,nrow=length(alpha_sequence),ncol=9)
      colnames(inventory_result) <- c("alpha","stockout_ratio", "av_inventory",
                                      "av_onhand_inventory", "fill_rate",
                                      "av_test_demand",
                                      "scaled_av_onhand_inventory",
                                      "av_shortage_items",
                                      "scaled_av_shortage_items")

      a=1 # Debug
      for (a in 1:length(alpha_sequence)){
        # Do one inventory sim for every alpha:
        iter_alpha <- alpha_sequence[a]
        res <- one_inventory_evaluation(demand_forecast = demand_forecast,
                                        demand_test = demand_test,
                                        alpha = iter_alpha,
                                        plot.out = plot.out,
                                        mode = mode,
                                        hist_demand = hist_demand)
        inventory_result[a,] <- res
      }
      return(inventory_result)
    }
}





