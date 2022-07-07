#' Rolling Origin experiment with Inventory Evaluation
#'
#' Make a rolling origin setup with a recurring forecast for different models
#' and evaluate these forecasts on inventory performance.
#'
#' This function makes the setup of a rolling origin for an example dataset.
#' For different models, a forecast is made for every rolling origin. These
#' forecasts can be plot, but are then evaluated on their inventory performance
#' via an inventory simulation.
#'
#' @param data Example dataset that will be used to perform rolling origin
#' experiment. Default set on "M3".
#'
#' @param models A vector with all the different model names. Default is set
#' to c("Naive", "ETS").
#'
#' @param plot.ro Parameter to plot a forecast plot of every model with the
#' rolling origin forecast and prediction intervals.
#'
#'
#' @author  Yves R. Sagaert
#'
#' @return A matrix with the inventory evaluation results for all series, for
#' all models, for a predefined sequence of alphas. The results are summarised
#' by stockout ratio, average inventory, average onhand inventory, fill rate,
#' average test demand, scaled onhand inventory, average shortage items and
#' scaled average shortage items. The scaling happens with the average test
#' demand.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' rolling_origin_inventory(data="M3", model=c("Naive","ETS"), plot.ro=TRUE)
#' }
#'
rolling_origin_inventory <- function(data="M3", models=c("Naive","ETS"),
                                     plot.ro=TRUE){

  # test_competition_M3

  # library(Mcomp)
  # M3
  data_list <- subset(Mcomp::M3,"monthly", "industry")
  # View(data_list)
  # names(data_list[[1]])
  # series <- data_list[[1]]
  # series$x
  # series$xx
  ori_max <- length(data_list[[1]]$xx)
  s=1 # Debug
  for (s in 1:10){
    series <- data_list[[s]]
    # Make a rolling origin forecast for h=1 x 18 steps
    ori_max <- length(series$xx)
    empty_vector <- matrix(NA,nrow=1,ncol=18)
    model1 <- list(empty_vector, empty_vector)
    model2 <- list(empty_vector, empty_vector)

    # r=2 # Debug
    for (r in 1:ori_max){
      # Growing training window with rolling origin
      if (r==1){
        ytrain <- series$x
      } else {
        ytrain <- stats::ts(data=c(series$x,series$xx[1:(r-1)]),
                               start=stats::start(series$x),
                               frequency = stats::frequency(series$x))
      }
      # Model 1: Naive
      model1_fc <- forecast::naive(ytrain,h=1)
      model1[[1]][r] <- as.vector(model1_fc$mean)
      model1[[2]][r] <- (as.vector(model1_fc$upper[,1])-
                           as.vector(model1_fc$mean))/stats::qnorm(0.9)
      # Model 2: ETS
      model2_fc <- forecast::forecast(forecast::ets(ytrain),h=1)
      model2[[1]][r] <-  as.vector(model2_fc$mean)
      model2[[2]][r] <- (as.vector(model2_fc$upper[,1])-
                           as.vector(model2_fc$mean))/stats::qnorm(0.9)
    }
    # model_forecasts <- list(list(df,dfs),list(df,dfs))
    model_forecasts <- list(model1,model2)
    # plot all one-step ahead forecasts
    if (plot.ro){
      for (mm in 1:length(models)){
        model <- forecast::naive(series$x,h=ori_max)
        plotmodel <- model_forecasts[[mm]]
        k=1 # Debug
        for (k in 1:ori_max){
          model$mean[k] <- plotmodel[[1]][k]
          model$upper[k,1] <- plotmodel[[2]][k]*stats::qnorm(0.9)+plotmodel[[1]][k]
          model$upper[k,2] <- plotmodel[[2]][k]*stats::qnorm(0.975)+plotmodel[[1]][k]
          model$lower[k,1] <- plotmodel[[1]][k] - plotmodel[[2]][k]*stats::qnorm(0.9)
          model$lower[k,2] <- plotmodel[[1]][k] - plotmodel[[2]][k]*stats::qnorm(0.975)
        }
        plot(model, main=paste("Rolling forecast for",models[mm]))
      }
    }
    inventory_res=inventory(model_forecasts,demand_test = series$xx,hist_demand = series$x)
    # View(res)
    # Handle the table with summarised results
    if (s==1){
      # make
      inventory_result <- cbind(s,inventory_res)
    } else {
      # append
      inventory_result <- rbind(inventory_res,cbind(s,inventory_res))
    }
  }
return(inventory_result)
}









