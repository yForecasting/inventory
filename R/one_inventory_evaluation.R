#' One Inventory Evaluation
#'
#' Evaluates a forecast via an inventory simulation
#'
#' This function returns the output of an inventory simulation based on the
#' forecast input.
#'
#' @param demand_forecast The forecast. This can be a forecast object, a list
#' of mean and sd of the forecast or a point cloud of the distribution of the
#' forecast.
#'
#' @param demand_test A vector containing the actual demand for the inventory
#' evaluation. The length of the test demand should be equal to the length of
#' the demand forecast.
#'
#' @param alpha The target service level of the inventory simulation. This is
#' set by default on 0.05.
#'
#' @param plot.out Parameter to plot a graphic, default set on FALSE.
#'
#' @param mode Shape of the distribution, default set on 'normal'.
#'
#' @param hist_demand A vector of historical demand of the series, where the demand
#' forecast model is build on. Default is set on NA.
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
#' test_series <- rnorm(12,100,1)
#' forecast <- series[(length(series)-12):length(series)]
#' one_inventory_evaluation(demand_forecast=list(forecast,sd=rep(3,12)),
#' demand_test=test_series, hist_demand=series, plot.out=TRUE)
#' }
one_inventory_evaluation <- function(demand_forecast,
                                     demand_test, alpha=0.05, plot.out=FALSE,
                                     mode="normal",hist_demand=NA){
  # 1) a forecast model class -> extract mean + sd (over the horizon)
  if ("forecast" %in% class(demand_forecast)){
    forecast_model <- demand_forecast
    demand_forecast <- forecast_model$mean
    demand_forecast_sd <- (forecast_model$upper[,1]-forecast_model$mean)/
      stats::qnorm(0.9)
    # Explain: Lo80-Hi80 contains 80% of the points. So we have 10% quantile
    # and 90% quantile. To calculate sigma, we use s=(u-m)/qnorm(0.9).
  } else if ("list" %in% class(demand_forecast)){
    # 2) a list of mean + sd (over the horizon)
    demand_list <- demand_forecast
    demand_forecast <- demand_list[[1]]
    demand_forecast_sd <- demand_list[[2]]
  } else if ("matrix" %in% class(demand_forecast)){
    # 3) a cloud of points from the forecast distribution
    #    -- one distr (in the rows) per horizon (cols)
    mode <- "distribution"
    # vector of distributions
    distr_demand_forecast <- demand_forecast
    demand_forecast <- apply(distr_demand_forecast,MARGIN=2,FUN=mean)
  }
  # Initiate variables
  I_start <- 0 # start inventory position
  none <- rep(0,length(demand_test)) # empty vector to generate the rest
  I_vector <- c(0,none) # real inventory over time
  I_onhand <- c(0,none) # on hand inventory, actual inventory taking space=cost

  D_fromstock <- none # demand directly from stock (no backordering)
  scaled_accumulated_bias <- none
  SL_stockout <- none # service level parameter: stockout
  SL_shortage_items <- none # service level parameter: shortage items in
  # absolute value
  # alpha <- 0.05 # service level parameter: desired stockout probability
  SL_fill_rate <- 0 # fill rate: how much items % are delivered from the orders

  O_vector <- none # order quantity over time

  # mode = deterministic / normal distribution / distribution
  # plan for stochastic simulation and service levels

  I_vector[1] <- I_start
  I_onhand[1] <- max(0,I_start,na.rm=TRUE)

  # simulation loop one-step ahead, rolling window evaluation
  i <- 1 #Debug
  for (i in 1:length(demand_test)){
    # Assumption: normal distributed forecast distribution
    # Assumption: base stock level (no reorder point)
    # Backorders cleared via I_vector: backorders are taken into account

    # Take into account the uncertainty of the forecast via sd vector
    to_order <- stats::qnorm(p=1-alpha, mean=demand_forecast[i],
                        sd=demand_forecast_sd[i]) - I_vector[i]

    # Update the planned production in this period
    # Assumption: don't throw away inventory and no negative orders
    O_vector[i] <- max(0,to_order,na.rm=TRUE)

    # 2. Inventory
    # Update the inventory position
    # I_vector = inventory position, backlog, in pipeline
    I_vector[i+1] <- I_vector[i] + O_vector[i] - demand_test[i]
    delivery_time <- 0
    D_fromstock[i] <- max(0,min(demand_test[i],(I_onhand[i] + O_vector[i-delivery_time])))
    I_onhand[i+1] <- max(0,I_onhand[i] + O_vector[i-delivery_time] - demand_test[i],na.rm=TRUE)
  }
    # Metric for accumulated forecast error

    # Calculate accumulated bias and divide by mean of test period to get
    # somewhat an 0-reference line (mean train period could also)

    # if (is.null(demand_forecast)){
    # forecast is a vector: mode deterministic/normal
    scaled_accumulated_bias[i] <- (sum(demand_forecast[1:i]) -
                                     sum(demand_test[1:i]))/mean(demand_test)
    # Test: SAB should be related to nr of stockouts

    # 4. Test stockout update
    SL_shortage_items <- demand_test-D_fromstock # should not be negative
    SL_stockout <- as.numeric(!(demand_test==D_fromstock))
    SL_stockout_ratio <- mean(SL_stockout)
    # cycle service level = probability out of stock

    # Average inventory position
    # I_vector: oh+pipeline-backorders
    average_inventory <- mean(I_vector,na.rm=TRUE)
    average_onhand_inventory <- mean(I_onhand,na.rm=TRUE)
    average_shortage_items <- mean(SL_shortage_items,na.rm=TRUE)

    SL_fill_rate <- sum(D_fromstock)/sum(demand_test)

    # make plot
    if (plot.out==TRUE){
      # show plot to see what is really happening in detail
      if (is.na(hist_demand[1]) & length(hist_demand)==1){
        # don't show the historical demand
      } else {
        #Plot the demand situation, history, forecast, test demand
        show_hist_demand <- hist_demand[(length(hist_demand)-
                                           min(2*length(demand_test),
                                               length(hist_demand))):length(hist_demand)]
        values = c(show_hist_demand,demand_test, demand_forecast, O_vector,
                   I_vector,
                   scaled_accumulated_bias) ####
        plot(c(show_hist_demand,NA*demand_test),
             ylim=c(min(values,na.rm=TRUE),max(values,na.rm=TRUE)),
             type='l',
             lwd=2,
             main=paste0("Inventory simulation (",round(100*SL_stockout_ratio,1),
                         "% stockout, ",
                         round(average_inventory,1)," avI, ",
                         round(average_onhand_inventory,1)," avIoh, ",
                         round(SL_fill_rate,2)," FR",")"),
             ylab="Volume",xlab="Time")
        graphics::lines(c(show_hist_demand*NA,demand_test),col='red',lwd=2)
        graphics::lines(c(show_hist_demand*NA,demand_forecast),col='blue',lwd=2)


        #Plot the produced quantities
        for (i in 1:length(demand_test)){
          graphics::lines(length(show_hist_demand)+i,O_vector[i],
                type="h",col="orange",lwd=2)
          # graph is misleading when dealing with negative inventory (backorders)
          # as the production bar always starts from 0
        }

        #Plot the real inventory position
        for (i in 1:(length(demand_test)+1)){
          graphics::lines(length(show_hist_demand)+i-0.8,I_vector[i],
                type="h",col="brown",lwd=2)
        }

        graphics::legend("topleft", lwd=2, col=c('black','red','blue','orange',
                                       'brown'),
               legend=c('demand','actuals','forecast','production',
                        'I actual'))
        #graphics::lines(c(show_hist_demand*NA,scaled_accumulated_bias),
              #type="p", lwd=2, cex=0.5, col="darkred")
      }

    }

    # anything you add here in the return should be added to the matrix line 77
    # in the function alphas_inventory_evaluation.
    return(c(alpha,
             SL_stockout_ratio,
             average_inventory,
             average_onhand_inventory,
             SL_fill_rate,mean(demand_test),
             average_onhand_inventory/mean(demand_test),
             average_shortage_items,
             average_shortage_items/mean(demand_test)))

    # scaled_accumulated_bias
    # more detail: SL_stockout, D_fromstock, demand_test + O_vector (logistics)
    # export above in fun alfas: in array stacked
  }





