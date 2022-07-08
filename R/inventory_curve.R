#' Inventory Curve Plot
#'
#' Plot a trade-off curve between fill rate and average on-hand inventory.
#'
#' The plot shown visualised a curve for every model via a sequence of alphas
#' where the 1-fill rate is shown on the y-axis and the average on-hand
#' inventory is shown at the x-axis.
#'
#' @param models_inv A list of matrices with the results from inventory
#' evaluation.
#'
#' @param modelnames A vector with the names of the model for the legend of
#' the plot.
#'
#' @param colx Data used on the x-axis. Default set on
#' "scaled_av_onhand_inventory". This can also be set on "av_onhand_inventory"
#'  or "av_inventory".
#'
#' @param coly Data used on the y-axis. Default set on "fill_rate", which will
#' then show 1-Fill Rate. Other options are "av_shortage_items" and
#' "scaled_av_shortage_items".
#'
#' @param xlab Label used on the x-axis. Default set on "Average on hand
#' inventory".
#'
#' @param ylab Label used on the y-axis. Default set on "1 - Fill Rate".
#'
#' @param zoom Boolean variable to set the plot to zoom to the relevant data.
#' Default is set on TRUE.
#'
#' @param line_type Type of lines used in the plot.
#'
#' @param line_width Width of the lines in the plot. Default set on 1.
#'
#' @param line_pch A vector with the symbols used for every line.
#' Default vector provided.
#'
#' @param  line_colors A vector with colors used for the lines in the plot.
#' Default vector provided.
#'
#'
#' @author  Yves R. Sagaert
#'
#' @return A plot with the trade-off curves between service level (fill rate)
#' and inventory (average onhand inventory).
#'
#' @export
#'
#' @examples
#' \dontrun{
#' set.seed(101010)
#' series <- rnorm(60,100,1)
#' test_series <- rnorm(12,100,10)
#' forecast <- series[(length(series)-12):length(series)]
#' ri1 <- alphas_inventory_evaluation(demand_forecast=list(forecast,sd=rep(3,12)),
#' demand_test=test_series, hist_demand=series, plot.out=TRUE)
#' ri2 <- alphas_inventory_evaluation(demand_forecast=list(forecast-5,sd=rnorm(12,6,2)),
#' demand_test=test_series, hist_demand=series, plot.out=TRUE)
#' inventory_curve(models_inv=list(ri1,ri2),
#' modelnames=c("Trial model1","Trial model2"),zoom=FALSE)
#' }
# previously called fun alpha_sequence_inventory_plot

inventory_curve <- function(models_inv, modelnames,
                                colx="scaled_av_onhand_inventory",
                                coly="fill_rate",
                                xlab="Average on hand inventory",
                                ylab="1 - Fill Rate",
                                zoom=TRUE,
                                line_type=c(1,2,3),
                                line_width=rep(1,length(modelnames)),
                                line_pch=c(0,1,2,5,6,4,3),
                                line_colors=c("#7FC97F", "#BEAED4", "#FDC086",
                                              "#FFFF99", "#386CB0", "#F0027F",
                                              "#BF5B17")){
  # joint format data -- preformat first
  if ("matrix" %in% class(models_inv)){
    # determine colnames
    if ("m" %in% colnames(models_inv)){
      # There is no model column in the data; so only 1 model
      models_inv = list(models_inv)
      # rest is captured in the data ok section below
    } else {
      # For all models in the matrix
      models_mat <- data.frame(models_inv)
      models <- unique(models_mat[,"m"])
      m=1 # Debug
      for (m in 1:length(models)){
        model_nr <- models[m]
        model_data <- sqldf::sqldf(paste0("SELECT alpha,
                        AVG(",coly,") AS ",coly,",
                        AVG(",colx,") AS ",colx,"
                        FROM models_mat
                        WHERE m = ",model_nr,"
                        GROUP BY alpha ORDER BY alpha DESC"))
        # egdat <- sqldf::sqldf("SELECT alpha,
        #           AVG(fill_rate) AS fill_rate,
        #           AVG(scaled_av_onhand_inventory) AS scaled_av_onhand_inventory
        #           FROM models_mat
        #           GROUP BY alpha ORDER BY alpha DESC")
        # Save results in list
        if (m==1){
          models_inv <- list(model_data)
        } else
          # models_inv <- list(models_inv,model_data)
          models_inv <- append(models_inv,NA)
          models_inv[[m]] <- model_data

      }
    }
  }
  # format data ok:
  if (class(models_inv) == "list"){
  # plot average onhand inventory (y) versus fill rate (x)
  # get the data
  models_av_oh_inv<- lapply(models_inv, colname=colx,
                            FUN=lambda <- function(df, colname){return(df[,colname])})
  models_fr <- lapply(models_inv, colname=coly,
                      FUN=lambda <- function(df, colname){return(df[,colname])})

  if (zoom==TRUE){
    # intelligent zoom
    xlim <- c(min(unlist(models_av_oh_inv),na.rm=TRUE),
              max(unlist(models_av_oh_inv)[(1-unlist(models_fr))>0],na.rm=TRUE))
  } else {
    # show all
    xlim <- c(min(unlist(models_av_oh_inv),na.rm=TRUE),
              max(unlist(models_av_oh_inv),na.rm=TRUE))
  }

  ylim <- c(min(1-unlist(models_fr),na.rm=TRUE),
            max(1-unlist(models_fr),na.rm=TRUE))


  plot(xlim,ylim,type="n",xlab=xlab, ylab=ylab,
       xlim=xlim,ylim=ylim,xaxs="i",yaxs="i")

  m=1 #debug
  for (m in 1:length(models_av_oh_inv)){
    graphics::lines(models_av_oh_inv[[m]],1-models_fr[[m]],col=line_colors[m],
          type="l",pch=line_pch[m],cex=0.5,lty=line_type[m],lwd=line_width[m])
    graphics::lines(models_av_oh_inv[[m]],1-models_fr[[m]],col=line_colors[m],
          type="p",pch=line_pch[m],cex=0.5,lty=line_type[m],lwd=line_width[m])
  }
  graphics::legend("topright",legend=modelnames,lty=line_type,lwd=line_width, pch=line_pch,
         col=line_colors)
  } else {
    warning("The input format is not correct.")
  }
}
