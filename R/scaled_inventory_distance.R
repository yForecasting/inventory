#' Minimum Scaled Inventory Distance
#'
#' Calculate the inventory evaluation metric SID
#'
#' SID is the minimum scaled inventory distance which makes the trade-off between
#' fill-rate service level and average onhand inventory.
#'
#' @param inventory_result A matrix with the results from inventory evaluation.
#'
#' @param scaled Use the scaled SID by default. The use of unscaled inventory
#' distance may skew results to series with large shortages or inventory.
#' Default set on TRUE.
#'
#' @author  Yves R. Sagaert
#'
#' @return Numeric value of SID.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' example_inventory_result=data.frame(m=c(1,1,1,2,2,2),
#' av_onhand_inventory=c(10,11,12,20,25,30),
#' fill_rate=c(0.9,0.92,0.95,1,1,1),
#' scaled_av_onhand_inventory = c(0.20, 0.22, 0.24, 0.40, 0.50, 0.60),
#' av_shortage_items = c(5,4,2,0,0,0),
#' scaled_av_shortage_items=c(0.10, 0.08, 0.04, 0,0,0))
#'
#' scaled_inventory_distance(inventory_result=example_inventory_result)
#'
#' scaled_inventory_distance(inventory_result=example_inventory_result, scaled=FALSE)
#' }
scaled_inventory_distance <- function(inventory_result, scaled = TRUE){
  if (scaled==TRUE){
    colname_inv <- "scaled_av_onhand_inventory"
    colname_shortage <- "scaled_av_shortage_items"
  } else {
    # Scaled is false
    colname_inv <- "av_onhand_inventory"
    colname_shortage <- "av_shortage_items"
  }
  # Case 1: s m alpha #b[(b[,"s"]==1),] - not unless filter for series here
  # Case 2: m alpha
  if (colnames(inventory_result)[1]=="m" || colnames(inventory_result)[2]=="m"){
    models_id <- unique(inventory_result[,"m"])
    im=1 # Debug
    sid = matrix(NA,nrow=4,ncol=length(models_id))
    rownames(sid) <- c("SID", colname_inv,
                       "fill_rate",
                       colname_shortage)
    for (im in models_id){
      inv_res <- inventory_result[(inventory_result[,"m"]==im),]
      # SID = Sqrt(scaled_av_inventory^2 + scaled_av_shortage_items^2)
      sid_vector = sqrt(inv_res[,colname_inv]^2 +
                             inv_res[,colname_shortage]^2)
      ii <- which(min(sid_vector)==sid_vector)
      sid[1,im] <- min(sid_vector)
      sid[2,im] <- inv_res[ii,colname_inv]
      sid[3,im] <- inv_res[ii,"fill_rate"]
      sid[4,im] <- inv_res[ii,colname_shortage]

    }
  }
  # Case 3: alpha
  if (colnames(inventory_result)[1]=="alpha"){
    # SID = Sqrt(scaled_av_inventory^2 + scaled_av_shortage_items^2)
    sid = min(sqrt(inventory_result[,colname_inv]^2 +
            inventory_result[,colname_shortage]^2))

  }
  return(sid)
}
