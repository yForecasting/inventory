#' inventory: A package for evaluating forecasts on inventory performance
#'
#' This package evaluates a demand forecast on inventory performance via
#' inventory simulation. A trade-off is shown between
#' the service level / fill rate and the average on-hand inventory.
#'
#' @author Yves R. Sagaert \email{yves.r.sagaert@gmail.com}
#' @docType package
#' @name inventory
# Flow of contribution: fork - code - document - check - pull request
# Add dependency via devtools - usethis::use_package(“utils”)
"_PACKAGE"

# Check on installation:
# if (!require("devtools")){install.packages("devtools")}
# devtools::install_github("yForecasting/inventory", force=TRUE)
# library(inventory)


#CRAN
# check - check(args = c('--as-cran')) - check_built - release
# checklist via: use_release_issue

# Flow of functions
# rolling_origin_inventory -> (calls) inventory
# inventory
# -> alphas_inventory_evaluation
# -> one_inventory_evaluation
