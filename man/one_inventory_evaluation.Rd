% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/one_inventory_evaluation.R
\name{one_inventory_evaluation}
\alias{one_inventory_evaluation}
\title{One Inventory Evaluation}
\usage{
one_inventory_evaluation(
  demand_forecast,
  demand_test,
  alpha = 0.05,
  plot.out = FALSE,
  mode = "normal",
  hist_demand = NA
)
}
\arguments{
\item{demand_forecast}{The forecast. This can be a forecast object, a list
of mean and sd of the forecast or a point cloud of the distribution of the
forecast.}

\item{demand_test}{A vector containing the actual demand for the inventory
evaluation. The length of the test demand should be equal to the length of
the demand forecast.}

\item{alpha}{The target service level of the inventory simulation. This is
set by default on 0.05.}

\item{plot.out}{Parameter to plot a graphic, default set on FALSE.}

\item{mode}{Shape of the distribution, default set on 'normal'.}

\item{hist_demand}{A vector of historical demand of the series, where the demand
forecast model is build on. Default is set on NA.}
}
\value{
A numeric vector with: used service level parameter alpha, stockout
rate, average inventory, average on-hand inventory, fill-rate, average test
demand, ratio average on-hand inventory on average test demand. A plot of
the simulation can be shown.
}
\description{
Evaluates a forecast via an inventory simulation
}
\details{
This function returns the output of an inventory simulation based on the
forecast input.
}
\examples{
\dontrun{
series <- rnorm(60,100,1)
test_series <- rnorm(12,100,1)
forecast <- series[(length(series)-12):length(series)]
one_inventory_evaluation(demand_forecast=list(forecast,sd=rep(3,12)),
demand_test=test_series, hist_demand=series, plot.out=TRUE)
}
}
\author{
Yves R. Sagaert
}
