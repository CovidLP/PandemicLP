% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mcmc_functions.R
\name{density.pandemicEstimated}
\alias{density.pandemicEstimated}
\title{Draw estimated density of the parameters for the pandemic model}
\usage{
\method{density}{pandemicEstimated}(x, ...)
}
\arguments{
\item{x}{Output of the \code{\link{pandemic_model}} function}

\item{...}{Additional parameters passed on the \code{\link[rstan]{stan_dens}} function}
}
\description{
Uses stan's stan_dens function to draw the marginal posterior for the relevant parameters of the estimated model.
Defined as a method for the stats::density generic function.
}
\examples{
\dontrun{
dataMG = load_covid("Brazil","MG")
estimMG = pandemic_model(dataMG)
density(estimMG)}
}
\seealso{
\code{\link{pandemic_model}} and \code{\link[rstan]{stan_dens}}.
}
