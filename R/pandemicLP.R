#' pandemicLP: A package for testing the predict function for the pandemic predict package.
#'
#' \if{html}{\figure{logo.jpeg}{options: width="120px"}}\cr
#' The pandemicLP package provides functions to fit a generalized logistic curve for long
#' term prediction of epidemic and pandemic count data. Four main functions are the focus:
#' \itemize{
#' \item \link{load_covid} - Load COVID19 data from online repositories.
#' \item \link{pandemic_model} - estimates the model.
#' \item \link{posterior_predict.pandemicEstimated} - S3 method for the rstan::posterior_predict generic function.
#' \item \link{plot.pandemicPredicted} - S3 method for the graphics::plot generic function.
#' }
#'
#' Theoretical foundation can be found in \href{http://est.ufmg.br/covidlp/home/pt/}{est.ufmg.br/covidlp/home/pt}
#'
#' @docType package
#' @name pandemicLP
#' @useDynLib pandemicLP, .registration=TRUE
#' @import Rcpp
#' @import methods
#' @importFrom rstantools posterior_predict
#' @export posterior_predict
NULL
#> NULL
