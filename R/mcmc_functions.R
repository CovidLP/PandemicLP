# Auxiliary function to build the correct parameters string vector
build_params <- function(object, waves){
  if (grepl("multi", object$model_name)){
    out <- c(paste0("a[",waves,"]"), paste0("b[",waves,"]"), paste0("c[",waves,"]"),
             paste0("alpha[",waves,"]"), paste0("delta[",waves,"]"))
    if (!is.null(object$seasonal_effect))
      out <- c(out, paste0(paste0("d_", 1:length(object$seasonal_effect)), "[", waves, "]"))
  }
  else{
    out <- c("a", "b", "c", "f")
    if (!is.null(object$seasonal_effect))
      out <- c(out, paste0("d_", 1:length(object$seasonal_effect)))
  }
  if (grepl("negbin", object$model_name))
    out <- c(out, "phi")

  out
}

#' @importFrom methods setOldClass
methods::setOldClass("pandemicEstimated")

#' @importFrom rstan traceplot
traceplot_pandemicEstimated = function(object, waves = 1:object$n_waves, ...){
  rstan::traceplot(object$fit, pars = build_params(object, waves), ...)
}

#' Draw traceplot of the parameters for the pandemic model
#'
#' Uses stan's traceplot function to draw the traceplots for the relevant parameters of the estimated model.
#' @param object Output of the \code{\link{pandemic_model}} function
#' @param waves If the estimated model has more than 1 wave, this
#' parameter controls which waves parameters are shown. Default are all
#' waves.
#' @param ... Aditional parameters passed on to the \code{\link[rstan]{traceplot}} function
#' @seealso \code{\link{pandemic_model}} and \code{\link[rstan]{traceplot}}
#' @examples
#' \dontrun{
#' dataMG = load_covid("Brazil","MG")
#' estimMG = pandemic_model(dataMG)
#' traceplot(estimMG)}
#' @exportMethod traceplot
setMethod("traceplot","pandemicEstimated",traceplot_pandemicEstimated)

#' Draw estimated density of the parameters for the pandemic model
#'
#' Uses stan's stan_dens function to draw the marginal posterior for the relevant parameters of the estimated model.
#' Defined as a method for the stats::density generic function.
#' @param x Output of the \code{\link{pandemic_model}} function
#' @param waves If the estimated model has more than 1 wave, this
#' parameter controls which waves parameters are shown. Default are all
#' waves.
#' @param ... Additional parameters passed on the \code{\link[rstan]{stan_dens}} function
#' @seealso \code{\link{pandemic_model}} and \code{\link[rstan]{stan_dens}}.
#' @examples
#' \dontrun{
#' dataMG = load_covid("Brazil","MG")
#' estimMG = pandemic_model(dataMG)
#' density(estimMG)}
#' @importFrom stats density
#' @importFrom rstan stan_dens
#' @method density pandemicEstimated
#' @export
density.pandemicEstimated = function(x, waves = 1:x$n_waves, ...){
        rstan::stan_dens(x$fit,pars=build_params(x, waves),...)
}
