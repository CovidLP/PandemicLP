#' @importFrom methods setOldClass
methods::setOldClass("pandemicEstimated")

traceplot_pandemicEstimated = function(object){
  traceplot(object$fit,pars=c("a","b","c","f"))
}

#' Draw traceplot of the parameters for the pandemic model
#'
#' Uses stan's traceplot function to draw the traceplots for the relevant parameters of the estimated model.
#' @param object Output of the \code{\link{pandemic_model}} function
#' @seealso \code{\link{pandemic_model}}
#' @examples
#' \dontrun{
#' dataMG = load_covid("Brazil","MG")
#' estimMG = pandemic_model(dataMG)
#' traceplot(estimMG)}
#' @importFrom rstan traceplot
#' @exportMethod traceplot
setMethod("traceplot","pandemicEstimated",traceplot_pandemicEstimated)
