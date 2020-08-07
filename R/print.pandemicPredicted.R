#' Prints prediction summary of pandemic model
#'
#' S3 method designed to summarize the prediction obtained by the
#' \link{posterior_predict.stanpandemic} function. It is not necessary to call function
#' directly, only visualize the object directly, unless it is desired to change the default arguments.
#' @param summaryFun Use this function to summarize the predictions. Default argument is mean, but can be any function on a vector
#' @param printPred Valid values are 'Long' or 'Short'. Note that 'Short' will show cumulative predictions
#' @seealso \link{posterior_predict.stanpandemic} and \link{plot.pandemicPredicted}
#' @examples
#' \dontrun{dataMG = load_covid("Brazil","MG")
#' estimMG = stan_pandemic(dataMG)
#' predMG = posterior_predict(estimMG)
#' predMG}
#' @export
print.pandemicPredicted = function(object,summaryFun = median,printPred = "Long",truncView = 3){
  cat("\nPredicted pandemic ",object$cases_deaths," for ",object$locale,". Can be plotted with plot().\n",sep="")
  if (printPred == "Long"){
    dates = max(object$data$date) + 1:ncol(object$predictive_Long)
    preds = apply(object$predictive_Long,2,summaryFun)
    names(preds) = dates
  }
  else if (printPred == "Short"){
    dates = max(object$data$date) + 1:ncol(object$predictive_Short)
    preds = apply(object$predictive_Short,2,summaryFun)
    names(preds) = dates
  }
  else stop("printPred must be \'Long\' or \'Short\'")
  cat("\nShowing predictive ",deparse(substitute(summaryFun))," for the ",tolower(printPred)," term predictions for ",object$locale,".\n\n",sep="")
  if (truncView >= (length(preds)/2) | (!truncView))
    print.default(preds)
  else {
    print.default(preds[1:truncView])
    cat("   ...\n\n")
    print.default(preds[(length(preds)-truncView+1):length(preds)])
  }
  cat("\n*For customized view, see help(print.pandemicPredicted)",sep="")
  cat("\n**For more details, see help(pandemicPredicted-objects)\n",sep="")

  invisible(object)
}
