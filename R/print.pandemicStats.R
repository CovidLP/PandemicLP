#' Print method for \code{pandemicStats} objects
#'
#' S3 method designed for \code{pandemicStats} objects. It displays a compact summary of the
#' 95\% credible intervals for the predictions calculated by the pandemic model.
#'
#' @method print pandemicStats
#'
#' @param object An object of S3 class \code{pandemicStats}.
#'
#' @seealso \code{\link{pandemic_stats}}  \code{\link{pandemicStats-objects}}
#'
#' @export

print.pandemicStats <-function(object, ...){

  cat("\n95% Credible Intervals for ",ifelse(object$data$case_type=="confirmed", "confirmed", "death"),
      " cases in ",object$data$location,"\n", sep = "")

  ST = nrow(object$ST_predict)
  LT = nrow(object$LT_predict)

  cat("\nShort-term Predictions:\n")
  width<-max(sapply(object$ST_predict, nchar))
  print(head(format(object$ST_predict, width=width, justify = "right"), n=3))
  cat("   ...")
  colnames(object$ST_predict)<- NULL
  print(tail(format(object$ST_predict, width=width, justify = "right"), n=3))

  cat("\n------")
  cat("\nLong-term Predictions:\n")
  width1<-max(sapply(object$LT_predict, nchar))
  print(head(format(object$LT_predict, width=width1, justify = "right"), n=3))
  cat("   ...")
  colnames(object$LT_predict)<- NULL
  print(tail(format(object$LT_predict, width=width1-1, justify = "right"), n=3))

  cat("\n------")
  cat("\nTotal Number of Cases:\n")
  total = data.frame(object$LT_summary$total_cases_LB, object$LT_summary$total_cases_med,
                            object$LT_summary$total_cases_UB)
  colnames(total)<- c("q2.5","med","q97.5")
  print(total, row.names = F)

  cat("\n------")
  cat("\nPeak Dates:\n")
  peak = data.frame(object$LT_summary$peak_date_LB, object$LT_summary$peak_date_med,
                         object$LT_summary$peak_date_UB)
  colnames(peak)<- c("q2.5","med","q97.5")
  print(peak, row.names = F)

  cat("\n------")
  cat("\nEnd Dates:\n")
  end = data.frame(object$LT_summary$end_date_LB, object$LT_summary$end_date_med,
                    object$LT_summary$end_date_UB)
  colnames(end)<- c("q2.5","med","q97.5")
  print(end, row.names = F)

  cat("\n------\n")
  cat("*For more information see ?`pandemicStats-objects`\n")
  cat("*For details on the calculations, see ?pandemic_stats\n")


  invisible(object)

}
