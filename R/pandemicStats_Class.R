#' pandemicStats objects: 95% Credible Interval predictions from PandemicLP model
#'
#' The \code{pandemic_stats} function returns an object of S3 class
#' \code{pandemicStats} in a list format containing the components described below. \cr
#' \cr
#'
#' @name pandemicStats-objects
#'
#' @section Elements for \code{pandemicStats} objects:
#' \describe{
#'   \item{\code{data}}{
#'   A list containing a data frame with the pandemic data observed, a character string with the location name,
#'   and a character string indicating wheter the cases predicted are confirmed cases or deaths.
#'   }
#'   \item{\code{ST_predict}}{
#'   A data frame with a 95% credible interval for the cumulative cases predicted in the short-term.
#'   }
#'   \item{\code{LT_predict}}{
#'   A data frame with a 95% credible interval for the new cases predicted in the long-term.
#'   }
#'   \item{\code{LT_summary}}{
#'   A list with a 95% credible interval for the predicted total number of cases, peak
#'   and end dates of the pandemic.
#'   }
#'   \item{\code{mu}}{
#'   A data frame with the median value of the mean number of new cases for each day. Starting from the first
#'   day of the pandemic to the last day of the long-term horizon.
#'   }
#'
#' }
#'
NULL
