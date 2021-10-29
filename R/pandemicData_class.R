#' pandemicData objects: Pandemic Data
#'
#' The \code{\link{load_covid}} function and \code{\link{format_data}} function return
#' an object of S3 class \code{pandemicData} in a list format containing
#' the components described below.
#'
#' @name pandemicData-objects
#'
#' @section Elements for \code{pandemicData} object:
#' \describe{
#'   \item{\code{data}}{
#'   data frame with the number of cumulative cases, new cases or/and cumulative
#'   deaths and new deaths for each date in the specified location.}
#'   \item{\code{name}}{
#'   string with the location name.}
#'   \item{\code{population}}{
#'   numeric object that contains the population size of the given location.}
#' }
#'
NULL
