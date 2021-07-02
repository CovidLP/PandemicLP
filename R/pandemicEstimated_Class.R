#' pandemicEstimated objects: Fitted PandemicLP model
#'
#' The \pkg{PandemicLP} model-fitting functions return an object of S3 class
#' \code{pandemicEstimated}, which is a list containing the components described below.
#'
#'
#' @name pandemicEstimated-objects
#'
#' @section Elements for \code{pandemicEstimated} objects:
#' \describe{
#'   \item{\code{model_name}}{
#'   the model name used.
#'   }
#'   \item{\code{family}}{
#'   string indicating data distribution.
#'   }
#'   \item{\code{multiwaves}}{
#'   number of waves.
#'   }
#'   \item{\code{seasonal_effect}}{
#'   string vector of the  weekdays' name with sazonal effect.
#'   }
#'   \item{\code{cases.type}}{
#'   the type of cases of interest used in modeling the epidemic.
#'   }
#'   \item{\code{config.inputs}}{
#'   a list with the main input arguments used in \code{\link{pandemic_model}}.
#'   }
#'   \item{\code{priors}}{
#'   a list with information on the prior distributions used and model restrictions (if there are any).
#'   }
#'    \item{\code{fit}}{
#'   an object of S4 Class \code{\link[rstan]{stanfit}} representing the fitted results via
#'   \code{\link[rstan]{sampling}}. For additional information about this element \code{fit},
#'   see \code{\link[rstan]{stanfit}}.
#'   }
#'   \item{\code{Y}}{
#'   a list with the \code{data}.
#'   }
#' }
#'
NULL
