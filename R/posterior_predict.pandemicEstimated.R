# Environment created to exchange objects between package functions
pandemic_environment = new.env()

#' Draw from the posterior predictive distribution for pandemic data
#'
#' The posterior predictive distribution is the distribution of the outcome
#' implied by the model after using the observed data to update our beliefs
#' about the unknown parameters in the model. Simulating data from the posterior
#' predictive distribution using the observed predictors is useful for checking
#' the fit of the model. Drawing from the posterior predictive distribution at
#' interesting values of the predictors also lets us visualize how a manipulation
#' of a predictor affects (a function of) the outcome(s). With new observations of
#' predictor variables we can use the posterior predictive distribution to generate
#' predicted outcomes.
#' @param object An object of class \code{pandemicEstimated} created by function \code{\link{pandemic_model}}.
#' @param horizonLong How far into the future the long-term prediction is desired.
#' @param horizonShort How far into the future the short-term prediction is desired.
#' @references
#' CovidLP Team, 2020. CovidLP: Short and Long-term Prediction for COVID-19. Departamento de Estatistica. UFMG,
#' Brazil. URL: \url{http://est.ufmg.br/covidlp/home/en/}
#' @return An object of class \code{pandemicPredicted}. It includes the sampled predictive distribution
#' the model used to predict, which is the same as the one used to estimate the data. This object can be used
#' directly into the plot function and contains the following elements:
#' \item{\code{predictive_Long}}{
#'   A \code{M x horizonLong} matrix with the full sample of the predictive distribution
#'   for the long-term prediction, where M is the sample size.
#'   The prediction is for daily new cases.
#'   }
#'   \item{\code{predictive_Short}}{
#'   A \code{M x horizonShort} matrix with the full sample of the predictive distribution
#'   for the short-term prediction, where M is the sample size.
#'   The prediction is for daily cumulative cases.
#'   }
#'   \item{\code{data}}{
#'   The data passed on from the \code{\link{pandemicEstimated-objects}} under the element \code{Y$data}.
#'   }
#'   \item{\code{location}}{
#'   A string with the name of the location.
#'   }
#'   \item{\code{cases_type}}{
#'   A string with either "confirmed" or "deaths" to represent the type of data that has been fitted and predicted.
#'   }
#'   \item{\code{pastMu}}{
#'   The fitted means of the data for the observed data points.
#'   }
#'   \item{\code{futMu}}{
#'   The predicted means of the data for the predicted data points.
#'   }
#' Function \code{\link{pandemic_stats}} provides a few useful statistics based on the predictions.
#'
#' @seealso \code{\link{pandemic_model}}, \code{\link{pandemic_stats}} and \code{\link{plot.pandemicPredicted}}.
#' @examples
#' \dontrun{
#' dataMG = load_covid("Brazil","MG")
#' estimMG = pandemic_model(dataMG)
#' predMG = posterior_predict(estimMG)
#' predMG}
#' @importFrom methods slot
#' @export
posterior_predict.pandemicEstimated = function(object,horizonLong = 500, horizonShort = 14){

  if (class(object) != "pandemicEstimated") stop("Please use the output of the pandemic_model() function.")
  if (horizonShort <= 0) stop("Horizons must be positive.")
  if (horizonLong < horizonShort) stop("Long-term horizon may not be lesser than short term horizon.")
  if (horizonLong > 1000) stop("Long-term horizon larger than 1000 is not currently supported.")

  chains = as.data.frame(object$fit)
  pop = object$Y$population

  M = nrow(chains) ## Total iterations
  NA_replacement = 2*object$Y$population ## Set a NA replacement

  finalTime = sum(grepl("mu",names(chains))) ## How many mu's

  # generate points from the marginal predictive distribution
  pred = generatePredictedPoints_pandemic(M,chains,1000, NA_replacement, object$model_name, finalTime)
  methods::slot(object$fit,"sim")$fullPred = list()
  methods::slot(object$fit,"sim")$fullPred$thousandLongPred = pred$yL # For internal use
  if (object$cases.type == "confirmed")
    methods::slot(object$fit,"sim")$fullPred$thousandShortPred = pred$yS + object$Y$data$cases[nrow(object$Y$data)]
  else
    methods::slot(object$fit,"sim")$fullPred$thousandShortPred = pred$yS + object$Y$data$deaths[nrow(object$Y$data)]
  methods::slot(object$fit,"sim")$fullPred$thousandMus = pred$mu
  y.futL = methods::slot(object$fit,"sim")$fullPred$thousandLongPred[,1:horizonLong]
  y.futS = methods::slot(object$fit,"sim")$fullPred$thousandShortPred[,1:horizonShort]
  errorCheck = which(methods::slot(object$fit,"sim")$fullPred$thousandShortPred[,ncol(methods::slot(object$fit,"sim")$fullPred$thousandShortPred)] > pop)
  if (length(errorCheck)){
    message(paste0(length(errorCheck)," samples were removed from the prediction due to unrealistic results."))
    methods::slot(object$fit,"sim")$fullPred$thousandShortPred = methods::slot(object$fit,"sim")$fullPred$thousandShortPred[-errorCheck,]
    methods::slot(object$fit,"sim")$fullPred$thousandLongPred = methods::slot(object$fit,"sim")$fullPred$thousandLongPred[-errorCheck,]
    y.futL = y.futL[-errorCheck,]
    y.futS = y.futS[-errorCheck,]
  }

  output <- list(predictive_Long = y.futL, predictive_Short = y.futS,
                 data = object$Y$data, location = object$Y$name, cases_type = object$cases.type,
                 pastMu = as.data.frame(object$fit)[grep("mu",names(chains))],
                 futMu = pred$mu[,1:horizonLong],fit = object$fit,errors = errorCheck)

  class(output) = "pandemicPredicted"
  return(output)
}

# Auxiliary function for posterior_predict.pandemicEstimated
# c for chains, h for horizon, n for NA value and m for model
#' @importFrom stats rpois
#' @importFrom stats rgamma
generatePredictedPoints_pandemic = function(M,c,h,n,m,ft){
  y = mu = matrix(-Inf,ncol = h,nrow = M)
  if (grepl("poisson",m))
    for (i in 1:h){
      mu[,i] = exp(log(c$f)+log(c$a)+log(c$c)-(c$c*(ft+i))-(c$f+1)*log(c$b+exp(-c$c*(ft+i)) ))
      y[,i] = stats::rpois(M,mu[,i])
    } else if (grepl("negbin",m))
      for (i in 1:h){
        mu[,i] = log(c$f)+log(c$a)+log(c$c)-(c$c*(ft+i))-(c$f+1)*log(c$b+exp(-c$c*(ft+i)) ) # log scale
        y[,i] = stats::rpois(M,stats::rgamma(M,mu[,i]*c$aGamma,c$aGamma))
      } else
        stop(paste("Unknown model",m))

  if (any(is.na(y))){
    message(paste("Prediction had",sum(is.na(y)),"NA values. Replaced with large value for identification."))
    y.fut[is.na(y.fut)] = n
  }

  list(yL=y, yS = t(apply(y,1,cumsum)), mu = mu)
}
