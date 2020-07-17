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
#' @param object An object of class pandemicEstimated created by function stan_pandemic.
#' @param horizonLong How far into the future the long term prediction is desired.
#' @param horizonShort How far into the future the short term prediction is desired.
#' @return An object of class pandemicPredicted. It includes the sampled predictive distribution,
#' the model used to predict, which is the same as the one used to estimate the data, and relevant
#' information for the plot function to be used directly.
#' @seealso \code{\link{pandemic_model}} and \code{\link{plot.pandemicPredicted}}.
#' #' @examples
#' \dontrun{
#' dataMG = load_covid("Brazil","MG")
#' estimMG = pandemic_model(dataMG)
#' predMG = posterior_predict(estimMG)
#' predMG}
#' @export
posterior_predict.pandemicEstimated = function(object,horizonLong = 300, horizonShort = 14){

  if (class(object) != "pandemicEstimated") stop("Please use the output of the pandemic_model() function.")

  chains = as.data.frame(object$fit)
  pop = object$Y$population

  M = nrow(chains) ## Total iterations
  NA_replacement = 2*object$Y$population ## Set a NA replacement

  finalTime = sum(grepl("mu",names(chains))) ## How many mu's

  # generate points from the marginal predictive distribution
  pred = generatePredictedPoints_pandemic(M,chains,horizonLong, NA_replacement, object$model,finalTime)
  y.futL = pred$yL
  if (object$cases.type == "confirmed")
    y.futS = pred$yS[,1:horizonShort] + object$Y$data$cases[nrow(object$Y$data)]
  else
    y.futS = pred$yS[,1:horizonShort] + object$Y$data$deaths[nrow(object$Y$data)]

  output <- list(predictive_Long = y.futL, predictive_Short = y.futS,
                 data = object$Y$data, locale = object$Y$name, cases_deaths = object$cases.type)

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
      mu[,i] = log(c$f)+log(c$a)+log(c$c)-(c$c*(ft+i))-(c$f+1)*log(c$b+exp(-c$c*(ft+i)) ) # log scale
      y[,i] = stats::rpois(M,exp(mu[,i]))
    } else if (grepl("negbin",m))
      for (i in 1:h){
        mu[,i] = log(c$f)+log(c$a)+log(c$c)-(c$c*(ft+i))-(c$f+1)*log(c$b+exp(-c$c*(ft+i)) ) # log scale
        y[,i] = stats::rpois(M,stats::rgamma(M,exp(mu[,i])*c$aGamma,c$aGamma))
      } else
        stop(paste("Unknown model",m))

  if (any(is.na(y))){
    message(paste("Prediction had",sum(is.na(y)),"NA values. Replaced with large value for identification."))
    y.fut[is.na(y.fut)] = n
  }

  list(yL=y, yS = t(apply(y,1,cumsum)))
}
