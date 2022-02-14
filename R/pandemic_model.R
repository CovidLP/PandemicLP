#' @title Bayesian growth curve models for epidemiological data via Stan
#'
#' @name pandemic_model
#'
#' @description Bayesian inference for modeling epidemiological data or Covid-19 pandemic data using growth curve models.
#' This function draws the posterior samples of the parameters of the growth curve models available in the \code{PandemicLP} package. The sampling algorithm is \code{"NUTS"},
#' which is the No-U-Turn sampler variant of Hamiltonian Monte Carlo (Hoffman and Gelman 2011, Betancourt 2017).
#'
#'
#' See which models are available in the \code{PandemicLP} package in \code{\link{models}}.
#'
#'
#' See \code{\link{posterior_predict.pandemicEstimated}} to make predictions, \code{\link{pandemic_stats}} to provide
#' a few useful statistics based on the predictions and
#' \code{\link{plot.pandemicPredicted}} to plot the predicted values.
#'
#' @param Y an object of class \code{\link{pandemicData-objects}} created by function \code{\link{load_covid}}
#' or a list providing the epidemiological data for the model.
#' The elements of this Y list are:
#'
#' \describe{
#'   \item{\code{data}:}{
#'   a data frame with a \code{date} column and at least one of the following:  \code{cases}, \code{new_cases}, \code{deaths}, \code{new_deaths}.
#'   Bellow are descriptions of each of these columns:
#'
#'   \describe{
#'   \item{\code{date}:}{a date vector. It should be of class 'Date' and format 'YYYY-MM-DD'.}
#'
#'   \item{\code{cases}:}{a numeric vector with the time series values of the cumulative number of cases.}
#'
#'   \item{\code{new_cases}:}{a numeric vector with the time series values of the number of new confirmed cases.}
#'
#'   \item{\code{deaths}:}{a numeric vector with the time series values of the cumulative number of deaths.}
#'
#'   \item{\code{new_deaths}:}{a numeric vector with the time series values of the number of new deaths.}
#'   }
#'
#'   The data frame should be ordered by date in ascending order.
#'
#'   }
#'
#'   \item{\code{name}:}{a string providing the name of Country/State/Location of the epidemiological data.}
#'
#'   \item{\code{population}:}{a positive integer specifying the population size of the
#'    Country/State/Location selected.}
#'   }
#'
#'
#'   For formatting epidemiological data (not provided by the \code{load_covid} function) in the specified Y list format,
#'   see the ??? function, the vignette ???? or \strong{Examples} section in
#'    \code{\link{covid19BH}}.
#'
#'@param case_type a string providing the type of cases of interest in modelling the epidemic.
#'Current options are \code{"confirmed"} for confirmed cases or \code{"deaths"} for deaths. The default is \code{"confirmed"}.
#'This argument is not required when data frame \code{Y$data} (on the input argument \code{Y}) contains only information
#'from one of the data series \code{new_cases} or \code{new_deaths}.
#'
#'@param family "poisson" or "negbin". This argument indicates the data distribution.
#'The default is \code{family="poisson"}.
#'
#' @param seasonal_effect string vector indicating the days of the week in which seasonal effect was observed.
#' The vector can contain the full weekday name (sunday to saturday) or the first 3 letters,
#'  up to a maximum of three weekdays. For details go to \code{\link{models}}.
#'
#' @param n_waves a integer positive. This argument indicates the number of waves to be adjusted by mean curve.
#' The default is 1. For details go to \code{\link{models}}.
#'
#' @param p a numerical value greater than 0 and less than or equal to 1. It is
#'the percentage of the maximum cumulative total number of cases until the end of the
#'epidemic in relation to the population of the location. The default is \code{p = 0.08}. This is a model restriction.
#' See more on the \code{\link{models}}.
#'
#'@param phiTrunc a positive real number (or zero). This argument indicates a truncation on the priori of
#' the 'phi' parameter of the Negative Binomial models. This input argument is required only when \code{family="negbin"}.
#' The default is \code{phiTrunc=0}. See more on the \code{\link{models}}.
#'
#'@param fTrunc a positive real number (or zero). This argument indicates a truncation on the priori of
#' the 'f' parameter of the Negative Binomial model with single wave. This input argument is required only when \code{family="negbin"}.
#' The default is \code{fTrunc=1}. See more on the \code{\link{models}}.
#'
#'@param chains a positive integer specifying the number of Markov chains. The default is \code{1},
#' which is default value used by the CovidLP app (\url{http://est.ufmg.br/covidlp/home/en/}).
#'
#' @param warmup a positive integer specifying the number of warmup (aka burnin) iterations per chain.
#' These warmup samples are not used for inference.  The default is \code{2000}, if \code{family="negbin"}
#' the value default becomes \code{warmup=5000}.
#'
#' @param thin  a positive integer specifying the period for saving samples. The default is \code{3},
#' which is the default value used by the CovidLP app (\url{http://est.ufmg.br/covidlp/home/en/}).
#'.
#'
#' @param sample_size  a positive integer specifying the posterior sample's size per chain that will be used for inference.
#' The total number of iterations per chain is:
#'
#'  \code{warmup} + \code{thin} * \code{sample_size}
#'
#'  The default is \code{1000}, which is the default value used by CovidLP app (\url{http://est.ufmg.br/covidlp/home/en/}).
#'
#' @param init specification of the initial values of the parameters per chain. The default is \code{"random"}.
#' Go to  \code{\link{models}} for more info about model parameters.
#' Any parameters whose values are not specified will receive initial values generated as described in
#' \code{init = "random"}. Specification of the initial values for \code{\link{pandemic_model}} can only be via list.
#' See the detailed documentation for the init argument via list in \code{\link[rstan]{stan}}. Alternatively
#' it can be an output of the \code{pandemic_model()} function, which uses the last stored iteration
#' from that object as the initial values. If the models are different, an analogy is made.
#'
#' @param ... other arguments passed to the function. These are optional arguments for the \code{\link[rstan]{sampling}}  (\pkg{rstan} package).
#' Additional arguments can be \code{control}, \code{cores}, etc...
#'
#' @param covidLPconfig \code{TRUE} or \code{FALSE}: flag indicating whether to use default
#' values of the CovidLP app as input arguments. This argument is disabled when \code{family="negbin"}.
#'
#' If \code{covidLPconfig = TRUE}, the \code{\link[rstan]{sampling}} uses the following configuration:
#' \code{chains = 1}, \code{warmup = 5000}, \code{thin = 3},  \code{sample_size = 1000},
#'
#' \code{control} = \code{list(max_treedepth = 50, adapt_delta = 0.999)},
#' \code{p = 0.08} for
#'
#' \code{case_type = "confirmed"} or \code{p = 0.02} for \code{case_type  = "deaths"},
#' \code{init} a list with default initial values for the parameters of each model available.
#'
#' When using \code{covidLPconfig = TRUE} the convergence of the chains is not guaranteed.
#' It only replicates the results of the fitted model with the contemplated data in
#' the CovidLP app (\url{http://est.ufmg.br/covidlp/home/en/}).
#' For \code{covidLPconfig = FALSE}: each argument will be set to its default value,
#' unless the user specifies otherwise.
#'
#'
#' @return An object of S3 Class \code{\link{pandemicEstimated-objects}} representing the fitted results.
#' The \code{fit} component of the \code{pandemicEstimated} class is an object of S4 Class \code{\link[rstan]{stanfit}}.
#'
#' @seealso \code{\link{load_covid}}, \code{\link{posterior_predict.pandemicEstimated}},
#' \code{\link{pandemic_stats}} and \code{\link{plot.pandemicPredicted}};
#' \code{\link{summary.pandemicEstimated}}. See which models are available in the \code{PandemicLP}
#' package in \code{\link{models}}.
#'
#' @references
#' CovidLP Team, 2020. CovidLP: Short and Long-term Prediction for COVID-19. Departamento de Estatistica. UFMG,
#' Brazil. URL: \url{http://est.ufmg.br/covidlp/home/en/}
#'
#' @examples
#' ##result of the pandemic_model function may take a few minutes
#'
#'### generalized logistic poisson model: ###############
#' \dontrun{
#' Y0=load_covid(country_name="Brazil",state_name="SP",last_date='2020-04-25')
#' plot(Y0,cases="new")
#' output0=pandemic_model(Y0)
#' print(output0)
#' #convergence diagnostics
#' traceplot(output0)
#' density(output0)
#' stan_ac(output0$fit,pars=c("a","b","c","f"))
#'
#' Y1=load_covid(country_name="Brazil",state_name="SP",last_date='2020-06-18')
#' plot(Y1,cases="new")
#' output1=pandemic_model(Y1,case_type="deaths",covidLPconfig=TRUE)
#' print(output1)
#' #convergence diagnostics
#' traceplot(output1)
#' density(output1)
#' stan_ac(output1$fit,pars=c("a","b","c","f"))
#'
#'
#' Y2=load_covid(country_name="Argentina",last_date='2020-05-07')
#' plot(Y2,cases="new")
#' output2=pandemic_model(Y2,covidLPconfig=TRUE)
#' print(output2)
#' #convergence diagnostics
#' traceplot(output2)
#' density(output2)
#' stan_ac(output2$fit,pars=c("a","b","c","f"))
#'
#'
#' #including initial values for parameters:
#' inits3=list(
#'  list(a=95,b=0.8,c=0.3,f=1.1)
#' )
#' output3=pandemic_model(Y2,init=inits3,chains=1,warmup=3000)
#' print(output3)
#' #convergence diagnostics
#' traceplot(output3)
#' density(output3)
#' stan_ac(output3$fit,pars=c("a","b","c","f"))
#'
#' #initival values for 2 chains:
#' inits4=list(
#'  list(a=95,b=0.8,c=0.3,f=1.1), list(f=1.01)
#' )
#' output4=pandemic_model(Y1,init=inits4,chains=2,warmup=3000)
#' print(output4)
#' # show all initival values input by user:
#' output4$config.inputs$use_inputs$init
#' #convergence diagnostics
#' traceplot(output4)
#' density(output4)
#' stan_ac(output4$fit,pars=c("a","b","c","f"))
#'
#' ### seasonal model: ###############
#' output5=pandemic_model(Y0,seasonal_effect=c("sunday","monday"))
#' print(output5)
#' #convergence diagnostics
#' traceplot(output5)
#' density(output5)
#' stan_ac(output5$fit,pars=c("a","b","c","f","d_1","d_2"))
#'
#' ## or, for 'seasonal_effect': strings vector with the 3 initial letters of the weekday(s)
#' Y3=load_covid(country_name="Brazil",state_name="MG",last_date='2020-09-05')
#' plot(Y3,cases="new")
#' #weekdays effect : sunday and monday:
#' output6=pandemic_model(Y3,seasonal_effect=c("sun","mon"),covidLPconfig=TRUE)
#' print(output6)
#' #convergence diagnostics
#' traceplot(output6)
#' density(output6)
#' stan_ac(output6$fit,pars=c("a","b","c","f","d_1","d_2"))
#'
#' ### multi_waves(2) model: ######################
#' Y4=load_covid(country_name="United States of America",last_date='2020-09-27')
#' plot(Y4,cases="new")
#' output7=pandemic_model(Y4,n_waves=2,covidLPconfig=TRUE)
#' print(output7)
#' #convergence diagnostics
#' traceplot(output7)
#' density(output7)
#' stan_ac(output7$fit,pars=c("a1","b1","c1","alpha1","delta1","a2","b2","c2","alpha2","delta2"))
#'}
#'
#'
#' @export

pandemic_model <- function(Y, case_type = "confirmed",family="poisson", seasonal_effect = NULL, n_waves = 1, p = 0.08,
                          phiTrunc = 0, fTrunc = 1, chains = 1, warmup = 2e3, thin = 3,
                          sample_size = 1e3, init = "random", ..., covidLPconfig = FALSE) {

  points <- list(...)

  if(!is.null(points[["algorithm"]])) stop("The input 'algorithm' of the Stan sampler cannot be used: The sampling algorithm is 'NUTS' in pandemic_model function.")


  ############### preparing data and warning for user when data is not load_covid

  Y$data <- accum_to_new(Y)

  if(is.null(Y$name[[1]])) stop("name of Country/State/Location should be informed in Y$name as character")
  if(!(is.character(Y$name[[1]]))) stop("name of Country/State/Location should be informed in Y$name as character")


  if(is.null(Y$population)) stop("Country/State/Location population should be informed in Y$population")
  if(!(is.numeric(Y$population))) stop("Country/State/Location population should be informed in Y$population as.numeric or as.integer")

  ########## warning for user for the inputs:  case_type, p, init, family, phiTrunc, fTrunc

  case_type <- tolower(case_type)
  if(case_type != "deaths" && case_type != "confirmed") stop("ERROR input 'case_type': choose 'deaths' or 'confirmed' for the fit model")

  if(!is.null(data_cases)){    #data_cases=NULL indicator Y$data with both 'new_cases' and 'new_deaths'.
  if(data_cases){case_type <- "confirmed"} else {case_type <- "deaths"} #data_cases=TRUE: user data with 'new_cases',=FALSE with 'new_deaths'
  }

  family <- tolower(family)
  if(family != "poisson" && family != "negbin") stop("This package supports only the negative binomial and poisson distributions for the data.")

  if(!is.numeric(p) | p <= 0 | p >1) stop("p should be a percent of Country/State/Location population, 0 < p <= 1")

  if(is.function(init)) stop("pandemic_model does not allow initial values via function. See you help(pandemic_model)")

  if(family == "negbin"){
  if(!is.numeric(phiTrunc) | !is.numeric(fTrunc) | phiTrunc < 0 | fTrunc < 0) stop("phiTrunc, fTrunc should be a positive real or zero")
  } else if(phiTrunc != 0 | fTrunc != 1){
    warning("The phiTrunc and fTrunc input arguments are disabled for models with poisson distribution.")
    }


  ####  warning: seasonal_effect
  if(!is.null(seasonal_effect)){
    if(!is.character(seasonal_effect)) stop("ERROR input 'seasonal_effect': vector of a maximum of three week days (sunday to saturday)")
    if(length(seasonal_effect) > 3) stop("ERROR input 'seasonal_effect': vector of a maximum of three week days (sunday to saturday)")
    seasonal_effect <- tolower(seasonal_effect)

  #transforms 3 initial letters of the weekday's name in full name
    for (s in 1:length(seasonal_effect))
      if (seasonal_effect[s] == "sun")
        seasonal_effect[s] = "sunday" else if (seasonal_effect[s] == "mon")
          seasonal_effect[s] = "monday" else if (seasonal_effect[s] == "tue")
            seasonal_effect[s] = "tuesday" else if (seasonal_effect[s] == "wed")
              seasonal_effect[s] = "wednesday" else if (seasonal_effect[s] == "thu")
                seasonal_effect[s] = "thursday" else if (seasonal_effect[s] == "fri")
                  seasonal_effect[s] = "friday" else if (seasonal_effect[s] == "sat")
                    seasonal_effect[s] = "saturday"

    days <- c("sunday", "monday", "tuesday", "wednesday", "thursday","friday", "saturday")
    if(!(seasonal_effect[1] %in% days)  ) stop("ERROR input 'seasonal_effect': vector of a maximum of three week days (sunday to saturday)")
    if(!(seasonal_effect[2] %in% c(days, NA))  ) stop("ERROR input 'seasonal_effect': vector of a maximum of three week days (sunday to saturday)")
    if(!(seasonal_effect[3] %in% c(days, NA))  ) stop("ERROR input 'seasonal_effect': vector of a maximum of three week days (sunday to saturday)")
    ### warning: unsupported  seasonal effect with multiplewaves >=2:

    #if(n_waves!=1) stop("current version of the PandemicLP package only supports seasonal effect for the one wave models")
  }

  if (any(table(seasonal_effect) > 1)) stop("ERROR input 'seasonal_effect': cannot repeat weekday")

  ### warning: n_waves:

  if( !is.numeric(n_waves) | n_waves != floor(n_waves) | n_waves <= 0 ) stop("input 'n_waves' must be a positive integer.")

  #observation:
  # if !(is.null(seasonal_effect)) -->  seasonal model, in current version!


  ######## warning for user when covidLPconfig = TRUE:
  if(covidLPconfig){
  if(family == "negbin") {
    covidLPconfig <- FALSE
    warning("The covidLPconfig setting is disabled for models with negative binomial distribution.")
  }

  if(!is.null(points[["control"]])) stop("The input 'control' cannot be used when covidLPconfig = TRUE: CovidLPconfig settings control sampler behavior")

  if(chains!=1 | warmup!=2e3 | thin!=3 | sample_size !=1e3 | init != "random" | !is(init, "pandemicEstimated") ){
    warning("There is at least one configuration different from the ones provided in CovidLPconfig: CovidLPconfig settings will be used")
  }

  if(  p != 0.08 ){
    if(case_type=="confirmed"){
      warning("There is at least one configuration different from the ones provided in CovidLPconfig: CovidLPconfig settings will be used")
    }
    if(case_type == "deaths" && p != 0.02){
      warning("There is at least one configuration different from the ones provided in CovidLPconfig: CovidLPconfig settings will be used")
    }
  }

  }

  ######################################## fitted model:

  fit <- fitmodel(Y=Y,data_cases=data_cases,family=family,case_type=case_type,
               seasonal_effect=seasonal_effect, n_waves=n_waves, p = p, phiTrunc = phiTrunc,
               fTrunc=fTrunc, chains=chains, warmup=warmup,
                thin=thin, sample_size=sample_size, init=init,..., covidLPconfig = covidLPconfig)

  class(fit) <- "pandemicEstimated"

  return(fit)

}
