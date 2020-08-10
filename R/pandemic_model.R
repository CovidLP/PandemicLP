#' @title Bayesian logistic model for pandemic data via Stan
#'
#' @name pandemic_model
#'
#' @description Bayesian inference for modeling epidemiological data or Covid-19 pandemic data using logistic models.
#' This function draws the posterior samples of the parameters of the logistic model. The generalized logistic model
#' was chosen because it allows asymmetry, which is a characteristic of epidemiological data curves.\cr
#' \cr
#' See more about the model in the \strong{Details} section. \cr
#' \cr
#' See \code{\link{posterior_predict.pandemicEstimated}} to make predictions and
#' \code{\link{plot.pandemicPredicted}} to plot the predicted values.
#'
#' @param Y  A list providing the epidemiological data for the model.
#' The elements of this list are:
#'
#' \describe{
#'   \item{\code{data}:}{
#'   A data frame with at least the following columns:
#'
#'   \describe{
#'   \item{\code{date}:}{a date vector. It should be of class 'Date' and format 'YYYY-MM-dd'.}
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
#'   \item{\code{name}:}{a character string providing the name of Country/State/Location of the epidemiological data.}
#'
#'   \item{\code{population}:}{a positive integer specifying the population size of the
#'    Country/State/Location selected.}
#'   }
#'
#'   \cr
#'
#'   To provide Covid-19 data for Brazilian states and some other countries in the specified Y format,
#'   you can use the \code{\link{load_covid}} function.\cr
#'   \cr
#'   For formatting other epidemiological data in the specified Y format, see the \strong{Examples} section in
#'    \code{\link{covid19BH}}.
#'
#' @param case_type character string providing the type of cases of interest in modeling the epidemic.
#' Current options are \code{"confirmed"} for confirmed cases or \code{"deaths"} for deaths.
#' The default is \code{"confirmed"}.
#'
#' @param p a numerical value greater than 0 and less than or equal to 1. It's a percentage.
#' The maximum cumulative total number of cases for the end of the epidemic in a location should be
#' p percent of its population. The default is \code{p = 1}. This is a model restriction.
#' See more in the \strong{Details} section.
#'
#' @param chains A positive integer specifying the number of Markov chains. The default is \code{1},
#' which is default value used by the CovidLP app (\url{http://est.ufmg.br/covidlp/home/en/}).
#'
#' @param warmup A positive integer specifying the number of warmup (aka burnin) iterations per chain.
#' These warmup samples are not used for inference.  The default is \code{2000}.
#'
#' @param thin  A positive integer specifying the period for saving samples. The default is \code{3},
#' which is the default value used by the CovidLP app (\url{http://est.ufmg.br/covidlp/home/en/}).
#'.
#'
#' @param sample_size  A positive integer specifying the posterior sample's size per chain that will be used for inference.
#' The total number of iterations per chain is: \cr
#'  \code{warmup} + \code{thin} * \code{sample_size}\cr
#'  The default is \code{1000}, which is the default value used by CovidLP app (\url{http://est.ufmg.br/covidlp/home/en/}).
#'
#' @param init specification of the initial values of the parameters per chain. The default is \code{"random"}.
#' See the \strong{Details} section for more info about model parameters.
#' Any parameters whose values are not specified will receive initial values generated as described in
#' \code{init = "random"}. Specification of the initial values for \code{\link{pandemic_model}} can only be via list.
#' See the detailed documentation for the init argument via list in \code{\link[rstan]{stan}}.
#'
#' @param ... Other arguments passed to the function. These are optional arguments for the \code{\link[rstan]{sampling}}  (\code{rstan} package).
#' Additional arguments can be \code{control}, \code{cores}, etc...
#'
#' @param covidLPconfig \code{TRUE} or \code{FALSE}: flag indicating whether to use default
#' values of the CovidLP app as input arguments.\cr
#' If \code{covidLPconfig = TRUE}, the \code{\link[rstan]{sampling}} uses the following configuration:
#' \code{chains = 1}, \code{warmup = 5000}, \code{thin = 3},  \code{sample_size = 1000},
#' \code{init} = \code{list(list(a = 100 , b1 = log(1), c = 0.5, f = 1.01))}, \code{algorithm = "NUTS"},
#' \code{control} = \code{list(max_treedepth = 50, adapt_delta = 0.999)},
#' \code{p = 0.08} for \code{case_type = "confirmed"} or \code{p = 0.02} for \code{case_type  = "deaths"}.\cr
#' When using \code{covidLPconfig = TRUE} the convergence of the chains is not guaranteed.
#' It only replicates the results of the fitted model with the contemplated data in
#' the CovidLP app (\url{http://est.ufmg.br/covidlp/home/en/}).
#' For \code{covidLPconfig = FALSE}: each argument will be set to its default value,
#' unless the user specifies otherwise.
#'
#' @details
#' The time series of the cumulative number of cases of an epidemic can be modeled by the logistic curve,
#' with the Poisson distribuition, for both confirmed cases and deaths. The generalized logistc curve allows
#' the adjustment of an asymmetric curve. This can be a relevant characteristic. The Covid-19 pandemic data curve,
#' for example, has shown to have positive asymmetry. The derivative of the generalized logistic growth formula
#' is used to model the number of new daily cases: \cr
#' \cr
#' Y(t) ~ Poisson(mu(t)) \cr
#'
#' mu(t) =  \code{(a-d)*f*c*e^(-c*t) / (b + e^(-c*t))^(f+1)};  \cr
#' \cr
#' where Y(t) is the number of new cases at time t = 1, 2, ... ;\cr
#' parameters \code{a > 0 ,   b >= 0 ,  c > 0 ,  f > 0}  and \code{d = 0} is assumed. \cr
#' \cr
#'
#' Properties:\cr
#' 1.	Infection rate: controlled by parameter c;\cr
#' 2.	Asymptote of the logistic curve: \code{a/b^f} . It represents the cumulative total number of cases at the end of the epidemic;\cr
#' 3.	Asymmetry: parameter f controls the curve's asymmetry;\cr
#' 4.	\code{b = 0}: exponential growth; \cr
#' \cr
#'
#' Non-informative Priors:\cr
#' a ~  Gamma(0.1, 0.1) \cr
#' b ~ LogNormal(0, 20) \cr
#' c ~ Gamma(2, 9) \cr
#' f ~ Gamma(0.01, 0.01) \cr
#' \cr
#'
#' Model restrictions:\cr
#' 1.\code{a/b^f  <  p*population;  p in (0,1] }  :
#' The maximum value of the cumulative total number of cases at the end of the epidemic is p percent of the population.
#' p is a constant specified by the user as an input argument. The default is 1.
#' This model restriction is necessary mainly for data at the beginning of the pandemic.
#' Due to great uncertainty associated with the predictions, posterior samples can generate
#' much higher estimates than expected (unrealistic estimates) if this restriction is not applied. \cr
#' 2.  \code{f > 1} : This restriction forces a positive asymmetric fitted curve, which means that the number of cases
#' decreases slower than it increases. This behavior was observed for the Covid-19 data curve.\cr
#' \cr
#'
#' In future versions of the \pkg{PandemicLP} package, the user will be allowed to change the priors selected.
#'
#' @return An object of S3 Class \code{\link{pandemicEstimated-objects}} representing the fitted results.
#' The \code{fit} component of the \code{pandemicEstimated} class is an object of S4 Class \code{\link[rstan]{stanfit}}.
#'
#' @seealso \code{\link{load_covid}}, \code{\link{posterior_predict.pandemicEstimated}},
#' \code{\link{pandemic_stats}} and \code{\link{plot.pandemicPredicted}}.
#'
#' @references
#' CovidLP Team, 2020. CovidLP: Short and Long-term Prediction for COVID-19. Departamento de Estatistica. UFMG,
#' Brazil. URL: \url{http://est.ufmg.br/covidlp/home/en/}
#'
#' @examples
#' ##result of the pandemic_model function may take a few minutes
#'
#' \dontrun{
#' Y1=load_covid(country_name="Brazil",state_name="SP",last_date='2020-04-25')
#' output1=pandemic_model(Y1)
#' print(output1)
#' #convergence diagnostics
#' traceplot(output1)
#' stan_ac(output1$fit,pars=c("a","b","c","f"))
#' stan_dens(output1$fit,pars=c("a","b","c","f"))
#'
#' Y2=load_covid(country_name="Argentina",last_date='2020-05-07')
#' output2=pandemic_model(Y2,covidLPconfig=TRUE)
#' print(output2)
#' #convergence diagnostics
#' traceplot(output2)
#' stan_ac(output2$fit,pars=c("a","b","c","f"))
#' stan_dens(output2$fit,pars=c("a","b","c","f"))
#'
#' #including initial values for parameters:
#' inits3=list(
#'  list(a=95,b=0.8,c=0.3,f=1.1)
#' )
#' output3=pandemic_model(Y2,init=inits3,chains=1,warmup=3000)
#' print(output3)
#' #convergence diagnostics
#' traceplot(output3)
#' stan_ac(output3$fit,pars=c("a","b","c","f"))
#' stan_dens(output3$fit,pars=c("a","b","c","f"))
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
#' stan_ac(output4$fit,pars=c("a","b","c","f"))
#' stan_dens(output4$fit,pars=c("a","b","c","f"))}
#'
#' @importFrom rstan sampling
#'
#' @importClassesFrom rstan stanfit
#'
#' @export

pandemic_model = function(Y, case_type = "confirmed", p = 1, chains = 1, warmup = 2e3, thin = 3,
                        sample_size = 1e3, init = "random", ..., covidLPconfig = FALSE) {

  model_name="poisson_static_generalized_logistic"

  if(missing(Y)) stop("Y is a required argument. See help(pandemic_model)")
  if(!("list" %in% class(Y))) stop("Y should be a list. See help(pandemic_model)")


  if(!("data" %in% names(Y)) | !("name" %in% names(Y)) |!("population" %in% names(Y))) stop("object list Y should have elementes 'data', 'name', 'population'. See help(pandemic_model)")
  if(!("data.frame" %in% class(Y$data))) stop("Y$data should be a data.frame. See help(pandemic_model)")
  if(!("date" %in% names(Y$data)) | !("cases" %in% names(Y$data)) |!("deaths" %in% names(Y$data))  |!("new_cases" %in% names(Y$data))  |!("new_deaths" %in% names(Y$data)) ) stop("Y$data should be a data.frame with column names: 'date', 'cases', 'deaths', 'new_cases', 'new_deaths'. See help(pandemic_model)")
  if(!("Date" %in% class(Y$data$date))) stop("Y$data$date should be of class 'Date' and format 'YYYY-MM-dd' " )
  if(!(class(Y$data$cases) %in% c("integer","numeric")) | !(class(Y$data$deaths) %in% c("integer","numeric")) | !(class(Y$data$new_cases) %in% c("integer","numeric")) | !(class(Y$data$new_deaths) %in% c("integer","numeric"))) stop( "Y$data: values in 'cases', 'deaths', 'new_cases' and 'new_deaths' columns should be as.integer or as.numeric")

  #data processing: new_cases, new_deaths < 0:
  while(any(Y$data$new_cases <0)){
    pos <- which(Y$data$new_cases <0)
    for(j in pos){
      Y$data$new_cases[j-1] = Y$data$new_cases[j] + Y$data$new_cases[j-1]
      Y$data$new_cases[j] = 0
      Y$data$cases[j-1] = Y$data$cases[j]
    }
  }

  while(any(Y$data$new_deaths <0)){
    pos <- which(Y$data$new_deaths <0)
    for(j in pos){
      Y$data$new_deaths[j-1] = Y$data$new_deaths[j] + Y$data$new_deaths[j-1]
      Y$data$new_deaths[j] = 0
      Y$data$deaths[j-1] = Y$data$deaths[j]
    }
  }


  if(is.null(Y$name[[1]]) == TRUE) stop("name of Country/State/Location  should be informed in Y$name as character")
  if(!("character" %in% class(Y$name[[1]]))) stop("name of Country/State/Location should be informed in Y$name as character")


  if(is.null(Y$population) == TRUE) stop("Country/State/Location population should be informed in Y$population")
  if(!(class(Y$population) %in% c("integer","numeric"))) stop("Country/State/Location population should be informed in Y$population as.numeric or as.integer")


  if(case_type!="deaths" && case_type!="confirmed") stop("ERROR input 'case_type': choose 'deaths' or 'confirmed' for the fit model")

  if(class(p)!="numeric" | p <= 0 | p >1) stop("p should be a percent of Country/State/Location population, 0<p<=1")


  if(class(init)=="function") stop("pandemic_model does not allow initial values via function. See you help(pandemic_model)")


  if(covidLPconfig && case_type=="confirmed"){
    p=0.08
  }

  if(covidLPconfig && case_type=="deaths"){
    p=0.08*0.25
  }


  if(case_type=="confirmed"){cases="new_cases"}
  if(case_type=="deaths"){cases="new_deaths"}    #nature of the occurrence

  i=which(colnames(Y$data)==cases)


  t=dim(Y$data)[1]

  data_stan = list(y=Y$data[[i]], n=t, pop=Y$population, p=p)


  params = c("a","b","c","f","mu")

  number_iterations= warmup + thin*sample_size


  if(covidLPconfig){

    warmup= 5e3; thin=3; sample_size= 1e3; chains= 1

    number_iterations= warmup + thin*sample_size

    init= list(
      list(a=100 ,b1=log(1) ,c=0.5,f=1.01)   #init pandemic model only via list
    )

    mod_sim<- try(rstan::sampling(stanmodels[[model_name]], data = data_stan,
                                  algorithm="NUTS",
                                  pars = params,
                                  chains = chains,...,
                                  init = init,
                                  iter = number_iterations, warmup = warmup, thin = thin,
                                  control = list(max_treedepth = 50, adapt_delta=0.999),
                                  verbose = FALSE, open_progress=FALSE, show_messages=FALSE))

  } else {   #init: user, default="randon"  and control=NULL,  both default of stan
    #user can change 'algorithm'


    if(class(init)=="list") {   #including b1, excluding b : user view

      for(j in 1:length(init)){
        k=which(names(init[[j]])=="b")

        if(length(k)!=0){
          init[[j]]$b1=log(init[[j]]$b)
          init[[j]]=init[[j]][-k]
        }

      }
    }

    ##if(init="random")   # OK! for sampling

    mod_sim<- try(rstan::sampling(stanmodels[[model_name]], data = data_stan,
                                  pars = params,
                                  init=init,
                                  chains = chains,...,
                                  iter = number_iterations, warmup = warmup, thin = thin,
                                  verbose = FALSE, open_progress=FALSE, show_messages=FALSE))
  }


  if(class(mod_sim) != "try-error"){


    if(class(init)=="list") {   #including b1, excluding b : user view

      for(j in 1:length(init)){
        k=which(names(init[[j]])=="b")

        if(length(k)!=0){
          init[[j]]$b1=log(init[[j]]$b)
          init[[j]]=init[[j]][-k]
        }

      }
    }

    ##if(init="random")   # OK! for sampling


  priors=list(prior.a= "Gamma(0.1,0.1)" ,prior.b= "LogNormal(0,20)"  ,
              prior.c= "Gamma(2,9)" ,prior.f= "Gamma(0.01,0.01)",
              restrictions=list(paste0("a/b^f<"," ",p,"*population"),"f>1"))

  if(class(init)=="list"){  # init =  a list or "random"

    for(j in 1:length(init)){
      kk=which(names(init[[j]])=="b1")    #if user input initial value for b or covidLPconfig=TRUE

      if(length(kk)!=0){
        init[[j]]$b=exp(init[[j]]$b1)
        l=which(names(init[[j]])=="b1")
        init[[j]]=init[[j]][-l]                  #including b, excluding b1 :  user view

      }
    }

    names(init)=paste0("chain_id:", 1:length(init))

  }                  #else   init="random"


  use_inputs=list(warmup= warmup, thin=thin, sample_size= sample_size,
                  number_chains= chains , p=p,
                  init=init)


  output=list(model_name="poisson: static generalized logistic (d=0)",cases.type=case_type,
              config.inputs=list(covidLPconfig=covidLPconfig,use_inputs=use_inputs),
              priors=priors,fit=mod_sim,Y=Y)
  class(output)="pandemicEstimated"


  return(output)

} else {
  print("ERROR sampling STAN")

}

}
