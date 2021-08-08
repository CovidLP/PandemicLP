##### hidden environment:
#pandemic_environment$weekdays=c("sunday", "monday", "tuesday", "wednesday", "thursday",
#                                "friday", "saturday")


####### auxiliar function "seasonal_code( dates,s_e): coding the days of the week with seasonal effect
# dates = observed dates vector; s_e = seasonal_effects =  string vector with full weekdays' name

seasonal_code=function(dates,s_e){

  if(is.null(s_e)) code=NULL else {       #model doesn't have seasonal effect

    week=c("sunday", "monday", "tuesday", "wednesday", "thursday",
           "friday", "saturday")[as.POSIXlt(as.Date(dates[1:7]))$wday + 1]

    code=c()
    for(i in 1:length(s_e)){
      code[i]=which(week==s_e[i])
    }
  }
  return(code)
}


### auxiliar function 'config_stan'
#provide the settings for the STAN sampler

config_stan=function(Y,s_code,family,n_waves,p,case_type,phiTrunc,fTrunc,warmup,thin,sample_size,chains,init,covidLPconfig){

  if(covidLPconfig){   # for all models, exception negbin

    warmup= 5e3; thin=3; sample_size= 1e3; chains= 1

    if(n_waves>=2) {warmup=8e3}  # multiwaves models

    if(case_type=="confirmed"){ p=0.08 }
    if(case_type=="deaths") {p=0.08*0.25}

  }

#  if(family=="negbin"){     #on the family negbin the covidLPconfig argument is disabled
#    warmup=5e3
#    }

  number_iterations= warmup + thin*sample_size

if(case_type=="confirmed") cases="new_cases"
if(case_type=="deaths") cases="new_deaths"    #nature of the occurrence

i=which(colnames(Y$data)==cases) # i-column data Y for  data stan
t=dim(Y$data)[1]                 # n = t  ; n= nrows(Y$data) for data stan

data_stan = list(y=Y$data[[i]], n=t, pop=Y$population, p=p) #for all model

#####poisson models without seasonal effect:

if(is.null(s_code) && family=="poisson"){

  if(n_waves==1){
    # model generalized logistic doesn't have seasonal_effect

    model_name="pandemicModels_singleWave_poisson"

    params = c("a","b","c","f", "mu")

    if(covidLPconfig){
      init <- list(
        list(a = 100, b1 = log(1), c = .5, f = 1.01)
      )

    }


  } else {
    # multiwaves model without seasonal effect

    model_name="pandemicModels_multiWave_poisson"     #stanmodel name

    params = c("a","b","c","alpha","delta","mu")

    data_stan$nCurves=n_waves
    data_stan$w1=rep(0,n_waves)
    data_stan$w2=rep(0,n_waves)
    data_stan$w3=rep(0,n_waves)

      if(covidLPconfig){
        #delta=seq(1,ceiling((n_waves-1)*(t/n_waves))+1,by=floor(t/n_waves))
        #if(length(delta)>n_waves) delta=delta[1:n_waves]                      #problem only when small t (<50) and great n_waves (n_waves> 4)

        init <- list(
        list(a = rep(1,n_waves), b1 = rep(log(1),n_waves), c = rep(0.5,n_waves),
             alpha=rep(0.01,n_waves), delta=seq(1,ceiling((n_waves-1)*(t/n_waves))+1,by=floor(t/n_waves))) #delta=seq(0,(n_waves-1)*50,by=50)
      )
    }

  }

}

#####poisson models with seasonal effect

if (!is.null(s_code) && family=="poisson") { #poisson models with seasonal effect

  if(n_waves==1){        # model generalized logistic with seasonal_effect

   model_name="pandemicModels_SeasonalsingleWave_poisson"
  params = c("a","b","c","f",paste0("d_",1:length(s_code)),"mu")
  s_code = c(s_code,0,0)

  data_stan$w1=s_code[1]
  data_stan$w2=s_code[2]
  data_stan$w3=s_code[3]

  if(covidLPconfig){
    init <- list(
      list(a = 100, b1 = log(1), c = .5, f = 1.01, d_1=1,d_2=1,d_3=1)
    )
  }

  } else {   #multiwave with seasonal effect

    model_name="pandemicModels_multiWave_poisson"     #stanmodel name

    params = c("a","b","c","alpha","delta",paste0("d_",1:length(s_code)), "mu")
    s_code = c(s_code,0,0)

    data_stan$nCurves=n_waves
    data_stan$w1=rep(s_code[1],n_waves)
    data_stan$w2=rep(s_code[2],n_waves)
    data_stan$w3=rep(s_code[3],n_waves)

    if(covidLPconfig){
      #delta=seq(1,ceiling((n_waves-1)*(t/n_waves))+1,by=floor(t/n_waves))
      #if(length(delta)>n_waves) delta=delta[1:n_waves]                      #problem only when small t (<50) and great n_waves (n_waves> 4)

     init <- list(
        list(a = rep(1,n_waves), b1 = rep(log(1),n_waves), c = rep(0.5,n_waves),
             alpha=rep(0.01,n_waves), delta=seq(1,ceiling((n_waves-1)*(t/n_waves))+1,by=floor(t/n_waves)),
             d_1=rep(1,n_waves), d_2=rep(1,n_waves), d_3=rep(1,n_waves))
      )

    }

  }

}


##### negbin models without seasonal effect:

if(is.null(s_code) && family=="negbin"){

  if(n_waves==1){
    # negbin 1 wave without seasonal_effect

    model_name="pandemicModels_singleWave_negbin"

    params = c("a","b","c","f","phi","mu")

    data_stan$w1=0
    data_stan$w2=0
    data_stan$w3=0
    data_stan$fTrunc=fTrunc
    data_stan$phiTrunc=phiTrunc

#    if(covidLPconfig){  #negbin não terá covidLPconfig=TRUE
#      init <- list(
#        list(a = 100, b1 = log(1), c = .5, f=f)
#      )
#   }


  } else {
    #negbin multiwaves model without seasonal effect

    model_name="pandemicModels_multiWave_negbin"     #stanmodel name

    params = c("a","b","c","alpha","delta","phi","mu")

    data_stan$nCurves=n_waves
    data_stan$w1=rep(0,n_waves)
    data_stan$w2=rep(0,n_waves)
    data_stan$w3=rep(0,n_waves)
    data_stan$phiTrunc=phiTrunc

#    if(covidLPconfig){  #negbin models don't have covidLPconfig
#      init <- list(
#        list(a = rep(1,n_waves), b1 = rep(log(1),n_waves), c = rep(0.5,n_waves),
#             alpha=rep(1,n_waves), delta=seq(0,(n_waves-1)*50,by=50))
#      )
#    }

  }

}


##### negbin models with seasonal effect

if (!is.null(s_code) && family=="negbin") {

  if(n_waves==1){        # negbin one wave model  with seasonal_effect

    model_name="pandemicModels_singleWave_negbin"
    params = c("a","b","c","f","phi",paste0("d_",1:length(s_code)),"mu")
    s_code = c(s_code,0,0)

    data_stan$w1=s_code[1]
    data_stan$w2=s_code[2]
    data_stan$w3=s_code[3]
    data_stan$fTrunc=fTrunc
    data_stan$phiTrunc=phiTrunc

#  if(covidLPconfig){
#      init <- list(
#        list(a = 100, b1 = log(1), c = .5, f = 1.01, d_1=1,d_2=1,d_3=1)
#      )
#    }

  } else {   #multiwave with seasonal effect

    model_name="pandemicModels_multiWave_negbin"     #stanmodel name

    params = c("a","b","c","alpha","delta", "phi", paste0("d_",1:length(s_code)), "mu")
    s_code = c(s_code,0,0)

    data_stan$nCurves=n_waves
    data_stan$w1=rep(s_code[1],n_waves)
    data_stan$w2=rep(s_code[2],n_waves)
    data_stan$w3=rep(s_code[3],n_waves)
    data_stan$phiTrunc=phiTrunc

#    if(covidLPconfig){
#      init <- list(
#        list(a = rep(1,n_waves), b1 = rep(log(1),n_waves), c = rep(0.5,n_waves),
#             alpha=rep(1,n_waves), delta=seq(0,(n_waves-1)*50,by=50),
#             d_1=rep(1,n_waves), d_2=rep(1,n_waves), d_3=rep(1,n_waves))
#      )
#    }

  }

}



out=list(data_stan=data_stan,params=params,init=init,p=p,phiTrunc=phiTrunc,fTrunc=fTrunc,
         warmup=warmup,thin=thin, model_name=model_name,
         sample_size=sample_size,chains=chains,number_iterations=number_iterations)

return(out)
}

######## auxiliar functon including_auxparameters(init): only for models with auxiliar parameters
# provides the replacement of the initial value of the parameter, given by the user, with the respective initial
#value of the auxiliary parameter.

including_auxparameters=function(init){    #if init="random" :  this auxiliar function is not necessary

  if(class(init)=="list") {

    for(j in 1:length(init)){     #including b1_1, excluding b1 (multiwaves): user view

      k=which(names(init[[j]])=="b")

      if(length(k)!=0){
        init[[j]]$b1= log(init[[j]]$b)
        init[[j]]=init[[j]][-k]
      }

    }


  } else init = init  #init = 'random'


  return(init)
}


######## auxiliar functon excluding_auxparameters(init): only for models with auxiliar parameters
# provides the replacement of the initial value of the auxiliar parameter, used by sampler STAN, with the respective
#initial value of the parameter of interest.

excluding_auxparameters=function(init){   #if init="random" :  this auxiliar function is not necessary

if(class(init)=="list"){

  for(j in 1:length(init)){
    kk=which(names(init[[j]])=="b1")    #if user input initial value for b1 or covidLPconfig=TRUE

    if(length(kk)!=0){
      init[[j]]$b=exp(init[[j]]$b1)
      l=which(names(init[[j]])=="b1")
      init[[j]]=init[[j]][-l]                  #including b1, excluding b1_1 (multiwaves) :  user view

    }
  }


} else {init=init}   #init = 'random'

  names(init)=paste0("chain_id:", 1:length(init))

  return(init)

}


### auxiliar function: Excludes parameters from a stanfit object.
#Used for the seasonal model to remove unused d_i parameters

## Obsolete ##
# remove_beta_from_stanfit = function(fitObj, pars_to_remove){
#
#   copy = fitObj
#   for (p in pars_to_remove){
#     slot(copy,"sim")$samples[[1]][[p]] = NULL
#     slot(copy,"sim")$pars_oi = slot(copy,"sim")$pars_oi[-grep(p,slot(copy,"sim")$pars_oi)]
#     slot(copy,"sim")$dims_oi[[p]] = NULL
#     slot(copy,"sim")$fnames_oi = slot(copy,"sim")$fnames_oi[-grep(p,slot(copy,"sim")$fnames_oi)]
#     slot(copy,"sim")$n_flatnames = slot(copy,"sim")$n_flatnames - 1
#   }
#   copy
# }


##### auxiliar funtion: fitmodel(...)
### provides adjustment of the model according to the configurations of the STAN sampler
#requested by the user

#' @importFrom rstan sampling
#'
#' @importClassesFrom rstan stanfit


fitmodel=function(Y,data_cases=data_cases,family, case_type,seasonal_effect,n_waves,p,
                  phiTrunc, fTrunc,
                  chains, warmup, thin, sample_size, init,...,covidLPconfig){

########### stan configuration

  s_code=seasonal_code(Y$data$date,seasonal_effect)    #codifing seasonal_effect

  config=config_stan(Y,s_code,family,n_waves,p,case_type,phiTrunc,fTrunc,warmup,thin,sample_size,     #confit stan
                     chains,init,covidLPconfig)

  init=including_auxparameters(config$init)    #replace initial values, by user, by initial values of the auxiliar paremeters ( when necessary) for datastan

  ############  stan fit

  if(covidLPconfig){

    if(n_waves==1){                                    #gen logistic or  gen logistic with seasonal effect
      control=list(max_treedepth = 50, adapt_delta=0.999)
    } else {                                               # multiwaves model
      control=list(max_treedepth = 15, adapt_delta=0.995)
    }

  mod_sim<- try(rstan::sampling(stanmodels[[config$model_name]], data = config$data_stan,
                                  algorithm="NUTS",
                                  pars = config$params,
                                  chains = config$chains,...,
                                  init = init,
                                  iter = config$number_iterations, warmup = config$warmup,
                                  thin = config$thin,
                                  control = control,
                                  verbose = FALSE, open_progress=FALSE, show_messages=FALSE))

  } else {

    mod_sim<- try(rstan::sampling(stanmodels[[config$model_name]], data = config$data_stan,
                                  algorithm="NUTS",
                                  pars = config$params,
                                  init=init,
                                  chains = config$chains,...,
                                  iter = config$number_iterations, warmup = config$warmup,
                                  thin = config$thin,
                                  verbose = FALSE, open_progress=FALSE, show_messages=FALSE))

}


  if(class(mod_sim) != "try-error"){

########### preparing output:

    if(is.null(seasonal_effect)){

      if(n_waves==1) {                    #gen logistic

      priors=list(prior.a= "Gamma(0.1,0.1)" ,prior.b= "LogNormal(0,20)"  ,
                  prior.c= "Gamma(2,9)" ,prior.f= "Gamma(0.01,0.01)")

      if(family=="poisson"){
        priors$restrictions=list(paste0("a/b^f<"," ",p,"*population"),"f>1")
        name="poisson: static generalized logistic"
      } else {
        priors$phi="Gamma(0.1,0.1)"
        priors$restrictions=list(paste0("a/b^f<"," ",p,"*population"),paste0("f>",fTrunc),
                                 paste0("phi>",phiTrunc))
        name="negbin: static generalized logistic"
      }


      } else {   #multiwaves model without seasonal effect:

          priors=list(prior.a_i= "Gamma(0.1,0.1)" ,prior.b_i= "LogNormal(0,20)"  ,
                      prior.c_i= "Gamma(2,9)" ,prior.alpha_i= "Gamma(0.01,0.01)",
                      prior.delta_i="Normal(0,100)")

          if(family=="negbin"){
            priors[["phi"]]="Gamma(0.1,0,1)"
            priors$restrictions=list(paste0("a_i/b_i<"," ",p,"*population"),
                                     paste0("phi>"," ",phiTrunc))
            name=paste0("negbin: multi_waves(",n_waves,")")
          } else {
            priors$restrictions=paste0("a_i/b_i<"," ",p,"*population")
            name=paste0("poisson: multi_waves(",n_waves,")")
          }

        }

    } else {     #with seasonal effect

      if(n_waves==1){
      priors=list(prior.a= "Gamma(0.1,0.1)" ,prior.b= "LogNormal(0,20)"  ,
                  prior.c= "Gamma(2,9)" ,prior.f= "Gamma(0.01,0.01)",prior.d_i="Gamma(2,1)")

      if(family=="poisson"){
        priors$restrictions=list(paste0("a/b^f<"," ",p,"*population"),"f>1")
        name="poisson: static seasonal generalized logistic"
      } else {
        priors$phi="Gamma(0.1,0.1)"
        priors$restrictions=list(paste0("a/b^f<"," ",p,"*population"),paste0("f>",fTrunc),
                                 paste0("phi>",phiTrunc))
        name="negbin: static generalized logistic"
      }


      } else { #n_waves >=2 with seasonal effect:

        priors=list(prior.a_i= "Gamma(0.1,0.1)" ,prior.b_i= "LogNormal(0,20)"  ,
                    prior.c_i= "Gamma(2,9)" ,prior.alpha_i= "Gamma(0.01,0.01)",
                    prior.delta_i="Normal(0,100)", prior.d_i="Gamma(2,1)")

        if(family=="negbin"){
          priors[["phi"]]="Gamma(0.1,0,1)"
          priors$restrictions=list(paste0("a_i/b_i<"," ",p,"*population"),
                                   paste0("phi> ",phiTrunc))
        name=paste0("negbin: multi_waves(",n_waves,")")
        } else {
        priors$restrictions=paste0("a_i/b_i<"," ",p,"*population")
        name=paste0("poisson: multi_waves(",n_waves,")")
        }


    }


    }


    init=excluding_auxparameters(init) #replace initial values auxiliar parameters for initical valules of the original parameters.


    use_inputs=list(warmup= config$warmup, thin=config$thin, sample_size= config$sample_size,
                    number_chains= config$chains , p=config$p,phiTrunc=config$phiTrunc,
                    fTrunc=config$fTrunc, init=init)

    #output Y: only one type of case 'new_cases' or 'new_deaths' by user
    if(!is.null(data_cases)){
      if(data_cases){Y$data=Y$data[ ,c("date","new_cases","cases")]
      } else {
      Y$data=Y$data[ ,c("date","new_deaths","deaths")]
      }
    }

    fitted_median <- apply(as.data.frame(mod_sim)[grep("mu",names(mod_sim))], 2, median)
    names(fitted_median) <- paste0("error_",1:length(fitted_median))
    output=list(model_name=name,family=family,n_waves=n_waves,
                seasonal_effect=seasonal_effect, cases.type=case_type,
                config.inputs=list(covidLPconfig=covidLPconfig,use_inputs=use_inputs),
                priors=priors,fit=mod_sim,Y=Y,
                nominal_errors = config$data_stan$y - fitted_median,
                relative_errors = (config$data_stan$y - fitted_median)/config$data_stan$y)

    return(output)

  } else {
    print("ERROR sampling STAN")

  }

}



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
#'   a data frame with at least the 2 columns \code{date} and \code{new_cases} (or \code{new_deaths}).
#'   It can also contain all following columns:
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
#' See the detailed documentation for the init argument via list in \code{\link[rstan]{stan}}.
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
#' Y4=load_covid(country_name="US",last_date='2020-09-27')
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

pandemic_model = function(Y, case_type = "confirmed",family="poisson", seasonal_effect=NULL, n_waves=1, p = 0.08,
                          phiTrunc=0, fTrunc=1, chains = 1, warmup = 2e3, thin = 3,
                          sample_size = 1e3, init = "random", ..., covidLPconfig = FALSE) {

  points = list(...)

  if(!is.null(points[["algorithm"]])) stop("The input 'algorithm' of the Stan sampler cannot be used: The sampling algorithm is 'NUTS' in pandemic_model function")


  ############### preparing data and warning for user  whend data is not load_covid

  if(missing(Y)) stop("Y is a required argument. See help(pandemic_model)")
  if(!("list" %in% class(Y)) && !("pandemicData" %in% class(Y))) stop("Y should be a list or an objectof S3 Class 'pandemicData'. See help(pandemic_model) and help(load_covid)")

  names(Y)=tolower(names(Y))
  if(!("data" %in% names(Y)) | !("name" %in% names(Y)) |!("population" %in% names(Y))) stop("object list Y should have elementes 'data', 'name', 'population'. See help(pandemic_model)")
  if(!("data.frame" %in% class(Y$data))) stop("Y$data should be a data.frame. See help(pandemic_model)")

  data_cases = NULL  #indicator of the Y$data full: both 'new_cases' and 'new_deaths'.
  names(Y$data)=tolower(names(Y$data))

  # Y$data with 'new_deaths' and without 'new_cases':
  if(!("new_cases" %in% names(Y$data)) && "new_deaths" %in% names(Y$data) && class(Y$data$new_deaths) %in% c("integer","numeric")){
  data_cases=FALSE
  Y$data=cbind(Y$data,new_cases=Y$data$new_deaths)
  #Y$data with 'new_cases' and without 'new_deaths':
  } else if(!("new_deaths" %in% names(Y$data)) && "new_cases" %in% names(Y$data) && class(Y$data$new_cases) %in% c("integer","numeric")){
    data_cases=TRUE
    Y$data=cbind(Y$data,new_deaths=Y$data$new_cases)
  } else if(!("new_cases" %in% names(Y$data)) && !("new_deaths" %in% names(Y$data))) stop("data frame Y$data should have at least one of the 'new_cases' or 'new_deaths' data series.")


  #Y$data without either 'cumulative cases':
  if("new_cases" %in% names(Y$data) && !("cases" %in% names(Y$data)) && class(Y$data$new_cases) %in% c("integer","numeric")) {
    Y$data=cbind(Y$data,cases=cumsum(Y$data$new_cases))
  }
  if("new_deaths" %in% names(Y$data) && !("deaths" %in% names(Y$data)) && class(Y$data$new_deaths) %in% c("integer","numeric")) {
    Y$data=cbind(Y$data,deaths=cumsum(Y$data$new_deaths))
  }


  if(!("date" %in% names(Y$data)) | !("cases" %in% names(Y$data)) |!("deaths" %in% names(Y$data))  |!("new_cases" %in% names(Y$data))  |!("new_deaths" %in% names(Y$data)) ) stop("Y$data should be a data.frame with column names: 'date' and at least one of the 'new_cases' or 'new_deaths'. See help(pandemic_model)")
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


  if(is.null(Y$name[[1]])) stop("name of Country/State/Location  should be informed in Y$name as character")
  if(!("character" %in% class(Y$name[[1]]))) stop("name of Country/State/Location should be informed in Y$name as character")


  if(is.null(Y$population)) stop("Country/State/Location population should be informed in Y$population")
  if(!(class(Y$population) %in% c("integer","numeric"))) stop("Country/State/Location population should be informed in Y$population as.numeric or as.integer")

  ########## warning for user for the inputs:  case_type, p, init, family, phiTrunc, fTrunc

  case_type=tolower(case_type)
  if(case_type!="deaths" && case_type!="confirmed") stop("ERROR input 'case_type': choose 'deaths' or 'confirmed' for the fit model")

  if(!is.null(data_cases)){    #data_cases=NULL indicator Y$data with both 'new_cases' and 'new_deaths'.
  if(data_cases){case_type="confirmed"} else {case_type="deaths"} #data_cases=TRUE: user data with 'new_cases',=FALSE with 'new_deaths'
  } else {
    case_type=case_type
  }

  family=tolower(family)
  if(family!="poisson" && family!="negbin") stop("This package supports only the negative binomial and poisson distributions for the data.")

  if(class(p)!="numeric" | p <= 0 | p >1) stop("p should be a percent of Country/State/Location population, 0<p<=1")

  if(class(init)=="function") stop("pandemic_model does not allow initial values via function. See you help(pandemic_model)")

  if(family=="negbin"){
  if(class(phiTrunc)!="numeric" | class(fTrunc)!="numeric" |  phiTrunc < 0  |  fTrunc < 0) stop("phiTrunc, fTrunc should be a positive real or zero")
  } else if(phiTrunc!=0 | fTrunc!=1){
    warning("The phiTrunc and fTrunc input arguments are disabled for models with poisson distribution.")
    }


  ####  warning: seasonal_effect
  if(!is.null(seasonal_effect)){
    if(class(seasonal_effect) != "character") stop("ERROR input 'seasonal_effect': vector of a maximum of three week days (sunday to saturday)")
    if(length(seasonal_effect) > 3) stop("ERROR input 'seasonal_effect': vector of a maximum of three week days (sunday to saturday)")
    seasonal_effect=tolower(seasonal_effect)

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

    days=c("sunday", "monday", "tuesday", "wednesday", "thursday","friday", "saturday")
    if(!(seasonal_effect[1] %in% days)  ) stop("ERROR input 'seasonal_effect': vector of a maximum of three week days (sunday to saturday)")
    if(!(seasonal_effect[2] %in% c(days,NA))  ) stop("ERROR input 'seasonal_effect': vector of a maximum of three week days (sunday to saturday)")
    if(!(seasonal_effect[3] %in% c(days,NA))  ) stop("ERROR input 'seasonal_effect': vector of a maximum of three week days (sunday to saturday)")
    ### warning: unsupported  seasonal effect with multiplewaves >=2:

    #if(n_waves!=1) stop("current version of the PandemicLP package only supports seasonal effect for the one wave models")
  }

  if (any(table(seasonal_effect)>1)) stop("ERROR input 'seasonal_effect': cannot repeat weekday")

  ### warning: n_waves:

  if( !(class(n_waves) %in% c("numeric","integer")) | n_waves <=0 ) stop("input 'n_waves' must be a positive integer.")
  #if(n_waves>=4) warning("current version of the PandemicLP package tested with up to 3 waves.")

  #observation:
  # if !(is.null(seasonal_effect)) -->  seasonal model, in current version!


  ######## warning for user whend used covidLPconfig=TRUE:

  if(covidLPconfig && family=="negbin") {
    covidLPconfig=FALSE
    warning("The covidLPconfig setting is disabled for models with negative binomial distribution.")
  }

  if(covidLPconfig){

    if(!is.null(points[["control"]])) stop("The input 'control' cannot be used when covidLPconfig = TRUE: CovidLPconfig settings control sampler behavior")


    if(chains!=1 | warmup!=2e3 | thin!=3 | sample_size !=1e3 | init != "random" ){
      warning("There is at least one configuration different from the ones provided in CovidLPconfig: CovidLPconfig settings will be used")
    }

    if(  p!=0.08 ){
      if(case_type=="confirmed"){
        warning("There is at least one configuration different from the ones provided in CovidLPconfig: CovidLPconfig settings will be used")
      }
      if(case_type=="deaths" && p!=0.02){
        warning("There is at least one configuration different from the ones provided in CovidLPconfig: CovidLPconfig settings will be used")
      }
    }

  }

  ######################################## fitted model:

  fit=fitmodel(Y=Y,data_cases=data_cases,family=family,case_type=case_type,
               seasonal_effect=seasonal_effect, n_waves=n_waves, p=p,phiTrunc=phiTrunc,
               fTrunc=fTrunc, chains=chains, warmup=warmup,
                thin=thin, sample_size=sample_size, init=init,..., covidLPconfig=covidLPconfig)

  class(fit)="pandemicEstimated"

  return(fit)

}
