#### Auxiliary function: transforming cumulative data in daily data and vice-versa, and adding to the dataset ####
accum_to_new <- function(Y){
  if(missing(Y)) stop("Y is a required argument. See help(pandemic_model)")
  if(!is.list(Y) && !is(Y, "pandemicData")) stop("Y should be a list or an object of S3 Class 'pandemicData'. See help(pandemic_model) and help(load_covid).")

  names(Y) <- tolower(names(Y))
  if(!(all(c("data", "name", "population") %in% names(Y)))) stop("object list Y should have elementes 'data', 'name', 'population'. See help(pandemic_model).")
  if(!is.data.frame(Y$data)) stop("Y$data should be a data.frame. See help(pandemic_model).")

  data_cases <-  NULL  #indicator of the Y$data full: both 'new_cases' and 'new_deaths'.
  names(Y$data) <- tolower(names(Y$data))

  # Y$data without either 'new_deaths' and 'new_cases':
  if(!any(c("new_cases", "new_deaths") %in% names(Y$data))) {
    if("cases" %in% names(Y$data)){
      n_lines = nrow(Y$data)
      col_new_cases = c()
      i <- as.integer(1)
      while (i <= n_lines){
        if (i == 1){
          i_new_cases = Y$data[i, "cases" ]
          col_new_cases = c(col_new_cases, i_new_cases)
        } else{
          i_new_cases = diff(Y$data[c(i-1, i), "cases" ])
          if (i_new_cases >= 0){
            col_new_cases = c(col_new_cases, i_new_cases)
          } else{
            col_new_cases = c(col_new_cases, 0)
          }
        }
        i = i+1
      }
      Y$data <- cbind(Y$data, new_cases = col_new_cases)
    } else{
      n_lines = nrow(Y$data)
      col_new_deaths <- c()
      i <- as.integer(1)
      while (i <= n_lines){
        if (i == 1){
          i_new_deaths = Y$data[i, "deaths" ]
          col_new_deaths = c(col_new_deaths, i_new_deaths)
        } else{
          i_new_deaths = diff(Y$data[c(i-1, i), "deaths" ])
          if (i_new_deaths >= 0){
            col_new_deaths = c(col_new_deaths, i_new_deaths)
          } else{
            col_new_deaths = c(col_new_deaths, 0)
          }
        }
        i = i+1
      }
      Y$data = cbind(Y$data, new_deaths = col_new_deaths)
    }
  }

  # Y$data with 'new_deaths' and without 'new_cases':
  if(!("new_cases" %in% names(Y$data)) && "new_deaths" %in% names(Y$data) && is.numeric(Y$data$new_deaths)){
    data_cases <- FALSE
    Y$data <- cbind(Y$data, new_cases = Y$data$new_deaths)
    #Y$data with 'new_cases' and without 'new_deaths':
  } else if(!("new_deaths" %in% names(Y$data)) && "new_cases" %in% names(Y$data) && is.numeric(Y$data$new_cases)){
    data_cases <- TRUE
    Y$data <- cbind(Y$data, new_deaths = Y$data$new_cases)
  }

  #Y$data without either 'cumulative cases':
  if("new_cases" %in% names(Y$data) && !("cases" %in% names(Y$data)) && is.numeric(Y$data$new_cases)) {
    Y$data <- cbind(Y$data, cases = cumsum(Y$data$new_cases))
  }
  if("new_deaths" %in% names(Y$data) && !("deaths" %in% names(Y$data)) && is.numeric(Y$data$new_deaths)) {
    Y$data <- cbind(Y$data, deaths = cumsum(Y$data$new_deaths))
  }

  if(!all(c("date", "cases", "deaths", "new_cases", "new_deaths") %in% names(Y$data))) stop("Y$data should be a data.frame with column names: 'date' and at least one of the 'new_cases', 'new_deaths', 'deaths' or 'cases'. See help(pandemic_model)")
  if(!is(Y$data$date, "Date")) stop("Y$data$date should be of class 'Date' and format 'YYYY-MM-dd' " )
  if(!all(is.numeric(Y$data$cases), is.numeric(Y$data$deaths), is.numeric(Y$data$new_cases), is.numeric(Y$data$new_deaths))) stop( "Y$data: values in 'cases', 'deaths', 'new_cases' and 'new_deaths' columns should be as.integer or as.numeric")

  #data processing: new_cases, new_deaths < 0:
  while(any(Y$data$new_cases < 0)){
    pos <- which(Y$data$new_cases < 0)
    for(j in pos){
      Y$data$new_cases[j - 1] = Y$data$new_cases[j] + Y$data$new_cases[j - 1]
      Y$data$new_cases[j] = 0
      Y$data$cases[j - 1] = Y$data$cases[j]
    }
  }

  while(any(Y$data$new_deaths < 0)){
    pos <- which(Y$data$new_deaths < 0)
    for(j in pos){
      Y$data$new_deaths[j - 1] = Y$data$new_deaths[j] + Y$data$new_deaths[j - 1]
      Y$data$new_deaths[j] = 0
      Y$data$deaths[j - 1] = Y$data$deaths[j]
    }
  }
  return(Y$data)
}

#### App configuration ####
CovidLP <- function(t, n_waves){
  list(
    poisson = list(mu_params = ifelse(n_waves == 1,
                                      list(a = 100, b1 = log(1), c = .5, f = 1.01),
                                      list(a = rep(1, n_waves), b1 = rep(log(1), n_waves), c = rep(0.5, n_waves),
                                           alpha = rep(0.01, n_waves), delta = seq(1, ceiling((n_waves - 1) * (t / n_waves)) +
                                                                                     1, by = floor(t / n_waves)))),
                   seasonal <- list(d_1 = rep(1, n_waves), d_2 = rep(1, n_waves),
                                    d_3 = rep(1, n_waves))
    )
  )
}

#### Auxiliary function for the prior parameters ####
prior_aux <- function(par){
  if(par == "a_alpha") return(0.1)
  if(par == "a_beta") return(0.1)
  if(par == "mu_delta") return(0)
  if(par == "sigma2_delta") return(100)
  if(par == "c_alpha") return(2)
  if(par == "c_beta") return(9)
  if(par == "alpha_alpha") return(0.01)
  if(par == "alpha_beta") return(0.01)
  if(par == "d_1_alpha") return(2)
  if(par == "d_1_beta") return(1)
  if(par == "d_2_alpha") return(2)
  if(par == "d_2_beta") return(1)
  if(par == "d_3_alpha") return(2)
  if(par == "d_3_beta") return(1)
  if(par == "mu_b_1") return(0)
  if(par == "sigma2_b_1") return(20)
  if(par == "phi_alpha") return(0.1)
  if(par == "phi_beta") return(0.1)
  if(par == "f_alpha") return( 0.01)
  if(par == "f_beta") return( 0.01)


}

#pandemic_environment$weekdays=c("sunday", "monday", "tuesday", "wednesday", "thursday",
#                                "friday", "saturday")


#### Auxiliary function "seasonal_code( dates,s_e): coding the days of the week with seasonal effect ####
# dates = observed dates vector; s_e = seasonal_effects =  string vector with full weekdays' name
seasonal_code <- function(dates, s_e){

  if(is.null(s_e)) code <- NULL else {       #model doesn't have seasonal effect

    week=c("sunday", "monday", "tuesday", "wednesday", "thursday",
           "friday", "saturday")[as.POSIXlt(as.Date(dates[1:7]))$wday + 1]

    code=c()
    for(i in 1:length(s_e)){
      code[i] <- which(week == s_e[i])
    }
  }
  return(code)
}


#### Auxiliary function 'config_stan' ####
config_stan <- function(Y,s_code,family,n_waves,p,case_type,phiTrunc,fTrunc,warmup,thin,sample_size,chains,init,prior_parameters,covidLPconfig){

  number_iterations <- warmup + thin * sample_size

  if (case_type == "confirmed") cases <- "new_cases"
  if (case_type == "deaths") cases <- "new_deaths"    #nature of the occurrence

  i <- which(colnames(Y$data) == cases) # i-column data Y for  data stan
  t <- dim(Y$data)[1]                   # n = t  ; n= nrows(Y$data) for data stan

  data_stan <- list(y = Y$data[[i]], n = t, pop = Y$population, p = p) #for all models

  if (covidLPconfig && family == "poisson"){ # negbin models don't have a recommended setting yet

    warmup <- 5e3; thin=3; sample_size<- 1e3; chains <- 1

    if (n_waves >= 2) {warmup=8e3}  # multiwaves models
    if (case_type == "confirmed") p <- 0.08
    if (case_type == "deaths") p <- 0.08 * 0.25

    fun <- CovidLP(t, n_waves)[[family]]
    data_stan <- c(data_stan, fun[["mu_params"]], fun[["seasonal"]])
  }

  #### guido shorter code - begin
  model_name <- "pandemicModels_"
  params <- c("a", "b", "c")
  if (n_waves == 1){
    model_name <- paste0(model_name, "singleWave_")
    params <- c(params, "f")
  } else {
    model_name <- paste0(model_name, "multiWave_")
    params <- c(params, "alpha", "delta")
    data_stan$nCurves <- n_waves
  }
  if (family == "negbin"){
    data_stan$fTrunc <- fTrunc # Ignored in multi wave
    data_stan$phiTrunc <- phiTrunc
    params <- c(params, "phi")
  }
  if (!is.null(s_code)){
    params <- c(params, paste0("d_",1:length(s_code)))
    if (family == "poisson" && n_waves == 1) model_name <- "pandemicModels_SeasonalsingleWave_" # Special case
  }
  s_code = c(s_code, 0, 0, 0)
  data_stan$w1 <- rep(s_code[1], n_waves)
  data_stan$w2 <- rep(s_code[2], n_waves)
  data_stan$w3 <- rep(s_code[3], n_waves)
  model_name <- paste0(model_name, family)
  params <- c(params, "mu")
  #### guido shorter code - end

  ## Prior Parameters

  pars <- c("a_alpha","a_beta","mu_delta","sigma2_delta","c_alpha","c_beta","alpha_alpha","alpha_beta",
            "d_1_alpha","d_1_beta","d_2_alpha","d_2_beta","d_3_alpha","d_3_beta","mu_b_1","sigma2_b_1",
            "phi_alpha","phi_beta","f_alpha","f_beta")
  for (par in pars) {
    if(is.null(prior_parameters[[par]]))  data_stan[[par]] <- prior_aux(par)
    else data_stan[[par]] <- prior_parameters[[par]]
  }

  stopifnot(data_stan$a_alpha > 0, data_stan$a_beta > 0, data_stan$sigma2_delta > 0, data_stan$c_alpha > 0,
            data_stan$c_beta > 0, data_stan$alpha_alpha > 0, data_stan$alpha_beta  > 0, data_stan$d_1_alpha >0,
            data_stan$d_1_beta > 0, data_stan$d_2_alpha > 0, data_stan$d_2_beta > 0, data_stan$d_3_alpha >0,
            data_stan$d_3_beta > 0, data_stan$sigma2_b_1 > 0, data_stan$phi_alpha >0, data_stan$phi_beta >0,
            data_stan$f_alpha > 0, data_stan$f_beta >0  )
  # if(data_stan$a_alpha < 0){
  #   print("Parameter a_alpha out of limits ")
  # }
  #
  #   if(data_stan$a_beta < 0){
  #     print("Parameter a_beta out of limits ")
  #   }
  #
  #   if(data_stan$sigma2_delta < 0){
  #     print("Parameter sigma2_delta out of limits ")
  #   }
  #
  #   if(data_stan$c_alpha < 0){
  #     print("Parameter c_alpha out of limits ")
  #   }
  #
  #   if(data_stan$c_beta < 0){
  #     print("Parameter c_beta out of limits ")
  #   }
  #
  #   if(data_stan$alpha_alpha < 0){
  #     print("Parameter alpha_alpha out of limits ")
  #   }
  #
  #   if(data_stan$alpha_beta < 0){
  #     print("Parameter alpha_beta out of limits ")
  #   }
  #
  #   if(data_stan$d_1_alpha < 0){
  #     print("Parameter d_1_alpha out of limits ")
  #   }
  #
  #   if(data_stan$d_1_beta < 0){
  #     print("Parameter d_1_beta out of limits ")
  #   }
  #
  #   if(data_stan$d_2_alpha < 0){
  #     print("Parameter d_2_alpha out of limits ")
  #   }
  #
  #   if(data_stan$d_2_beta < 0){
  #     print("Parameter d_2_beta out of limits ")
  #   }
  #
  #   if(data_stan$d_3_alpha < 0){
  #     print("Parameter d_3_alpha out of limits ")
  #   }
  #
  #   if(data_stan$d_3_beta < 0){
  #     print("Parameter d_3_beta out of limits ")
  #   }
  #
  #   if(data_stan$sigma2_b_1 < 0){
  #     print("Parameter sigma2_b_1 out of limits ")
  #   }
  #
  #   if(data_stan$phi_alpha < 0){
  #     print("Parameter phi_alpha out of limits ")
  #   }
  #
  #   if(data_stan$phi_beta < 0){
  #     print("Parameter phi_beta out of limits ")
  #   }
  #
  #   if(data_stan$f_alpha < 0){
  #     print("Parameter f_alpha out of limits ")
  #   }
  #
  #   if(data_stan$f_beta < 0){
  #     print("Parameter f_beta out of limits ")
  #   }

  # #####poisson models without seasonal effect:
  #
  #   if (is.null(s_code) && family == "poisson"){
  #
  #     if(n_waves==1){
  #       # model generalized logistic doesn't have seasonal_effect
  #
  #       model_name="pandemicModels_singleWave_poisson"
  #
  #       params = c("a","b","c","f", "mu")
  #
  #       if(covidLPconfig){
  #         init <- list(
  #           list(a = 100, b1 = log(1), c = .5, f = 1.01)
  #         )
  #
  #       }
  #
  #
  #     } else {
  #       # multiwaves model without seasonal effect
  #
  #       model_name="pandemicModels_multiWave_poisson"     #stanmodel name
  #
  #       params = c("a","b","c","alpha","delta","mu")
  #
  #       data_stan$nCurves=n_waves
  #       data_stan$w1=rep(0,n_waves)
  #       data_stan$w2=rep(0,n_waves)
  #       data_stan$w3=rep(0,n_waves)
  #
  #         if(covidLPconfig){
  #           #delta=seq(1,ceiling((n_waves-1)*(t/n_waves))+1,by=floor(t/n_waves))
  #           #if(length(delta)>n_waves) delta=delta[1:n_waves]                      #problem only when small t (<50) and great n_waves (n_waves> 4)
  #
  #           init <- list(
  #           list(a = rep(1,n_waves), b1 = rep(log(1),n_waves), c = rep(0.5,n_waves),
  #                alpha=rep(0.01,n_waves), delta=seq(1,ceiling((n_waves-1)*(t/n_waves))+1,by=floor(t/n_waves))) #delta=seq(0,(n_waves-1)*50,by=50)
  #         )
  #       }
  #
  #     }
  #
  #   }
  #
  # #####poisson models with seasonal effect
  #
  # if (!is.null(s_code) && family=="poisson") { #poisson models with seasonal effect
  #
  #   if(n_waves==1){        # model generalized logistic with seasonal_effect
  #
  #    model_name="pandemicModels_SeasonalsingleWave_poisson"
  #   params = c("a","b","c","f",paste0("d_",1:length(s_code)),"mu")
  #   s_code = c(s_code,0,0)
  #
  #   data_stan$w1=s_code[1]
  #   data_stan$w2=s_code[2]
  #   data_stan$w3=s_code[3]
  #
  #   if(covidLPconfig){
  #     init <- list(
  #       list(a = 100, b1 = log(1), c = .5, f = 1.01, d_1=1,d_2=1,d_3=1)
  #     )
  #   }
  #
  #   } else {   #multiwave with seasonal effect
  #
  #     model_name="pandemicModels_multiWave_poisson"     #stanmodel name
  #
  #     params = c("a","b","c","alpha","delta",paste0("d_",1:length(s_code)), "mu")
  #     s_code = c(s_code,0,0)
  #
  #     data_stan$nCurves=n_waves
  #     data_stan$w1=rep(s_code[1],n_waves)
  #     data_stan$w2=rep(s_code[2],n_waves)
  #     data_stan$w3=rep(s_code[3],n_waves)
  #
  #     if(covidLPconfig){
  #       #delta=seq(1,ceiling((n_waves-1)*(t/n_waves))+1,by=floor(t/n_waves))
  #       #if(length(delta)>n_waves) delta=delta[1:n_waves]                      #problem only when small t (<50) and great n_waves (n_waves> 4)
  #
  #      init <- list(
  #         list(a = rep(1,n_waves), b1 = rep(log(1),n_waves), c = rep(0.5,n_waves),
  #              alpha=rep(0.01,n_waves), delta=seq(1,ceiling((n_waves-1)*(t/n_waves))+1,by=floor(t/n_waves)),
  #              d_1=rep(1,n_waves), d_2=rep(1,n_waves), d_3=rep(1,n_waves))
  #       )
  #
  #     }
  #
  #   }
  #
  # }
  #
  #
  # ##### negbin models without seasonal effect:
  #
  # if(is.null(s_code) && family=="negbin"){
  #
  #   if(n_waves==1){
  #     # negbin 1 wave without seasonal_effect
  #
  #     model_name="pandemicModels_singleWave_negbin"
  #
  #     params = c("a","b","c","f","phi","mu")
  #
  #     data_stan$w1=0
  #     data_stan$w2=0
  #     data_stan$w3=0
  #     data_stan$fTrunc=fTrunc
  #     data_stan$phiTrunc=phiTrunc
  #
  # #    if(covidLPconfig){  #negbin não terá covidLPconfig=TRUE
  # #      init <- list(
  # #        list(a = 100, b1 = log(1), c = .5, f=f)
  # #      )
  # #   }
  #
  #
  #   } else {
  #     #negbin multiwaves model without seasonal effect
  #
  #     model_name="pandemicModels_multiWave_negbin"     #stanmodel name
  #
  #     params = c("a","b","c","alpha","delta","phi","mu")
  #
  #     data_stan$nCurves=n_waves
  #     data_stan$w1=rep(0,n_waves)
  #     data_stan$w2=rep(0,n_waves)
  #     data_stan$w3=rep(0,n_waves)
  #     data_stan$phiTrunc=phiTrunc
  #
  # #    if(covidLPconfig){  #negbin models don't have covidLPconfig
  # #      init <- list(
  # #        list(a = rep(1,n_waves), b1 = rep(log(1),n_waves), c = rep(0.5,n_waves),
  # #             alpha=rep(1,n_waves), delta=seq(0,(n_waves-1)*50,by=50))
  # #      )
  # #    }
  #
  #   }
  #
  # }
  #
  #
  # ##### negbin models with seasonal effect
  #
  # if (!is.null(s_code) && family=="negbin") {
  #
  #   if(n_waves==1){        # negbin one wave model  with seasonal_effect
  #
  #     model_name="pandemicModels_singleWave_negbin"
  #     params = c("a","b","c","f","phi",paste0("d_",1:length(s_code)),"mu")
  #     s_code = c(s_code,0,0)
  #
  #     data_stan$w1=s_code[1]
  #     data_stan$w2=s_code[2]
  #     data_stan$w3=s_code[3]
  #     data_stan$fTrunc=fTrunc
  #     data_stan$phiTrunc=phiTrunc
  #
  # #  if(covidLPconfig){
  # #      init <- list(
  # #        list(a = 100, b1 = log(1), c = .5, f = 1.01, d_1=1,d_2=1,d_3=1)
  # #      )
  # #    }
  #
  #   } else {   #multiwave with seasonal effect
  #
  #     model_name="pandemicModels_multiWave_negbin"     #stanmodel name
  #
  #     params = c("a","b","c","alpha","delta", "phi", paste0("d_",1:length(s_code)), "mu")
  #     s_code = c(s_code,0,0)
  #
  #     data_stan$nCurves=n_waves
  #     data_stan$w1=rep(s_code[1],n_waves)
  #     data_stan$w2=rep(s_code[2],n_waves)
  #     data_stan$w3=rep(s_code[3],n_waves)
  #     data_stan$phiTrunc=phiTrunc
  #
  # #    if(covidLPconfig){
  # #      init <- list(
  # #        list(a = rep(1,n_waves), b1 = rep(log(1),n_waves), c = rep(0.5,n_waves),
  # #             alpha=rep(1,n_waves), delta=seq(0,(n_waves-1)*50,by=50),
  # #             d_1=rep(1,n_waves), d_2=rep(1,n_waves), d_3=rep(1,n_waves))
  # #      )
  # #    }
  #
  #   }
  #
  # }

  #### Test: Using the result from another run for initial values
  if (is(init, "pandemicEstimated")){
    new_init <- list()
    pars <- names(init$fit)
    all_iterations <- as.array(init$fit)
    #last <- dim(all_iterations)[1]

    for (c in 1:chains){
      # Set up
      temp_init <- list()
      last_iter <- colMeans(all_iterations[, (c - 1) %% init$fit@sim$chains + 1, ]) ## Make sure to cycle correctly through old chains

      if (n_waves == 1) # Waves parameters - begin
        if (init$n_waves == 1){
          temp_init$a <- last_iter[pars == "a"]
          temp_init$b <- last_iter[pars == "b"]
          temp_init$c <- last_iter[pars == "c"]
          temp_init$f <- last_iter[pars == "f"]
        } else {
          temp_init$a <- min(last_iter[pars == "a[1]"],
                             p * Y$population * last_iter[pars == "b[1]"] ^ 1.01 * 0.999) # Avoiding errors
          temp_init$b <- last_iter[pars == "b[1]"]
          temp_init$c <- last_iter[pars == "c[1]"]
          temp_init$f <- 1.01
        }
      else
        if (init$n_waves == 1){
          temp_init$a <- array(rep(last_iter[pars == "a"], n_waves))
          temp_init$b <- rep(last_iter[pars == "b"], n_waves)
          temp_init$c <- array(rep(last_iter[pars == "c"], n_waves))
          temp_init$alpha <- rep(0.01,n_waves)
          temp_init$delta <- seq(1, ceiling((n_waves - 1) * (t / n_waves)) + 1,
                                 by = floor(t / n_waves))
        } else {
          extra <- max(0, n_waves - init$n_waves)
          temp_init$a <- array(c(last_iter[match(paste0("a[", 1:min(n_waves,
                                                                    init$n_waves),"]"), pars)],
                                 rep(0.01, extra)))
          temp_init$b <- c(last_iter[match(paste0("b[", 1:min(n_waves,
                                                              init$n_waves),"]"), pars)],
                           rep(1, extra))
          temp_init$c <- array(c(last_iter[match(paste0("c[", 1:min(n_waves,
                                                                    init$n_waves),"]"), pars)],
                                 rep(0.5, extra)))
          temp_init$alpha <- c(last_iter[match(paste0("alpha[", 1:min(n_waves,
                                                                      init$n_waves),"]"), pars)],
                               rep(0.01, extra))
          temp_init$delta <- c(last_iter[match(paste0("delta[", 1:min(n_waves,
                                                                      init$n_waves),"]"), pars)],
                               rep(0, extra))
        } # Waves parameters - end
      new_init[[c]] <- temp_init
    }
    init <- new_init
  }


  out=list(data_stan=data_stan,params=params,init=init,p=p,phiTrunc=phiTrunc,fTrunc=fTrunc,
           warmup=warmup,thin=thin, model_name=model_name,
           sample_size=sample_size,chains=chains,number_iterations=number_iterations)

  return(out)
}

#### Auxiliary functon including_auxparameters(init): only for models with auxiliar parameters ####
# provides the replacement of the initial value of the parameter, given by the user, with the respective initial
#value of the auxiliary parameter.
including_auxparameters=function(init){    #if init="random" :  this auxiliar function is not necessary

  if(is.list(init)) {

    for(j in 1:length(init)){     #including b1_1, excluding b1 (multiwaves): user view

      k = grep("b", init[[j]])

      if(length(k)){
        init[[j]]$b1= log(init[[j]]$b)
        init[[j]]=init[[j]][-k]
      }

    }


  } else init = init  #init = 'random'


  return(init)
}


#### Auxiliary functon excluding_auxparameters(init): only for models with auxiliar parameters ####
# provides the replacement of the initial value of the auxiliar parameter, used by sampler STAN, with the respective
#initial value of the parameter of interest.
excluding_auxparameters=function(init){   #if init="random" :  this auxiliar function is not necessary

  if(is.list(init)){

    for(j in 1:length(init)){
      kk=grep("b1", init[[j]])    #if user input initial value for b1 or covidLPconfig=TRUE

      if(length(kk)){
        init[[j]]$b=exp(init[[j]]$b1)
        init[[j]]=init[[j]][-kk]                  #including b1, excluding b1_1 (multiwaves) :  user view

      }
    }


  } else {init=init}   #init = 'random'

  names(init)=paste0("chain_id:", 1:length(init))

  return(init)

}

#### Auxiliary function: Excludes parameters from a stanfit object
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


#### Auxiliary funtion: fitmodel(...) ####
# provides adjustment of the model according to the configurations of the STAN sampler
#requested by the user
#' @importFrom rstan sampling
#'
#' @importClassesFrom rstan stanfit

fitmodel <- function(Y,data_cases=data_cases,family, case_type,seasonal_effect,n_waves,p,
                     phiTrunc, fTrunc,
                     chains, warmup, thin, sample_size, init,prior_parameters,...,covidLPconfig){

  ########### stan configuration

  s_code <- seasonal_code(Y$data$date, seasonal_effect)    #codifing seasonal_effect

  config <- config_stan(Y,s_code,family,n_waves,p,case_type,phiTrunc,fTrunc,warmup,thin,sample_size,     #confit stan
                        chains,init,prior_parameters,covidLPconfig)

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


  if(!is(mod_sim, "try-error")){

    ########### preparing output:

    if(is.null(seasonal_effect)){

      if(n_waves==1) {                    #gen logistic

        priors=list(prior.a= paste0("Gamma(",prior_parameters$a_alpha,",",prior_parameters$a_beta,")") ,prior.b= paste0("LogNormal(",prior_parameters$mu_b_1,",",prior_parameters$sigma2_b_1,")")  ,
                    prior.c= paste0("Gamma(",prior_parameters$c_alpha,",",prior_parameters$c_beta,")") ,paste0("Gamma(",prior_parameters$f_alpha,",",prior_parameters$f_beta,")"))

        if(family=="poisson"){
          priors$restrictions=list(paste0("a/b^f<"," ",p,"*population"),"f>1")
          name="poisson: static generalized logistic"
        } else {
          priors$phi= paste0("Gamma(",prior_parameters$phi_alpha,",",prior_parameters$phi_beta,")")
          priors$restrictions=list(paste0("a/b^f<"," ",p,"*population"),paste0("f>",fTrunc),
                                   paste0("phi>",phiTrunc))
          name="negbin: static generalized logistic"
        }


      } else {   #multiwaves model without seasonal effect:

        priors=list(prior.a_i= paste0("Gamma(",prior_parameters$a_alpha,",",prior_parameters$a_beta,")") ,paste0("LogNormal(",prior_parameters$mu_b_1,",",prior_parameters$sigma2_b_1,")") ,
                    prior.c_i= paste0("Gamma(",prior_parameters$c_alpha,",",prior_parameters$c_beta,")") ,paste0("Gamma(",prior_parameters$alpha_alpha,",",prior_parameters$alpha_beta,")"),
                    prior.delta_i=paste0("Normal(",prior_parameters$mu_delta,",",prior_parameters$sigma2_delta,")") )

        if(family=="negbin"){
          priors[["phi"]]= paste0("Gamma(",prior_parameters$phi_alpha,",",prior_parameters$phi_beta,")")
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
        priors=list(prior.a= paste0("Gamma(",prior_parameters$a_alpha,",",prior_parameters$a_beta,")") ,prior.b= paste0("LogNormal(",prior_parameters$mu_b_1,",",prior_parameters$sigma2_b_1,")") ,
                    prior.c= paste0("Gamma(",prior_parameters$c_alpha,",",prior_parameters$c_beta,")") ,prior.f= paste0("Gamma(",prior_parameters$f_alpha,",",prior_parameters$f_beta,")"),prior.d_i=paste0("Gamma(",prior_parameters$d_1_alpha,",",prior_parameters$d_1_beta,")"))

        if(family=="poisson"){
          priors$restrictions=list(paste0("a/b^f<"," ",p,"*population"),"f>1")
          name="poisson: static seasonal generalized logistic"
        } else {
          priors$phi= paste0("Gamma(",prior_parameters$phi_alpha,",",prior_parameters$phi_beta,")")
          priors$restrictions=list(paste0("a/b^f<"," ",p,"*population"),paste0("f>",fTrunc),
                                   paste0("phi>",phiTrunc))
          name="negbin: static generalized logistic"
        }


      } else { #n_waves >=2 with seasonal effect:

        priors=list(prior.a_i= paste0("Gamma(",prior_parameters$a_alpha,",",prior_parameters$a_beta,")") ,prior.b_i= paste0("LogNormal(",prior_parameters$mu_b_1,",",prior_parameters$sigma2_b_1,")")  ,
                    prior.c_i= paste0("Gamma(",prior_parameters$c_alpha,",",prior_parameters$c_beta,")") ,prior.alpha_i= paste0("Gamma(",prior_parameters$alpha_alpha,",",prior_parameters$alpha_beta,")"),
                    prior.delta_i=paste0("Normal(",prior_parameters$mu_delta,",",prior_parameters$sigma2_delta,")"), prior.d_i= paste0("Gamma(",prior_parameters$d_1_alpha,",",prior_parameters$d_1_beta,")") )

        if(family=="negbin"){
          priors[["phi"]]= paste0("Gamma(",prior_parameters$phi_alpha,",",prior_parameters$phi_beta,")")
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
    names(fitted_median) <- paste0("error_", 1:length(fitted_median))
    output <- list(model_name=name,family=family,n_waves=n_waves,
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

#### Capitalize country names: This function capitalizes country_name inputs in order to match the capitalization in the database. ####
#' Capitalize country names
#'
#' This function capitalizes country_name inputs in order to match the capitalization in the database.
#'
#' @param x A character string
#' @return Capitalized character string x to match the spelling and capitalization in the database.
#' @noRd

capitalize <- function(x) {
  s <- gsub("\\b(\\w)", "\\U\\1", x, perl = TRUE)
  s<- gsub("\\bAnd\\b", "and", s)
  s<- gsub("\\bOf\\b", "of", s)
  if(length(s)== 1 && s =="Cote D'Ivoire") {s <-"Cote d'Ivoire"}
  if(length(s)== 1 && s =="Eswatini") {s <-"eSwatini"}
  s
}

#### Auxiliary function to build the correct parameters string vector ####
build_params <- function(object, waves){
  if (grepl("multi", object$model_name)){
    out <- c(paste0("a[",waves,"]"), paste0("b[",waves,"]"), paste0("c[",waves,"]"),
             paste0("alpha[",waves,"]"), paste0("delta[",waves,"]"))
    if (!is.null(object$seasonal_effect))
      out <- c(out, paste0(paste0("d_", 1:length(object$seasonal_effect)), "[", waves, "]"))
  }
  else{
    out <- c("a", "b", "c", "f")
    if (!is.null(object$seasonal_effect))
      out <- c(out, paste0("d_", 1:length(object$seasonal_effect)))
  }
  if (grepl("negbin", object$model_name))
    out <- c(out, "phi")

  out
}

#### Auxiliary function for posterior_predict.pandemicEstimated. Provides the actual predictions ####
# c for chains, h for horizon, n for NA value, m for model, ft for final time and s for seasonal effects days
#' @importFrom stats rpois rgamma pnorm
generatePredictedPoints_pandemic = function(M,c,h,n,m,ft,s){

  .Deprecated("generatePredictedPoints_pandemicC")
  y = mu = matrix(-Inf,ncol = h,nrow = M)
  if (m == "poisson: static generalized logistic")
    for (i in 1:h){
      mu[,i] = exp(log(c$f)+log(c$a)+log(c$c)-(c$c*(ft+i))-(c$f+1)*log(c$b+exp(-c$c*(ft+i)) ))
      y[,i] = stats::rpois(M,mu[,i])
    } else if (m == "negbin: static generalized logistic")
      for (i in 1:h){
        mu[,i] = exp(log(c$f)+log(c$a)+log(c$c)-(c$c*(ft+i))-(c$f+1)*log(c$b+exp(-c$c*(ft+i)) ))
        y[,i] = stats::rpois(M,stats::rgamma(M,mu[,i]*c$phi,c$phi))
      } else if (m == "poisson: static seasonal generalized logistic"){
        d_1 = c$d_1; d_2 = ifelse(is.null(c$d_2),1,c$d_2); d_3 = ifelse(is.null(c$d_3),1,c$d_3)
        for (i in 1:h){
          mu[,i] = exp(log(c$f)+log(c$a)+log(c$c)-(c$c*(ft+i))-(c$f+1)*log(c$b+exp(-c$c*(ft+i)) ))*
            d_1^((s[1]>0) * !((ft+i - s[1]) %% 7)) * d_2^((s[2]>0) * !((ft+i - s[2]) %% 7)) * d_3^((s[3]>0) * !((ft+i - s[3]) %% 7))
          y[,i] = stats::rpois(M,mu[,i])
        }} else if (m == "poisson: multi_waves(2)")
          for (i in 1:h){
            mu[,i] = exp(log(c$a1)+log(c$c1)-(c$c1*(ft+i))-2*log(c$b1+exp(-c$c1*(ft+i)) ) + stats::pnorm(c$alpha1*(ft+i-c$delta1),log.p = TRUE))+
              exp(log(c$a2)+log(c$c2)-(c$c2*(ft+i))-2*log(c$b2+exp(-c$c2*(ft+i)) ) + stats::pnorm(c$alpha2*(ft+i-c$delta2),log.p = TRUE))
            y[,i] = stats::rpois(M,mu[,i])
          } else stop(paste("Unknown model",m))

  if (any(is.na(y))){
    message(paste("Prediction had",sum(is.na(y)),"NA values. Replaced with large value for identification."))
    y.fut[is.na(y.fut)] = n
  }

  list(yL=y, yS = t(apply(y,1,cumsum)), mu = mu)
}
