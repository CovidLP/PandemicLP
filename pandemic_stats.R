#' Relevant Statistics of the Pandemic Model
#'
#' This function provides short and long-term predictions for the pandemic. 95% credible intervals are
#' assigned for every future date predicted, as well as for the total number of cases, and dates for the peak
#' and end of the pandemic.\cr
#' Short-term predictions are made on the cumulative counts and are dependent on the previous assignment of the
#' \code{horizonShort} argument in \code{posterior_predict.pandemicEstimated} function. Long-term predictions
#' are based on the new cases and the argument \code{horizonLong} in \code{posterior_predict.pandemicEstimated}
#' function.
#'
#' @param object an object of S3 class \code{pandemicPredicted} created by function \code{posterior_predict}
#'
#' @return An object of class \code{pandemicStats}. This object is a list containing the following elements:
#' \itemize{
#'    \item{\code{data}}{
#'    A list with a data frame containing the observed pandemic data, a character string with the location name
#'    and a character string indicating the type of cases predicted.
#'    }
#'    \item{\code{ST_predict}}{
#'    A data frame with the Short-term predictions for the number of cumulative cases. For each future date predicted,
#'    the mean, median, 2.5 and 97.5 percentiles are provided.
#'    }
#'    \item{\code{LT_predict}}{
#'    A data frame with the Long-term predictions for the number of new cases. For each future date predicted,
#'    the mean, median, 2.5 and 97.5 percentiles are provided.
#'    }
#'    \item{\code{LT_summary}}{
#'    A list with the estimated total number of cases and the dates for the peak and end of the pandemic.
#'    In each metric, the median, 2.5 and 97.5 percentiles are provided. For more information, see the
#'    \strong{Details} section.\cr
#'    \emph{Warning: These calculations are restricted to the argument \code{horizonLong} in the
#'    \code{posterior_predict.pandemicEstimated} function. It might be necessary to increase the horizon to in
#'    order to properly calculate the end of the pandemic and total number of cases.}
#'    }
#'    \item{\code{mu}}{
#'    A data frame with the median values of the mean number of new cases for each date (starting from the first
#'    observed data point until the last date in the Long-term horizon).
#'    }
#' }
#'
#' @details
#'  \subsection{Total Number of Cases}{
#'  The total number of cases is obtained by adding the predicted new cases to the cumulative total
#'  already observed in the data.
#'  }
#'  \subsection{Estimated Peak Dates}{
#'  The 95% credible interval for the peak of cases is selected such that the two limiting dates of the 97.5
#'  percentile curve coincides with the highest value of the 2.5 percentile curve. This guarantees that all
#'  possible curves belonging to the confidence band will peak within the defined interval.
#'  }
#'  \subsection{End of the Pandemic Dates}{
#'  Represents the 99 percentile of the total number of cases in the pandemic.
#'  }
#'
#' @references
#' CovidLPTeam, 2020. CovidLP: Short and Long-term Prediction for COVID-19. Departamento de Estatistica. UFMG,
#' Brazil. URL: \url{http://est.ufmg.br/covidlp/home/en/}
#'
#' @examples
#' \dontrun{
#' italy = load_covid("italy")
#' estim = pandemic_model(italy, case_type = "confirmed", covidLPconfig = TRUE)
#' pred = posterior_predict(estim)
#' stats = pandemic_stats(pred)
#' stats
#' }
#'
#' @seealso
#' \link{\code{load_covid}} \link{\code{pandemic_model}} \link{\code{posterior_predict.pandemicEstimated}}
#' \link{\code{plot.pandemicPredicted}} \link{\code{print.pandemicStats}}
#'
#' @export

pandemic_stats <- function(object){

  if (class(object) != "pandemicPredicted") stop("Please use the output of the posterior_predict() function.")
  if( missing(object)) stop("object is a required argument. See help(pandemic_stats) for more information.")

  t = length(object$data[[1]])
  ST_horizon = ncol(object$predictive_Short)
  LT_horizon = ncol(object$predictive_Long)
  date_full <- as.Date( object$data$date[1]:(max(object$data$date) + LT_horizon), origin = "1970-01-01")

  ### list output ST predicition:
  ST_predict <- data.frame( date  = date_full[(t+1):(t+ST_horizon)],
                            q2.5  = apply(object$predictive_Short,2,quantile,.025),
                            med   = apply(object$predictive_Short,2,quantile,.5),
                            q97.5 = apply(object$predictive_Short,2,quantile,.975),
                            mean  = colMeans(object$predictive_Short))
  row.names(ST_predict) <- NULL

  ### total number of cases
  if(object$cases_type == "confirmed"){
    cumulative_y =  object$data$cases[t]    #casos acumulados até o tempo t.
  } else{
    cumulative_y =  object$data$deaths[t]
  }

  if(cumulative_y > 1000){
    lowquant <- apply(object$predictive_Long,2,quantile,.025)
    medquant <- apply(object$predictive_Long,2,quantile,.5)
    highquant <- apply(object$predictive_Long,2,quantile,.975) #faz o quantil dos numeros novos
  } else{
    lowquant <- c(cumulative_y , apply(object$predictive_Short,2,quantile,.025)) #raz o quantil dos numeros acumulados
    lowquant <- (lowquant - lag(lowquant, default = 0))[-1] #depois calcula os numero de novos casos a partir dos acumulados
    medquant <- c(cumulative_y, apply(object$predictive_Short,2,quantile,.5))
    medquant <- (medquant - lag(medquant,default = 0))[-1]
    highquant <- c(cumulative_y, apply(object$predictive_Short,2,quantile,.975))
    highquant <- (highquant - lag(highquant, default = 0))[-1]
  }

  TNC2.5 = sum(lowquant) + cumulative_y
  TNC50  = sum(medquant) + cumulative_y
  TNC97.5 = sum(highquant) + cumulative_y

  ### Calculates the peak and end dates
  peak2.5 <- peak50 <- peak97.5 <- NULL
  end2.5 <- end50 <- end97.5 <- NULL

  chain_mu <-cbind(object$pastMu, object$futMu)

  ### median dates
  mu50 <- apply(chain_mu, 2, quantile, probs = 0.5)      #quantil 50% das medias mu
  peak50 <- date_full[which.max(mu50)]

  q <- .99
  med_cumulative <- apply(as.matrix(mu50),2,cumsum)
  med_percent<- med_cumulative / med_cumulative[ t + LT_horizon ] #calcula o quanto cada numero acumulado representa do numero total de casos
  med_end <- which(med_percent - q > 0)[1] #pega o primeiro que ultrapassa 99% dos casos
  end50 <- date_full[med_end]

  ### calculates the upper and lower bound dates:
  mu25 <- apply(chain_mu, 2, quantile, probs = 0.025)   #curva do quantil 25% das médias mu
  mu975 <- apply(chain_mu, 2, quantile, probs = .975)   # curva do quantil 97.5% das médias mu

  Maxq2.5 <- which.max(mu25)  #indica o indice do max mu do quantil 2.5
  aux <- mu975 - mu25[Maxq2.5]  #subtrai o max do LI do vetor do vetor de medias do LS
  aux2 <- aux[ Maxq2.5 : (t + LT_horizon)] #seleciona aux do indice max do LI de mu ate t+L0
  val <- ifelse( length(aux2[aux2 < 0]) > 0, min(aux2[aux2 > 0]), aux[length(aux)])  #seleciona o minimo desse vetor, desde que seja um numero positivo
  date_max <- which(aux == val)

  aux <- mu975 - mu25[Maxq2.5]
  aux2 <- aux[1:Maxq2.5]
  val <- min(aux2[aux2>0])
  date_min <- which(aux == val)

  peak2.5  <- date_full[date_min]   #limite inferior data pico
  peak97.5 <- date_full[date_max]  #limite superior data pico

  ###calcula IC  fim da pandemia:

  low_cumulative <- apply(as.matrix(mu25),2,cumsum)
  low_percent <- low_cumulative / low_cumulative[t + LT_horizon]
  low_end <- which(low_percent - q > 0)[1]
  end2.5 <- date_full[low_end]             #limite inferior data fim

  high_cumulative <- apply(as.matrix(mu975),2,cumsum)
  high_percent <- high_cumulative / high_cumulative[t + LT_horizon]
  high_end <- which( high_percent - q > 0)[1]
  end97.5 <- date_full[high_end]            #limite superior data fim

  #### LT predictions:
  LT_predict <- data.frame( date  = date_full[(t+1):(t+LT_horizon)],
                            q2.5  = lowquant,
                            med   = medquant,
                            q97.5 = highquant,
                            mean  = colMeans(object$predictive_Long))   ##precisa? [,1:L0]
  row.names(LT_predict) <- NULL

  ## Long-term summary
  LT_summary <- list(total_cases_LB = TNC2.5,
                     total_cases_med = TNC50,
                     total_cases_UB = TNC97.5,
                     peak_date_LB = peak2.5 ,
                     peak_date_med = peak50 ,
                     peak_date_UB = peak97.5,
                     end_date_LB = end2.5,
                     end_date_med = end50,
                     end_date_UB = end97.5)

  muplot <- data.frame(date = date_full, mu = mu50)
  row.names(muplot) <-NULL

  dataplot <- list(data = object$data, location = object$location, case_type = object$cases_type)

  output <- list( data = dataplot, ST_predict = ST_predict, LT_predict = LT_predict,
                  LT_summary = LT_summary, mu = muplot )

  class(output) = "pandemicStats"
  return(output)

}
