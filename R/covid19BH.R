#' Covid-19 data for Belo Horizonte/MG - Brazil
#'
#' @description The purpose of this page is to show the user how to format the Y input argument
#' in \code{\link{pandemic_model}} function, when epidemiological data was obtained
#' outside of the \code{\link{load_covid}} function.\cr
#' \cr
#' The Covid-19 data for the city of Belo Horizonte, MG - Brazil will be used to illustrate
#' how to correctly format the epidemiological data required in the \code{pandemic_model} function.
#' See the \strong{Examples} section. \cr
#' \cr
#' For complete information on the required data format,
#' check the \code{Y} input argument description in \cr
#'\code{?pandemic_model}.
#'
#' @docType data
#' @name covid19BH
#'
#' @usage data(covid19BH)
#'
#' @format This data frame has 103 observations and 6 variables.
#' It contains the number of Covid-19 confirmed cases and deaths for the city of Belo Horizonte,
#' from the date of the first notified case in 2020-03-16 to 2020-06-26.\cr
#' \cr
#' \describe{
#'   \item{[,1] date}{dates in the YYYY-MM-DD format}
#'   \item{[,2] new_confirmed}{number of new cases}
#'   \item{[,3] new_deaths}{number of new deaths}
#'   \item{[,4] last_available_confirmed}{cumulative number of cases}
#'   \item{[,5] last_available_deaths}{cumulative number of deaths}
#'   \item{[,6] estimated_population_2019}{size of Belo Horizonte's population}
#'  }
#'
#'
#' @keywords datasets
#'
#' @seealso \code{\link{load_covid}}, \code{\link{pandemic_model}}, \code{\link{posterior_predict.pandemicEstimated}},
#' \code{\link{pandemic_stats}} and  \code{\link{plot.pandemicPredicted}}.
#'
#' @source \url{https://brasil.io/dataset/covid19}
#'
#' @references
#' CovidLP Team, 2020. CovidLP: Short and Long-term Prediction for COVID-19. Departamento de Estatistica. UFMG,
#' Brazil. URL: \url{http://est.ufmg.br/covidlp/home/en/}
#'
#'
#' @examples
#' ## formating the data frame for pandemic_model function
#' covid19BH
#' names(covid19BH)
#'
#' data = covid19BH
#' names(data) = c("date", "new_cases", "new_deaths", "cases", "deaths", "population")
#'
#' class(data$date)
#' data$date = as.Date(data$date)
#' class(data$date)
#'
#' data      #note:  data frame is ordered by date in descending order!
#' data = data[order(data$date, decreasing=FALSE), ]
#' data      #data frame ordered by dates in ascending order
#'
#' ## building the Y list required
#' pop = data$population[1]
#' Y = list(data = data, name = "Belo Horizonte/MG", population = pop)
#'
#' ## fitted model:
#' ##pandemic_model function may take a few minutes...
#' \dontrun{
#' outputBH = pandemic_model(Y, control = list(max_treedepth = 50, adapt_delta = 0.999))
#' print(outputBH)
#'
#' ##convergence diagnostics
#' traceplot(outputBH)
#' stan_ac(outputBH$fit, pars = c("a","b","c","f"))
#' stan_dens(outputBH$fit, pars = c("a","b","c","f"))
#'
#' ## making predictions
#' predictions = posterior_predict(outputBH)
#'
#' ## calculating prediction intervals and statistics
#' stats = pandemic_stats(predictions)
#'
#' ## plotting results
#' plot(predictions)}
NULL
