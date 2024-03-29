#' Covid-19 data for Belo Horizonte/MG - Brazil
#'
#' @description The purpose of this page is to show the user how to format the Y input argument
#' in the \code{\link{pandemic_model}} function, when epidemiological data was obtained
#' outside of the \code{\link{load_covid}} function.
#'
#'
#' The Covid-19 data for the city of Belo Horizonte, MG - Brazil will be used to illustrate
#' how to correctly format the epidemiological data required in the \code{pandemic_model} function,
#' using the function \code{\link{format_data}}.
#' See the \strong{Examples} section.
#'
#'
#' For complete information on the required data format,
#' check the \code{Y} input argument description in \code{?pandemic_model}.
#'
#' @docType data
#' @name covid19BH
#'
#' @usage covid19BH
#'
#' @format This data frame has 103 observations and 6 variables.
#' It contains the number of Covid-19 confirmed cases and deaths for the city of Belo Horizonte,
#' from the date of the first notified case in 2020-03-16 to 2020-06-26.
#'
#'
#' \enumerate{
#'   \item date - dates in the YYYY-MM-DD format
#'   \item new_confirmed - number of new cases
#'   \item new_deaths - number of new deaths
#'   \item last_available_confirmed - cumulative number of cases
#'   \item last_available_deaths - cumulative number of deaths
#'   \item estimated_population_2019 - size of Belo Horizonte's population
#'  }
#'
#'
#' @keywords datasets
#'
#' @seealso \code{\link{load_covid}}, \code{\link{format_data}}, \code{\link{pandemic_model}}, \code{\link{posterior_predict.pandemicEstimated}},
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
#'
#' #loading data
#' data <- covid19BH
#' data
#' names(data)
#'
#' #re-order data in ascending order
#' class(data$date)
#' data$date = as.Date(data$date)
#' class(data$date)
#' data <- data[order(data$date), ]
#' head(data)
#'
#' # building the Y list required
#' start <- data$date[1]
#' end <- data$date[nrow(data)]
#' cases <- data$last_available_confirmed
#' new_cases <- data$new_confirmed
#' deaths <- data$last_available_deaths
#' new_deaths <- data$new_deaths
#' pop <- data$estimated_population_2019[1]
#'
#' Y <- format_data(s_date = start, e_date = end,
#'                  cases = cases, n_cases = new_cases,
#'                  deaths = deaths, n_deaths = new_deaths,
#'                  name = "Belo Horizonte/MG", pop = pop)
#' Y
#' plot(Y)
#'
#' ## fitted model:
#' ##pandemic_model function may take a few minutes...
#' \dontrun{
#' outputBH = pandemic_model(Y, control = list(max_treedepth = 50, adapt_delta = 0.999))
#' outputBH
#'
#' summary(outputBH)
#'
#' ##convergence diagnostics
#' traceplot(outputBH)
#' density(outputBH)
#' stan_ac(outputBH$fit, pars = c("a","b","c","f"))
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
