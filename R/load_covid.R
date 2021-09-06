#' Load Covid-19 Data
#'
#' @description This function pulls Covid-19 data up to a certain date, for a specified country
#' (and state, if \code{country_name = "Brazil"}).The output of this function is in the correct format to be used directly into the model adjustment function
#' \code{\link{pandemic_model}} included in this package.
#'
#' @param country_name string specifying the country of interest.
#' Check \code{country_list()} for the list of countries available in the database.
#' @param state_name optional string specifying the state of interest - only brazilian states currently
#' available in the database. \code{state_name} should be either \code{NULL} or a string of length 2.
#' Check \code{state_list()} for the state abbreviations that will be used and the corresponding state names.
#' @param last_date optional date, character or factor argument specifying the last date in the data.
#' It should be in the YYYY-MM-DD or YYYY/MM/DD format. The default is the most recent date available in the database.
#'
#' @return An object of S3 class \code{pandemicData}. It is a list with 3 items:
#' \describe{
#'   \item{\code{data}:}{ data frame with the number of cumulative cases, new cases, cumulative deaths and new deaths associated
#'   with Covid-19 for each date, up to the \code{last_date} in the specified region.}
#'   \item{\code{name}:}{ string with the country name (and state name, if available).}
#'   \item{\code{population}:}{numeric object that contains the population size of the given region.}
#' }
#'
#' @examples
#' \dontrun{
#' load_covid("Brazil","MG")
#' load_covid(country_name = "India", last_date = "2020-06-15")
#' load_covid("United States of America")
#' load_covid(country_name = "italy")}
#'
#' @details
#' The current version of this function uses the \code{covid19br} package
#' to retrieve the data. Be aware that the country names might have
#' been altered between different package versions. Check \code{country_list()} for the
#' updated list of \code{country_name} options.
#'
#' @references
#' CovidLP Team, 2020. CovidLP: Short and Long-term Prediction for COVID-19. Departamento de Estatistica. UFMG,
#' Brazil. URL: \url{http://est.ufmg.br/covidlp/home/en/}
#'
#' @seealso  \code{\link{country_list}}, \code{\link{state_list}},  \code{\link{pandemic_model}},
#' \code{\link{posterior_predict.pandemicEstimated}}, \code{\link{pandemic_stats}} and
#' \code{\link{plot.pandemicPredicted}}.
#'
#' @importFrom covid19br downloadCovid19
#'
#' @export

load_covid <- function(country_name, state_name = NULL, last_date){

  country_list <- country_list()
  state_list<- state_list()[["state_abb"]]

  # Error checking
  if(nchar(state_name) == 2 && !is.null(state_name)) {state_name <- toupper(state_name)}
  if(is.character(country_name)) country_name <- capitalize(tolower(country_name)) else stop("country_name must be a character string")
  if(!is.null(state_name) && !is.character(state_name)) stop("state_name must be a string of length 2")
  if(!(country_name %in% country_list)) stop("This country_name could not be found in the database. Use country_list() for available options")
  if(country_name != "Brazil" && !is.null(state_name)) warning("Selected country_name does not have state_name options available")
  if(nchar(state_name) != 2 && !is.null(state_name)) stop("state_name must be a string of length 2")
  if(length(country_name) != 1 || length(state_name) > 1) stop("country_name and state_name arguments cannot be vectors.")
  if(!(state_name %in% state_list) && !is.null(state_name)) stop("This state_name could not be found in the database. Use state_list() for available options")

  if (country_name != "Brazil") { # Loading Brazil country and states data. Different database as other countries
    covidworld <- try(covid19br::downloadCovid19(level="world"))
    if (is(covidworld, "try-error")) stop("Something went wrong retrieving the data. If the problem persists, please try again later or contact us at covidlp.team@gmail.com.")
    current_date <- max(covidworld$date)
    initial_date <- min(covidworld$date)
  } else {
    if (is.null(state_name)){
      covidbr <- try(covid19br::downloadCovid19(level="brazil"))
      if (is(covidbr,"try-error")) stop("Something went wrong retrieving the data. If the problem persists, please try again later or contact us at covidlp.team@gmail.com.")
      current_date <- max(covidbr$date)
      initial_date <- min(covidbr$date)
    } else{
      covidstates <- try(covid19br::downloadCovid19(level="states"))
      if (is(covidstates,"try-error")) stop("Something went wrong retrieving the data. If the problem persists, please try again later or contact us at covidlp.team@gmail.com.")
      current_date <- max(covidstates$date)
      initial_date <- min(covidstates$date)
    }
  }
  if(missing(last_date)) last_date <- current_date
  if(last_date > current_date) warning(paste0("Invalid last_date. Database only contains data up to ", current_date))
  if(is.character(last_date) || is.factor(last_date)) {
    last_date<- try(as.Date(last_date))
    if(class(last_date) == "try-error" || is.na(last_date))
      stop("last_date format must be YYYY-MM-DD or YYYY/MM/DD")
  }
  if(last_date < initial_date) stop(paste0("last_date can't be earlier than ",initial_date))

  if(country_name == "Brazil"){ # Data treatment

    if(is.null(state_name)){

      # Avoiding error message from CRAN
      Y <- covidbr
      eval(parse(
        text = 'Y <- dplyr::select(Y, date, cases = accumCases,
                         deaths = accumDeaths,
                         new_cases = newCases,
                         new_deaths = newDeaths,
                         population = pop)
      Y <- dplyr::filter(Y, Y$date >= initial_date & Y$date <= last_date)'
      ))


    } else {

      # Avoiding error message from CRAN
      Y <- covidstates
      eval(parse(
        text = '
        Y <- dplyr::select(Y, date, cases = accumCases,
                         deaths = accumDeaths,
                         new_cases = newCases,
                         new_deaths = newDeaths,
                         population = pop,
                         name = state)
      Y <- dplyr::arrange(Y, Y$name, Y$date)
      Y <- dplyr::filter(Y, Y$date >= initial_date & Y$date <= last_date & Y$name == state_name)'
      ))

    }

  } else{

    pop <- country_pop$pop[which(country_pop$country == country_name)]

    # Avoiding error message from CRAN
    Y <- covidworld
    eval(parse(
      text = '
      Y <- dplyr::select(Y, date, cases = accumCases,
                       deaths = accumDeaths,
                       new_cases = newCases,
                       new_deaths = newDeaths,
                       name = country)
    Y <- dplyr::filter(Y, Y$date >= initial_date & Y$date <= last_date & Y$name == country_name)'
    ))

  }

  # Inconsistencies fail safe
  while(any(Y$new_cases < 0)){
    pos <- which(Y$new_cases < 0)
    for(j in pos){
      Y$new_cases[j-1] = Y$new_cases[j] + Y$new_cases[j-1]
      Y$new_cases[j] = 0
      Y$cases[j-1] = Y$cases[j]
    }
  }

  while(any(Y$new_deaths < 0)){
    pos <- which(Y$new_deaths < 0)
    for(j in pos){
      Y$new_deaths[j-1] = Y$new_deaths[j] + Y$new_deaths[j-1]
      Y$new_deaths[j] = 0
      Y$deaths[j-1] = Y$deaths[j]
    }
  }

  if(dim(Y)[1] == 0) warning("last_date assignment resulted in an empty data frame.")

  list_out = list(data = as.data.frame(Y[,1:5]),
                  name = ifelse(is.null(state_name), paste0(country_name), paste0(country_name,"_",state_name)),
                  population = ifelse(country_name=="Brazil", Y$population, pop))
  class(list_out) = "pandemicData"
  return(list_out)

}

#' Capitalize country names
#'
#' This function capitalizes country_name inputs in order to match the capitalization in the database.
#'
#' @param x A character string
#'
#' @return Capitalized character string x to match the spelling and capitalization in the database.
#'
#' @noRd
#'

capitalize <- function(x) {
    s <- gsub("\\b(\\w)", "\\U\\1", x, perl = TRUE)
    s<- gsub("\\bAnd\\b", "and", s)
    s<- gsub("\\bOf\\b", "of", s)
    if(length(s)== 1 && s =="Cote D'Ivoire") {s <-"Cote d'Ivoire"}
    if(length(s)== 1 && s =="Eswatini") {s <-"eSwatini"}
    s
}




