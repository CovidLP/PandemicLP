#' Format epidemiological data
#'
#' @description The purpose of this function is to format any data input
#' to be used directly in the \code{\link{pandemic_model}} function as the
#' \code{Y} argument.
#'
#' @param s_date  start date of the time series. It should be in the YYYY-MM-DD format.
#' @param e_date  end date of the time series. It should be in the YYYY-MM-DD format.
#' @param cases numeric vector containing the cumulative case counts per day from the start date
#' until the end date.
#' @param n_cases numeric vector containing the new case counts per day from the start date
#' until the end date.
#' @param deaths numeric vector containing the cumulative death counts per day from the start date
#' until the end date.
#' @param n_deaths numeric vector containing the new death counts per day from the start date
#' until the end date.
#' @param name string specifying the location name.
#' @param pop numeric object that contains the population size of the given location.
#'
#' @return An object of S3 class \code{pandemicData}. It is a list with 3 items:
#' \describe{
#'   \item{\code{data}:}{ data frame with the number of cumulative cases, new cases
#'   or/and cumulative deaths and new deaths for each date, from \code{s_date} up to the \code{e_date} in the specified region.}
#'   \item{\code{name}:}{ string with the location name.}
#'   \item{\code{population}:}{numeric object that contains the population size of the given location.}
#' }
#'
#' @examples
#' \dontrun{
#'
#' #illustrative epidemiological data
#' cases <- c(0,1,4,7,10,19)
#' n_cases <- c(cases[1], diff(cases))
#'
#' #format data to use in pandemic_model
#' data <- format_data(s_date = "2021-01-01", e_date = "2021-01-06",
#'                     cases = cases, n_cases = n_cases,
#'                     name = "Example", pop = 1234)
#'
#' data
#' plot(data)
#' }
#'
#' @details
#' This function requires that at least one of the data inputs \code{cases,
#' n_cases, deaths, n_deaths} is provided. If only cumulative data was inserted,
#' \code{format_data} will automatically calculate the new case counts. On the contrary,
#' if only new case counts were provided, then the cumulative counts will be calculated.
#' Check \code{\link{covid19BH}} for a complete example on how to use this function.
#'
#'
#' @seealso  \code{\link{covid19BH}}
#'
#' @export



format_data <- function(s_date, e_date, cases = NULL, n_cases = NULL,
                        deaths = NULL, n_deaths = NULL, name, pop){

  # Error check for name, population and dates
  if(is.character(name)) name <- capitalize(tolower(name)) else stop("name must be a character string")
  if(missing(pop)) stop("'pop' argument missing")
  if(missing(s_date)|| missing(e_date)) stop("s_date and e_date must be specified")
  if(e_date < s_date) stop("e_date must be later than s_date")
  if(is.character(s_date) || is.factor(s_date) || is.character(e_date) || is.factor(e_date)) {
    s_date <- try(as.Date(s_date))
    e_date <- try(as.Date(e_date))
    if(class(s_date) == "try-error" || is.na(s_date) || class(e_date) == "try-error" || is.na(e_date))
      stop("s_date and e_date format must be YYYY-MM-DD or YYYY/MM/DD")
  }

  #Data without 'cumulative cases', so create it from 'new cases'
  if(!(is.null(n_cases)) && is.null(cases) && is.numeric(n_cases)) {
    cases <- cumsum(n_cases)
  }
  if(!(is.null(n_deaths)) && is.null(deaths) && is.numeric(n_deaths)){
    deaths <- cumsum(n_deaths)
  }

  #Data without 'new cases', so create it from 'cumulative cases'
  if(!(is.null(cases)) && is.null(n_cases) && is.numeric(cases)) {
    n_cases <- c(cases[1], diff(cases))
  }
  if(!(is.null(deaths)) && is.null(n_deaths) && is.numeric(deaths)){
    n_deaths <- c(deaths[1], diff(deaths))
  }

  #Data input missing
  if(is.null(cases) & is.null(deaths) & is.null(n_cases) & is.null(n_deaths))
    stop("Data should be provided for at least one of the following: 'cases', 'deaths', 'n_cases', or 'n_deaths' ")

  date <- seq(s_date, e_date, by = "days")

  #Error check - length of dates does not match length of data
  if(length(date) != length(cases) && !(is.null(cases)) |
     length(date) != length(deaths) && !(is.null(deaths)) |
     length(date) != length(n_cases) && !(is.null(n_cases))|
     length(date) != length(n_deaths) && !(is.null(n_deaths)))
    stop("length of date vector is not equal to the length of data provided")

  #Warning if cumulative data is not non decreasing
  if(!(is.null(cases)) && is.unsorted(cases) )
    warning("'cases' should be a non-decreasing sequence of numbers. Some corrections might have been made to the data.")
  if(!(is.null(deaths)) && is.unsorted(deaths) )
    warning("'deaths' should be a non-decreasing sequence of numbers. Some corrections might have been made to the data.")

  #Warning if cumulative data does not match new data
  if(!(is.null(n_cases)) && !(isTRUE(all.equal(n_cases[2:length(n_cases)], diff(cases)))) )
    warning("Make sure cumulative case counts 'cases' represents the sum of new case counts 'n_cases' each day")
  if(!(is.null(n_deaths)) && !(isTRUE(all.equal(n_deaths[2:length(n_deaths)], diff(deaths)))) )
    warning("Make sure cumulative death counts 'deaths' represents the sum of new death counts 'n_deaths' each day")


  #create data frame according to the data input provided
  if( !(is.null(n_cases)) & !(is.null(n_deaths)) ){
    data <-  data.frame( date = date, cases = cases, deaths = deaths,
                         new_cases = n_cases, new_deaths = n_deaths)
  } else if( is.null(n_cases)){
    data <-  data.frame( date = date, deaths = deaths, new_deaths = n_deaths)
  } else if( is.null(n_deaths) ){
    data <-  data.frame( date = date, cases = cases, new_cases = n_cases)
  }

  #data processing: new_cases, new_deaths < 0:
  while(any(data$new_cases < 0)){
    pos <- which(data$new_cases < 0)
    for(j in pos){
      data$new_cases[j - 1] = data$new_cases[j] + data$new_cases[j - 1]
      data$new_cases[j] = 0
      data$cases[j - 1] = data$cases[j]
    }
  }

  while(any(data$new_deaths < 0)){
    pos <- which(data$new_deaths < 0)
    for(j in pos){
      data$new_deaths[j - 1] = data$new_deaths[j] + data$new_deaths[j - 1]
      data$new_deaths[j] = 0
      data$deaths[j - 1] = data$deaths[j]
    }
  }


  list_out <- list(data = data, name = name, population = as.numeric(pop))
  class(list_out) = "pandemicData"
  return(list_out)

}
