#' List of countries available in the Covid-19 database
#'
#' @import dplyr
#' @importFrom curl has_internet
#'
#' @description This function provides a list of 183 country names inside the Covid-19 database.
#' The \code{country_name} argument inside the \code{\link{load_covid}} function should be spelled exactly
#' as they are listed here. The user must be online for this function to work.
#'
#' @source \url{https://github.com/CSSEGISandData/COVID-19}
#'
#' @seealso \code{\link{load_covid}}
#'
#' @export

country_list <- function(){

  if(has_internet() == F) stop("The user must be online to use this function")

  baseURL = "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series"
  covid <-read.csv(file.path(baseURL,"time_series_covid19_confirmed_global.csv"), check.names=FALSE, stringsAsFactors=FALSE) %>%
    rename(country = `Country/Region`) %>%
    filter(!(country %in% c("Diamond Princess","Holy See","Taiwan*","MS Zaandam","Western Sahara")))

  country_list <- distinct(covid,country)
  sort(country_list$country)

}
