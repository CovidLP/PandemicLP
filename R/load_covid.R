#' Load Covid-19 Data
#'
#' This function pulls Covid-19 data up to a certain date, for a specified country (and state, if \code{country_name = "Brazil"}).
#' The output of this function is in the correct format to be used directly into the model adjustment function
#' \code{\link{pandemic_model}} included in this package.\cr
#' \cr
#' The user must online for this function to work.
#'
#' @import dplyr
#' @importFrom curl has_internet
#' @importFrom tidyr pivot_longer
#'
#' @param country_name Character string specifying the country of interest.
#' Check \code{country_list()} for the list of countries available in the database.
#' @param state_name Optional character string specifying the state of interest - only brazilians states currently
#' available in the database. \code{state_name} should be \code{NULL} or a string of length 2.
#' Check \code{state_list()} for the state abbreviations that will be used and the corresponding state names.
#' @param last_date Optional date, character or factor argument specifying the last date in the data.
#' It should be in the YYYY-MM-DD or YYYY/MM/DD format.The default is the most recent date available in the database.
#'
#' @return A list with 3 items.
#' \itemize{
#'   \item The first item is a data frame with the number of cumulative cases, new cases, cumulative deaths and new deaths associated
#'   with Covid-19 for each date, up to the last_date in the specified region.
#'   \item The second item is a character string with the country name (and state name, if available).
#'   \item The third item is a numeric object that contains the population size of the given region.
#' }
#'
#' @examples load_covid("Brazil","MG")
#' load_covid(country_name = "India", last_date = "2020-06-15")
#' load_covid("US")
#' load_covid(country_name = "italy")
#'
#' @source \url{https://github.com/CSSEGISandData/COVID-19}
#' \url{https://github.com/covid19br/covid19br.github.io}
#'
#' @references
#' CovidLPTeam, 2020. CovidLP: Short and Long-term Prediction for COVID-19. Departamento de Estatistica. UFMG,
#' Brazil. URL: \url{http://est.ufmg.br/covidlp/home/en/}
#'
#' @seealso  \code{\link{country_list}}      \code{\link{state_list}}    \code{\link{pandemic_model}}
#' \code{\link{posterior_predict.pandemicEstimated}}     \code{\link{plot.pandemicPredicted}}
#'
#' @export
#'

load_covid <- function(country_name, state_name, last_date){

  if(has_internet() == F) stop("The user must be online to use this function")

  baseURLbr <-"https://raw.githubusercontent.com/covid19br/covid19br.github.io/master/dados"
  baseURL<-"https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series"

  covidbr <- read.csv(file.path(baseURLbr,"EstadosCov19.csv"), check.names=FALSE, stringsAsFactors=FALSE)
  covidworld <- read.csv(file.path(baseURL,"time_series_covid19_confirmed_global.csv"), check.names=FALSE, stringsAsFactors=FALSE)

  country_list <- country_list()
  state_list<- distinct(covidbr, estado)

  if (country_name != "Brazil") {
    current_date <- as.Date(variable.names(covidworld[dim(covidworld)[2]]), format = "%m/%d/%y")
  }else {
      current_date <- as.Date(max(covidbr$data))
      }
  if(missing(state_name)) state_name <- NULL
  if(missing(last_date)) last_date <- current_date
  if(nchar(state_name) == 2 && is.null(state_name) == F) {state_name <- toupper(state_name)}
  if(class(country_name) == "character" ) country_name <- capitalize(tolower(country_name))
  if(class(country_name) != "character") stop("country_name must be a character string")
  if(is.null(state_name) == F && class(state_name) != "character") stop("state_name must be a character string")
  if(!(country_name %in% country_list)) stop("This country_name could not be found in the database")
  if(country_name != "Brazil" && is.null(state_name) == F) warning("country_name selected does not have state_name options available")
  if(nchar(state_name)!= 2 && is.null(state_name) == F) stop ("state_name must be a string of length 2")
  if(!(state_name %in% state_list$estado) && is.null(state_name) == F) stop("This state_name could not be found in the database")
  if(class(last_date) == "character" || class(last_date) == "factor") {
    last_date<- try(as.Date(last_date))
    if(class(last_date) == "try-error" || is.na(last_date) == T || format(last_date,"%Y") < "2020")
      stop("last_date format must be YYYY-MM-DD or YYYY/MM/DD")
  }
  if(last_date > current_date) warning(paste0("Invalid last_date. Database only contains data up to ", current_date))
  if(last_date < "2020-01-23") stop("last_date can't be earlier than 2020-01-23")

  br_pop
  country_pop

  if(country_name == "Brazil"){

    if(is.null(state_name) == T){

      pop <- as.numeric(br_pop$pop[which(br_pop$uf == "BR")])

      Y <- read.csv(file.path(baseURLbr,"BrasilCov19.csv"), check.names=FALSE, stringsAsFactors=FALSE) %>%
        rename(date = data,
               cases = casos.acumulados,
               deaths = obitos.acumulados,
               new_cases = novos.casos,
               new_deaths = obitos.novos) %>%
        mutate(date = as.Date(date)) %>%
        select(date, cases, deaths, new_cases, new_deaths) %>%
        arrange(date) %>% filter(date >= '2020-01-23' & date <= last_date)

    } else{

      pop <- as.numeric(br_pop$pop[which(br_pop$uf == state_name)])

      Y <- covidbr %>%
        rename(name = estado,
               date = data,
               cases = casos.acumulados,
               deaths = obitos.acumulados,
               new_cases = novos.casos,
               new_deaths = obitos.novos) %>%
        mutate(date = as.Date(date)) %>%
        arrange(name, date) %>% filter(date >= '2020-01-23' & date <= last_date & name == state_name) %>%
        select(date, cases, deaths, new_cases, new_deaths)

    }
  } else{

    covid19_confirm <- covidworld %>%
      select(-Lat, -Long) %>%
      pivot_longer(-(1:2), names_to = "date", values_to = "confirmed")%>%
      mutate(date = as.Date(date, format="%m/%d/%y")) %>%
      rename(country = `Country/Region`, state = `Province/State`)

    covid19_deaths <- read.csv(file.path(baseURL,"time_series_covid19_deaths_global.csv"), check.names=FALSE, stringsAsFactors=FALSE) %>%
      select(-Lat, -Long) %>%
      pivot_longer(-(1:2), names_to = "date", values_to ="deaths")%>%
      mutate(date = as.Date(date, format="%m/%d/%y")) %>%
      rename(country = `Country/Region`, state = `Province/State`)

    covid19 <- left_join(covid19_confirm, covid19_deaths, by = c("state", "country", "date"))

    pop <- country_pop$pop[which(country_pop$country == country_name)]

    Y <- covid19 %>% filter(country == country_name) %>%
      mutate(confirmed_new = confirmed - lag(confirmed, default=0),
             deaths_new = deaths - lag(deaths, default=0)) %>%
      arrange(date,state)%>%
      group_by(date, country) %>%
      summarize(cases = sum(confirmed, na.rm = T),
                deaths = sum(deaths, na.rm = T),
                new_cases = as.integer(sum(confirmed_new, na.rm = T)),
                new_deaths = as.integer(sum(deaths_new, na.rm = T))) %>%
      select(date, cases, deaths, new_cases, new_deaths)%>%
      arrange(date) %>% filter(date >= '2020-01-23' & date <= last_date)

  }

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

  if(dim(Y)[1] == 0) warning("last_date assignment resulted in an empty data frame")

  list_out = list(data = as.data.frame(Y),
                  name = ifelse(is.null(state_name), paste0(country_name), paste0(country_name,"_",state_name)),
                  population = pop)
  list_out

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
  if(s == "Us") {s <-"US"}
  if(s == "Cote D'ivoire") {s <-"Cote d'Ivoire"}
  s
}
