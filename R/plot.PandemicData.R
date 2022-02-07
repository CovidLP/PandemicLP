#Pulling auxiliary functions
source("R/utils.R")

#' Plot pandemic data
#'
#' S3 method that plots the predicted data into an interactive graphic.
#' @param x Output of function \code{\link{load_covid}}.
#' @param y This parameter does nothing
#' @param cases A string which indicates whether new cases, cumulative cases or both plots should be generated.
#' The argument must be a string being either 'new', 'cumulative' or 'both'.
#' @param color A string which indicates whether the plot should be colorful or in gray scales.
#' The argument must be a string being either TRUE or FALSE.
#' @param ... Currently unused.
#' @return A list containing two objects, new and cumulative "new" shows the plot for the new cases/deaths
#' and "cumulative" shows the plot for the cumulative cases/deaths. If any of them did not get plotted due to lack of prediction or due to the \code{cases}
#' argument, its value will return NULL.
#'
#' By default, only the plot with new cases is generated.
#' This is changed with the \code{cases} argument.
#' \item{\code{new}}{
#'   The plotted new cases/deaths. The plots are for daily new confirmed cases and daily new deaths.
#'   }
#'   \item{\code{cumulative}}{
#'   The plotted cumulative cases/deaths. The plots are for daily cumulative cases or daily cumulative deaths.
#'   }
#' @seealso \code{\link{load_covid}}.
#' @references
#' CovidLP Team, 2020. CovidLP: Short and Long-term Prediction for COVID-19. Departamento de Estatistica. UFMG,
#' Brazil. URL: \url{http://est.ufmg.br/covidlp/home/en/}
#' @examples
#' \dontrun{
#' dataMG = load_covid(country_name="Brazil",state_name = "MG",last_date="2020-10-01")
#' plot(dataMG)
#' dataJapan = load_covid(country_name="Japan",last_date="2020-10-01")
#' plot(dataJapan)
#' }
#' @importFrom plotly plot_ly add_trace layout
#'
#' @method plot pandemicData
#' @export
plot.pandemicData <- function(x, y, cases = "new", color = TRUE, ...){

  if(is(x, "pandemicData") | is.list(x)) {
    test <- 'ok'
  } else {
    stop("Please use the output of the load_covid() function or a list.")
  }

  cases <- tolower(cases)
  if(!(cases %in% c("both","new","cumulative"))) stop("Invalid \'cases\' argument. Please read \'help(plot.pandemicData)\' for available options.")

  if(cases = "new" && !any(c("new_cases", "new_deaths")) %in% names(x$data)) warning("Check if you entered the \'cases'\ parameter correctly")
  if(cases = "cumulative" && !any(c("cases", "deaths")) %in% names(x$data)) warning("Check if you entered the \'cases'\ parameter correctly")

  x$data <- accum_to_new(x)

  ## x$data without either 'new_deaths' and 'new_cases':
  #if(!any(c("new_cases", "new_deaths") %in% names(x$data))) {
  #  if("cases" %in% names(x$data)){
  #    n_lines = nrow(x$data)
  #    col_new_cases = c()
  #    i <- as.integer(1)
  #    while (i <= n_lines){
  #      if (i == 1){
  #        i_new_cases = x$data[i, "cases" ]
  #        col_new_cases = c(col_new_cases, i_new_cases)
  #      } else{
  #        i_new_cases = diff(dado_2$data[c(i-1, i), "cases" ])
  #        if (i_new_cases >= 0){
  #          col_new_cases = c(col_new_cases, i_new_cases)
  #        } else{
            col_new_cases = c(col_new_cases, 0)
          }
        }
        i = i+1
      }
      x$data <- cbind(x$data, new_cases = col_new_cases)
    } else{
      n_lines = nrow(x$data)
      col_new_deaths <- c()
      i <- as.integer(1)
      while (i <= n_lines){
        if (i == 1){
          i_new_deaths = x$data[i, "deaths" ]
          col_new_deaths = c(col_new_deaths, i_new_deaths)
        } else{
          i_new_deaths = diff(dado_2$data[c(i-1, i), "deaths" ])
          if (i_new_deaths >= 0){
            col_new_deaths = c(col_new_deaths, i_new_deaths)
          } else{
            col_new_deaths = c(col_new_deaths, 0)
          }
        }
        i = i+1
      }
      x$data = cbind(x$data, new_deaths = col_new_deaths)
    }
  }

  # x$data with 'new_deaths' and without 'new_cases':
  if(!("new_cases" %in% names(x$data)) && "new_deaths" %in% names(x$data) && is.numeric(x$data$new_deaths)){
    data_cases <- FALSE
    x$data <- cbind(x$data, new_cases = x$data$new_deaths)
  #x$data with 'new_cases' and without 'new_deaths':
  } else if(!("new_deaths" %in% names(x$data)) && "new_cases" %in% names(x$data) && is.numeric(x$data$new_cases)){
    data_cases <- TRUE
    x$data <- cbind(x$data, new_deaths = x$data$new_cases)
  }

  #x$data without either 'cumulative cases':
  if("new_cases" %in% names(x$data) && !("cases" %in% names(x$data)) && is.numeric(x$data$new_cases)) {
    x$data <- cbind(x$data, cases = cumsum(x$data$new_cases))
  }
  if("new_deaths" %in% names(x$data) && !("deaths" %in% names(x$data)) && is.numeric(x$data$new_deaths)) {
    x$data <- cbind(x$data, deaths = cumsum(x$data$new_deaths))
  }

  if(!all(c("date", "cases", "deaths", "new_cases", "new_deaths") %in% names(x$data))) stop("x$data should be a data.frame with column names: 'date' and at least one of the 'new_cases' or 'new_deaths'. See help(pandemic_model)")
  if(!is(x$data$date, "Date")) stop("x$data$date should be of class 'Date' and format 'YYYY-MM-dd' " )
  if(!all(is.numeric(x$data$cases), is.numeric(x$data$deaths), is.numeric(x$data$new_cases), is.numeric(x$data$new_deaths))) stop( "x$data: values in 'cases', 'deaths', 'new_cases' and 'new_deaths' columns should be as.integer or as.numeric")

  #data processing: new_cases, new_deaths < 0:
  while(any(x$data$new_cases < 0)){
    pos <- which(x$data$new_cases < 0)
    for(j in pos){
      x$data$new_cases[j - 1] = x$data$new_cases[j] + x$data$new_cases[j - 1]
      x$data$new_cases[j] = 0
      x$data$cases[j - 1] = x$data$cases[j]
    }
  }

  while(any(x$data$new_deaths < 0)){
    pos <- which(x$data$new_deaths < 0)
    for(j in pos){
      x$data$new_deaths[j - 1] = x$data$new_deaths[j] + x$data$new_deaths[j - 1]
      x$data$new_deaths[j] = 0
      x$data$deaths[j - 1] = x$data$deaths[j]
    }
  }

  cat(paste0("Plotting Data.\n"))

  if(cases == "both") {
    terms <- c("new","cumulative")
  } else
    terms = cases

  plots = list()
  dados = x$data
  blu <- 'rgb(100, 140, 240)'
  dblu <- 'rgb(0, 0, 102)'
  red <- 'rgb(200, 30, 30)'
  dred <- 'rgb(100, 30, 30)'
  dt_format <- "%d/%b/%y"

  title <- ifelse(grepl("_", x$name, fixed = TRUE),
                  sub("_", "/", x$name),
                  x$name)

  for(selTerm in terms){

    if(selTerm == "new"){
      data_plot_cases = dados$new_cases
      data_plot_deaths = dados$new_deaths
    } else {
      data_plot_cases = dados$cases
      data_plot_deaths = dados$deaths
    }

    cases_deaths_test <- ifelse(ncol(dados) == 5, "Complete", ifelse(is.null(data_plot_deaths), "Cases", "Deaths"))
    if(cases_deaths_test == "Complete"){
      yaxis <- ifelse(selTerm == "new","New Cases per Day", "Cumulative Cases")

      fig2 <- plotly::add_trace(plotly::plot_ly(dados), x = dados$date, y = data_plot_cases,
                                type = 'scatter', mode = 'lines+markers', name = "Confirmed cases",
                                hoverinfo = "x+y",
                                marker = list(
                                  color =  ifelse(color == TRUE,blu,'rgb(160,160,160)'),
                                  line = list(color = ifelse(color == TRUE,dblu,'rgb(160,160,160)'), width = 1),
                                  size = 5
                                ),
                                line = list(
                                  color = ifelse(color == TRUE,blu,'rgb(160,160,160)'),
                                  width = 1.5
                                )
      )

      fig2 <- plotly::add_trace(fig2,x = dados$date, y = data_plot_deaths,
                                type = 'scatter', mode = 'lines+markers', name = "Deaths",
                                hoverinfo = "x+y",
                                marker = list(
                                  color = ifelse(color == TRUE,red,'rgb(96,96,96)'),
                                  line = list(color = ifelse(color == TRUE,dred,'rgb(96,96,96)'), width = 1),
                                  size = 5
                                ),
                                line = list(
                                  color = ifelse(color == TRUE,red,'rgb(96,96,96)'),
                                  width = 1.5
                                )
      )
    } else {
      yaxis <- ifelse(selTerm == "new",ifelse(cases_deaths_test == "Cases","New Cases per Day","New Deaths per Day"),
                      ifelse(cases_deaths_test == "Cases","Cumulative Cases","Cumulative Deaths"))
      if(cases_deaths_test == "Cases"){
        dados_plot = data_plot_cases
      } else{
        dados_plot = data_plot_deaths
      }

      fig2 <- plotly::add_trace(plotly::plot_ly(dados),x = dados$date, y = dados_plot,
                                type = 'scatter', mode = 'lines+markers', name = ifelse(cases_deaths_test == "Cases","Confirmed cases","Confirmed Deats"),
                                hoverinfo = "x+y",
                                marker = list(
                                  color =  ifelse(color == TRUE,ifelse(cases_deaths_test == "Cases",blu,red),'rgb(160,160,160)'),
                                  line = list(color = ifelse(color == TRUE,ifelse(cases_deaths_test == "Cases",dblu,dred),'rgb(160,160,160)'), width = 1),
                                  size = 5
                                ),
                                line = list(
                                  color = ifelse(color == TRUE,ifelse(cases_deaths_test == "Cases",blu,red),'rgb(160,160,160)'),
                                  width = 1.5
                                )
      )
    }

    fig2 <- plotly::layout(fig2,title = title,
                           ## Add pred dates to the x axis
                           xaxis = list(title = "",
                                        tickangle = -90,
                                        ## Add pred dates to the x axis
                                        dtick = 14*86400000,
                                        tickformat = dt_format
                           ),
                           yaxis = list(
                             title = yaxis,
                             hoverformat = '.0f', hoverinfo = "x+y"
                           ),           # Show only date/value on hover
                           legend = list(
                             x = 0.03,
                             y = 0.97,                             # Legend position
                             bgcolor = 'rgba(240, 240, 240, 0.5)'
                           ),
                           font = list(family = "Arial", size = 10)
                           )

    plots[[selTerm]] = fig2
  }

  if(cases == "both") terms = "newcumulative" else terms = cases
  cat("The generated plot(s) can be stored in a variable.\n")
  if (grepl("new",terms)) cat("New Cases plot: variable$new\n")
  if (grepl("cumulative",terms)) cat("Cumulative Cases plot: variable$cumulative\n")

  class(plots) = "plottedPandemicData"
  return(plots)
}
