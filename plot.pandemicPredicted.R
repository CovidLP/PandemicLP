#' Plot pandemic predictions
#'
#' S3 method that plots the predicted data into an interactive graphic.
#' @param object Output of function \code{\link{posterior_predict.pandemicEstimated}}.
#' @return A list containing two plots, long and short. "long" shows the long-term predictions
#' and "short" the short-term predictions. If any of them did not get plotted due to lack of prediction,
#' its value will return NULL.
#' \item{\code{long}}{
#'   The plotted long-term prediction. The predictions are made on daily new confirmed cases or daily new deaths.
#'   }
#'   \item{\code{short}}{
#'   The plotted short-term prediction. The predictions are made on daily cumulative cases or daily cumulative deaths.
#'   }
#' @seealso \code{\link{posterior_predict.pandemicEstimated}}, \code{\link{pandemic_stats}} and \code{\link{plottedPandemic-objects}}.
#' @references
#' CovidLP Team, 2020. CovidLP: Short and Long-term Prediction for COVID-19. Departamento de Estatistica. UFMG,
#' Brazil. URL: \url{http://est.ufmg.br/covidlp/home/en/}
#' @examples
#' \dontrun{
#' dataMG = load_covid("Brazil","MG")
#' estimMG = pandemic_model(dataMG)
#' predMG = posterior_predict(estimMG)
#' statsMG = pandemic_stats(predMG)
#' plot(predMG)}
#' @importFrom plotly plot_ly add_trace layout
#'
#' @export
plot.pandemicPredicted <- function(object){
  if(object[[5]] == "confirmed"){
    cat("Plotting confirmed cases\n")
    if(ncol(object[[1]]) < 1){
      message("There are no long term predictions")
      long = NULL
    } else {
      outputs <- pandemic_stats(object)

      dados <- outputs$data$data
      data <- outputs$LT_predict
      pred_summary <- outputs$LT_summary
      metric_leg <- ifelse(object[[5]] == "confirmed","cases","deaths")
      last_date_n <- min(data$date) - 1
      pred_time <- length(data$date)
      mu_plot <- outputs$mu

      printDate <- function(date){
        monthsEn <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
        out <- paste0(lubridate::day(date), "/", monthsEn[lubridate::month(date)])

        return(out)
      }

      fig2 <- plotly::add_trace(plotly::plot_ly(data),x = ~date, y = ~med, type= "scatter", name = "Prediction", mode = "markers",
                                marker=list(color='rgb(0,0,0)', dash='solid', width=2.5))

      fig2 <- plotly::add_trace(fig2,x = ~dados$date, y = ~dados$new_cases, type = 'scatter', mode = 'lines+markers', hoverinfo = "x+y", name = "Observed Data", mode = "lines+markers",
                                hoverinfo = "x+y",
                                marker = list(
                                  color = 'rgb(100, 140, 240)',
                                  line = list(color = 'rgb(0, 0, 0)', width = 1)),
                                line = list(
                                  color = 'rgb(100, 140, 240)',
                                  width = 1.5)


      )

      fig2 <- plotly::layout(fig2,title = paste("Prediction of New Cases - ",object[[4]]),
                             annotations = list(text = paste0(
                               "<b>Peak:</b> ", printDate(pred_summary$peak_date_med),
                               "<br><b>Peak 95% CI:</b> ",
                               paste0("(", printDate(pred_summary$peak_date_LB),", ",
                                      printDate(pred_summary$peak_date_UB), ")"),

                               "<br><b>Total number of ", metric_leg, ":</b> ",
                               round(pred_summary$total_cases_med,0), "<br>",
                               "<b>95% CI:</b> ",
                               paste0("(", round(pred_summary$total_cases_LB, 0),
                                      ", ", round(pred_summary$total_cases_UB, 0), ")"),
                               "<br><b>End (99%) of ",metric_leg,":</b> ",
                               printDate(pred_summary$end_date_med),
                               "<br><b>95% CI:</b> ",
                               paste0("(", printDate(pred_summary$end_date_LB),", ",
                                      printDate(pred_summary$end_date_UB),")" ),
                               "</span>"
                             ),x = 0.97, y = 0.97, xref = "paper", yref = "paper",
                             font = list(family = "Arial", size = 10), align = "right",
                             showarrow = FALSE),
                             xaxis = list(title = "date"),
                             yaxis = list(title = "New Cases per Day"),
                             shapes = list(type = "line", opacity = 0.7, line = list(color = "black", width = 1),
                                           y0 = 0, y1 = 1,
                                           x0 = pred_summary$peak_date_LB, x1 = pred_summary$peak_date_LB)
      )

      fig2 <- plotly::add_trace(fig2,
                                x = c(dados$date[which(dados$date == last_date_n)],
                                      data$date[1:pred_time]),
                                y = c(dados$new_cases[which(dados$date == last_date_n)],
                                      data$q2.5[1:pred_time]),
                                showlegend = F,
                                name = "95% CI",
                                type = 'scatter',
                                mode = 'lines', hoverinfo = "x+y",
                                #fill='tonexty',
                                #dash='solid',
                                line = list(color = 'rgba(0, 0, 0, 1)',width=0)
      )

      fig2 <- plotly::add_trace(fig2,
                                x = c(dados$date[which(dados$date == last_date_n)],
                                      data$date[1:pred_time]),
                                y = c(dados$new_cases[which(dados$date == last_date_n)],
                                      data$q97.5[1:pred_time]),
                                type = 'scatter',
                                mode = 'lines', hoverinfo = "x+y",
                                fill = 'tonexty',
                                name = "95% CI",
                                # fillcolor = 'rgba(150, 150, 150, 0.5)',
                                fillcolor = 'rgba(100, 100, 100, 0.5)',
                                line = list(color = 'rgba(0, 0, 0, 1)', width = 0)
      )


      fig2 <- plotly::add_trace(fig2,
                                x = mu_plot$date, y = mu_plot$mu,
                                type = 'scatter', mode = 'lines', hoverinfo = "none",
                                name = ("Estimated Mean"),
                                line = list(color = 'rgb(230, 115, 0)', dash = 'solid',
                                            width=1.5) ## width = 2.5)
      )

      cat("Long term plot created successfully\n")
      long <- fig2
    }

    if(ncol(object[[2]]) < 1){
      message("There are no short term predictions")
      short = NULL
    } else {
      outputs <- pandemic_stats(object)

      dados <- outputs$data$data
      data <- outputs$ST_predict
      last_date_n <- min(data$date) - 1
      pred_time <- length(data$date)

      data_max <- as.Date(object[[3]][nrow(object[[3]]),1])

      fig2 <- plotly::add_trace(plotly::plot_ly(data),x = ~date, y = ~med, type= "scatter", name = "Prediction", mode = "markers",
                                marker=list(color='rgb(0,0,0)', dash='solid', width=2.5))

      fig2 <- plotly::add_trace(fig2,x = ~dados$date, y = ~dados$cases, type = 'scatter', mode = 'lines+markers', hoverinfo = "x+y", name = "Observed Data", mode = "lines+markers",
                                hoverinfo = "x+y",
                                marker = list(
                                  color = 'rgb(100, 140, 240)',
                                  line = list(color = 'rgb(0, 0, 0)', width = 1)),
                                line = list(
                                  color = 'rgb(100, 140, 240)',
                                  width = 1.5)


      )

      fig2 <- plotly::layout(fig2,title = paste("Prediction of New Cases - ",object[[4]]),
                             xaxis = list(title = "date"),
                             yaxis = list(title = "Accumulated New Cases"),
                             shapes = list(type = "line", opacity = 0.7, line = list(color = "black", width = 1),
                                           y0 = 0, y1 = 1,
                                           x0 = data_max, x1 = data_max)
      )

      fig2 <- plotly::add_trace(fig2,
                                x = c(dados$date[which(dados$date == last_date_n)],
                                      data$date[1:pred_time]),
                                y = c(dados$cases[which(dados$date == last_date_n)],
                                      data$q2.5[1:pred_time]),
                                showlegend = F,
                                name = "95% CI",
                                type = 'scatter',
                                mode = 'lines', hoverinfo = "x+y",
                                #fill='tonexty',
                                #dash='solid',
                                line = list(color = 'rgba(0, 0, 0, 1)',width=0)
      )

      fig2 <- plotly::add_trace(fig2,
                                x = c(dados$date[which(dados$date == last_date_n)],
                                      data$date[1:pred_time]),
                                y = c(dados$cases[which(dados$date == last_date_n)],
                                      data$q97.5[1:pred_time]),
                                type = 'scatter',
                                mode = 'lines', hoverinfo = "x+y",
                                fill = 'tonexty',
                                name = "95% CI",
                                # fillcolor = 'rgba(150, 150, 150, 0.5)',
                                fillcolor = 'rgba(100, 100, 100, 0.5)',
                                line = list(color = 'rgba(0, 0, 0, 1)', width = 0)
      )


      cat("Short term plot created successfully\n")
      short <- fig2
    }

  } else {
    cat("Plotting death cases\n")
    if(ncol(object[[1]]) < 1){
      message("There are no long term predictions")
      long = NULL
    } else {
      outputs <- pandemic_stats(object)

      dados <- outputs$data$data
      data <- outputs$LT_predict
      pred_summary <- outputs$LT_summary
      metric_leg <- ifelse(object[[5]] == "confirmed","cases","deaths")
      last_date_n <- min(data$date) - 1
      pred_time <- length(data$date)
      mu_plot <- outputs$mu

      printDate <- function(date){
        monthsEn <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
        out <- paste0(lubridate::day(date), "/", monthsEn[lubridate::month(date)])

        return(out)
      }

      fig2 <- plotly::add_trace(plotly::plot_ly(data),x = ~date, y = ~med, type= "scatter", name = "Prediction", mode = "markers",
                                marker=list(color='rgb(0,0,0)', dash='solid', width=2.5))

      fig2 <- plotly::add_trace(fig2,x = ~dados$date, y = ~dados$new_deaths, type = 'scatter', mode = 'lines+markers', hoverinfo = "x+y", name = "Observed Data", mode = "lines+markers",
                                hoverinfo = "x+y",
                                marker = list(
                                  color = 'rgb(100, 140, 240)',
                                  line = list(color = 'rgb(0, 0, 0)', width = 1)),
                                line = list(
                                  color = 'rgb(100, 140, 240)',
                                  width = 1.5)


      )

      fig2 <- plotly::layout(fig2,title = paste("Prediction of Deaths - ",object[[4]]),
                             annotations = list(text = paste0(
                               "<b>Peak:</b> ", printDate(pred_summary$peak_date_med),
                               "<br><b>Peak 95% CI:</b> ",
                               paste0("(", printDate(pred_summary$peak_date_LB),", ",
                                      printDate(pred_summary$peak_date_UB), ")"),

                               "<br><b>Total number of ", metric_leg, ":</b> ",
                               round(pred_summary$total_cases_med,0), "<br>",
                               "<b>95% CI:</b> ",
                               paste0("(", round(pred_summary$total_cases_LB, 0),
                                      ", ", round(pred_summary$total_cases_UB, 0), ")"),
                               "<br><b>End (99%) of ",metric_leg,":</b> ",
                               printDate(pred_summary$end_date_med),
                               "<br><b>95% CI:</b> ",
                               paste0("(", printDate(pred_summary$end_date_LB),", ",
                                      printDate(pred_summary$end_date_UB),")" ),
                               "</span>"
                             ),x = 0.97, y = 0.97, xref = "paper", yref = "paper",
                             font = list(family = "Arial", size = 10), align = "right",
                             showarrow = FALSE),
                             xaxis = list(title = "date"),
                             yaxis = list(title = "New Deaths per Day"),
                             shapes = list(type = "line", opacity = 0.7, line = list(color = "black", width = 1),
                                           y0 = 0, y1 = 1,
                                           x0 = pred_summary$peak_date_LB, x1 = pred_summary$peak_date_LB)
      )

      fig2 <- plotly::add_trace(fig2,
                                x = c(dados$date[which(dados$date == last_date_n)],
                                      data$date[1:pred_time]),
                                y = c(dados$new_deaths[which(dados$date == last_date_n)],
                                      data$q2.5[1:pred_time]),
                                showlegend = F,
                                name = "95% CI",
                                type = 'scatter',
                                mode = 'lines', hoverinfo = "x+y",
                                #fill='tonexty',
                                #dash='solid',
                                line = list(color = 'rgba(0, 0, 0, 1)',width=0)
      )

      fig2 <- plotly::add_trace(fig2,
                                x = c(dados$date[which(dados$date == last_date_n)],
                                      data$date[1:pred_time]),
                                y = c(dados$new_deaths[which(dados$date == last_date_n)],
                                      data$q97.5[1:pred_time]),
                                type = 'scatter',
                                mode = 'lines', hoverinfo = "x+y",
                                fill = 'tonexty',
                                name = "95% CI",
                                # fillcolor = 'rgba(150, 150, 150, 0.5)',
                                fillcolor = 'rgba(100, 100, 100, 0.5)',
                                line = list(color = 'rgba(0, 0, 0, 1)', width = 0)
      )


      fig2 <- plotly::add_trace(fig2,
                                x = mu_plot$date, y = mu_plot$mu,
                                type = 'scatter', mode = 'lines', hoverinfo = "none",
                                name = ("Estimated Mean"),
                                line = list(color = 'rgb(230, 115, 0)', dash = 'solid',
                                            width=1.5) ## width = 2.5)
      )

      cat("Long term plot created successfully\n")
      long <- fig2
    }

    if(ncol(object[[2]]) < 1){
      message("There are no short term predictions")
      short = NULL
    } else {
      outputs <- pandemic_stats(object)

      dados <- outputs$data$data
      data <- outputs$ST_predict
      last_date_n <- min(data$date) - 1
      pred_time <- length(data$date)

      data_max <- as.Date(object[[3]][nrow(object[[3]]),1])

      fig2 <- plotly::add_trace(plotly::plot_ly(data),x = ~date, y = ~med, type= "scatter", name = "Prediction", mode = "markers",
                                marker=list(color='rgb(0,0,0)', dash='solid', width=2.5))

      fig2 <- plotly::add_trace(fig2,x = ~dados$date, y = ~dados$deaths, type = 'scatter', mode = 'lines+markers', hoverinfo = "x+y", name = "Observed Data", mode = "lines+markers",
                                hoverinfo = "x+y",
                                marker = list(
                                  color = 'rgb(100, 140, 240)',
                                  line = list(color = 'rgb(0, 0, 0)', width = 1)),
                                line = list(
                                  color = 'rgb(100, 140, 240)',
                                  width = 1.5)


      )

      fig2 <- plotly::layout(fig2,title = paste("Prediction of Deaths - ",object[[4]]),
                             xaxis = list(title = "date"),
                             yaxis = list(title = "Accumulated Deaths"),
                             shapes = list(type = "line", opacity = 0.7, line = list(color = "black", width = 1),
                                           y0 = 0, y1 = 1,
                                           x0 = data_max, x1 = data_max)
      )

      fig2 <- plotly::add_trace(fig2,
                                x = c(dados$date[which(dados$date == last_date_n)],
                                      data$date[1:pred_time]),
                                y = c(dados$deaths[which(dados$date == last_date_n)],
                                      data$q2.5[1:pred_time]),
                                showlegend = F,
                                name = "95% CI",
                                type = 'scatter',
                                mode = 'lines', hoverinfo = "x+y",
                                #fill='tonexty',
                                #dash='solid',
                                line = list(color = 'rgba(0, 0, 0, 1)',width=0)
      )

      fig2 <- plotly::add_trace(fig2,
                                x = c(dados$date[which(dados$date == last_date_n)],
                                      data$date[1:pred_time]),
                                y = c(dados$deaths[which(dados$date == last_date_n)],
                                      data$q97.5[1:pred_time]),
                                type = 'scatter',
                                mode = 'lines', hoverinfo = "x+y",
                                fill = 'tonexty',
                                name = "95% CI",
                                # fillcolor = 'rgba(150, 150, 150, 0.5)',
                                fillcolor = 'rgba(100, 100, 100, 0.5)',
                                line = list(color = 'rgba(0, 0, 0, 1)', width = 0)
      )

      cat("Short term plot created successfully\n")
      short <- fig2
    }

  }

  cat("The generated plots can be stored in a variable.\n")
  cat("Long term plot: variable$long\n")
  cat("Short term plot: variable$short\n")
  my_list <- list(long = long, short = short)

  class(my_list) = "plottedPandemic"
  return(my_list)
}
