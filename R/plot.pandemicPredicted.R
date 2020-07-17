#' Plot pandemic predictions
#'
#' S3 method that plots the predicted data into an interactive graphic.
#' @param object Output of function posterior_predict.stanpandemic.
#' @return A list containing two plots, long and short. If any of them wasn't plotted due to lack of prediction, its value returns NULL.
#' @seealso \code{\link{posterior_predict.pandemicEstimated}}.
#' @examples
#' \dontrun{
#' dataMG = load_covid("Brazil","MG")
#' estimMG = stan_pandemic(dataMG)
#' predMG = posterior_predict(estimMG)
#' plot(predMG)}
#' @importFrom plotly plot_ly
#' @importFrom plotly add_trace
#' @importFrom plotly layout
#' @export
plot.pandemicPredicted <- function(object){
  if(object[[5]] == "confirmed"){
    cat("Plotting confirmed cases\n")
    if(ncol(object[[1]]) < 1){
      message("There are no long term predictions")
      long = NULL
    } else {
      data_max <- as.Date(object[[3]][nrow(object[[3]]),1])

      q25  = apply(object[[1]], 2, quantile, .025)
      med  = apply(object[[1]], 2, median)
      q975 = apply(object[[1]], 2, quantile, .975)

      data <- cbind(q25,med,q975)

      dates <- as.Date(data_max) + 1
      data_a <- as.Date(data_max) + 1
      i = 1
      while(i < length(med)){
        data_new <- data_a + 1
        data_a <- as.Date(data_new)
        dates <- c(dates,data_new)
        i = i + 1
      }
      dates <- as.character(dates)
      data <- data.frame(dates,data)
      data$dates <- as.Date(data$dates)


      dados <- object[[3]]
      max_med <- max(med)
      high.dat.med <- data$dates[which(data$med == max_med)]
      metric_leg <- ifelse(object[[5]] == "confirmed","cases","deaths")
      total_number <- sum(dados$new_cases,data$med)
      total_number_25 <- sum(dados$new_cases,data$q25)
      total_number_975 <- sum(dados$new_cases,data$q975)


      fig2 <- plotly::add_trace(plotly::plot_ly(data),
                                x = ~dates, y = ~med, type= "scatter", name = "Prediction", mode = "markers",
                                marker=list(color='rgb(0,0,0)', dash='solid', width=2.5))

      fig2 <- plotly::add_trace(fig2,x = ~dados$date, y = ~dados$new_cases, type = 'scatter', mode = 'lines+markers', hoverinfo = "x+y", name = "Observed Data", mode = "lines+markers",
                  hoverinfo = "x+y",
                  marker = list(
                    color = 'rgb(100, 140, 240)',
                    line = list(color = 'rgb(0, 0, 0)', width = 1)),
                  line = list(
                    color = 'rgb(100, 140, 240)',
                    width = 1.5))

      fig2 <- plotly::layout(fig2,title = paste("Prediction of New Cases - ",object[[4]]),
                                      annotations = list(text = paste0(
                                        "<b>Peak:</b> ", high.dat.med,
                                        "<br><b>Total number of ", metric_leg, ":</b> ",
                                        round(total_number,0), "<br>",
                                        "<b>95% CI:</b> ",
                                        paste0("(", round(total_number_25, 0),
                                               ", ", round(total_number_975, 0), ")")
                                      ),x = 0.97, y = 0.97, xref = "paper", yref = "paper",
                                      font = list(family = "Arial", size = 10), align = "right",
                                      showarrow = FALSE),
                              xaxis = list(title = "date"),
                              yaxis = list(title = "New Cases per Day"),
                              shapes = list(type = "line", opacity = 0.7, line = list(color = "black", width = 1),
                                            y0 = 0, y1 = 1,
                                            x0 = data_max, x1 = data_max)
      )


      #fig2 <- fig2 %>% add_trace(x = ~data$dates, y = ~data$q25, type= "scatter", mode = "lines",showlegend = F,
      #                           fillcolor='rgba(150,150,150,0.5)',hoverinfo="x+y",
      #                           fill='tonexty',
      #                           line=list(color='rgb(76,153,0)', dash='solid', width=0))

      #fig2 <- fig2 %>% add_trace(x = ~data$dates, y = ~data$q975, type= "scatter", mode = "lines",showlegend = F,
      #                           fillcolor='rgba(150,150,150,0.5)',hoverinfo="x+y",
      #                           fill='tonexty',
      #                           line=list(color='rgb(76,153,0)', dash='solid', width=0))
      cat("Long term plot created successfully\n")
      long <- fig2
    }

    if(ncol(object[[2]]) < 1){
      message("There are no short term predictions")
      short = NULL
    } else {
      data_max <- as.Date(object[[3]][nrow(object[[3]]),1])

      q25  = apply(object[[2]],2 , quantile, .025)
      med  = apply(object[[2]], 2, median)
      q975 = apply(object[[2]],2 , quantile, .975)

      data <- cbind(q25,med,q975)

      dates <- as.Date(data_max) + 1
      data_a <- as.Date(data_max) + 1
      i = 1
      while(i < length(med)){
        data_new <- data_a + 1
        data_a <- as.Date(data_new)
        dates <- c(dates,data_new)
        i = i + 1
      }
      dates <- as.character(dates)
      data <- data.frame(dates,data)
      data$dates <- as.Date(data$dates)

      fig2 <- plotly::add_trace(plotly::plot_ly(data),x = ~dates, y = ~med, type= "scatter", name = "Prediction", mode = "markers",
                                               marker=list(color='rgb(0,0,0)', dash='solid', width=2.5))

      dados <- object[[3]]

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

      #fig2 <- fig2 %>% add_trace(x = ~data$dates, y = ~data$q25, type= "scatter", mode = "lines",showlegend = F,
      #                           fillcolor='rgba(150,150,150,0.5)',hoverinfo="x+y",
      #                           fill='tonexty',
      #                           line=list(color='rgb(76,153,0)', dash='solid', width=0))

      #fig2 <- fig2 %>% add_trace(x = ~data$dates, y = ~data$q975, type= "scatter", mode = "lines",showlegend = F,
      #                           fillcolor='rgba(150,150,150,0.5)',hoverinfo="x+y",
      #                           fill='tonexty',
      #                           line=list(color='rgb(76,153,0)', dash='solid', width=0))
      cat("Short term plot created successfully\n")
      short <- fig2
    }

  } else {
    cat("Plotting death cases\n")
    if(ncol(object[[1]]) < 1){
      message("There are no long term predictions")
      long = NULL
    } else {
      data_max <- as.Date(object[[3]][nrow(object[[3]]),1])

      q25  = apply(object[[1]], 2, quantile, .025)
      med  = apply(object[[1]], 2, median)
      q975 = apply(object[[1]], 2, quantile, .975)

      data <- cbind(q25,med,q975)

      dates <- as.Date(data_max) + 1
      data_a <- as.Date(data_max) + 1
      i = 1
      while(i < length(med)){
        data_new <- data_a + 1
        data_a <- as.Date(data_new)
        dates <- c(dates,data_new)
        i = i + 1
      }
      dates <- as.character(dates)
      data <- data.frame(dates,data)
      data$dates <- as.Date(data$dates)

      fig2 <- plotly::add_trace(plotly::plot_ly(data),x = ~dates, y = ~med, type= "scatter", name = "Prediction", mode = "markers",
                                               marker=list(color='rgb(0,0,0)', dash='solid', width=2.5))

      dados <- object[[3]]
      max_med <- max(med)
      high.dat.med <- data$dates[which(data$med == max_med)]
      metric_leg <- ifelse(object[[5]] == "confirmed","cases","deaths")
      total_number <- sum(dados$new_deaths,data$med)
      total_number_25 <- sum(dados$new_deaths,data$q25)
      total_number_975 <- sum(dados$new_deaths,data$q975)

      fig2 <- plotly::add_trace(fig2,x = ~dados$date, y = ~dados$new_deaths, type = 'scatter', mode = 'lines+markers', hoverinfo = "x+y", name = "Observed Data", mode = "lines+markers",
                  hoverinfo = "x+y",
                  marker = list(
                    color = 'rgb(100, 140, 240)',
                    line = list(color = 'rgb(0, 0, 0)', width = 1)),
                  line = list(
                    color = 'rgb(100, 140, 240)',
                    width = 1.5))

      fig2 <- plotly::layout(fig2,title = paste("Prediction of Deaths - ",object[[4]]),
                                      annotations = list(text = paste0(
                                        "<b>Peak:</b> ", high.dat.med,
                                        "<br><b>Total number of ", metric_leg, ":</b> ",
                                        round(total_number,0), "<br>",
                                        "<b>95% CI:</b> ",
                                        paste0("(", round(total_number_25, 0),
                                               ", ", round(total_number_975, 0), ")")
                                      ),x = 0.97, y = 0.97, xref = "paper", yref = "paper",
                                      font = list(family = "Arial", size = 10), align = "right",
                                      showarrow = FALSE),
                              xaxis = list(title = "date"),
                              yaxis = list(title = "Deaths per Day"),
                              shapes = list(type = "line", opacity = 0.7, line = list(color = "black", width = 1),
                                            y0 = 0, y1 = 1,
                                            x0 = data_max, x1 = data_max)
      )

      #fig2 <- fig2 %>% add_trace(x = ~data$dates, y = ~data$q25, type= "scatter", mode = "lines",showlegend = F,
      #                           fillcolor='rgba(150,150,150,0.5)',hoverinfo="x+y",
      #                           fill='tonexty',
      #                           line=list(color='rgb(76,153,0)', dash='solid', width=0))

      #fig2 <- fig2 %>% add_trace(x = ~data$dates, y = ~data$q975, type= "scatter", mode = "lines",showlegend = F,
      #                           fillcolor='rgba(150,150,150,0.5)',hoverinfo="x+y",
      #                           fill='tonexty',
      #                           line=list(color='rgb(76,153,0)', dash='solid', width=0))
      cat("Long term plot created successfully\n")
      long <- fig2
    }

    if(ncol(object[[2]]) < 1){
      message("There are no short term predictions")
      short = NULL
    } else {
      data_max <- as.Date(object[[3]][nrow(object[[3]]),1])

      q25  = apply(object[[2]], 2, quantile, .025)
      med  = apply(object[[2]], 2, median)
      q975 = apply(object[[2]], 2, quantile, .975)

      data <- cbind(q25,med,q975)

      dates <- as.Date(data_max) + 1
      data_a <- as.Date(data_max) + 1
      i = 1
      while(i < length(med)){
        data_new <- data_a + 1
        data_a <- as.Date(data_new)
        dates <- c(dates,data_new)
        i = i + 1
      }
      dates <- as.character(dates)
      data <- data.frame(dates,data)
      data$dates <- as.Date(data$dates)

      fig2 <- plotly::add_trace(plotly::plot_ly(data),x = ~dates, y = ~med, type= "scatter", name = "Prediction", mode = "markers",
                                               marker=list(color='rgb(0,0,0)', dash='solid', width=2.5))

      dados <- object[[3]]

      fig2 <- plotly::add_trace(fig2,x = ~dados$date, y = ~dados$deaths, type = 'scatter', mode = 'lines+markers', hoverinfo = "x+y", name = "Observed Data", mode = "lines+markers",
                  hoverinfo = "x+y",
                  marker = list(
                    color = 'rgb(100, 140, 240)',
                    line = list(color = 'rgb(0, 0, 0)', width = 1)),
                  line = list(
                    color = 'rgb(100, 140, 240)',
                    width = 1.5))

      fig2 <- plotly::layout(fig2,title = paste("Prediction of Deaths - ",object[[4]]),
                              xaxis = list(title = "date"),
                              yaxis = list(title = "Accumulated Deaths"),
                              shapes = list(type = "line", opacity = 0.7, line = list(color = "black", width = 1),
                                            y0 = 0, y1 = 1,
                                            x0 = data_max, x1 = data_max)
      )

      #fig2 <- fig2 %>% add_trace(x = ~data$dates, y = ~data$q25, type= "scatter", mode = "lines",showlegend = F,
      #                           fillcolor='rgba(150,150,150,0.5)',hoverinfo="x+y",
      #                           fill='tonexty',
      #                           line=list(color='rgb(76,153,0)', dash='solid', width=0))

      #fig2 <- fig2 %>% add_trace(x = ~data$dates, y = ~data$q975, type= "scatter", mode = "lines",showlegend = F,
      #                           fillcolor='rgba(150,150,150,0.5)',hoverinfo="x+y",
      #                           fill='tonexty',
      #                           line=list(color='rgb(76,153,0)', dash='solid', width=0))
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
