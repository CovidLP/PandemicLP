## ----setup, include = FALSE---------------------------------------------------
library(pandemicLP)
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## -----------------------------------------------------------------------------
MGdata = load_covid("Brazil","MG")

## -----------------------------------------------------------------------------
MGestimated = pandemic_model(MGdata, case_type = "deaths", covidLPconfig = TRUE)

## -----------------------------------------------------------------------------
MGestimated

## ---- fig.asp=2/(sqrt(5)+1), fig.align='center', fig.width=4------------------
traceplot(MGestimated)+theme(legend.position = "")
rstan::stan_dens(MGestimated$fit,pars=c("a","b","c","f"))

## -----------------------------------------------------------------------------
MGpredicted = posterior_predict(MGestimated,horizonLong=200)
MGpredicted

## ---- fig.asp=2/(sqrt(5)+1), fig.width=8, fig.align='center', warnings = FALSE----
MGplots = plot(MGpredicted)
MGplots$long
MGplots$short

