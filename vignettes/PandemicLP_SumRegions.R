## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(PandemicLP)
library(stats)

## -----------------------------------------------------------------------------
regions <- c("PR", "SC","RS")
last_date <- "2020-10-01"
case_type <- "deaths"

