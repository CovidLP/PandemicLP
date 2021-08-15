## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(PandemicLP)

## ---- fig.asp=2/(sqrt(5)+1), fig.width=8, fig.align='center', warnings = FALSE----
MGdata <- load_covid("Brazil","MG")
plot(MGdata)$new

