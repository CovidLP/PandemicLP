# Saves dataset with population size for each country and each brazilian state
# https://github.com/thaispaiva/app_COVID19/tree/master/R/pop

popURL = "https://raw.githubusercontent.com/thaispaiva/app_COVID19/master/R/pop"

country_pop <- read.csv(file.path(popURL,"pop_WR.csv"))
br_pop <- read.csv(file.path(popURL,"pop_BR.csv"))

usethis::use_data(country_pop, br_pop, internal = TRUE, overwrite = TRUE)
