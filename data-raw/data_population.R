# Saves dataset with population size for each country and the list
# of countries available to model using PandemicLP

#data set for country's population
country_pop <- utils::read.csv("data-raw/pop_WR.csv")

#data set with the list of countries
covid <- covid19br::downloadCovid19(level="world")

countries <- unique(covid$country)
countries <- countries[-which(countries %in% c("Diamond Princess",
                      "MS Zaandam","Summer Olympics 2020", "Vatican",
                      "Taiwan","Palestine"))]
countries <- sort(countries)

#generate R\sysdata.rda file
usethis::use_data(country_pop, countries, internal = TRUE, overwrite = TRUE)
