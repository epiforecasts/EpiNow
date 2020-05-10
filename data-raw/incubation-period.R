library(data.table)

## Parameters values sourced from: 
## * https://annals.org/aim/fullarticle/2762808/incubation-period-coronavirus-disease-2019-covid-19-from-publicly-reported

covid_incubation_period <- data.table(
  as_reported = "5.06 (log SD 0.418)",
  mean = 1.621, mean_sd = 0.0640, 
  sd = 0.418, sd_sd = 0.0691,
  source = "lauer"
)

usethis::use_data(covid_incubation_period, overwrite = TRUE)

