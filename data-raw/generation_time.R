library(data.table)
library(magrittr)

## We use the method outlined here: https://www.eurosurveillance.org/content/10.2807/1560-7917.ES.2020.25.17.2000257;jsessionid=rInK19BuiVTvCRbDk5xILdmk.i-0b3d9850f4681504f-ecdclive
## to estimate the generation time based on the incubation time estimated 
## here: https://annals.org/aim/fullarticle/2762808/incubation-period-coronavirus-disease-2019-covid-19-from-publicly-reported
## Code for this estimation process is available here: https://github.com/seabbs/COVID19

## Load raw MCMC output
gi <- data.table::setDT(readRDS("data-raw/gi.rds"))
## Check mean and standard deviation
gi[, .(mean = median(mean), mean_sd = sd(mean), 
       sd = median(sd), sd_sd = sd(sd))]


##Generic mean gamma sampler
mean_rgamma <- function(samples, mean, sd) {
  theta <- sd^2/mean
  k <- mean/theta
  samples <- stats::rgamma(samples, shape = k, scale = theta)
  
  return(samples)
}

samples <- purrr::map2(gi$mean, gi$sd, ~ mean_rgamma(1000, .x, .y)) %>%
  purrr::map( ~ c(0, round(.) %>%
                    table %>%
                    {. / sum(.)}))


max_sample_length <- samples %>%
  purrr::map_dbl(length) %>%
  max()

pad_samples <- samples %>%
  purrr::map(~ c(., rep(0, max_sample_length - length(.))))

covid_generation_time <- matrix(unlist(pad_samples), ncol = length(pad_samples))

usethis::use_data(covid_generation_time, overwrite = TRUE)


covid_generation_time <- 

usethis::use_data(covid_generation_time, overwrite = TRUE)

