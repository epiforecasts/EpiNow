#' Fit the negative binomial version of EpiEstim
#'
#' @param nowcast_dir The directory of the nowcast to fit to
#'
#' @return
#' @export
#' @importFrom rstan sampling extract
#'
#' @examples
#' 
#'   intervals <- EpiNow::covid_generation_times
#'   nowcast <- readRDS(nowcast_dir)[type %in% "infection_upscaled"][, type := NULL]
#'   nowcast <- nowcast[, sample := as.numeric(sample)]
#'   nowcast <- nowcast[sample < 10]
#'   rt_prior <- list(mean = 2.6, sd = 2)
estimate_R0_stan <- function(nowcast, intervals, rt_prior) {
  

  ## Make sure there are no missing dates
  nowcast_grid <- data.table::copy(nowcast)[,
       .(date = seq(min(date), max(date), by = "days"),
         sample = list(1:max(sample)), 
         import_status = list(list("local", "imported")))][,
       .(sample = unlist(sample), import_status), by = c("date")][,
       .(import_status = unlist(import_status)), by = c("date", "sample")]
  
  nowcast <-  merge(nowcast,nowcast_grid, 
                    by = c("date", "sample", "import_status"), all.y = TRUE)
  
  nowcast <-  nowcast[is.na(cases), cases := 0 ][,
                      .(sample = as.numeric(sample), date = date, 
                        cases, import_status)]
  nowcast <- data.table::setorder(nowcast, import_status, sample, date)
  
  ## Split into local and imported cases
  local_cases <- nowcast[import_status == "local"][, import_status := NULL]
  imported_cases <- nowcast[import_status == "imported"][, import_status := NULL]
  
  ## Define model parameters
  data <- list(t = length(unique(local_cases$date)), # Length of time series
               k = length(unique(local_cases$sample)),
               window = 7, # R estimation window
               r_mean  = rt_prior$mean, # Mean of R prior
               r_sd = rt_prior$sd) # SD of R prior
  
  ## Sample supplied interval distributions with replacement
  interval_indexes <- sample(1:ncol(intervals), data$k, replace = TRUE)
  data$intervals <- intervals[, interval_indexes]

  ## Set the length of the supplied interval
  data$n <- dim(intervals)[1]
  
  ## Format local cases as a matrix
  data$obs_local <- matrix(local_cases$cases,
                           byrow = FALSE,
                           nrow = data$t,
                           ncol = data$k)

  
  ## Format imported cases as a matrix
  data$obs_imported <- matrix(imported_cases$cases,
                              byrow = FALSE,
                              nrow = data$t,
                              ncol = data$k)



  init_fun <- function(){list(R = rgamma(n = data$t, 
                                          shape = (rt_prior$mean / rt_prior$sd)^2, 
                                          scale = (rt_prior$sd^2) / rt_prior$mean),
                              phi = runif(1, 0, 1))}
  
  ## Load the stan model used for estimation
  model <- rstan::stan_model("inst/stan/estimateR.stan")
  
  message(paste0("Running for ",data$k," SI & nowcast samples"))
  message(paste0("and ",data$t," time steps..."))
  
  fit <- rstan::sampling(model,
                         data = data,
                         init = init_fun,
                         chains = 4,
                         iter = 2000, 
                         cores = 4,
                         refresh = 50)
  
  samples <- rstan::extract(fit)
  
  
  return(samples)
}