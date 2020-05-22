#' Fit the negative binomial version of EpiEstim
#'
#' @param nowcast_dir The directory of the nowcast to fit to
#' @return
#' @export
#' @importFrom rstan sampling extract
#' @importFrom data.table copy merge.data.table setorder
#' @examples
#'   ## Path to a nowcast
#'   nowcast_dir <- "../covid-global/national/Belgium/latest/nowcast.rds"
#'   intervals <- EpiNow::covid_generation_times
#'   nowcast <- readRDS(nowcast_dir)[type %in% "infection_upscaled"][, type := NULL]
#'   nowcast <- nowcast[, sample := as.numeric(sample)]
#'   nowcast <- nowcast[sample < 101]
#'   rt_prior <- list(mean = 1, sd = 0.5)
#'   ## mean = 0 -> exp(1) prior otherwise truncated normal
#'   disp_prior <- list(mean = 0, sd = 0.1)
#'   model <- "poisson"
#'   min_cases <- 10
#'   windows <- c(1, 7, 14)
#'   verbose <- TRUE
estimate_R0_stan <- function(nowcast, intervals, rt_prior, model = "poisson",
                             min_cases, verbose = FALSE) {
  

  ## Make sure there are no missing dates
  nowcast_grid <- data.table::copy(nowcast)[,
       .(date = seq(min(date), max(date), by = "days"),
         sample = list(unique(sample)), 
         import_status = list(list("local", "imported")))][,
       .(sample = unlist(sample), import_status), by = c("date")][,
       .(import_status = unlist(import_status)), by = c("date", "sample")]
  
  nowcast <-  data.table::merge.data.table(
    nowcast,nowcast_grid, 
    by = c("date", "import_status", "sample"), all.y = TRUE)
  
  nowcast <-  nowcast[is.na(cases), cases := 0 ][,
                      .(sample = as.numeric(sample), date = date, 
                        cases, import_status)]
  nowcast <- data.table::setorder(nowcast, import_status, sample, date)
  
  ## Split into local and imported cases
  local_cases <- nowcast[import_status == "local"][, import_status := NULL]
  imported_cases <- nowcast[import_status == "imported"][, import_status := NULL]
  
  
  ## Define the wait time based on the minimum case thresold
  wait_time <- data.table::copy(local_cases)[, time := 1:.N - 1, by = sample]
  wait_time <- unique(wait_time[cases >= min_cases][date == min(date)]$time)
  wait_time <- ifelse(wait_time < max(windows), max(windows), wait_time)
  
  ## Define model parameters
  data <- list(t = length(unique(local_cases$date)), # Length of time series
               k = length(unique(local_cases$sample)),
               w = length(windows),
               wait_time = wait_time,
               windows = array(sort(windows)), # R estimation window
               r_mean  = rt_prior$mean, # Mean of R prior
               r_sd = rt_prior$sd,
               phi_mean  = disp_prior$mean, # Mean of R prior
               phi_sd = disp_prior$sd) # SD of R prior) # SD of R prior
  
  ## Set model to poisson or negative binomial
  if (model %in% "poisson") {
    data$model_type <- 1
  }else if (model %in% "negbin"){
    data$model_type <- 2
  }
  
  ## Sample supplied interval distributions with replacement
  interval_indexes <- sample(1:ncol(intervals), data$k, replace = TRUE)
  data$intervals <- as.matrix(intervals[, interval_indexes])
                           

  ## Set the length of the supplied interval
  data$n <- nrow(data$intervals)
  
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

  ## Initialise within the prior on R and with low overdispersion
  init_fun <- function(){list(R = array(rep(rgamma(n = data$t - wait_time, 
                                                    shape = (rt_prior$mean / rt_prior$sd)^2, 
                                                    scale = (rt_prior$sd^2) / rt_prior$mean),
                                             data$w),dim = c(data$w, data$t - wait_time)),
                             phi = rexp(1, 1))}
  
  ## Load the stan model used for estimation
  estimateR <- rstan::stan_model("inst/stan/estimateR.stan")
  
  if (verbose) {
    message(paste0("Running for ",data$k," samples and ", data$t - wait_time," time steps..."))
  }

  
  fit <- rstan::sampling(estimateR,
                         data = data,
                         init = init_fun,
                         chains = 4,
                         iter = 2000, 
                         cores = 4,
                         refresh = ifelse(verbose, 50, 0))
  
  samples <- rstan::extract(fit)
  
  return(list(fit, samples))
}