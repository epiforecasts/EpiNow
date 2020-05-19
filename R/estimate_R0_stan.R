#' Fit the negative binomial version of EpiEstim
#'
#' @param nowcast_dir The directory of the nowcast to fit to
#'
#' @return
#' @export
#' @importFrom rstan sampling extract
#' @importFrom data.table copy merge.data.table setorder
#' @examples
#'   ## Path to a nowcast
#'   nowcast_dir <- "../national/France/latest/nowcast.rds"
#'   intervals <- EpiNow::covid_generation_times
#'   nowcast <- readRDS(nowcast_dir)[type %in% "infection_upscaled"][, type := NULL]
#'   nowcast <- nowcast[, sample := as.numeric(sample)]
#'   nowcast <- nowcast[sample < 21]
#'   rt_prior <- list(mean = 2.6, sd = 2)
#'   windows <- c(2, 7)
estimate_R0_stan <- function(nowcast, intervals, rt_prior, verbose = FALSE) {
  

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
  
  ## Define model parameters
  data <- list(t = length(unique(local_cases$date)), # Length of time series
               k = length(unique(local_cases$sample)),
               w = length(windows),
               windows = array(windows), # R estimation window
               r_mean  = rt_prior$mean, # Mean of R prior
               r_sd = rt_prior$sd) # SD of R prior
  
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
  init_fun <- function(){list(R = array(rep(rgamma(n = data$t, 
                                                    shape = (rt_prior$mean / rt_prior$sd)^2, 
                                                    scale = (rt_prior$sd^2) / rt_prior$mean),
                                             data$k * data$w),dim = c(data$k, data$w, data$t)),
                              phi = array(rexp(data$k, 1)))}
  
  ## Load the stan model used for estimation
  model <- rstan::stan_model("inst/stan/estimateR.stan")
  
  if (verbose) {
    message(paste0("Running for ",data$k," samples"))
    message(paste0("and ", data$t," time steps..."))
  }

  
  fit <- rstan::sampling(model,
                         data = data,
                         init = init_fun,
                         chains = 4,
                         iter = 2000, 
                         cores = 4,
                         refresh = ifelse(verbose, 50, 0))
  
  samples <- rstan::extract(fit)
  
  # out <- data.table::data.table(
  #   date = seq(min(local_cases$date), 
  #              max(local_cases$date), by = "days"),
  #   R = purrr::map(dim(sample$R)
  # )
  # R <- samples$R[, 1, ]
  # 
  # phi <- 
  
  return(list(fit, samples))
}