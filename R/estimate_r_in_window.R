#' Estimate r in a set time window
#'
#' @param onsets A list of samples datasets nested within the dataset sampled from.
#' @param min_time Numeric, the minimum time to fit the model to.
#' @param max_time Numeric, the maximum time to fit the model to.
#' @param bootstrap_samples Numeric, defaults to 1000. The number of samples to take when
#' bootstrapping little r to account for model uncertainty.
#'
#' @return A list of 3 dataframes containing estimates for little r, doubling time and
#' model goodness of fit.
#' @export
#' @importFrom purrr map map_dbl
#' @importFrom HDInterval hdi
#' @importFrom data.table setDTthreads rbindlist copy
#'
#' @examples
#'
#'
estimate_r_in_window <- function(onsets = NULL,
                                 min_time = NULL,
                                 max_time = NULL,
                                 bootstrap_samples = 1000) {
  
  data.table::setDTthreads(1)
  
  r <- 
    purrr::map(
      onsets, 
      ~ EpiNow::estimate_little_r(.,
                                  min_time = min_time,
                                  max_time = max_time)[,
            sampled_r := list(stats::rnorm(bootstrap_samples, r, sd))]
        )
  
  r <- data.table::rbindlist(r, idcol = "sample")

  ## Summarise r
  r <- r[, .(sampled_r = unlist(sampled_r)), 
         by = c("sample", "r", "sd", "fit_meas")]
    

  summarise_r <- data.table::copy(r)[,.(
    bottom  = purrr::map_dbl(list(HDInterval::hdi(sampled_r, credMass = 0.9)), ~ .[[1]]),
    top = purrr::map_dbl(list(HDInterval::hdi(sampled_r, credMass = 0.9)), ~ .[[2]]),
    lower  = purrr::map_dbl(list(HDInterval::hdi(sampled_r, credMass = 0.5)), ~ .[[1]]),
    upper = purrr::map_dbl(list(HDInterval::hdi(sampled_r, credMass = 0.5)), ~ .[[2]]),
    mean = mean(sampled_r, na.rm = TRUE),
    median = median(sampled_r, na.rm = TRUE))
    ]


  ## Summarise doubling time
  cols <- colnames(summarise_r)
  summarise_doubling <- data.table::copy(summarise_r)[, 
                                    (cols) := lapply(.SD,EpiNow::estimate_doubling_time),
                                    .SDcols = cols] 

  ## Flip credible intervals
  summarise_doubling <- summarise_doubling[,
    .(bottom = top, top = bottom, lower = upper,
      upper = lower, mean, median)]
    
  ## Sumamrise goodness of fit
  summarise_fit <- r[,.(
    bottom = purrr::map_dbl(list(HDInterval::hdi(fit_meas, 
                                                 credMass = 0.9)), ~ .[[1]]),
    top = purrr::map_dbl(list(HDInterval::hdi(fit_meas, 
                                              credMass = 0.9)), ~ .[[2]]),
    lower = purrr::map_dbl(list(HDInterval::hdi(fit_meas, 
                                                credMass = 0.5)), ~ .[[1]]),
    upper = purrr::map_dbl(list(HDInterval::hdi(fit_meas, 
                                                credMass = 0.5)), ~ .[[2]]),
    mean = mean(fit_meas, na.rm = TRUE),
    median = median(fit_meas, na.rm = TRUE))
    ]

  out <- list(summarise_r, summarise_doubling, summarise_fit)

  names(out) <- c("little_r", "doubling_time", "goodness_of_fit")

  return(out)
}
