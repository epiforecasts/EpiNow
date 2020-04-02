#' Fit an integer adjusted exponential or gamma distribution
#'
#'
#' @param delays Numeric vector of reporting delays
#' @param samples Numeric, number of samples to take
#' @param dist Character string, which distribution to fit. Defaults to exponential (`"exp"`) but
#' gamma is also supported (`"gamma"`).
#' @return
#' @export
#' @import Rcpp
#' @import methods
#' @importFrom rstan sampling extract
#' @useDynLib EpiNow, .registration=TRUE
#' @examples
#'
dist_fit <- function(delays = NULL, samples = NULL, dist = "exp") {

  if (is.null(samples)) {
    samples <- 1000
  }

  if (samples < 1000) {
    samples <- 1000
  }

  ## Model parameters
  lows <- delays - 1
  lows <- ifelse(lows <=0, 1e-6, lows)
  ups <- delays + 1

  data <- list(N = length(delays),
               low = lows,
               up = ups,
               iter = samples + 1000,
               warmup = 1000)

  if (dist %in% "exp") {
    model <- stanmodels$exp_fit
    data <- c(data, lam_mean = mean(delays))

  }else if (dist %in% "gamma") {
    model <- stanmodels$gamma_fit
  }
  
  ## Set adapt delta based on the sample size
  if (length(delays) <= 30) {
    adapt_delta <- 0.999
  } else {
    adapt_delta <- 0.9
  }

  ## Fit model
  fit <- rstan::sampling(
    model,
    data = data,
    control = list(adapt_delta = adapt_delta),
    chains = 2,
    cores = 2,
    refresh = 0)


  return(fit)
}
