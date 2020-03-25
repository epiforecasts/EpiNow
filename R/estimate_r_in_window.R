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
#' @importFrom purrr map_dfr
#' @importFrom dplyr mutate group_by summarise ungroup mutate_if
#' @importFrom tidyr unnest
#' @importFrom data.table setDT
#'
#' @examples
#'
#'
estimate_r_in_window <- function(onsets = NULL,
                                 min_time = NULL,
                                 max_time = NULL,
                                 bootstrap_samples = 1000) {
  r <- onsets %>%
    purrr::map_dfr(
      ~ EpiNow::estimate_little_r(.,
                                             min_time = min_time,
                                             max_time = max_time) %>%
        dplyr::mutate(sampled_r = list(
          stats::rnorm(bootstrap_samples, r, sd))
        ),
      .id = "sample"
    )

  ## Summarise r
  r <- tidyr::unnest(r, "sampled_r")

  summarise_r <- r

  summarise_r <- data.table::setDT(summarise_r)[,.(
    bottom  = purrr::map_dbl(list(HDInterval::hdi(sampled_r, credMass = 0.9)), ~ .[[1]]),
    top = purrr::map_dbl(list(HDInterval::hdi(sampled_r, credMass = 0.9)), ~ .[[2]]),
    lower  = purrr::map_dbl(list(HDInterval::hdi(sampled_r, credMass = 0.5)), ~ .[[1]]),
    upper = purrr::map_dbl(list(HDInterval::hdi(sampled_r, credMass = 0.5)), ~ .[[2]]),
    median = median(sampled_r, na.rm = TRUE))
    ]


  ## Summarise doubling time
  summarise_doubling <- summarise_r %>%
    dplyr::mutate_if(is.numeric, estimate_doubling_time)

  ## Flip credible intervals
  summarise_doubling <- summarise_doubling %>%
    dplyr::mutate(
      bottom = summarise_doubling$top,
      top = summarise_doubling$bottom,
      lower = summarise_doubling$upper,
      upper = summarise_doubling$lower,
    ) %>%
    dplyr::mutate_at(.vars = c("bottom", "lower", "median", "upper", 'top'),
                     ~ ifelse(. < 0, Inf, .))

  ## Sumamrise goodness of fit
  summarise_fit <- r

  summarise_fit <- data.table::setDT(summarise_fit)[,.(
    bottom = quantile(fit_meas, 0.025, na.rm = TRUE),
    top = quantile(fit_meas, 0.975, na.rm = TRUE),
    lower = quantile(fit_meas, 0.25, na.rm = TRUE),
    upper = quantile(fit_meas, 0.75, na.rm = TRUE),
    median = median(fit_meas, na.rm = TRUE))
    ]

  out <- list(summarise_r, summarise_doubling, summarise_fit)

  names(out) <- c("little_r", "doubling_time", "goodness_of_fit")

  return(out)
}
