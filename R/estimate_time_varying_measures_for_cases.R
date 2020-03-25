
#' Estimate time-varying measures for cases
#'
#' @param start_rate_of_spread_est A character string in the form of a date ("2020-01-01")
#' @inheritParams estimate_R0
#' @return
#' @export
#'
#' @importFrom tidyr nest
#' @importFrom dplyr mutate everything select filter
#' @importFrom purrr map_dbl
#' @importFrom HDInterval hdi
#' @examples
#'
#'
estimate_time_varying_measures_for_cases <- function(cases = NULL,
                                                     serial_intervals = NULL,
                                                     si_samples = NULL, rt_samples = NULL,
                                                     start_rate_of_spread_est = NULL,
                                                     window = NULL, rt_prior = NULL){
  ## Estimate time-varying R0
  message("Estimate time-varying R0")
  R0_estimates <- cases %>%
    EpiNow::estimate_R0(serial_intervals = serial_intervals,
                                   si_samples = si_samples, rt_samples = rt_samples,
                                   window = window, rt_prior = rt_prior) %>%
    tidyr::unnest(R) %>%
    dplyr::group_by(date) %>%
    dplyr::summarise(
      bottom  = purrr::map_dbl(list(HDInterval::hdi(R, credMass = 0.9)), ~ .[[1]]),
      top = purrr::map_dbl(list(HDInterval::hdi(R, credMass = 0.9)), ~ .[[2]]),
      lower  = purrr::map_dbl(list(HDInterval::hdi(R, credMass = 0.5)), ~ .[[1]]),
      upper = purrr::map_dbl(list(HDInterval::hdi(R, credMass = 0.5)), ~ .[[2]]),
      median = median(R, na.rm = TRUE),
      mean = mean(R, na.rm = TRUE),
      std = sd(R, na.rm = TRUE),
      prob_control = sum(R < 1) / dplyr::n()) %>%
    dplyr::ungroup()

  ## Estimate time-varying little r
  message("Estimate time-varying rate of growth")


  if (!is.null(start_rate_of_spread_est)) {
    little_r_estimates <- cases %>%
      dplyr::filter(date >= as.Date(start_rate_of_spread_est))
  }else{
    little_r_estimates <- cases
  }

  little_r_estimates <- little_r_estimates %>%
    group_by(date) %>%
    dplyr::summarise(cases = sum(cases, na.rm  = TRUE)) %>%
    dplyr::ungroup() %>%
    tidyr::nest(data = dplyr::everything()) %>%
    dplyr::mutate(overall_little_r = list(EpiNow::estimate_r_in_window(data)),
                  time_varying_r = list(EpiNow::estimate_time_varying_r(data,
                                                                                   window = window)
                  )) %>%
    dplyr::select(-data)


  out <- list(R0_estimates, little_r_estimates)

  return(out)
}
