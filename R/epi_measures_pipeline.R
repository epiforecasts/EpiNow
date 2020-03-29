#' Estimate time-varying measures and forecast
#'
#' @param nowcast A nowcast as produced by `nowcast_pipeline`
#' @param rt_windows Numeric vector, windows over which to estimate time-varying R. The best performing window will be 
#' selected per serial interval sample by default (based on which window best forecasts current cases).
#' @param rate_window Numeric, the window to use to estimate the rate of spread.
#' @inheritParams estimate_R0
#' @return
#' @export
#' @importFrom tidyr gather nest unnest drop_na
#' @importFrom dplyr filter group_by ungroup mutate select summarise n group_split bind_rows arrange
#' @importFrom purrr safely compact map_dbl map pmap transpose
#' @importFrom HDInterval hdi
#' @importFrom furrr future_map
#' @importFrom data.table setDT
#' @examples 
#'
epi_measures_pipeline <- function(nowcast = NULL,
                                                       serial_intervals = NULL,
                                                       min_est_date = NULL,
                                                       si_samples = NULL, rt_samples = NULL,
                                                       rt_windows = 7, rate_window = 7,
                                                       rt_prior = NULL, forecast_model = NULL,
                                                       horizon = NULL) {

  ## Estimate time-varying R0
  safe_R0 <- purrr::safely(EpiNow::estimate_R0)

  message("Estimate time-varying R0")
  data_list <-  dplyr::group_split(nowcast, type, sample, keep = TRUE)


  estimates <- furrr::future_map(data_list, function(data) {
    estimates <- safe_R0(cases = data,
            serial_intervals = serial_intervals,
            rt_prior = rt_prior,
            si_samples = si_samples,
            rt_samples = rt_samples,
            windows = rt_windows,
            min_est_date = min_est_date, 
            forecast_model = forecast_model,
            horizon = horizon)[[1]]

    if (!is.null(estimates$rts)) {
     estimates$rts <-  dplyr::mutate(estimates$rts[[1]], type = data$type[1],
                                     sample = data$sample[1])
    }
    
    if (!is.null(estimates$cases)) {
      estimates$cases <-  dplyr::mutate(estimates$cases[[1]], type = data$type[1],
                                      sample = data$sample[1])
    }
    
    return(estimates)
    }, .progress = TRUE)
  
  ## Transpose list ordering
  estimates <- purrr::transpose(estimates)
  
  ## Clean up NULL rt estimates and bind together
  R0_estimates <- purrr::compact(estimates$rts)
  R0_estimates <- dplyr::bind_rows(R0_estimates)

  
  ## Generic HDI return function
  return_hdi <- function(vect = NULL, mass = NULL, index = NULL) {
    as.numeric(purrr::map_dbl(list(HDInterval::hdi(vect, credMass = mass)), ~ .[[index]]))
  }

  message("Summarising time-varying R0")

  R0_estimates_sum <- data.table::setDT(R0_estimates)[, .(
    bottom  = return_hdi(R, 0.9, 1),
    top = return_hdi(R, 0.9, 2),
    lower  = return_hdi(R, 0.5, 1),
    upper = return_hdi(R, 0.5, 2),
    median = median(R, na.rm = TRUE),
    mean = mean(R, na.rm = TRUE),
    std = sd(R, na.rm = TRUE),
    prob_control = (sum(R < 1) / .N),
    mean_window = mean(window), 
    sd_window = sd(window)),
    by = .(type, date, rt_type)
    ][, R0_range := purrr::pmap(
      list(mean, bottom, top),
      function(mean, bottom, top) {
        list(point = mean,
             lower = bottom, 
             upper = top)
      }),]


  R0_estimates_sum <- dplyr::arrange(R0_estimates_sum, date)

  
  message("Summarising forecast cases")
  
  if (!is.null(estimates$cases)) {
    
    ## Clean up case forecasts
    cases_forecast <- purrr::compact(estimates$cases)
    cases_forecast <- dplyr::bind_rows(cases_forecast)
    
    ## Summarise case forecasts
    sum_cases_forecast <- data.table::setDT(cases_forecast)[, .(
      bottom  = return_hdi(cases, 0.9, 1),
      top = return_hdi(cases, 0.9, 2),
      lower  = return_hdi(cases, 0.5, 1),
      upper = return_hdi(cases, 0.5, 2),
      median = median(cases, na.rm = TRUE),
      mean = mean(cases, na.rm = TRUE),
      std = sd(cases, na.rm = TRUE)),
      by = .(type, date, rt_type)
      ][, range := purrr::pmap(
        list(mean, bottom, top),
        function(mean, bottom, top) {
          list(point = mean,
               lower = bottom, 
               upper = top)
        }),]
    
    sum_cases_forecast <- dplyr::arrange(sum_cases_forecast, date)
  }

  ## Estimate time-varying little r
  message("Estimate time-varying rate of growth")

  if (!is.null(min_est_date)) {
    little_r_estimates <- nowcast %>%
      dplyr::filter(date >= (min_est_date - lubridate::days(rate_window)))
  }else{
    little_r_estimates <- nowcast
  }

  ## Sum across cases and imports
  little_r_estimates <- little_r_estimates %>%
    group_by(type, sample, date) %>%
    dplyr::summarise(cases = sum(cases, na.rm  = TRUE)) %>%
    dplyr::ungroup() %>%
    tidyr::drop_na()

  ## Nest by type and sample then split by type only
  little_r_estimates_list <- little_r_estimates %>%
    dplyr::group_by(type, sample) %>%
    tidyr::nest() %>%
    dplyr::ungroup() %>%
    dplyr::group_split(type, keep = TRUE)

  ## Pull out unique list
  little_r_estimates_res <- little_r_estimates %>%
    dplyr::select(type) %>%
    unique()

  ## Estimate overall
  little_r_estimates_res$overall_little_r <- furrr::future_map(little_r_estimates_list,
                                                        ~ EpiNow::estimate_r_in_window(.$data), 
                                                        .progress = TRUE)

  ## Estimate time-varying
  little_r_estimates_res$time_varying_r <- furrr::future_map(little_r_estimates_list,
                                                             ~ EpiNow::estimate_time_varying_r(.$data,
                                                                                               window = rate_window),
                                                             .progress = TRUE)


  out <- list(R0_estimates_sum, little_r_estimates_res, R0_estimates)
  names(out) <- c("R0", "rate_of_spread", "raw_R0")

  if (!is.null(estimates$cases)) {
    
    out$case_forecast <- sum_cases_forecast
    out$raw_case_forecast <- cases_forecast

  }
  
  return(out)
}

