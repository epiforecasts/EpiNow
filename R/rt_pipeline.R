#' Real-time Pipeline
#'
#' @description Combine fitting a delay distribution, constructing a set of
#' complete sampled linelists, nowcast cases by onset date, and estimate
#' the time-varying effective reproduction number and rate of spread.
#' @param linelist A dataframe of of cases (by row) containing the following variables:
#' `import_status` (values "local" and "imported"), `date_onset`, `date_confirm`, `report_delay`.
#' @param delay_cutoff_date Character string, in the form "2020-01-01". Cutoff date to use
#' to estimate the delay distribution.
#' @param samples Numeric, the number of pseudo linelists to generate. Defaults to 1000.
#' @param earliest_allowed_onset A character string in the form of a date ("2020-01-01") indiciating the earliest
#' allowed onset.
#' @param serial_intervals A matrix with columns representing samples and rows representing the probability of the serial intervel being on
#' that day. Defaults to `EpiNow::covid_serial_intervals`.
#' @param min_forecast_cases Numeric, defaults to 200. The minimum number of cases required in the last 7 days
#' of data in order for a forecast to be run. This prevents spurious forecasts based on highly uncertain Rt estimates.
#' @param rt_samples Numeric, the number of samples to take from the estimated R distribution for each time point.
#' @param verbose Logical, defaults to `FALSE`. Should internal nowcasting progress messages be returned.
#' @return NULL
#' @export
#' @inheritParams epi_measures_pipeline
#' @inheritParams report_estimates
#' @inheritParams nowcast_pipeline
#' @importFrom data.table setDT
#' @importFrom lubridate days
#' 
#' @examples
#' 
rt_pipeline <- function(cases = NULL, imported_cases = NULL, linelist = NULL,
                        target_folder = NULL, target_date = NULL, delay_cutoff_date = NULL,
                        predict_lag = 0, samples = 1000,si_samples = 1, rt_samples = 5,
                        rt_windows = 1:7, rate_window = 7, earliest_allowed_onset = NULL,
                        merge_actual_onsets = TRUE, delay_only = FALSE, 
                        approx_delay = FALSE, bootstraps = 1, bootstrap_samples = 100,
                        max_delay = 120, verbose = FALSE, serial_intervals = NULL, rt_prior = NULL, 
                        save_plots = TRUE, nowcast_lag = 4, incubation_period = 5, forecast_model = NULL,
                        horizon = 0, report_forecast = FALSE, report_delay_fns = NULL,
                        onset_modifier = NULL, min_forecast_cases = 200) {
 
 
# Convert input to DT -----------------------------------------------------

  cases <- data.table::setDT(cases)
  imported_cases <- data.table::setDT(imported_cases)
  linelist <- data.table::setDT(linelist)
  
 # Set up folders ----------------------------------------------------------

 latest_folder <- file.path(target_folder, "latest")
 target_folder <- file.path(target_folder, target_date)
 
  if (!dir.exists(target_folder)) {
    dir.create(target_folder, recursive = TRUE)
  }

 # Default input -----------------------------------------------------------

  if (is.null(serial_intervals)) {
    if (verbose) {
      message("Using default sample of serial intervals with mean (sd) of 4.7 (2.9)")
    }

    serial_intervals <- EpiNow::covid_serial_intervals
  }


  if (is.null(rt_prior)) {
    if (verbose) {
      message("Using default Rt prior of 2.6 (2)")
    }
    rt_prior <- list(
      mean_prior = 2.6,
      std_prior = 2)
  }
 
 
# Control errors by changing options --------------------------------------

 ##Define the minimum number of recent cases required for a forecast to be run
 if (!is.null(min_forecast_cases)) {
   current_cases <- data.table::copy(cases)[date <= max(date)][
     date >= (max(date) - lubridate::days(7))
   ][, .(cases = sum(cases, na.rm = TRUE))]$cases

   
   ## If cases in the last week are fewer than this number then turn off forecasting.
   if (min_forecast_cases > current_cases) {
     horizon <- 0
     report_forecast <- FALSE
     forecast_model <- NULL
   }
 }
 
 
 ## Define the min plotting (and estimate date as the first date that
 ## at least 5 local cases were reported minus the incubation period
 min_plot_date <- data.table::copy(cases)[
   import_status %in% "local"][cases >= 5][
     ,.(date = min(date, na.rm = TRUE) - lubridate::days(incubation_period))]$date
 
  # Format input ------------------------------------------------------------

  ## Reformat linelist for use in nowcast_pipeline
  linelist <- linelist[, .(date_onset_symptoms = date_onset, 
                           date_confirmation = date_confirm,
                           delay_confirmation = report_delay,
                           import_status)]

  ##Reformat cases
  cases <- cases[, confirm := cases][,cases := NULL]

  # Run a nowcast -----------------------------------------------------------


  nowcast <- EpiNow::nowcast_pipeline(
    reported_cases = cases, linelist = linelist,
    date_to_cast = target_date,  date_to_cutoff_delay = delay_cutoff_date,
    earliest_allowed_onset = earliest_allowed_onset,
    merge_actual_onsets = merge_actual_onsets, samples = samples,
    delay_only = delay_only, nowcast_lag = nowcast_lag,
    verbose = verbose, report_delay_fns = report_delay_fns,
    bootstraps = bootstraps, bootstrap_samples = bootstrap_samples,
    onset_modifier = onset_modifier, approx_delay = approx_delay,
    max_delay = max_delay)

  saveRDS(nowcast,  paste0(target_folder, "/nowcast.rds"))

  rm(formatted_linelist, report_delay_fns, linelist)
  # Estimate time-varying parameters ----------------------------------------

  epi_estimates <-
    EpiNow::epi_measures_pipeline(
          count_linelist = nowcast[type == "nowcast"],
          min_est_date = min_plot_date + lubridate::days(incubation_period),
          serial_intervals = serial_intervals,
          si_samples = si_samples, rt_samples = rt_samples,
          rate_window = rate_window, rt_windows = rt_windows,
          rt_prior = rt_prior, forecast_model = forecast_model, 
          horizon = horizon, verbose = verbose)

  saveRDS(epi_estimates,  paste0(target_folder, "/time_varying_params.rds"))
  
  ##Remove raw estimates now saved
  epi_estimates$raw_R0 <- NULL
  epi_estimates$raw_case_forecast <- NULL
  
 # Summarise results -------------------------------------------------------

 EpiNow::report_estimates(cases = cases, nowcast = nowcast, 
                          reff_estimates = epi_estimates$R0,
                          littler_estimates = epi_estimates$rate_of_spread,
                          case_forecast = epi_estimates$case_forecast,
                          incubation_period = incubation_period,
                          target_folder = target_folder, target_date = target_date,
                          min_plot_date = min_plot_date, save_plots = save_plots, 
                          report_forecast = report_forecast)  

 # Copy all results to latest folder ---------------------------------------
  
  ## Save all results to a latest folder as well
  suppressWarnings(
    if (dir.exists(latest_folder)) {
      unlink(latest_folder)
    })
    
  suppressWarnings(
    dir.create(latest_folder)
  )

  suppressWarnings(
    file.copy(file.path(target_folder, "."),
              latest_folder, recursive = TRUE)
  )


  ## Clean everything not in use
  rm(list = ls())
  
  return(invisible(NULL))
}
