#' Real-time Pipeline
#'
#' @description Combine fitting a delay distribution, constructing a set of
#' complete sampled linelists, nowcast cases by onset date, and estimate
#' the time-varying effective reproduction number and rate of spread.
#' @param linelist A dataframe of of cases (by row) containing the following variables:
#' `import_status` (values "local" and "imported"), `date_onset`, `date_confirm`, `report_delay`.
#' @param target_date Character string, in the form "2020-01-01". Date to cast.
#' @param delay_cutoff_date Character string, in the form "2020-01-01". Cutoff date to use
#' to estimate the delay distribution.
#' @param samples Numeric, the number of pseudo linelists to generate. Defaults to 1000.
#' @param earliest_allowed_onset A character string in the form of a date ("2020-01-01") indiciating the earliest
#' allowed onset.
#' @param serial_intervals A matrix with columns representing samples and rows representing the probability of the serial intervel being on
#' that day. Defaults to `EpiNow::covid_serial_intervals`.
#' @param rt_samples Numeric, the number of samples to take from the estimated R distribution for each time point.
#' @param verbose Logical, defaults to `FALSE`. Should internal nowcasting progress messages be returned.
#' @return NULL
#' @export
#' @inheritParams epi_measures_pipeline
#' @inheritParams report_estimates
#' @inheritParams nowcast_pipeline
#' @importFrom dplyr filter pull rename
#' @importFrom lubridate days
#' 
#' @examples
#' 
rt_pipeline <- function(cases = NULL, imported_cases = NULL, linelist = NULL,
                        target_folder = NULL, target_date = NULL, delay_cutoff_date = NULL,
                        predict_lag = 0, samples = 1000, si_samples = 1, rt_samples = 10,
                        rt_windows = 1:7, rate_window = 7, earliest_allowed_onset = NULL,
                        merge_actual_onsets = TRUE, delay_only = FALSE,
                        verbose = FALSE, serial_intervals = NULL, rt_prior = NULL, save_plots = TRUE,
                        nowcast_lag = 4, incubation_period = 5, forecast_model = NULL,
                        horizon = 0, report_forecast = FALSE, report_delay_fns = NULL,
                        onset_modifier = NULL) {
 
 
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

  # Format input ------------------------------------------------------------

  ## Reformat linelist for use in nowcast_pipeline
  formatted_linelist <- 
    dplyr::rename(linelist, 
                  date_onset_symptoms = date_onset,
                  date_confirmation = date_confirm,
                  delay_confirmation = report_delay)

  ##Reformat cases
  cases <- 
    dplyr::rename(cases, 
                  confirm = cases)

  ## Define the min plotting (and estimate date as the first date that
  ## at least 5 local cases were reported minus the incubation period
  min_plot_date <-  
    dplyr::filter(cases,
                  import_status %in% "local", 
                  confirm >= 5) %>% 
    dplyr::pull(date) %>% 
    {min(., na.rm = TRUE) - lubridate::days(incubation_period)}
  
  # Run a nowcast -----------------------------------------------------------

  nowcast <- EpiNow::nowcast_pipeline(reported_cases = cases, linelist = formatted_linelist,
                                      date_to_cast = target_date,  date_to_cutoff_delay = delay_cutoff_date,
                                      earliest_allowed_onset = earliest_allowed_onset,
                                      merge_actual_onsets = merge_actual_onsets, samples = samples,
                                      delay_only = delay_only, nowcast_lag = nowcast_lag,
                                      verbose = verbose, report_delay_fns = report_delay_fns,
                                      onset_modifier = onset_modifier)


  saveRDS(nowcast,  paste0(target_folder, "/nowcast.rds"))

  # Estimate time-varying parameters ----------------------------------------

  epi_estimates <-
    dplyr::filter(nowcast, type %in% "nowcast") %>%
    EpiNow::epi_measures_pipeline(min_est_date = min_plot_date + lubridate::days(incubation_period),
                                  serial_intervals = serial_intervals,
                                  si_samples = si_samples, rt_samples = rt_samples,
                                  rate_window = rate_window, rt_windows = rt_windows,
                                  rt_prior = rt_prior, forecast_model = forecast_model, 
                                  horizon = horizon, verbose = verbose)

  saveRDS(epi_estimates,  paste0(target_folder, "/time_varying_params.rds"))
  
 # Summarise results -------------------------------------------------------

 EpiNow::report_estimates(cases = cases, nowcast = nowcast, 
                          reff_estimates = epi_estimates$R0,
                          littler_estimates = epi_estimates$rate_of_spread,
                          case_forecast = epi_estimates$case_forecast,
                          incubation_period = incubation_period, target_folder = target_folder,
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

  return(invisible(NULL))
}
