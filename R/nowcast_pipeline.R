#' Run a complete nowcast
#'
#' @param reported_cases A dataframe of reported cases
#' @param linelist A linelist of report dates and onset dates
#' @param date_to_cutoff_delay A character
#' @param date_to_case A data indicating when to cast up to.
#' @param samples Numeric, the number of samples to take.
#' @param merge_actual_onsets Logical, defaults to `TRUE`.
#'  Should linelist onset dates be used where available?
#' @param delay_only Logical, defaults to `FALSE`. Should estimates be made based on estimated onset dates without nowcasting.
#' @param verbose Logical, defaults to `FALSE`. Should internal nowcasting progress messages be returned.
#' @param nowcast_lag Numeric, defaults to 4. The number of days by which to lag nowcasts. Helps reduce bias due to case upscaling.
#' @param report_delay_fns List of functions as produced by `EpiNow::get_delay_sample_fn`
#' @inheritParams generate_sample_linelist
#' @return
#' @export
#' @importFrom dplyr filter count bind_rows group_by summarise n rename
#' @importFrom lubridate days
#' @importFrom purrr map safely map_dfr map_lgl compact
#' @importFrom furrr future_map future_map_dfr future_map2_dfr
#' @examples
#'
#'
nowcast_pipeline <- function(reported_cases = NULL, linelist = NULL,
                             date_to_cast = NULL, date_to_cutoff_delay = NULL,
                             earliest_allowed_onset = NULL,
                             merge_actual_onsets = TRUE,
                             delay_only = FALSE,
                             verbose = FALSE,
                             samples = 1,
                             report_delay_fns = NULL,
                             nowcast_lag = 4) {
 
  if (is.null(report_delay_fns)) {
    
    ## Get the distribution of reporting delays
    ## Look at reporting delays over the two weeks
    if (is.null(date_to_cutoff_delay)) {
      date_to_cutoff_delay <- min(linelist$date_confirmation, na.rm = TRUE)
    }
    
    message("Fitting reporting delay between the ", date_to_cutoff_delay, " and the ", date_to_cast)
    ## Filter linelist for target delay distribution dates
    filtered_linelist <- linelist %>%
      dplyr::filter(date_confirmation >= date_to_cutoff_delay,
                    !is.na(delay_confirmation),
                    date_confirmation <= date_to_cast)
    
    ## Fit the delay distribution and draw posterior samples
    fitted_delay_fn <- EpiNow::get_delay_sample_fn(filtered_linelist, samples = samples)
    


  }else{
    fitted_delay_fn <- report_delay_fns
  }

  ## Group linelists by day
  linelist_by_day <- linelist %>%
    dplyr::filter(import_status == "local") %>%
    EpiNow::split_linelist_by_day()

  ## Filter out imported cases and repeat linelist step
  imported_linelist <- linelist %>%
    dplyr::filter(import_status == "imported")

  if (nrow(imported_linelist) > 0) {
    imported_linelist_by_day <- EpiNow::split_linelist_by_day(imported_linelist)
  }

  ## Filter reported cases based on the nowcasting date
  reported_cases <- reported_cases %>%
    dplyr::filter(date <= date_to_cast)

  ## Split cases into local and imported
  local_cases <- reported_cases %>%
    dplyr::filter(import_status == "local")

  imported_cases <- reported_cases %>%
    dplyr::filter(import_status == "imported")


# Nowcasting for each samples or vector of samples ------------------------

  nowcast_inner <- function(delay_fn, verbose = NULL) {
    ## Make case based pseudo linelist
    ## Sample onset dates using reporting delays
    ## Populate psuedo linelist with known linelist data
    if (verbose) {
      message("Sampling from reporting delay to generate pseudo linelists")
    }


    populate_list <- function(case_df = NULL, linelist_df = NULL) {
      linelist_from_case_counts(case_df, delay_fn = delay_fn) %>%
        generate_sample_linelist(observed_linelist = linelist_df, merge_actual_onsets =  merge_actual_onsets,
                                 earliest_allowed_onset = earliest_allowed_onset)
    }

    populated_linelist <- populate_list(local_cases, linelist_by_day)

    if (sum(imported_cases$confirm) > 0) {
      if (nrow(imported_linelist) == 0 & !merge_actual_onsets) {
        ## Not used in this scenario but required as something is needed as input
        imported_linelist_by_day <- linelist_by_day
      }

      imported_populated_linelist <- populate_list(imported_cases, imported_linelist_by_day)
    }

    ## Function to summarise cases
    summarise_cases <- function(df) {
        dplyr::count(df, date_onset) %>%
        tidyr::complete(date_onset = seq(min(.$date_onset), max(.$date_onset), by = "day"),
                            fill = list(n = 0)) %>%
        dplyr::rename(date = date_onset,
                      cases = n)
    }

    if (delay_only) {
      if (verbose) {
        message("Estimating cases by onset date without nowcasting")
      }

      cases_by_onset <- populated_linelist %>%
        summarise_cases() %>%
        dplyr::mutate(type = "from_delay", import_status = "local")

    }

    # Summarise imported cases

    if (sum(imported_cases$confirm) > 0) {
      imported_cases_by_onset <- imported_populated_linelist %>%
        summarise_cases() %>%
        dplyr::mutate(type = "from_delay", import_status = "imported")
    }

    ## Sample using  binomial
    if (verbose) {
      message("Running nowcast")
    }

    ## Estimate case counts by onset
    case_counts_by_onset <- populated_linelist %>%
      dplyr::count(date_onset) %>%
      dplyr::rename(date = date_onset, cases = n) %>%
      tidyr::complete(date = seq(min(.$date), max(.$date), by = "day"),
                      fill = list(cases = 0))


    ## sample neg bin
    sample_bin <- EpiNow::sample_onsets(
      onsets = case_counts_by_onset$cases,
      dates = case_counts_by_onset$date,
      cum_freq = delay_fn(1:nrow(case_counts_by_onset), dist = TRUE),
      report_delay = 0,
      samples = 1
    )[[1]] %>%
      dplyr::mutate(type = "nowcast", import_status = "local")

    ## Add in delay only estimates
    if (delay_only) {
      if (verbose) {
        message("Joining nowcasts and preparing output")
      }

      out <- sample_bin %>%
        dplyr::bind_rows(cases_by_onset)

      if (nrow(imported_cases) > 0) {
        out <- out %>%
          dplyr::bind_rows(
            imported_cases_by_onset
          )
      }
    }else{
      out <- sample_bin
    }

    ## Add in imported cases for nowcast if present
    if (sum(imported_cases$confirm) > 0) {
      out <- out %>%
        dplyr::bind_rows(
          imported_cases_by_onset %>%
            dplyr::mutate(type = "nowcast")
        )
    }

    ## Add confidence if missing and filter by lag
    out <- out %>%
      dplyr::mutate(confidence = ifelse(is.na(confidence), 1, confidence))

    return(out)
  }



# Nowcast samples ---------------------------------------------------------
  message("Nowcasting using fitted delay distributions")
  out <- furrr::future_map_dfr(fitted_delay_fn,
                               ~ nowcast_inner(delay_fn = ., verbose),
                               .progress = TRUE,
                               .id = "sample")
  
  ## Add a nowcast lag across samples
  out <- out %>% 
    dplyr::filter(date <= (max(date, na.rm = TRUE) - lubridate::days(nowcast_lag)))

  return(out)
}
