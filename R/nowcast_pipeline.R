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
#' @param delay_sub_samples Numeric, defaults to 1. When set to 1 all data is used to fit a single delay
##' distribution where uncertainty is only propagated in the uncertainty of the fit. If set to more than one
##' the supplied delay data is sampled this many times (with samples equalling the overall number of samples divided 
##' by the number of sub samples each time).
#' @param onset_modifier data.frame containing a `date` variable and a function `modifier` variable. This is used 
#' to modify estimated cases by onset date. `modifier` must be a function that returns a proportion when called 
#' (enables inclusion of uncertainty) and takes the following arguments: `n` (samples to return) and `status` ("local" or "import").
#' @param approx_delay  Logical, defaults to `FALSE`. Should delay sampling be approximated using case counts. Not appropriate
#' when case numbers are low. Useful for high cases counts as decouples run time and resource usage from case count.
#' @inheritParams generate_pseudo_linelist
#' @inheritParams sample_delay
#' @return
#' @export
#' @importFrom lubridate days
#' @importFrom purrr map safely map_dfr map_lgl compact map2_dbl
#' @importFrom furrr future_map future_options
#' @importFrom data.table .N as.data.table := setDT rbindlist
#' @examples
#'
#' 
nowcast_pipeline <- function(reported_cases = NULL, linelist = NULL,
                             date_to_cast = NULL, date_to_cutoff_delay = NULL,
                             earliest_allowed_onset = NULL,
                             merge_actual_onsets = TRUE,
                             approx_delay = FALSE,
                             max_delay = 120,
                             delay_only = FALSE,
                             verbose = FALSE,
                             samples = 1,
                             report_delay_fns = NULL,
                             bootstraps = 1, bootstrap_samples = 1000,
                             nowcast_lag = 4,
                             onset_modifier = NULL) {
   
 
# Fit delay distribution --------------------------------------------------
 
  
  if (is.null(report_delay_fns)) {
    
    ## Get the distribution of reporting delays
    ## Look at reporting delays over the two weeks
    if (is.null(date_to_cutoff_delay)) {
      date_to_cutoff_delay <- min(linelist$date_confirmation, na.rm = TRUE)
    }
    
    if (verbose) {
      message("Fitting reporting delay between the ", date_to_cutoff_delay, " and the ", date_to_cast)
    }

    ## Filter linelist for target delay distribution dates
    filtered_linelist <- data.table::as.data.table(linelist)[
      date_confirmation >= date_to_cutoff_delay][
      !is.na(delay_confirmation)
    ][date_confirmation <= date_to_cast]
    
    ## Fit the delay distribution and draw posterior samples
    fitted_delay_fn <- EpiNow::get_delay_sample_fn(filtered_linelist, samples = samples,
                                                   bootstraps = bootstraps, 
                                                   bootstrap_samples = bootstrap_samples)
    
  }else{
    fitted_delay_fn <- report_delay_fns
    merge_actual_onsets <- FALSE
  }


# Organise inputted linelist ----------------------------------------------

  if (!approx_delay) {
    ## Split linelist into day chunks
    ## Used to merge actuals with estimated onsets
    if (merge_actual_onsets) {
      linelist <- data.table::setDT(linelist)
      ## Group linelists by day
      linelist_by_day <- EpiNow::split_linelist_by_day(
        data.table::copy(linelist)[import_status == "local"]
        )
      
      ## Filter out imported cases and repeat linelist step
      imported_linelist <- data.table::copy(linelist)[import_status == "imported"]

      
      
      if (nrow(imported_linelist) > 0) {
        imported_linelist_by_day <- EpiNow::split_linelist_by_day(imported_linelist)
      }
    }else{
      linelist_by_day <- NULL
      imported_linelist_by_day <- NULL
    }
  }
  




# Organise input case counts ----------------------------------------------

  reported_cases <- data.table::setDT(reported_cases)
  
  ## Filter reported cases based on the nowcasting date
  reported_cases <- reported_cases[date <= date_to_case]

  ## Split cases into local and imported
  local_cases <- data.table::copy(reported_cases)[import_status == "local"]

  imported_cases <- reported_cases[import_status == "imported"]

 
 
# Generate a pseudo linelist ----------------------------------------------

  if (!approx_delay) {
    if (verbose) {
      message("Generating a pseudo linelists")
    }
    
    populate_list <- function(case_df = NULL, linelist_df = NULL) {
        generate_pseudo_linelist(count_linelist = linelist_from_case_counts(case_df),
                                 observed_linelist = linelist_df, 
                                 merge_actual_onsets =  merge_actual_onsets)
    }
    
    populated_linelist <- populate_list(local_cases, linelist_by_day)
    
    if (sum(imported_cases$confirm) > 0) {
      imported_populated_linelist <- populate_list(imported_cases, imported_linelist_by_day)
    }
  }

# Argument conversion -----------------------------------------------------

if (!is.null(onset_modifier)) {
  onset_modifier <- data.table::setDT(onset_modifier)
}
 
# Nowcasting for each samples or vector of samples ------------------------

  nowcast_inner <- function(sample_delay_fn = NULL, verbose = NULL) {
    
    if (!approx_delay) {
      ## Sample onset dates using reporting delays
      if (verbose) {
        message("Sampling from reporting delay linelist")
      }
      
      sampled_linelist <- sample_delay(linelist = populated_linelist,
                                       delay_fn = sample_delay_fn,
                                       earliest_allowed_onset = earliest_allowed_onset)
      
      if (sum(imported_cases$confirm) > 0) {
        
        imported_sampled_linelist <- sample_delay(linelist = imported_populated_linelist,
                                                  delay_fn = sample_delay_fn,
                                                  earliest_allowed_onset = earliest_allowed_onset)
      }
      
      
      ## Function to summarise cases
      summarise_cases <- function(df) {
        df_cnt <- df[, .(cases = .N), by = date_onset]
        
        df_cnt <- df_cnt[df_cnt[,.(date_onset= seq(min(date_onset), max(date_onset), by = "days"))], on=.(date_onset)]
        df_cnt <- df_cnt[is.na(cases), cases := 0 ][,.(date = date_onset, cases)]
        return(df_cnt)
      }
      
      ## Summarise local cases
      cases_by_onset <- summarise_cases(sampled_linelist)

      # Summarise imported cases
      
      if (sum(imported_cases$confirm) > 0) {
        imported_cases_by_onset <- summarise_cases(imported_sampled_linelist)
      }
    }else{
      ## Apply to local cases 
      cases_by_onset <- sample_approx_delay(reported_cases = local_cases, 
                                            delay_fn = sample_delay_fn,
                                            max_delay = max_delay,
                                            earliest_allowed_onset = earliest_allowed_onset)
      
      ## Apply to imported cases if present
      if (sum(imported_cases$confirm) > 0) {
        imported_cases_by_onset <- sample_approx_delay(reported_cases = imported_cases, 
                                                       delay_fn = sample_delay_fn,
                                                       max_delay = max_delay,
                                                       earliest_allowed_onset = earliest_allowed_onset)
      }
      
      
      
      
    }

    
    ## Add in type variables and modify by onset if needed
    cases_by_onset <- cases_by_onset[, `:=`(type = "from_delay", import_status = "local")]
    
    ## Adjusted onset cases based on proportion if supplied
    if (!is.null(onset_modifier)) {
      cases_by_onset <- cases_by_onset[onset_modifier, on = 'date'][!is.na(cases)][,
                cases := as.integer(purrr::map2_dbl(cases, modifier, ~ .x * .y(n = 1, status = "local")))][,modifier := NULL]
      
    }
    
    ## Add in variable type and modify if needed for imported cases
    if (sum(imported_cases$confirm) > 0) {
      imported_cases_by_onset <- imported_cases_by_onset[, `:=`(type = "from_delay",
                                                                import_status = "imported")]
      
      if (!is.null(onset_modifier)) {
        imported_cases_by_onset <-  imported_cases_by_onset[onset_modifier, on = 'date'][!is.na(cases)][,
                 cases := as.integer(purrr::map2_dbl(cases, modifier, ~ .x * .y(n = 1, status = "import")))][,modifier := NULL]
      }
    }
      
    
    ## Sample using  binomial
    if (verbose) {
      message("Running nowcast")
    }

    ## sample neg bin
    sample_bin <- data.table::setDT(EpiNow::sample_onsets(
      onsets = cases_by_onset$cases,
      dates = cases_by_onset$date,
      cum_freq = sample_delay_fn(1:nrow(cases_by_onset), dist = TRUE),
      report_delay = 0,
      samples = 1
    )[[1]])
    
    sample_bin <- sample_bin[, `:=`(type = "nowcast", import_status = "local")]

    if (sum(imported_cases$confirm) > 0) {
      ## sample neg bin
      imported_sample_bin <- data.table::setDT(EpiNow::sample_onsets(
        onsets = imported_cases_by_onset$cases,
        dates = imported_cases_by_onset$date,
        cum_freq = sample_delay_fn(1:nrow(imported_cases_by_onset), dist = TRUE),
        report_delay = 0,
        samples = 1
      )[[1]])
      
      imported_sample_bin <- imported_sample_bin[, `:=`(type = "nowcast", 
                                                        import_status = "imported")]
      
    }

    ## Add in delay only estimates
    if (delay_only) {
      if (verbose) {
        message("Joining nowcasts and preparing output")
      }

      out <- data.table::rbindlist(sample_bin, cases_by_onset)

      if (nrow(imported_cases) > 0) {
        out <- data.table::rbindlist(out, imported_cases_by_onset)
      }
    }else{
        out <- sample_bin
    }

    ## Add in imported cases for nowcast if present
    if (sum(imported_cases$confirm) > 0) {
      out <- data.table::rbindlist(out, imported_sample_bin) 
    }

    ## Add confidence if missing and filter by lag
    out[is.na(confidence), confidence := 1]
    
    return(out)
  }



# Nowcast samples ---------------------------------------------------------
  if (verbose) {
    message("Nowcasting using fitted delay distributions")
  }

  out <- furrr::future_map(fitted_delay_fn,
                              ~ nowcast_inner(sample_delay_fn = ., verbose = FALSE),
                               .progress = verbose,
                               .options = furrr::future_options(scheduling = 20))
  
  out <- data.table::rbindlist(out, idcol = "sample")
  
  ## Add a nowcast lag across samples
  out <- out[data <=  (max(date, na.rm = TRUE) - lubridate::days(nowcast_lag))]

  return(out)
}
