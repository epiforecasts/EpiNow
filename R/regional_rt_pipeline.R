#' Regional Realtime Pipeline
#'
#' @description Runs a national realtime pipeline followed by a pipeline for each 
#' individual region.
#' @param cases A dataframe of cases (`confirm`) by date of confirmation (`date`), import status (`import_status`; ("imp)), and region (`region`).
#' @param linelist A dataframe of of cases (by row) containing the following variables:
#' `import_status` (values "local" and "imported"), `date_onset`, `date_confirm`, `report_delay`, and `region`. If a national linelist is not available a proxy linelist may be 
#' used but in this case `merge_onsets` should be set to `FALSE`.
#' @param regional_delay Logical defaults to `FALSE`. Should reporting delays be estimated by region.
#' @param merge_onsets Logical defaults to `FALSE`. Should available onset data be used. Typically if `regional_delay` is
#' @param case_limit Numeric, the minimum number of cases in a region required for that region to be evaluated. Defaults to 10.
#' set to `FALSE` this should also be `FALSE`
#' @param regions_in_parallel Logical, should regions be run in parallel or sequentially (allowing for)
#' within pipeline parallisation. Defaults to `TRUE`.
#' @param verbose Logical, defaults to `FALSE`. Should progress messages be shown for each reigon?
#' @param ... 
#' @inheritParams rt_pipeline
#' @return NULL
#' @export
#' @importFrom furrr future_map future_options
#' @importFrom tidyr complete drop_na
#' @importFrom dplyr count filter rename filter group_by pull ungroup
#' @examples
#' 
#' ## Code
#' regional_rt_pipeline
regional_rt_pipeline <- function(cases = NULL, linelist = NULL, target_folder = "results", 
                                 regional_delay = FALSE, merge_onsets = FALSE,
                                 case_limit = 40, onset_modifier = NULL,
                                 bootstraps = 1, 
                                 bootstrap_samples = 1000,
                                 regions_in_parallel = TRUE,
                                 verbose = FALSE,
                                 samples = 1000, ...) {
  
  
  ## Control parameters
  target_date <- as.character(max(cases$date))
  
  message("Running pipeline for ", target_date)
  
  
  ## Check for regions more than required cases
  eval_regions <- cases %>% 
    dplyr::group_by(region, date) %>% 
    dplyr::count(wt = cases) %>% 
    dplyr::filter(n >= case_limit) %>% 
    dplyr::pull(region) %>% 
    unique
  
  ## Exclude zero regions
  cases <- cases %>% 
    tidyr::drop_na(region) %>% 
    dplyr::filter(region %in% eval_regions)
  message("Running the pipeline for: ",
          paste(eval_regions, collapse = ", "))
  
  ## Make sure all dates have cases numbers
  cases <- cases %>% 
    group_by(region) %>% 
    tidyr::complete(date = seq(min(date), max(date), by = "day"),
                    import_status = c("imported", "local"),
                    fill = list(cases = 0)) %>% 
    dplyr::ungroup()
  

  ## regional pipelines
  regions <- unique(cases$region)
  
  if (!regional_delay) {
    message("Using a national linelist so not merging onsets and fitting a single reporting delay")
    merge_onsets <- FALSE
    
    ## Fit the delay distribution
    report_delay_fns <-  linelist %>%
      dplyr::rename(delay_confirmation = report_delay) %>% 
      EpiNow::get_delay_sample_fn(samples = samples, bootstraps = bootstraps, 
                                  bootstrap_samples = bootstrap_samples)  
    
  }else{
    report_delay_fns <- NULL
  }
  
  message("Running pipelines by region")
  
  region_rt <- purrr::partial(EpiNow::rt_pipeline,
                              target_date = target_date, 
                              merge_actual_onsets = merge_onsets, 
                              samples = samples, 
                              report_delay_fns = report_delay_fns,
                              verbose = verbose,
                              ...)
  
  ## Function to run the pipeline in a region
  run_region <- function(target_region) { 
    message("Running Rt pipeline for ", target_region)
    
    
    regional_cases <- 
      dplyr::filter(cases, region %in% target_region)
    
    if (regional_delay) {
      regional_linelist <- 
        dplyr::filter(linelist, region %in% target_region)
    }else{
      regional_linelist <- linelist
    }
    
    if (!is.null(onset_modifier)) {
      region_onset_modifier <-
        dplyr::filter(onset_modifier, region %in% target_region)

      region_onset_modifier <- 
        dplyr::select(region_onset_modifier, -region)
  
    }else{
      region_onset_modifier <- NULL
    }
    
    region_rt(cases = regional_cases,
              linelist = regional_linelist,
              onset_modifier = region_onset_modifier,
              target_folder = file.path(target_folder, target_region))

    rm(list = ls())
    
    return(invisible(NULL))
    
  }
  
  if (regions_in_parallel) {
    out <- furrr::future_map(regions, run_region, .progress = TRUE,
                             .options = furrr::future_options(scheduling = Inf,
                                                              globals = c("region_rt", "target_date",
                                                                          "merge_onsets", "samples", 
                                                                          "report_delay_fns", "verbose",
                                                                          "cases", "linelist", "onset_modifier",
                                                                          "regions", "target_folder")))
  }else{
    out <- purrr::map(regions, run_region)
  }
  
    
  return(invisible(NULL))
}