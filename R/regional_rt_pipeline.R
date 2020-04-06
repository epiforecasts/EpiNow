#' Regional Realtime Pipeline
#'
#' @description Runs a national realtime pipeline followed by a pipeline for each 
#' individual region.
#' @param cases A dataframe of cases (`confirm`) by date of confirmation (`date`), import status (`import_status`; ("imp)), and region (`region`).
#' @param linelist A dataframe of of cases (by row) containing the following variables:
#' `import_status` (values "local" and "imported"), `date_onset`, `date_confirm`, `report_delay`, and `region`. If a national linelist is not available a proxy linelist may be 
#' used but in this case `merge_onsets` should be set to `FALSE`.
#' @param national Logical defaults to `FALSE`. Should a national summary nowcast be made.
#' @param regional_delay Logical defaults to `FALSE`. Should reporting delays be estimated by region.
#' @param merge_onsets Logical defaults to `FALSE`. Should available onset data be used. Typically if `regional_delay` is
#' @param case_limit Numeric, the minimum number of cases in a region required for that region to be evaluated. Defaults to 10.
#' set to `FALSE` this should also be `FALSE`
#' @param regions_in_parallel Logical, should regions be run in parallel or sequentially (allowing for)
#' within pipeline parallisation. Defaults to `TRUE`.
#' @param ... 
#' @inheritParams rt_pipeline
#' @return NULL
#' @export
#' @importFrom furrr future_map
#' @importFrom tidyr complete
#' @importFrom dplyr count filter rename filter group_by pull
#' @examples
#' 
#' ## Code
#' regional_rt_pipeline
regional_rt_pipeline <- function(cases = NULL, linelist = NULL, target_folder = "results", 
                                 national = FALSE, regional_delay = FALSE, merge_onsets = FALSE,
                                 case_limit = 40,
                                 regions_in_parallel = TRUE,
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
    dplyr::filter(region %in% eval_regions)
  message("Running the pipeline for: ",
          paste(eval_regions, collapse = ", "))
  
  ## Make sure all dates have cases numbers
  cases <- cases %>% 
    tidyr::complete(date = seq(min(date), max(date), by = "day"),
                    import_status = c("imported", "local"),
                    fill = list(cases = 0))
  
  
  if (national) {
    ## National cast
    national_cases <- cases %>% 
      dplyr::count(date, import_status, wt = cases) %>% 
      dplyr::rename(cases = n)
    
    ## Run and save analysis pipeline
    message("Running national Rt pipeline")
    
    rt_pipeline(
      cases = national_cases,
      linelist = linelist,
      target_folder = file.path(target_folder, "national"),
      target_date = target_date, 
      merge_actual_onsets = merge_onsets, 
      samples = samples, ...)
    
  }

  ## regional pipelines
  regions <- unique(cases$region)
  
  if (!regional_delay) {
    message("Using a national linelist so not merging onsets and fitting a single reporting delay")
    merge_onsets <- FALSE
    
    ## Fit the delay distribution
    report_delay_fns <-  linelist %>%
      dplyr::rename(delay_confirmation = report_delay) %>% 
      EpiNow::get_delay_sample_fn(samples = samples)  
    
  }else{
    report_delay_fns <- NULL
  }
  
  message("Running pipelines by region")
  
  region_rt <- purrr::partial(EpiNow::rt_pipeline,
                              target_date = target_date, 
                              merge_actual_onsets = merge_onsets, 
                              samples = samples, 
                              report_delay_fns = report_delay_fns,
                              ...)
  
  ## Function to run the pipeline in a region
  run_region <- function(target_region) { 
    message("Running Rt pipeline for ", target_region)
    
    
    regional_cases <- cases %>% 
      dplyr::filter(region %in% target_region)
    
    if (regional_delay) {
      regional_linelist <- linelist %>% 
        dplyr::filter(region %in% target_region)
    }else{
      regional_linelist <- linelist
    }
    
    region_rt(cases = regional_cases,
              linelist = regional_linelist,
              target_folder = file.path(target_folder, target_region))

    
    
    return(invisible(NULL))
    
  }
  
  if (regions_in_parallel) {
    out <- furrr::future_map(regions, run_region, .progress = TRUE)
  }else{
    out <- purrr::map(regions, run_region)
  }
  
    
  return(invisible(NULL))
}