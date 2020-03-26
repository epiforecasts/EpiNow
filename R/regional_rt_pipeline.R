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
#' @param case_limit Numeric, the minimum number of cases in a region required for that region to be evaluated. Defaults to 20.
#' set to `FALSE` this should also be `FALSE`
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
                                 case_limit = 10,
                                 samples = 1000, ...) {
  
  
  ## Control parameters
  target_date <- as.character(max(cases$date))
  
  message("Running pipeline for ", target_date)
  
  
  ## Check for regions with fewer than required cases
  zero_regions <- cases %>% 
    dplyr::group_by(region, date) %>% 
    dplyr::count(wt = cases) %>% 
    dplyr::filter(n < case_limit) %>% 
    dplyr::pull(region) %>% 
    unique
  
  ## Exclude zero regions
  cases <- cases %>% 
    dplyr::filter(!region %in% zero_regions)
  
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
  
  if (regional_delay) {
    message("Using a national linelist so setting merge onsets to FALSE for regional analysis")
    merge_onsets <- FALSE
  }
  
  message("Running pipelines by region")
  
  out <- furrr::future_map(regions, function(target_region) { 
    message("Running Rt pipeline for ", target_region)
    
    
    regional_cases <- cases %>% 
      dplyr::filter(region %in% target_region)
    
    if (regional_delay) {
      regional_linelist <- linelist %>% 
        dplyr::filter(region %in% target_region)
    }else{
      regional_linelist <- linelist
    }
    
    rt_pipeline(
      cases = regional_cases,
      linelist = regional_linelist,
      target_folder = file.path(target_folder, target_region),
      target_date = target_date, 
      merge_actual_onsets = merge_onsets, 
      samples = samples, ...)
    
   return(invisible(NULL))
    
    }, .progress = TRUE)
    
  return(invisible(NULL))
}