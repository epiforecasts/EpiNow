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
#' @importFrom future.apply future_lapply
#' @importFrom data.table as.data.table setDT copy setorder
#' @examples
#' 
#' ## Code
#' regional_rt_pipeline
regional_rt_pipeline <- function(cases = NULL, linelist = NULL, target_folder = "results", 
                                 regional_delay = FALSE, merge_onsets = FALSE,
                                 case_limit = 40, onset_modifier = NULL,
                                 bootstraps = 1, 
                                 bootstrap_samples = 100,
                                 delay_defs = NULL,
                                 regions_in_parallel = TRUE,
                                 verbose = FALSE,
                                 samples = 1000, ...) {
   
  #reset samples if report delays are passed in
  if (!is.null(delay_defs)) {
    sample <- length(delay_defs)
    message("Report delays have been specified so samples is ignored")
  }
  
  ## Set input to data.table
  cases <- data.table::as.data.table(cases)
  if (!is.null(linelist)) {
    linelist <- data.table::as.data.table(linelist)
  }
  
  ## Control parameters
  target_date <- as.character(max(cases$date))
  
  message("Running pipeline for ", target_date)
   
  
  ## Check for regions more than required cases
  eval_regions <- data.table::copy(cases)[,.(cases = sum(cases, na.rm = TRUE)), 
                                          by = c("region", "date")][
                      cases >= case_limit]$region
  
  eval_regions <- unique(eval_regions)
  
  ## Exclude zero regions
  cases <- cases[!is.na(region)][region %in% eval_regions]
  
  
  message("Running the pipeline for: ",
          paste(eval_regions, collapse = ", "))
   
  ## Make sure all dates have cases numbers
  cases_grid <- cases[,.(date = seq(min(date), max(date), by = "days"), 
                         import_status = list(list("local", "imported"))),
                      by = "region"][,
                       .(import_status = unlist(import_status)), 
                       by = c("date", "region")]
  
  cases <- cases[cases_grid, on = c("date", "region", "import_status")][is.na(cases), cases := 0]
  cases <- data.table::setorder(cases, region, import_status, date)
 
  ## regional pipelines
  regions <- unique(cases$region)
  
  if (is.null(delay_defs)) {
    if (!regional_delay) {
      message("Fitting an overall reporting delay")
      merge_onsets <- FALSE
      
      ## Fit the delay distribution
      delay_defs <- 
        EpiNow::get_delay_dist(delays = linelist$report_delay,
                               samples = samples, bootstraps = bootstraps, 
                               bootstrap_samples = bootstrap_samples)  
      
    }
    
  }

  message("Running pipelines by region")
  
  ## Function to run the pipeline in a region
  run_region <- function(target_region, ...) { 
    message("Running Rt pipeline for ", target_region)
    
    regional_cases <- cases[region %in% target_region][, region := NULL]
    
    if (regional_delay & !is.null(linelist)) {
      regional_linelist <- linelist[region %in% target_region][, region := NULL]
    }else{
      regional_linelist <- linelist
    }
    
    if (!is.null(onset_modifier)) {
      region_onset_modifier <- data.table::setDT(onset_modifier)[region %in% target_region]
      region_onset_modifier <- region_onset_modifier[,region := NULL]
    }else{
      region_onset_modifier <- NULL
    }
    
    EpiNow::rt_pipeline(
      cases = regional_cases,
      linelist = regional_linelist,
      onset_modifier = region_onset_modifier,
      target_folder = file.path(target_folder, target_region),
      target_date = target_date, 
      merge_actual_onsets = merge_onsets, 
      samples = samples, 
      delay_defs = delay_defs,
      verbose = verbose,
      bootstraps = bootstraps,
      bootstrap_samples = bootstrap_samples,
      ...)
    
    return(invisible(NULL))}
  
  if (regions_in_parallel) {
    
    future.apply::future_lapply(regions, run_region,
                                ...,
                                future.scheduling = Inf)

  }else{
    purrr::map(regions, run_region)
  }
  
    
  return(invisible(NULL))
}