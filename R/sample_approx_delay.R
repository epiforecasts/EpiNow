
#' Approximate Sampling Reporting Delays using Reported Case Counts
#'
#' @param reported_cases A dataframe of reported cases (in date order) with the following variables:
#' `date` and `confirm`. 
#' @param max_delay Numeric, maximum delay to allow. Defaults to 120 days
#' @inheritParams sample_delay
#' @param direction Character string, defato "onset". Direction in which to map cases. Supports
#' either from report to onset ("onset") or from onset to report ("report")
#' @return A `data.table` of cases by date of onset
#' @export
#' @importFrom purrr map_dfc
#' @importFrom data.table data.table setorder
#' @examples
#' 
#' cases <- data.table::as.data.table(EpiSoon::example_obs_cases) 
#' 
#' cases <- cases[, confirm := as.integer(cases)] 
#' 
#' ## Reported case distribution
#' print(cases)
#' 
#' ## Total cases
#' sum(cases$confirm)
#' 
#' delay_fn <- function(n, dist, cum) {
#'    pgamma(n + 0.5, 2, 1) - pgamma(n - 0.49999, 2, 1)}
#' 
#' onsets <- sample_approx_delay(reported_cases = cases,
#'                               delay_fn = delay_fn)
#'    
#' ## Estimated onset distribution
#' print(onsets)
#'   
#' ## Check that sum is equal to reported cases
#' total_onsets <- median(
#'    purrr::map_dbl(1:100, 
#'                   ~ sum(sample_approx_delay(reported_cases = cases,
#'                    delay_fn = delay_fn)$cases))) 
#'                    
#' total_onsets
#'                        
#' reports <- sample_approx_delay(reported_cases = cases,
#'                               delay_fn = delay_fn,
#'                               direction = "report")
#' 
sample_approx_delay <- function(reported_cases = NULL, 
                                delay_fn = NULL,
                                max_delay = 120, 
                                earliest_allowed_onset = NULL,
                                direction = "onset") {
  
  if (direction %in% "onset") {
    direction_fn <- rev
  }else if (direction %in% "report") {
    direction_fn <- function(x){x}
  }
  ## Reverse cases so starts with current first
  reversed_cases <- direction_fn(reported_cases$confirm)
  
  ## Draw from the density fn of the delay dist
  delay_draw <- delay_fn(0:max_delay, dist = TRUE, cum = FALSE)
   
  ## Approximate onset cases
  onset_cases <- purrr::map_dfc(1:length(reversed_cases), 
                                ~ c(rep(0, . - 1), 
                                    reversed_cases[.] * 
                                      delay_draw,
                                    rep(0, length(reversed_cases) - .)))
  
  
  ## Set dates order based on direction mapping
  if (direction %in% "onset") {
    dates <- seq(min(reported_cases$date) - lubridate::days(length(delay_draw) - 1),
                 max(reported_cases$date), by = "days")
  }else if (direction %in% "report") {
    dates <- seq(min(reported_cases$date),
                 max(reported_cases$date)  + lubridate::days(length(delay_draw) - 1),
                                                             by = "days")
  }
  
  ## Summarises movements and sample for placement of non-integer cases
  case_sum <- direction_fn(rowSums(onset_cases))
  floor_case_sum <- floor(case_sum)
  sample_cases <- floor_cases_sum + 
    data.table::fifelse((runif(1:length(case_sum)) < (case_sum - floor_case_sum)),
                        1, 0)
  
  ## Summarise imputed onsets and build output data.table
  onset_cases <- data.table::data.table(
    date = dates,
    cases = sample_cases
  )
  
  ## Filter out all zero cases until first recorded case
  onset_cases <- data.table::setorder(onset_cases, date)
  onset_cases <- onset_cases[,cum_cases := cumsum(cases)][cum_cases != 0][,cum_cases := NULL]
  
  if (!is.null(earliest_allowed_onset)) {
    onset_cases <- onset_cases[date >= as.Date(earliest_allowed_onset)]
  }
  
  ## Filter out future cases that have yet to report
  if (direction %in% "report") {
    onset_cases <- onset_cases[date <= max(reported_cases$date)]
  }
  
  return(onset_cases)
}
