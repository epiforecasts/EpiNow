
#' Approximate Sampling Reporting Delays using Reported Case Counts
#'
#' @param reported_cases A dataframe of reported cases (in date order) with the following variables:
#' `date` and `confirm`. 
#' @param max_delay Numeric, maximum delay to allow. Defaults to 120 days
#' @inheritParams sample_delay
#' @return A `data.table` of cases by date of onset
#' @export
#' @importFrom purrr map_dfc
#' @importFrom data.table data.table setorder
#' @examples
#' 
#' cases <- EpiSoon::example_obs_cases %>% 
#' dplyr::rename(confirm = cases)
#' 
#' delay_fn <- function(n, dist, cum) {
#'    dgamma(n, 2, 1)}
#' 
#' onsets <- sample_approx_delay(reported_cases = cases,
#'                               delay_fn = delay_fn)
#' 
sample_approx_delay <- function(reported_cases = NULL, 
                                delay_fn = NULL,
                                max_delay = 120, 
                                earliest_allowed_onset = NULL) {
  
  ## Reverse cases so starts with current first
  reversed_cases <- rev(reported_cases$confirm)
  
  ## Draw from the density fn of the delay dist
  delay_draw <- delay_fn(0:max_delay, dist = TRUE, cum = FALSE)
   
  ## Approximate onset cases
  onset_cases <- purrr::map_dfc(1:length(reversed_cases), 
                                ~ c(rep(0, . - 1), 
                                    reversed_cases[.] * 
                                      delay_draw,
                                    rep(0, length(reversed_cases) - .)))
  
  ## Summarise imputed onsets and build output data.table
  onset_cases <- data.table::data.table(
    date = seq(min(reported_cases$date) - lubridate::days(length(delay_draw) - 1),
               max(reported_cases$date), by = "days"),
    ## This step will round to zero days when cases < 0 on average
    ## This can lead to a slight reduction in case count early on
    cases = as.integer(rev(rowSums(onset_cases)))
  )
  
  ## Filter out all zero cases until first recorded case
  onset_cases <- data.table::setorder(onset_cases, date)
  onset_cases <- onset_cases[,cum_cases := cumsum(cases)][cum_cases != 0][,cum_cases := NULL]
  
  if (!is.null(earliest_allowed_onset)) {
    onset_cases <- onset_cases[date >= as.Date(earliest_allowed_onset)]
  }
  
  return(onset_cases)
}
