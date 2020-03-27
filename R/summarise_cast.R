#' Summarise a nowcast
#'
#' @param cast A dataframe as produced by `nowcast_pipeline`
#' @param nowcast_lag Numeric defaults to 3. Then number of days to lag the nowcast 
#' based on confidence.
#' @param incubation_shift Numeric defaults to 5. 
#' The number of days to use to shift the incubation period
#' @return A summarised dataframe
#' @export
#' @importFrom tidyr gather
#' @importFrom dplyr filter group_by summarise ungroup
#' @importFrom purrr map_dbl
#' @importFrom HDInterval hdi
#' @examples
#'
#'
summarise_cast <- function(cast, nowcast_lag = 3, incubation_period = 5) {
  
  get_conf <- function(conf, import_status) {
    if(length(conf) == 2) {
      out <- conf[which(import_status == "local")]
    }else if(length(conf) == 1) {
        out <- conf
    }
    return(out)
  }
  
  cast %>%
    dplyr::group_by(sample, date, type) %>%
    dplyr::summarise(cases = sum(cases),
                     confidence = get_conf(confidence, import_status)) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(type, date) %>%
    dplyr::summarise(
      bottom  = purrr::map_dbl(list(HDInterval::hdi(cases, credMass = 0.9)), ~ .[[1]]),
      top = purrr::map_dbl(list(HDInterval::hdi(cases, credMass = 0.9)), ~ .[[2]]),
      lower  = purrr::map_dbl(list(HDInterval::hdi(cases, credMass = 0.5)), ~ .[[1]]),
      upper = purrr::map_dbl(list(HDInterval::hdi(cases, credMass = 0.5)), ~ .[[2]]),
      median = median(cases, na.rm = TRUE),
      mode = mode(cases, na.rm = TRUE), 
      mean = mean(cases, na.rm = TRUE),
      confidence = mean(confidence, na.rm = TRUE)) %>%
    dplyr::filter(date <= (max(date, na.rm = TRUE) - lubridate::days(nowcast_lag))) %>% 
    dplyr::mutate(date_onset = date) %>% ## onset date
    dplyr::mutate(date = date - incubation_period) %>% ## date of infection ~5 days prior
    dplyr::filter(confidence >= min_conf) %>%
    dplyr::ungroup()
}
