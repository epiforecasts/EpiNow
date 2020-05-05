#' Summarise a nowcast
#'
#' @param cast A dataframe as produced by `nowcast_pipeline`
#' @param incubation_shift Numeric defaults to 5. 
#' The number of days to use to shift the incubation period
#' @return A summarised dataframe
#' @export
#' @importFrom data.table copy setorder
#' @importFrom purrr map_dbl
#' @importFrom HDInterval hdi
#' @importFrom lubridate days
#' @examples
#'
#'
summarise_cast <- function(cast, incubation_period = 5) {
  
  get_conf <- function(conf, import_status) {
    if(length(conf) == 2) {
      out <- conf[which(import_status == "local")]
    }else if(length(conf) == 1) {
        out <- conf
    }
    return(out)
  }
  
  ## Make an explict copy
  summarised_cast <- data.table::copy(cast)
  
  ## SUmmarises cases by reference across sample, data and type
  summarised_cast <- summarised_cast[
    , .(cases = sum(cases), confidence = get_conf(confidence, import_status)),
    by = .(sample, date, type)]
  
  ## Create CI and other summary measures
  summarised_cast <- summarised_cast[, .(
    bottom  = as.numeric(purrr::map_dbl(list(HDInterval::hdi(cases, credMass = 0.9)), ~ .[[1]])),
    top = as.numeric(purrr::map_dbl(list(HDInterval::hdi(cases, credMass = 0.9)), ~ .[[2]])),
    lower  = as.numeric(purrr::map_dbl(list(HDInterval::hdi(cases, credMass = 0.5)), ~ .[[1]])),
    upper = as.numeric(purrr::map_dbl(list(HDInterval::hdi(cases, credMass = 0.5)), ~ .[[2]])),
    median = as.numeric(median(cases, na.rm = TRUE)),
    mean = as.numeric(mean(cases, na.rm = TRUE)),
    confidence = as.numeric(mean(confidence, na.rm = TRUE))
  ), by = .(date, type)][, date_onset := date][, date := date - lubridate::days(incubation_period)]

  data.table::setorder(summarised_cast, date)  
  
  return(summarised_cast)
  
}
