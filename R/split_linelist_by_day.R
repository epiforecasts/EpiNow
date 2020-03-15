
#' Convert a linelist into a nested tibble of linelists by day
#'
#' @param linelist Dataframe with the following variables date_onset_symptoms and date_confirmation
#'
#' @return A nested tibble with a linelist per day (daily_observed_linelist) variable containing date_onset and date_report and
#' a date_report variable
#' @export
#' @importFrom dplyr select group_split mutate select
#' @importFrom tidyr unnest drop_na
#' @importFrom tibble tibble
#' @importFrom purrr map
#' @importFrom data.table as.data.table .N
#' @examples
#'
#'
split_linelist_by_day <- function(linelist = NULL) {


  if (!all(is.na(linelist$date_onset))) {
    linelist <- data.table::as.data.table(linelist)
    linelist_by_day <- linelist[, list(date_onset = date_onset_symptoms, date_report = date_confirmation)
                                ][!is.na(date_onset)][, n := 1:.N, date_report]
  }else{
    linelist_by_day <- data.frame()
  }


  return(linelist_by_day)
}

