
#' Sample a linelist from case counts and a reporting delay distribution
#'
#' @param cases Dataframe with two variables: confirm (numeric) and date_report (date).
#'
#' @return A linelist grouped by day as a tibble with two variables: date_report, and daily_observed_linelist
#' @export
#' @importFrom dplyr filter mutate select
#' @importFrom tibble tibble
#' @importFrom purrr map2
#' @importFrom lubridate days
#' @importFrom data.table as.data.table .N
#' @examples
#'
#'
linelist_from_case_counts <- function(cases = NULL) {

  cases <- data.table::as.data.table(cases)
  cases_linelist <- suppressWarnings(cases[confirm > 0, .(date_report = rep(date,confirm),
                                                          date_onset = as.Date(NA_character_)),
                            ][, n := 1:.N, date_report])
  return(cases_linelist)
}

