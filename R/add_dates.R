#' Add dates from data
#'
#' @description Pulls the last n dates from a vector
#' @param dates Character vector of dates to pull from.
#' @param n Number of dates required
#'
#' @return Character vector of dates of length N
#' @export
#'
#' @examples
#'
#' dates <- rep(1:10)
#'
#' add_dates(dates, 3)
add_dates <- function(dates, n) {
  dates <-
    dates[seq(length(dates) + 1 - n,
              length(dates))]

  return(dates)
}
