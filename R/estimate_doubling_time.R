#' Estimate the doubling time
#'
#' @param r An estimate of the rate of change (r)
#'
#' @return A vector of numeric values
#' @export
#'
#' @examples
#'
estimate_doubling_time <- function(r) {
 log(2) * 1 / r
}
