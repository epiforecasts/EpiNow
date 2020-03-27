#' Format output for reporting
#'
#' @param value List of value to map into a string. Requires,
#'  `point`, `lower`, and `upper.`
#' @param round_type Function, type of rounding to apply. Defaults to `round`.
#' @param digits Numeric, defaults to 0. Amount of rounding to apply
#'
#' @return A character vector formatted for reporting
#' @export
#'
#' @importFrom purrr map_chr
#' @examples
#' 
#' value <- list(list(point = 1, lower = 0, upper = 3))
#' 
#' make_conf(value, round_type = round, digits = 0)
make_conf <- function(value, round_type = NULL, digits = 0) {
  
  if (is.null(round_type)) {
    round_type <- round
  }
  purrr::map_chr(value, ~ paste0(round_type(.$point, digits),
                                 " (", 
                                 round_type(.$lower, digits),
                                 " -- ", 
                                 round_type(.$upper, digits),
                                 ")"))
}