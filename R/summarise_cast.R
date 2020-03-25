#' Summarise a nowcast
#'
#' @param cast A dataframe as produced by `nowcast_pipeline`
#'
#' @return A summarised dataframe
#' @export
#' @importFrom tidyr gather
#' @importFrom dplyr filter group_by summarise ungroup
#' @examples
#'
#'
summarise_cast <- function(cast) {
  cast %>%
    dplyr::group_by(sample, date) %>%
    dplyr::summarise(cases = sum(cases),
                     confidence = confidence[1],
                     type = type[1]) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(type, date) %>%
    dplyr::summarise(
      bottom = quantile(cases, 0.025, na.rm = TRUE),
      lower = quantile(cases, 0.25, na.rm = TRUE),
      median = median(cases, na.rm = TRUE),
      upper = quantile(cases, 0.75, na.rm = TRUE),
      top = quantile(cases, 0.975, na.rm = TRUE),
      confidence = mean(confidence, na.rm = TRUE)) %>%
    dplyr::ungroup()
}
