
#' Estimate time varying r
#'
#' @param window integer value for window size in days (default = 7)
#'
#' @inheritParams estimate_r_in_window
#' @return A dataframe of r estimates over time summarisd across samples.
#' @export
#' @importFrom purrr safely
#' @importFrom furrr future_map2 future_options
#' @importFrom tibble tibble
#' @importFrom dplyr mutate
#' @importFrom tidyr unnest
#' @examples
#'
estimate_time_varying_r <- function(onsets, window = 7) {

  safe_estimate_r_window <- purrr::safely(
    EpiNow::estimate_r_in_window
    )

  windowed_r <- tibble::tibble(
    max_time = window:nrow(onsets[[1]]),
    date = onsets[[1]]$date[window:nrow(onsets[[1]])]
  ) %>%
    dplyr::mutate(
      min_time = max_time - window) %>%
    dplyr::mutate(
      estimates = furrr::future_map2(min_time, max_time,
                              ~ suppressMessages(
                                safe_estimate_r_window(onsets,
                                                       min_time = .x,
                                                       max_time = .y)[[1]]),
                              .progress = TRUE,
                              .options = furrr::future_options(scheduling = 20)),
      vars = list(names(estimates[[1]]))
    ) %>%
    tidyr::unnest(c("estimates", "vars")) %>%
    tidyr::unnest("estimates")

}
