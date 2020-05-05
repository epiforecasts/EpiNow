
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

  ## Set up results data.table
  windowed_r <- data.table::data.table(
    max_time = window:nrow(onsets[[1]]),
    date = onsets[[1]]$date[window:nrow(onsets[[1]])]
  )
  
  ## Add minimium window
  windowed_r <- windowed_r[, min_time := max_time - window]
  
  ## Estimate little r
  windowed_r <- windowed_r[, 
       estimates = furrr::future_map2(min_time, max_time,
            ~ suppressMessages(safe_estimate_r_window(onsets)),
            .progress = TRUE),
       vars = list(names(estimates[[1]]))]

  
  windowed_r <- windowed_r[, .(estimates = unlist(estimates),
                               vars = unlist(vars)),
                           by = c("date", "min_time", "max_time")] 
  
  windowed_r <-
    tidyr::unnest(windowed_r, "estimates")

}
