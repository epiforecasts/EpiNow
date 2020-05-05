
#' Estimate time varying r
#'
#' @param window integer value for window size in days (default = 7)
#'
#' @inheritParams estimate_r_in_window
#' @return A dataframe of r estimates over time summarisd across samples.
#' @export
#' @importFrom purrr safely
#' @importFrom furrr future_map2 future_options
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
       .(date, min_time, max_time,
         estimates = furrr::future_map2(min_time, max_time,
            ~ suppressMessages(safe_estimate_r_window(onsets, 
                                                      min_time = .x,
                                                      max_time = .y)[[1]]),
            .progress = TRUE))][, var := list(names(estimates[[1]]))]
  
  ## Remove first nesting layer
  windowed_r <- windowed_r[, .(estimates = purrr::flatten(estimates),
                               var = unlist(var)),
                           by = c("date", "min_time", "max_time")]
  
  windowed_r <- windowed_r[, .(windowed_r[, .(date, var)],
                               data.table::rbindlist(estimates))]


  return(windowed_r)
}
