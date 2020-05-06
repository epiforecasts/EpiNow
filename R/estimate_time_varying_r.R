
#' Estimate time varying r
#'
#' @param window integer value for window size in days (default = 7)
#'
#' @inheritParams estimate_r_in_window
#' @return A dataframe of r estimates over time summarisd across samples.
#' @export
#' @importFrom purrr safely map_lgl
#' @importFrom future.apply future_lapply
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
         estimates = future.apply::future_lapply(1:length(min_time),
               function(index){suppressMessages(safe_estimate_r_window(onsets, 
                                                                       min_time = min_time[index],
                                                                       max_time = max_time[index])[[1]])},
               future.scheduling = 10, future.packages = c("EpiNow", "purrr")))][,
       var := list(names(estimates[[1]]))]
  
  ## Remove first nesting layer
  windowed_r <- windowed_r[!purrr::map_lgl(estimates, is.null)][,
              .(estimates = purrr::flatten(estimates), var = unlist(var)),
                           by = c("date", "min_time", "max_time")]
  
  windowed_r <- windowed_r[, .(windowed_r[, .(date, var)],
                               data.table::rbindlist(estimates))]


  return(windowed_r)
}
