#' Get Timeseries from EpiNow
#'
#' @param results_dir A character string indicating the folder containing the `EpiNow`
#' results to extract.
#' @param date A Character string (in the format "yyyy-mm-dd") indicating the date to extract
#' data for. Defaults to "latest" which finds the latest results available.
#' @param summarised Logical, defaults to `FALSE`. Should full or summarised results be 
#' returned. 
#' @return
#' @export
#' @importFrom purrr map_dfr safely
#' @examples
#'
#'
#' \dontrun{
#' ## Assuming epiforecasts/covid is one repo higher
#' ## Summary results
#' get_timeseries("../covid/_posts/global/nowcast/results/", 
#'                summarised = TRUE)
#' 
#' ## Simulations
#' get_timeseries("../covid/_posts/global/nowcast/results/")
#' }
#' ## Code
#' get_timeseries
get_timeseries <- function(results_dir = NULL, date = NULL,
                           summarised = FALSE) {

  ## Assign to latest likely date if not given
  if (is.null(date)) {
    date <- "latest"
  }

  if (summarised) {
    nowcast <- "summarised_nowcast.rds"
    rt_index <- 1
  } else{
    nowcast <- "nowcast.rds"
    rt_index <- 3
  }
  
  ## Find all regions
  regions <- list.files(results_dir)
  names(regions) <- regions

  load_data <- purrr::safely(EpiNow::load_nowcast_result)

  ## Get rt values and combine
  rt <- purrr::map_dfr(regions, ~ load_data("time_varying_params.rds", .,
                                        result_dir = results_dir,
                                        date = date)[[1]][[rt_index]],
                        .id = "region")


  ## Get incidence values and combine
  incidence <- purrr::map_dfr(regions, ~ load_data(nowcast, .,
                                                   result_dir = results_dir,
                                                   date = date)[[1]],
                              .id = "region")

  out <- list(rt, incidence)
  names(out) <- c("rt", "incidence")

  return(out)
}
