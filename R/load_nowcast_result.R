#' Load nowcast results
#'
#' @param file Character string giving the result files name.
#' @param region Character string giving the region of interest.
#' @param date Target date (in the format `"yyyy-mm-dd`).
#' @param result_dir Character string giving the location of the target directory 
#'
#' @return
#' @export
#'
#' @examples
#' 
load_nowcast_result <- function(file = NULL, region = NULL, 
                                date = target_date, result_dir = results_dir) {
  file_path <- file.path(result_dir, region, date, file)
  object <- readRDS(file_path)
  
  return(object)
}