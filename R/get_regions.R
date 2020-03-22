

#' Get Folders with Nowcast Results
#'
#' @param results_dir A character string giving the directory in which results
#'  are stored (as produced by `regional_rt_pipeline`).
#'
#' @return A named character vector containing the results to plot.
#' @export
#' @importFrom stringr str_replace_all
#' @importFrom stringr str_to_title
#' @examples
#' 
#' ## Code 
#' get_regions
get_regions <- function(results_dir) {
  
  # Regions to include - based on folder names
  regions <- list.files(results_dir)
  
  ## Put into alphabetical order
  regions <- regions[order(regions)]
  
  names(regions) <- regions %>%
    stringr::str_replace_all("-", " ") %>%
    stringr::str_to_title()
  
  
 return(regions)
}
