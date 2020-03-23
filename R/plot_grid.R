

#' Plot a Grid of Plots
#'
#' @param plot_object A character string indicating the plot object to use as the 
#' base for the grid.
#' @param ...  Additional arguments to pass to `patchwork::plot_layout`
#' @inheritParams summarise_results
#' @return A `ggplot2` object combinng multiple plots
#' @export
#' @importFrom purrr map
#' @importFrom ggplot2 labs scale_x_date coord_cartesian
#' @importFrom stringr str_replace str_to_title
#' @importFrom patchwork wrap_plots plot_layout
#' @examples
#' 
#' ## Code 
#' plot_grid
plot_grid <- function(regions = NULL, plot_object = "bigr_eff_plot.rds", 
                      results_dir = "results", target_date = NULL, ...) {
  
plots <- suppressWarnings(
  purrr::map(regions, function(region) {
    plot <- EpiNow::load_nowcast_result(plot_object, region, 
                                        date = target_date, results_dir) +
      ggplot2::labs(title = region %>% 
                      stringr::str_replace("-", " ") %>% 
                      stringr::str_to_title()) +
      ggplot2::scale_x_date(date_breaks = "1 week", date_labels = "%b %d")
    
    return(plot)
  }))
  
  plot <- plots %>% 
    patchwork::wrap_plots() +
    patchwork::plot_layout(...)
  
  return(plot)
}