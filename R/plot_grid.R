

#' Plot a Grid of Plots
#'
#' @param plot_object A character string indicating the plot object to use as the 
#' base for the grid.
#' @param ...  Additional arguments to pass to `patchwork::plot_layout`
#' @inheritParams summarise_results
#' @return A `ggplot2` object combinng multiple plots
#' @export
#' @importFrom purrr map
#' @importFrom ggplot2 labs scale_x_date coord_cartesian guides
#' @importFrom stringr str_replace str_to_title
#' @importFrom patchwork wrap_plots plot_layout
#' @importFrom purrr safely
#' @examples
#' 
#' ## Code 
#' plot_grid
plot_grid <- function(regions = NULL, plot_object = "bigr_eff_plot.rds", 
                      results_dir = "results", target_date = NULL, ...) {
  
  
  ## Define fn for plot loading
  load_plot <- function(region) {
    plot <- EpiNow::load_nowcast_result(plot_object, region, 
                                        date = target_date, results_dir) +
      ggplot2::labs(title = stringr::str_to_title(
                      stringr::str_replace(region, "-", " ")
                      )) +
      ggplot2::scale_x_date(date_breaks = "1 week", date_labels = "%b %d")
    
    return(plot)
  }
  
  ## Make safe
  safe_load_plot <- purrr::safely(load_plot)
  
plots <- suppressMessages(
  purrr::map(regions, ~ safe_load_plot(.)[[1]]))

plots[-1] <- 
  purrr::map(plots[-1], function(plot){
    plot <- plot +
      ggplot2::guides(fill = FALSE)
    
    return(plot)
  })
  
  plot <- 
    patchwork::wrap_plots( plots) +
    patchwork::plot_layout(..., guides = "collect")
  
  return(suppressMessages(plot))
}