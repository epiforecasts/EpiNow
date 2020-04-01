
#' Plot a Summary of the Latest Results
#'
#' @param summary_results A dataframe as returned by `summarise_results` (the `data` object).
#' @param x_lab A character string giving the label for the x axis, defaults to region.
#'
#' @return A `ggplot2` object
#' @export
#' @importFrom ggplot2 ggplot aes geom_linerange geom_hline facet_wrap theme scale_color_viridis_d guides labs guide_legend
#' @importFrom cowplot theme_cowplot panel_border
#'
#' @examples
#' 
#' 
plot_summary <- function(summary_results, x_lab = "Region") {
  summary_results %>% 
  ggplot2::ggplot(ggplot2::aes(x = region, ymin = lower, 
                               ymax = upper, col = `Expected change in daily cases`)) +
    ggplot2::geom_linerange(size = 6, alpha = 0.8) +
    ggplot2::geom_hline(yintercept = 1, linetype = 2) +
    ggplot2::facet_wrap(~ metric, ncol = 1, scales = "free_y") +
    cowplot::theme_cowplot() +
    cowplot::panel_border() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90)) +
    ggplot2::scale_color_viridis_d(option = "cividis", end = 0.9, alpha = 0.7, drop = FALSE) +
    ggplot2::theme(legend.position = "bottom") +
    ggplot2::guides(col = ggplot2::guide_legend(nrow = 2)) +
    ggplot2::labs(x = x_lab)
}