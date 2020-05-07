
#' Plot a Summary of the Latest Results
#'
#' @param summary_results A dataframe as returned by `summarise_results` (the `data` object).
#' @param x_lab A character string giving the label for the x axis, defaults to region.
#' @param log_cases Logical, should cases be shown on a logged scale. Defaults to `FALSE`
#' @return A `ggplot2` object
#' @export
#' @importFrom ggplot2 ggplot aes geom_linerange geom_hline facet_wrap theme guides labs expand_limits guide_legend element_blank scale_color_manual .data
#' @importFrom cowplot theme_cowplot panel_border
#' @importFrom patchwork plot_layout
#'
#' @examples
#' 
#' 
plot_summary <- function(summary_results, x_lab = "Region", log_cases = FALSE) {
  
  
  ## generic plotting function
  inner_plot <- function(df) {
      ggplot2::ggplot(df, ggplot2::aes(x = region, 
                                       col = `Expected change in daily cases`)) +
      ggplot2::geom_linerange(aes(ymin = lower, ymax = upper), size = 4, alpha = 0.7) +
      ggplot2::geom_linerange(aes(ymin = mid_lower, ymax = mid_upper), size = 4, alpha = 1) +
      ggplot2::geom_hline(yintercept = 1, linetype = 2) +
      ggplot2::facet_wrap(~ metric, ncol = 1, scales = "free_y") +
      cowplot::theme_cowplot() +
      cowplot::panel_border() +
      ggplot2::scale_color_manual(   values = c(
        "Increasing" = "#e75f00",
        "Likely increasing" = "#fd9e49",
        "Likely decreasing" = "#5fa2ce",
        "Decreasing" = "#1170aa",
        "Unsure" = "#7b848f"), drop = FALSE) 
  }
  
  ## cases plot
  cases_plot <-  
    inner_plot(summary_results[metric %in% "New confirmed cases by infection date"]) +
    ggplot2::labs(x = x_lab, y = "") +
    ggplot2::expand_limits(y = 0) +
    ggplot2::theme(axis.title.x = ggplot2::element_blank(),
                   axis.text.x = ggplot2::element_blank()) +
    ggplot2::theme(legend.position = "none")
  
 if (log_cases) {
   cases_plot <- cases_plot +
     ggplot2::scale_y_log10()
 }
    
  ## rt plot
  rt_data <- summary_results[metric %in% "Effective reproduction no."] 
  rt_plot <- 
    inner_plot(rt_data) +
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90)) +
        ggplot2::theme(legend.position = "bottom") +
        ggplot2::guides(col = ggplot2::guide_legend(nrow = 2)) +
        ggplot2::labs(x = x_lab, y = "") +
        ggplot2::expand_limits(y = c(0, min(max(rt_data$upper), 3)))


  ##join plots together
  plot <- cases_plot + rt_plot + patchwork::plot_layout(ncol = 1)
    
  return(plot)
    
}