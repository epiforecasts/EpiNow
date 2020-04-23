
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
#' @importFrom dplyr filter
#'
#' @examples
#' 
#' 
plot_summary <- function(summary_results, x_lab = "Region", log_cases = FALSE) {
  
  
  ## generic plotting function
  inner_plot <- function(df) {
    df %>% 
      ggplot2::ggplot(ggplot2::aes(x = region, col = `Expected change in daily cases`)) +
      ggplot2::geom_linerange(aes(ymin = lower, ymax = upper), size = 4, alpha = 0.7) +
      ggplot2::geom_linerange(aes(ymin = mid_lower, ymax = mid_upper), size = 4, alpha = 1) +
      ggplot2::geom_hline(yintercept = 1, linetype = 2) +
      ggplot2::facet_wrap(~ metric, ncol = 1, scales = "free_y") +
      cowplot::theme_cowplot() +
      cowplot::panel_border() +
      ggplot2::scale_color_manual(values = c(
        "Increasing" = "#49536b",
        "Likely increasing" = "#8492b1ff",
        "Likely decreasing" = "#ffec62",
        "Decreasing" = "#d9bf05",
        "Unsure" = "#906490"), drop = FALSE) 
  }
  
  ## cases plot
  cases_plot <- summary_results %>% 
    dplyr::filter(metric %in% "New confirmed cases by infection date") %>% 
    inner_plot() +
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
  rt_plot <- summary_results %>% 
    dplyr::filter(metric %in% "Effective reproduction no.") %>% 
    {
      inner_plot(.) +
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90)) +
        ggplot2::theme(legend.position = "bottom") +
        ggplot2::guides(col = ggplot2::guide_legend(nrow = 2)) +
        ggplot2::labs(x = x_lab, y = "") +
        ggplot2::expand_limits(y = c(0, min(max(.$upper), 3)))
      
    }

  ##join plots together
  plot <- cases_plot + rt_plot + patchwork::plot_layout(ncol = 1)
    
  return(plot)
    
}