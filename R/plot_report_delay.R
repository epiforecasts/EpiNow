
#' Plot report delay
#'
#' @param time_delays A linelist containing `date_confirm` and `report_delay` columns
#' @param cutoff Date from when data will be used in the analysis.
#' @param drop_from_plot Date from which to display data.
#' @return A ggplot2 object
#' @export
#' @importFrom ggplot2 ggplot aes geom_line geom_ribbon geom_vline xlab ylab expand_limits
#' @importFrom cowplot theme_cowplot
#' @importFrom data.table as.data.table
#' @examples
#'
#'
plot_report_delay <- function(linelist, cutoff, drop_from_plot) {

  linelist <-
    data.table::as.data.table(linelist)[date_confirm >= drop_from_plot]
    
  ## distribution of delays over time
  p <- ggplot(linelist, aes(x = date_confirm, y = report_delay)) +
    geom_jitter(width = 0.2, alpha = 0.4) +
    geom_smooth(data = linelist %>%
                  filter(!is.na(report_delay)),
                col = "black") +
    xlab("Date of confirmation") +
    ylab("Days from onset to confirmation") +
    cowplot::theme_cowplot() +
    ggplot2::expand_limits(y = 0)

  if (!missing(cutoff)) {
    p <- p + geom_vline(xintercept = cutoff + 0.5, linetype = "dashed")
  }

  return(p)
}
