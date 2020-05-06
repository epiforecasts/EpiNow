#' Plot a Time Series with Confidence.
#'
#' @param data Dataframe containing the follwoing variables: `date`, `median`, `type`, `bottom`,
#'  `top`, `lower`, `upper`,  and `confidence`
#' @param outer_alpha Numeric, outer alpha level.
#' @param inner_alpha Numeric, inner alpha level.
#' @param plot_median Logical, defaults to `FALSE`. Should the median be plotted.
#' @param legend Character string defaults to "none". Should a legend be displayed.
#' @return A `ggplot2` object.
#' @export
#'
#' @importFrom ggplot2 ggplot aes geom_line scale_x_date geom_ribbon theme element_text scale_fill_manual theme labs guide_legend guides
#' @importFrom cowplot theme_cowplot
#' @importFrom data.table setDT copy .N rbindlist
#' @examples
#'
plot_confidence <- function(data, outer_alpha = 0.1, inner_alpha = 0.2,
                            plot_median = TRUE, legend = "none") {

  plot <-
    ggplot2::ggplot(data, ggplot2::aes(x = date,
                                 y = median,
                                 group = type))

  if (plot_median) {
    plot <- plot +
      ggplot2::geom_col(alpha = 0.4)
  }

  plot <- plot +
    ggplot2::geom_line(ggplot2::aes(y = bottom, alpha = confidence)) +
    ggplot2::geom_line(ggplot2::aes(y = top, alpha =  confidence)) +
    ggplot2::scale_alpha(range = c(0, 0.5)) +
    cowplot::theme_cowplot() +
    ggplot2::scale_x_date(date_breaks = "1 week", date_labels = "%b %d") +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90)) +
    ggplot2::scale_fill_manual(values = c("Nowcast" = "#344b85", "Forecast" = "#b33c00")) +
    ggplot2::labs(fill = "Estimate")


  ## Confident ribbons
  data <- data.table::setDT(data)
  conf_data <- data.table::copy(data)[confidence == 1][type %in% "nowcast"]

  if (nrow(conf_data) > 0) {
    plot <- plot +
      ggplot2::geom_ribbon(data = conf_data,
                           ggplot2::aes(ymin = bottom, ymax = top, fill = "Nowcast"),
                           alpha = outer_alpha) +
      ggplot2::geom_ribbon(data = conf_data,
                           ggplot2::aes(ymin = lower, ymax = upper, fill = "Nowcast"),
                           alpha = inner_alpha)
    
  }

  ## Not confident ribbons
  varying_conf_data <- data.table::rbindlist(list(conf_data[.N],
                                             data[confidence != 1]), fill = TRUE)

  if (nrow(varying_conf_data) > 1) {
    for (i in seq(2, nrow(varying_conf_data))) {
      plot <- plot +
        ggplot2::geom_ribbon(data = varying_conf_data[seq(i - 1, i),],
                             ggplot2::aes(ymin = bottom, ymax = top, fill = "Nowcast"),
                             alpha = varying_conf_data$confidence[i] * outer_alpha) +
        ggplot2::geom_ribbon(data = varying_conf_data[seq(i - 1, i),],
                             ggplot2::aes(ymin = lower, ymax = upper, fill = "Nowcast"),
                             alpha = varying_conf_data$confidence[i] * inner_alpha)

    }
  }
  
  plot <- plot +
    ggplot2::guides(fill = ggplot2::guide_legend(reverse = TRUE,
                                                 override.aes = list(alpha = 0.3)),
                    alpha = FALSE, color = FALSE) + 
    ggplot2::theme(legend.position = legend)

  return(plot)
}
