#' Plot a Time Series with Confidence.
#'
#' @param data Dataframe containing the follwoing variables: `date`, `median`, `type`, `bottom`,
#'  `top`, `lower`, `upper`,  and `confidence`
#' @param outer_alpha Numeric, outer alpha level.
#' @param inner_alpha Numeric, inner alpha level.
#' @param plot_median Logical, defaults to `FALSE`. Should the median be plotted.
#'
#' @return A `ggplot2` object.
#' @export
#'
#' @importFrom ggplot2 ggplot aes geom_line scale_x_date geom_ribbon theme element_text
#' @importFrom cowplot theme_cowplot
#' @examples
#'
plot_confidence <- function(data, outer_alpha = 0.1, inner_alpha = 0.2, plot_median = TRUE) {

  plot <- data %>%
    ggplot2::ggplot(ggplot2::aes(x = date,
                                 y = median,
                                 group = type))

  if (plot_median) {
    plot <- plot +
      ggplot2::geom_col(alpha = 0.3)
  }

  plot <- plot +
    ggplot2::geom_line(ggplot2::aes(y = bottom, alpha = confidence)) +
    ggplot2::geom_line(ggplot2::aes(y = top, alpha =  confidence)) +
    ggplot2::scale_alpha(range = c(0, 0.5)) +
    cowplot::theme_cowplot() +
    ggplot2::scale_x_date(date_breaks = "1 week", date_labels = "%b %d") +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90))


  ## Confident ribbons
  conf_data <- data %>%
    dplyr::filter(confidence == 1, type %in% "nowcast")

  plot <- plot +
    ggplot2::geom_ribbon(data = conf_data,
                         ggplot2::aes(ymin = bottom, ymax = top),
                         col = "#344b85",
                         alpha = outer_alpha) +
    ggplot2::geom_ribbon(data = conf_data,
                         ggplot2::aes(ymin = lower, ymax = upper),
                         col = "#344b85",
                         alpha = inner_alpha)

  ## Not confident ribbons
  varying_conf_data <- conf_data %>%
    slice(., nrow(.)) %>%
    bind_rows(
      data %>%
        dplyr::filter(confidence != 1)
    )


  if (nrow(varying_conf_data) > 1) {
    for (i in seq(2, nrow(varying_conf_data))) {
      plot <- plot +
        ggplot2::geom_ribbon(data = varying_conf_data[seq(i - 1, i),],
                             ggplot2::aes(ymin = bottom, ymax = top),
                             col = "#344b85",
                             alpha = varying_conf_data$confidence[i] * outer_alpha) +
        ggplot2::geom_ribbon(data = varying_conf_data[seq(i - 1, i),],
                             col = "#344b85",
                             ggplot2::aes(ymin = lower, ymax = upper),
                             alpha = varying_conf_data$confidence[i] * inner_alpha)

    }
  }

  return(plot)
}
