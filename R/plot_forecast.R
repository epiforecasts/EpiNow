#' Add a Forecast to a Plot
#' 
#' @param plot `ggplot2` plot.
#' @param forecast Dataframe containing a forecast with the following variables: `bottom`,
#' `top`, `lower`, and `upper`.
#'
#' @return A `ggplot2` plot
#' @export
#' @importFrom ggplot2 geom_ribbon aes geom_line
#' @examples
#' 
plot_forecast <- function(plot = NULL, forecast = NULL) {
  
  plot <- plot + 
  ggplot2::geom_ribbon(data = forecast,
                       ggplot2::aes(ymin = bottom, ymax = top,
                                    fill = "Forecast"),
                       alpha = 0.1) +
    ggplot2::geom_ribbon(data = forecast,
                         ggplot2::aes(ymin = lower, ymax = upper,
                                      fill = "Forecast"),
                         alpha = 0.2) + 
    ggplot2::geom_line(data = forecast, ggplot2::aes(y = bottom, alpha = 1)) +
    ggplot2::geom_line(data = forecast, ggplot2::aes(y = top, alpha =  1))
  
  return(plot)
}