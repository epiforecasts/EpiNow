#' Custom Map Theme
#'
#' @param map `ggplot2` map object 
#' @param continuous Logical defaults to `FALSE`. Is the fill variable continuous.
#'#@param variable_label A character string indicating the variable label to use. If not supplied then the underlying
#' variable name is used.
#' @param trans A character string specifying the transform to use on the specified metric. Defaults to no
#' transform ("identity"). Other options include log scaling ("log") and log base 10 scaling
#' ("log10"). For a complete list of options see \code{ggplot2::continous_scale}.
#' @param fill_labels A function to use to allocate legend labels. An example (used below) is \code{scales::percent},
#' which can be used for percentage data.
#' @param viridis_palette Character string indicating the \code{viridis} colour palette to use. Defaults
#' to "cividis". Options include "cividis", "magma", "inferno", "plasma", and "viridis". For additional details
#' @return A `ggplot2` object
#' @export
#'
#' @examples
#' 
#' 
#' ## Code 
#' theme_map
theme_map <- function(map = NULL, continuous = FALSE,
                      variable_label = NULL,
                      trans = "identity",
                      fill_labels = NULL,
                      viridis_palette = "cividis") {
  
  map <- map +
   cowplot::theme_map() +
    ggplot2::theme(legend.position = "bottom")
  
  # Add map details ---------------------------------------------------------
  
  if (continuous) {
    map <- map +
      ggplot2::guides(fill = ggplot2::guide_colorbar(title = variable_label,
                                                     barwidth = 15, barheight = 0.5)) +
      ggplot2::scale_fill_viridis_c(
        begin = 0,
        end = 0.9,
        trans = trans,
        alpha = 0.7,
        labels = fill_labels,
        option = viridis_palette,
        na.value = "lightgrey"
      )
    
  }else{
    map <- map +
      ggplot2::guides(fill = ggplot2::guide_legend(title = variable_label, ncol = 2)) +
      ggplot2::scale_fill_manual(
        breaks = c("Increasing", "Likely increasing", "Unsure", "Decreasing"),
        values = c("steelblue4", "skyblue3", "lightgoldenrod", "lightcoral"),
        na.value = "lightgrey")
  }
  
  
  return(map)
}