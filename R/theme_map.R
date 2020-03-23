#' Custom Map Theme
#'
#' @param map `ggplot2` map object 
#' @param continuous Logical defaults to `FALSE`. Is the fill variable continuous.
#'
#' @return A `ggplot2` object
#' @export
#'
#' @examples
#' 
#' 
#' ## Code 
#' theme_map
theme_map <- function(map = NULL, continuous = FALSE) {
  
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
      ggplot2::guides(fill = ggplot2::guide_legend(title = variable_label)) +
      ggplot2::scale_fill_viridis_d(
        begin = 0,
        end = 0.9,
        alpha = 0.7,
        labels = fill_labels,
        option = viridis_palette,
        na.value = "lightgrey"
      )
  }
  
  
  return(map)
}