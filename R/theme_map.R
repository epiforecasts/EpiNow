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
#' @param scale_fill Function to use for scaling the fill. Defaults to a custom `ggplot2::scale_fill_manual`
#' @param additional arguments passed to `scale_fill`
#' @param breaks Breaks to use in legend. Defaults to `ggplot2::waiver`.
#' @return A `ggplot2` object 
#' @importFrom ggplot2 waiver theme guides scale_fill_manual
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
                      scale_fill = NULL,
                      breaks = NULL, 
                      ...){
  

  if (is.null(scale_fill)) {
    scale_fill = ggplot2::scale_fill_manual
    values <- c(
      "Increasing" = "#1170aa",
      "Likely increasing" = "#5fa2ce",
      "Likely decreasing" = "#fc7d0b",
      "Decreasing" = "#c85200",
      "Unsure" = "#7b848f")
  }
  
  if (is.null(breaks)) {
    breaks <- ggplot2::waiver()
  }
  
  map <- map +
   cowplot::theme_map() +
    ggplot2::theme(legend.position = "bottom")
  
  # Add map details ---------------------------------------------------------
  
  if (continuous) {
    map <- map +
      ggplot2::guides(fill = ggplot2::guide_colorbar(title = variable_label,
                                                     barwidth = 15, barheight = 0.5)) +
      scale_fill(
        trans = trans,
        alpha = 0.7,
        labels = fill_labels,
        option = viridis_palette,
        na.value = "#c8d0d9"
      )
    
  }else{
    map <- map +
      ggplot2::guides(fill = ggplot2::guide_legend(title = variable_label, ncol = 2)) +
      scale_fill(
        values = values,
        labels = fill_labels,
        breaks = breaks,
        na.value = "#c8d0d9",
        drop = FALSE,
        ...
      )
  }
  
  
  return(map)
}