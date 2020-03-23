#' Generate a country map for a single variable.
#'
#'
#' @description This general purpose function can be used to generate a country map for a single variable. It has few defaults but
#' the data supplied must contain a \code{region_code} variable for linking to mapping data.
#' @param data Dataframe containing variables to be mapped. Must contain a \code{region_code} variable.
#' 
#' @inheritParams global_map
#' @return A \code{ggplot2} object containing a country map.
#' @export
#'
#' @importFrom rnaturalearth ne_countries ne_states
#' @importFrom dplyr left_join select filter
#' @importFrom countrycode countrycode
#' @importFrom ggplot2 ggplot aes geom_sf theme_minimal theme labs waiver
#' @importFrom rlang .data
#'
#' @examples
#'
#'
country_map <- function(data = NULL, country = NULL,
                       variable = NULL,
                       variable_label = NULL,
                       trans = "identity",
                       fill_labels = NULL,
                       viridis_palette = "cividis",
                       show_caption = TRUE) {
  
  
  
  if (is.null(variable_label)) {
    variable_label <- variable
  }
  

# Get shapes --------------------------------------------------------------


  country <- rnaturalearth::ne_countries(scale="large",
                                         country = country,
                                         returnclass = 'sf')
  
  regions <- rnaturalearth::ne_states(country, returnclass = "sf")
  
  
  regions_with_data <- regions %>% 
    dplyr::left_join(data,
                     by = c("provnum_ne" = "region_code"))


  
  
  if (is.null(fill_labels)) {
    fill_labels <- ggplot2::waiver()
  }
  

# Make map ----------------------------------------------------------------

  map <- regions_with_data %>% 
    ggplot() + 
    ggplot2::geom_sf(aes(fill = .data[[variable]]), col = "white", alpha = 0.8, size = 0.2) +
    ggplot2::geom_sf(data = country, col = "darkgrey", fill = NA, alpha = 1, size = 0.4) +
    EpiNow::theme_map(continuous = is.numeric(regions_with_data[[variable]]))

  
  
  # Return map --------------------------------------------------------------
  
  
  return(map)
}