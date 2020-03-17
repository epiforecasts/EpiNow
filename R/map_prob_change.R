#' Categorise a Probability
#'
#' @description Categorises a numeric variable into "Increasing" (< 0.05), 
#' "Likely increasing" (<0.2), "Unsure" (< 0.8), "Likely decreasing" (< 0.95), "Decreasing" (<= 1)
#' @param var Numeric variable to be categorised
#'
#' @return A character variable.
#' @export
#'
#' @examples
#' 
#' var <- seq(0.01, 1, 0.01)
#' 
#' map_prob_change(var)
map_prob_change <- function(var) {
  
  dplyr::case_when(var < 0.05 ~ "Increasing",
                   var < 0.2 ~  "Likely increasing", 
                   var < 0.8 ~ "Unsure", 
                   var < 0.95 ~ "Likely decreasing",
                   var <= 1 ~ "Decreasing") %>% 
    factor(levels = c("Increasing", "Likely increasing", "Unsure", 
                      "Likely decreasing", "Decreasing"))
}