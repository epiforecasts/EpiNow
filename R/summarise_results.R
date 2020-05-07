#' Summarise Realtime Results
#'
#' @param regions A character string containing the list of regions to extract results for 
#' (must all have results for the same target date).
#' @param results_dir A character string indicating the location of the results directory to extract results 
#' from.
#' @param target_date A character string indicating the target date to extract results for. All regions must have results 
#' for this date.
#' 
#' @importFrom purrr partial map_chr map_dbl map_chr
#' @importFrom data.table setorderv melt
#' @importFrom stringr str_split
#' @return
#' @export
#'
#' @examples
#' 
#' ## Code
#' 
#' summarise_results
summarise_results <- function(regions = NULL,
                              results_dir = "results",
                              target_date = NULL,
                              region_scale = NULL) {
  
   ## Utility functions
   load_data <- purrr::partial(load_nowcast_result,
                               date = target_date, result_dir = results_dir)
   

   ## Make reporting table
  estimates <- data.table::data.table(
    Region = names(regions),
    `New confirmed cases by infection date` = 
      purrr::map(regions, ~ load_data("current_cases.rds", .)),
    `Expected change in daily cases` = map_prob_change(
      purrr::map_dbl(regions, ~ load_data("prob_control_latest.rds", .))
      ),
    `Effective reproduction no.` =
      purrr::map(regions, ~ load_data("bigr_eff_latest.rds", .)),
    `Doubling/halving time (days)` = 
      purrr::map_chr(regions, ~ load_data("doubling_time_latest.rds", .))) 
   
  
  ## Make estimates numeric
  numeric_estimates <- estimates[,
                                 .(
                                   region = Region, 
                                   `New confirmed cases by infection date`, 
                                   `Effective reproduction no.`, 
                                   `Expected change in daily cases`  
                                 )] 
  
  numeric_estimates <- 
    data.table::melt(numeric_estimates,
                     measure.vars = c("New confirmed cases by infection date",
                                      "Effective reproduction no."),
                     variable.name = "metric", value.name = "value")

  numeric_estimates  <-  numeric_estimates[,
     `:=`(
       lower = purrr::map_dbl(value, ~ .[[1]]$lower),
       upper = purrr::map_dbl(value, ~ .[[1]]$upper),
       mid_lower = purrr::map_dbl(value, ~ .[[1]]$mid_lower),
       mid_upper = purrr::map_dbl(value, ~ .[[1]]$mid_upper)
     )][,
       metric :=  
         factor(metric, levels = c("New confirmed cases by infection date",
                           "Effective reproduction no."))]
  
  ## Rank countries by incidence countires
  high_inc_regions <- unique(
    data.table::setorderv(numeric_estimates, 
                         cols = "upper", order = -1)$region)
    
  numeric_estimates <- numeric_estimates[,
                          region :=  
                            factor(region, levels = high_inc_regions)]
  
  estimates <- 
    estimates[,
              `:=`(
                   `New confirmed cases by infection date` = EpiNow::make_conf(
                     purrr::map(`New confirmed cases by infection date`, ~ .[[1]]),
                     digits = 0),
                   `Effective reproduction no.` = EpiNow::make_conf(
                     purrr::map(`Effective reproduction no.`, ~ .[[1]]),
                      digits = 1))]
  
  
  
  estimates <- 
    estimates[, (region_scale) := "Region"]
  
  out <- list(estimates, numeric_estimates, high_inc_regions)
  
  names(out) <- c("table", "data", "regions_by_inc")
  
  return(out)
}
