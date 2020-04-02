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
#' @importFrom dplyr select mutate pull arrange desc rename_at
#' @importFrom tidyr gather
#' @importFrom tibble tibble
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
  estimates <- tibble::tibble(
    Region = names(regions),
    `New cases by infection date` = regions %>% 
      purrr::map(~ load_data("current_cases.rds", .)),
    `Expected change in daily cases` = regions %>% 
      purrr::map_dbl(~ load_data("prob_control_latest.rds", .)) %>% 
      map_prob_change(),
    `Effective reproduction no.` =  regions %>% 
      purrr::map(~ load_data("bigr_eff_latest.rds", .)),
    `Doubling time (days)` = regions %>% 
      purrr::map_chr(~ load_data("doubling_time_latest.rds", .))) 
   
  
  ## Make estimates numeric
  numeric_estimates <- estimates %>% 
    dplyr::select(region = Region, 
                  `New cases by infection date`, 
                  `Effective reproduction no.`, 
                  `Expected change in daily cases`) %>% 
    tidyr::gather(value = "value", key = "metric", -region, 
                  -`Expected change in daily cases`) %>% 
    dplyr::mutate(
      lower = purrr::map_dbl(value, ~ .[[1]]$lower),
      upper = purrr::map_dbl(value, ~ .[[1]]$upper),
      mid_lower = purrr::map_dbl(value, ~ .[[1]]$mid_lower),
      mid_upper = purrr::map_dbl(value, ~ .[[1]]$mid_upper)) %>% 
    dplyr::mutate(metric = metric %>% 
                    factor(levels = c("New cases by infection date",
                                      "Effective reproduction no.")))

  
  numeric_estimates <- numeric_estimates %>% 
    dplyr::mutate(
      region = region  %>% 
        factor(levels = numeric_estimates %>% 
                 dplyr::arrange(desc(upper)) %>% 
                 dplyr::pull(region) %>% 
                 unique())
    )
  
  
  estimates <- estimates %>% 
    dplyr::mutate(
      `New cases by infection date` =
        `New cases by infection date` %>% 
        purrr::map(~ .[[1]]) %>% 
        EpiNow::make_conf(digits = 0),
      `Effective reproduction no.` = 
        `Effective reproduction no.` %>% 
        purrr::map(~ .[[1]]) %>% 
        EpiNow::make_conf(digits = 1)
    )
  
  ## Rank countries by incidence countires
  high_inc_regions <- numeric_estimates %>% 
    dplyr::arrange(dplyr::desc(upper)) %>% 
    dplyr::pull(region) %>% 
    unique() %>% 
    as.character() 
  
  
  estimates <- estimates %>% 
    dplyr::rename_at(.vars = "Region", ~ region_scale)
  
  out <- list(estimates, numeric_estimates, high_inc_regions)
  
  names(out) <- c("table", "data", "regions_by_inc")
  
  return(out)
}