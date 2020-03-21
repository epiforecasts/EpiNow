
library(EpiNow)

results_dir <- "../CovidGlobalNow/results"
summary_dir <- "../CovidGlobalNow/summary"
target_date <- Sys.Date() - 2
map_fn <- function(data, ...) {
  dplyr::rename(data, country = region) %>% 
  dplyr::filter(!country %in% c("Hubei", "Hong Kong")) %>% 
  EpiNow::global_map(...)}


regional_scale <- "Country/Region"

## Regions to include - based on folder names
regions <- list.files(results_dir)

## Put into alphabetical order
regions <- regions[order(regions)]

names(regions) <- regions %>% 
  stringr::str_replace_all("-", " ") %>% 
  stringr::str_to_title()


### Load data
load_data <- purrr::partial(EpiNow::load_nowcast_result,
                            results_dir = results_dir)

results <- EpiNow::summarise_results(regions, results_dir, 
                                     target_date = target_date)


results$table %>% 
  dplyr::rename(region = Region) %>% 
  map_fn(variable = "Expected change in daily cases") +
  ggplot2::guides(fill = ggplot2::guide_legend(
    title = "Expected change in daily cases", ncol = 2))



results$data %>% 
  plot_summary(x_lab = regional_scale)



regions[names(regions) %in% results$regions_by_inc[1:6]] %>% 
  plot_grid(plot_object = "bigr_eff_plot.rds", 
            results_dir, target_date = target_date, ncol = 2)
