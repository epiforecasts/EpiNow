

#' Generate Regional Summary Output
#'
#' @param summary_dir A character string giving the directory
#'  in which to store summary of results.
#' @param target_date A character string giving the target date for which to extract results
#' (in the format "yyyy-mm-dd").
#' @return NULL
#' @export
#'
#' @inheritParams summarise_results
#' @importFrom stringr str_replace_all str_to_title
#' @importFrom purrr partial
#' @importFrom dplyr rename
#' @importFrom ggplot2 coord_cartesian guides guide_legend ggsave
#' @examples
#' 
#' \dontrun{
#' 
#'## Example asssumes that CovidGlobalNow (github.com/cmmid/CovidGlobalNow) is  
#'## in the directory above the root.
#' regional_summary(results_dir = "../CovidGlobalNow/results",
#'                  summary_dir = "../CovidGlobalNow/summary",
#'                  target_date = "2020-03-19",
#'                  region_scale = "Country/Region")
#'
#'## Example asssumes that CovidItalyNow (github.com/cmmid/CovidItalyNow) is  
#'## in the directory above the root.
#' regional_summary(results_dir = "../CovidItalyNow/results",
#'                  summary_dir = "../CovidItalyNow/summary",
#'                  target_date = "2020-03-19",
#'                  region_scale = "Region")
#' }
#' 

regional_summary <- function(results_dir = NULL, 
                             summary_dir = NULL,
                             target_date = NULL,
                             region_scale = "Region") {
   

  message("Extracting results from: ", results_dir)
  
## Make summary directory
if (!dir.exists(summary_dir)) {
  dir.create(summary_dir)
}

regions <- EpiNow::get_regions(results_dir)



## Get latest date
latest_date <- EpiNow::load_nowcast_result("latest_date.rds", region = regions[1],
                                           target_date, results_dir)

saveRDs(latest_date, "latest_date.rds")

## Summarise results as a table
results <- EpiNow::summarise_results(regions, results_dir,
                                     target_date = target_date,
                                     region_scale = region_scale)

message("Saving results summary table")

results$table <- results$table %>% 
  dplyr::mutate(`Expected change in daily cases` =
                  factor(`Expected change in daily cases`,
                levels = c("Increasing", "Likely increasing", "Unsure", 
                           "Likely decreasing", "Decreasing")))

saveRDS(results$table, file.path(summary_dir, "summary_table.rds"))
saveRDS(results$data, file.path(summary_dir, "summary_data.rds"))

message("Plotting results summary")

## Summarise cases and Rts
summary_plot <- results$data %>%
  EpiNow::plot_summary(x_lab = region_scale)


ggplot2::ggsave(file.path(summary_dir, "summary_plot.png"),
                dpi = 330, width = 12, height = 12)
 

message("Plotting summary Rt and case plots")

## Plot highest incidence countries
high_cases_rt_plot <- regions[names(regions) %in% results$regions_by_inc[1:6]] %>%
  plot_grid(plot_object = "bigr_eff_plot.rds",
            results_dir, target_date = target_date, ncol = 2) &
  ggplot2::coord_cartesian(ylim = c(0, 4))

suppressWarnings(
  ggplot2::ggsave(file.path(summary_dir, "high_cases_rt_plot.png"),
                  high_cases_rt_plot, dpi = 330, width = 12, height = 9)
)


high_cases_plot <- regions[names(regions) %in% results$regions_by_inc[1:6]] %>%
  EpiNow::plot_grid(plot_object = "plot_cases.rds",
            results_dir, target_date = target_date, ncol = 2)

suppressWarnings(
  ggplot2::ggsave(file.path(summary_dir, "high_cases_plot.png"), 
                  high_cases_plot, dpi = 330, width = 12, height = 9)
)


message("Plotting overall Rt and case plots")

## Plot all countries
rt_plot <- regions %>%
  EpiNow::plot_grid(plot_object = "bigr_eff_plot.rds",
            results_dir, target_date = target_date, ncol = 5) &
  ggplot2::coord_cartesian(ylim = c(0, 4))

suppressWarnings(
  ggplot2::ggsave(file.path(summary_dir, "rt_plot.png"), 
                  rt_plot, dpi = 330, width = 24, height = 3 * round(length(regions) / 4, 0), limitsize = FALSE)
  
)

cases_plot <- regions %>%
  plot_grid(plot_object = "plot_cases.rds",
            results_dir, target_date = target_date, ncol = 5)

suppressWarnings( 
  ggplot2::ggsave(file.path(summary_dir, "cases_plot.png"), 
                  cases_plot, dpi = 330, width = 24, height =  3 * round(length(regions) / 4, 0), limitsize = FALSE)
  )


return(invisible(NULL))
}