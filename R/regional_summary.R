

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
#' @inheritParams plot_summary
#' @importFrom stringr str_replace_all str_to_title
#' @importFrom purrr partial
#' @importFrom dplyr rename
#' @importFrom ggplot2 coord_cartesian guides guide_legend ggsave ggplot_build
#' @importFrom cowplot get_legend
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
                             region_scale = "Region",
                             log_cases = FALSE) {
   

  message("Extracting results from: ", results_dir)
  
## Make summary directory
if (!dir.exists(summary_dir)) {
  dir.create(summary_dir)
}

regions <- EpiNow::get_regions(results_dir)

if (target_date %in% "latest") {
  plot_date <- Sys.Date() 
}else{
  plot_date <- as.Date(target_date)
}

## Get latest date
latest_date <- EpiNow::load_nowcast_result("latest_date.rds", region = regions[1],
                                           target_date, results_dir)

saveRDS(latest_date, file.path(summary_dir, "latest_date.rds"))

## Summarise results as a table
results <- EpiNow::summarise_results(regions, results_dir,
                                     target_date = target_date,
                                     region_scale = region_scale)

message("Saving results summary table")

force_factor <- function(df) {
  df %>% 
    dplyr::mutate(`Expected change in daily cases` =
                    factor(`Expected change in daily cases`,
                           levels = c("Increasing", "Likely increasing", "Unsure", 
                                      "Likely decreasing", "Decreasing")))
  
}
results$table <- results$table %>% 
  force_factor()

results$data <- results$data %>% 
  force_factor()

##

saveRDS(results$table, file.path(summary_dir, "summary_table.rds"))
saveRDS(results$data, file.path(summary_dir, "summary_data.rds"))

message("Plotting results summary")

## Summarise cases and Rts
summary_plot <- EpiNow::plot_summary(results$data, x_lab = region_scale, log_cases = log_cases)


suppressWarnings(
  suppressMessages(
    ggplot2::ggsave(file.path(summary_dir, "summary_plot.png"),
                    dpi = 330, width = 12, height = ifelse(length(regions) > 60, 24, 12))
  )
)




message("Plotting summary Rt and case plots")

## Plot highest incidence countries
high_cases_rt_plot <- suppressWarnings(
  suppressMessages(
    regions[names(regions) %in% results$regions_by_inc[1:6]] %>%
      plot_grid(plot_object = "bigr_eff_plot.rds",
                results_dir, target_date = target_date, ncol = 2))
)


## Check the plots for forecast and adapt date and legend accordingly
data_date <- as.Date(max(
  ggplot2::ggplot_build(high_cases_rt_plot[[1]])$layout$panel_scales_x[[1]]$range$range
), origin = "1970-01-01"
)

legend <- 'gtable' %in% class(try(cowplot::get_legend(high_cases_rt_plot[[1]]), silent = TRUE))


## Adapt legend
  high_cases_rt_plot <- suppressWarnings( suppressMessages(
    high_cases_rt_plot &
  ggplot2::coord_cartesian(ylim = c(0, 3)) &
  ggplot2::scale_x_date(date_breaks = "1 week",
                                 date_labels = "%b %d",
                                 limits = c(as.Date(NA_character_),
                                            max(data_date, plot_date))) &
    ggplot2::theme(legend.position = ifelse(legend, "bottom", "none"))
  )
  )


suppressWarnings(
  suppressMessages(
  ggplot2::ggsave(file.path(summary_dir, "high_cases_rt_plot.png"),
                  high_cases_rt_plot, dpi = 330, width = 12, height = 9)
))


high_cases_plot <- suppressWarnings(
  suppressMessages(
    regions[names(regions) %in% results$regions_by_inc[1:6]] %>%
  EpiNow::plot_grid(plot_object = "plot_cases.rds",
            results_dir, target_date = target_date, ncol = 2) &
  ggplot2::scale_x_date(date_breaks = "1 week",
                                 date_labels = "%b %d",
                                 limits = c(as.Date(NA_character_), 
                                            max(data_date, plot_date))) &
    ggplot2::theme(legend.position = ifelse(legend, "bottom", "none"))
))


suppressWarnings(
  suppressMessages(
  ggplot2::ggsave(file.path(summary_dir, "high_cases_plot.png"), 
                  high_cases_plot, dpi = 330, width = 12, height = 9)
))


message("Plotting overall Rt and case plots")

## Plot all countries
rt_plot <- suppressWarnings(
  suppressMessages(
    regions %>%
      EpiNow::plot_grid(plot_object = "bigr_eff_plot.rds",
                        results_dir, target_date = target_date, ncol = 4) &
      ggplot2::coord_cartesian(ylim = c(0, 3)) &
      ggplot2::scale_x_date(date_breaks = "1 week",
                                     date_labels = "%b %d",
                                     limits = c(as.Date(NA_character_), max(data_date, plot_date))) &
      ggplot2::theme(legend.position = ifelse(legend, "bottom", "none"))
    ))

suppressWarnings(
  suppressMessages(
  ggplot2::ggsave(file.path(summary_dir, "rt_plot.png"), 
                  rt_plot, dpi = 330, width = 24, height = 5 * round(length(regions) / 3, 0), limitsize = FALSE)
  
))

cases_plot <- regions %>%
  plot_grid(plot_object = "plot_cases.rds",
            results_dir, target_date = target_date, ncol = 4) &
  ggplot2::theme(legend.position = ifelse(legend, "bottom", "none"))

suppressWarnings(
  suppressMessages( 
  ggplot2::ggsave(file.path(summary_dir, "cases_plot.png"), 
                  cases_plot, dpi = 330, width = 24, height =  5 * round(length(regions) / 3, 0), limitsize = FALSE)
  ))


return(invisible(NULL))
}