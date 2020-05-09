#' Report on Estimates
#'

#' @param littler_estimates Dataframe of little R estimates. As produced by `epi_measures_pipeline`.
#' @param case_forecast Dataframe of case forecasts as produced by `epi_measures_pipeline`.
#' @param target_folder Character string, name of the folder in which to save the results.
#' @param target_date Character string, in the form "2020-01-01". Date to cast.
#' @param report_forecast Logical, defaults to `FALSE`. Should the forecast be reported.
#' @param save_plots Logical, defaults to `TRUE`. Should plots be saved.
#' @importFrom purrr map pmap
#' @importFrom ggplot2 ggsave theme labs coord_cartesian scale_x_date geom_hline geom_vline
#' @importFrom cowplot theme_cowplot
#' @importFrom patchwork plot_layout
#' @importFrom data.table rbindlist copy as.data.table
#' @importFrom R.devices suppressGraphics
#' @inheritParams summarise_cast
#' @return
#' @export
#'
#' @examples
#' 
report_estimates <- function(summarised_nowcast = NULL,
                             reff_estimates = NULL, littler_estimates = NULL,
                             case_forecast = NULL, target_date = NULL,
                             incubation_period = 5, target_folder = NULL, 
                             min_plot_date = NULL, report_forecast = FALSE, 
                             save_plots = TRUE) {
  
  
 
# Detect NULL arguments ---------------------------------------------------

  if (is.null(case_forecast)) {
    report_forecast = FALSE
  }
  
  if (report_forecast) {
    horizon <- nrow(case_forecast)
  }else{
    horizon <- 0
  }

# Report on cases ---------------------------------------------------------

  ## Plot comparison of cases
  plot_cases <-  summarised_nowcast[date >= min_plot_date]

  ## Make median NA if type nowcast
  plot_cases[type == "nowcast", median := NA]
  
  plot_cases <- EpiNow::plot_confidence(plot_cases,
                                        legend = ifelse(report_forecast,
                                                        "bottom", "none")) +
    ggplot2::labs(y = "Daily cases", x = "Date") +
    ggplot2::geom_vline(xintercept = as.Date(target_date), linetype = 2) 
  
  
  if (report_forecast) {
    
    plot_cases <- 
      EpiNow::plot_forecast(plot = plot_cases, 
                            forecast = case_forecast)
  }
  
  if (save_plots) {
    suppressWarnings(
      suppressMessages(
        R.devices::suppressGraphics({
          ggplot2::ggsave(paste0(target_folder, "/cases_plot.png"),
                          plot_cases,
                          width = 12,
                          height = 3,
                          dpi = 320)
        })
      ))
    
  }
  
  saveRDS(plot_cases,  paste0(target_folder, "/plot_cases.rds"))  
  
# Munge time-varying ------------------------------------------------------

  
  ## Plot R estimates
  plot_bigr <- 
    EpiNow::plot_confidence(bigr_estimates[type %in% "nowcast"][date >= min_plot_date],
                            plot_median = FALSE, 
                            legend = ifelse(report_forecast, "bottom", "none")) +
    ggplot2::labs(y = "Effective Reproduction no.", x = "Date") +
    ggplot2::geom_hline(yintercept = 1, linetype = 2) +
    ggplot2::expand_limits(y = 0) +
    ggplot2::geom_vline(xintercept = as.Date(target_date), linetype = 2) 
  
  if (report_forecast) {
    plot_bigr <- 
      EpiNow::plot_forecast(plot =  plot_bigr, 
                            forecast = reff_estimates[rt_type %in% "forecast"])
  }
  
  if (save_plots) {
    ## Save plot
    suppressWarnings(
      suppressMessages(
        R.devices::suppressGraphics({
        ggplot2::ggsave(paste0(target_folder, "/bigr_eff_plot.png"),
                        plot_bigr,
                        width = 12,
                        height = 6,
                        dpi = 320)})
      ))
  }
  
  saveRDS(plot_bigr,
          paste0(target_folder, "/bigr_eff_plot.rds"))

  # Pull out and plot little R ----------------------------------------------
  
  plot_littler_data <- summarised_littler[date >= min_plot_date]
  
  ## Define generic plotting function
  plot_littler_fn <- function(littler_df, plot_var = "Rate of growth") {
    plot_littler <- 
      EpiNow::plot_confidence(littler_df[var %in% plot_var], 
                              plot_median = FALSE) +
      ggplot2::geom_hline(yintercept = 0, linetype = 2) +
      ggplot2::theme(legend.position = "none") +
      ggplot2::labs(y = "", x = "Date")
    
    return(plot_littler)
  }
  
  ## Plot each measure
  plot_littler <-  
    plot_littler_fn(plot_littler_data, 
                    plot_var = "Rate of growth") +
    ggplot2::coord_cartesian(ylim = c(-0.5, 0.5)) +
    ggplot2::labs(tag = "A")
  
  plot_doublingtime <-
    plot_littler_fn(plot_littler_data, 
                    plot_var = "Doubling/halving time (days)") +
    ggplot2::coord_cartesian(ylim = c(-40, 40)) +
    ggplot2::labs(tag = "B")
  
  plot_fit <-  
    plot_littler_fn(plot_littler_data, 
                    plot_var = "Adjusted R-squared") +
    ggplot2::labs(tag = "C")
  
  ## Combine plots
  plot_littler_summary <- suppressMessages(
    plot_littler +
      plot_doublingtime +
      plot_fit +
      patchwork::plot_layout(nrow = 3)
  )
  
  
  if (save_plots) {
    ## Save plot
    suppressWarnings(
      suppressMessages(
        R.devices::suppressGraphics({
        ggplot2::ggsave(paste0(target_folder, "/rate_spread_plot.png"),
                        plot_littler_summary,
                        width = 12,
                        height = 14,
                        dpi = 320)})
      ))
    
    
  }
  
  saveRDS(plot_littler_summary,
          paste0(target_folder, "/rate_spread_plot.rds"))
  
  ## Summary plots
  cases <- plot_cases +
    ggplot2::labs("A") + 
    ggplot2::theme(legend.position = "none")
  
  bigr <- plot_bigr +
    ggplot2::labs("B")
  
  rt_cases_plot <- suppressWarnings(
    suppressMessages(
      cases +
        bigr +
        patchwork::plot_layout(ncol = 1) &
        ggplot2::scale_x_date(date_breaks = "1 week",
                              date_labels = "%b %d",
                              limits = c(min_plot_date,
                                         ifelse(!report_forecast, max(cases$data$date), 
                                                NA)))
    ))
  
  if (save_plots) {
    ## Save plot
    suppressWarnings(
      suppressMessages(
        R.devices::suppressGraphics({
        ggplot2::ggsave(paste0(target_folder, "/rt_cases_plot.png"),
                        rt_cases_plot,
                        width = 12,
                        height = 8,
                        dpi = 320)})
      ))
  }
  
  saveRDS(rt_cases_plot,
          paste0(target_folder, "/rt_cases_plot.rds"))
  
  
  ## Regional summary
  region_summary <- data.table::data.table(
    measure = c("New confirmed cases by infection date",
                "Expected change in daily cases",
                "Effective reproduction no.",
                "Doubling/halving time (days)",
                "Adjusted R-squared"),
    estimate = c(EpiNow::make_conf(current_cases),
      as.character(EpiNow::map_prob_change(prob_control)),
      EpiNow::make_conf(R_latest, digits = 1),
      doubling_time_latest,
      adjusted_r_latest
    )
  )
  
  saveRDS(region_summary, paste0(target_folder, '/region_summary.rds'))
  
return(invisible(NULL)) 
}