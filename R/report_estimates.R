#' Report on Estimates
#'
#' @param cases A dataframe of cases (`confirm`) by date of confirmation (`date`) and import status (`import_status`)
#' @param nowcast A dataframe of nowcast cases as produced by `nowcast_pipeline`.
#' @param reff_estimates Dataframe of effective R estimates. As produced by `epi_measures_pipeline`.
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
report_estimates <- function(cases = NULL, nowcast = NULL,
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

  summarised_cast <- 
    EpiNow::summarise_cast(nowcast[import_status %in% "local"],
                                   incubation_period = incubation_period)
  

  
  ## Combine nowcast with observed cases by onset and report
  reported_cases <- cases[import_status %in% "local",
                    .(median = sum(confirm), 
                      type = "Observed by report date",
                      confidence = 1), by = "date"]
  
  
  ## Count cumulative cases
  all_cases <- data.table::rbindlist(list(summarised_cast, reported_cases), fill = TRUE)
  
  ## Save combined data
  saveRDS(all_cases,  paste0(target_folder, "/summarised_nowcast.rds"))
  
  ## Extract latest cases
  current_cases <- all_cases[type %in% "nowcast"][
    date == max(date)][, .(date, range = purrr::pmap(
      list(mean, bottom, top),
      function(mean, bottom, top) {
        list(point = mean,
             lower = bottom, 
             upper = top,
             mid_lower = lower,
             mid_upper = upper)
      }))]

  
  latest_date <- current_cases$date
  
  saveRDS(latest_date,  paste0(target_folder, "/latest_date.rds"))
  
  current_cases <- current_cases$range
  
  saveRDS(current_cases,  paste0(target_folder, "/current_cases.rds"))
  
  ## Plot comparison of cases
  plot_cases <-  all_cases[!type %in% "from_delay"][date >= min_plot_date]
  
  ## Make median NA if type nowcast
  plot_cases[type == "nowcast", median := NA]
  
  plot_cases <- EpiNow::plot_confidence(plot_cases,
                                        legend = ifelse(report_forecast,
                                                        "bottom", "none")) +
    ggplot2::labs(y = "Daily cases", x = "Date") +
    ggplot2::geom_vline(xintercept = as.Date(target_date), linetype = 2) 
  
  
  if (report_forecast) {
    
    case_forecast <- case_forecast[, 
                      date := date - lubridate::days(incubation_period)]
    
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

  
  ## Pull out R estimates
  bigr_estimates <- reff_estimates[rt_type %in% "nowcast"]
  
  ## Data.table of confidence estimates
  case_confidence <- data.table::copy(all_cases)[, .(type, confidence, date = date_onset)]
  
  ## Join confidence onto R estimates
  bigr_estimates <- case_confidence[bigr_estimates, on = c("type", "date")][
    !is.na(confidence)][, date_onset := date][, date := date - incubation_period]

  saveRDS(bigr_estimates,
          paste0(target_folder, "/bigr_estimates.rds"))
  
  # Pull out and plot big R -------------------------------------------------
  
  extract_bigr_values <- function(max_var, sel_var) {
    
    out <- EpiNow::pull_max_var(bigr_estimates, max_var,
                                sel_var, type_selected = "nowcast")
    
    return(out)
  }
  
  ## Pull summary measures
  R_max_estimate <- extract_bigr_values("median", "R0_range")
  
  
  saveRDS(R_max_estimate,
          paste0(target_folder, "/bigr_eff_max_estimate.rds"))
  
  R_latest <- extract_bigr_values("date", "R0_range")
  
  saveRDS(R_latest,
          paste0(target_folder, "/bigr_eff_latest.rds"))
  
  ## Pull out probability of control
  prob_control <- extract_bigr_values("date", "prob_control")
  prob_control <-  signif( prob_control, 2)
  
  saveRDS(prob_control,
          paste0(target_folder, "/prob_control_latest.rds"))
  
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
    
    effr_forecast <- reff_estimates[rt_type %in% "forecast"][, 
              date := date - lubridate::days(incubation_period)]
      
    plot_bigr <- 
      EpiNow::plot_forecast(plot =  plot_bigr, 
                            forecast = effr_forecast)
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
  
  case_confidence <- case_confidence[type %in% "nowcast"]
  
  littler_estimates$time_varying_r[[1]] <- 
    case_confidence[littler_estimates$time_varying_r[[1]], on = "date"][
      !is.na(confidence)][, date_onset := date][, 
            date := date - incubation_period][, type := NULL]
  
  saveRDS(littler_estimates,
          paste0(target_folder, "/rate_spread_estimates.rds"))
  
  ## get overall estimates
  report_overall <- data.table::copy(littler_estimates)[,
    .(report_overall = purrr::map(overall_little_r,
                                  ~ purrr::map_dfr(., function(estimate) {
                                    paste0(
                                      signif(estimate$mean, 2), " (",
                                      signif(estimate$bottom, 2), " -- ", signif(estimate$top, 2),
                                      ")")
                                  })), type)][, .(data.table::as.data.table(type),
                                                  data.table::rbindlist(report_overall))]
  
  report_overall <- report_overall[, .(Data = type,
                                     `Rate of growth` = little_r,
                                     `Doubling/halving time (days)` = doubling_time,
                                     `Adjusted R-squared` = goodness_of_fit)]

  saveRDS(report_overall,
          paste0(target_folder, "/rate_spread_overall_summary.rds"))
  
  clean_double <- function(var, type) {
    var <- signif(var, 2)
    return(var)
  }
  
  ## get latest estimates
  report_latest <-  littler_estimates[, .(type,
    latest = purrr::map(time_varying_r, function(estimate) {
      estimate <- estimate[date == max(date)]
      
      estimate$bottom <- clean_double(estimate$bottom, type = estimate$vars[1])
      estimate$top <- clean_double(estimate$top, type = estimate$vars[1])
      estimate$mean <- clean_double(estimate$mean, type = estimate$vars[1])
      
      out <- data.table::data.table(
        vars = estimate$var,
        range = paste0(estimate$mean, " (",
                       estimate$bottom, " -- ", estimate$top,
                       ")")
      )
      
      return(out)
    }))] 
  
  report_latest <- report_latest[, .(data.table::as.data.table(type),
                                     data.table::rbindlist(latest))][,
                                    .(type, vars, range)]
  
  report_latest <- data.table::dcast(report_latest, type ~ vars, value.var = "range")

  report_latest <- report_latest[, .(Data = type,
                                     `Rate of growth` = little_r,
                                     `Doubling/halving time (days)` = doubling_time,
                                     `Adjusted R-squared` = goodness_of_fit)]
  
  saveRDS(report_latest,
          paste0(target_folder, "/rate_spread_latest_summary.rds"))
  
  
  ## Get individual estimates
  rate_spread_latest <- report_latest[Data == "nowcast"]$`Rate of growth`
  
  
  saveRDS(rate_spread_latest,
          paste0(target_folder, "/rate_spread_latest.rds"))
  
  doubling_time_latest <- report_latest[Data == "nowcast"]$`Doubling/halving time (days)`
  
  saveRDS(doubling_time_latest,
          paste0(target_folder, "/doubling_time_latest.rds"))
  
  adjusted_r_latest <- report_latest[Data == "nowcast"]$`Adjusted R-squared`
  
  saveRDS(adjusted_r_latest,
          paste0(target_folder, "/adjusted_r_latest.rds"))
  
  ## Prepare data for plotting
  plot_littler_data <- littler_estimates[type %in% "nowcast"][,
                                .(data.table::as.data.table(type),
                                  data.table::rbindlist(time_varying_r))][
                                  date >= min_plot_date][,
        var := factor(var, levels = c("little_r", "doubling_time", "goodness_of_fit"),
                           labels = c("Rate of growth",
                                      "Doubling/halving time (days)",
                                      "Adjusted R-squared"))]
  
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
    ggplot2::coord_cartesian(ylim=c(-40, 40)) +
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