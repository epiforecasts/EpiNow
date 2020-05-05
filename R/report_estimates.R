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
#' @importFrom dplyr rename filter mutate count group_by ungroup mutate_at pull select case_when bind_rows left_join bind_rows
#' @importFrom tidyr drop_na unnest
#' @importFrom tibble tibble
#' @importFrom purrr map pmap
#' @importFrom ggplot2 ggsave theme labs coord_cartesian scale_x_date geom_hline geom_vline
#' @importFrom cowplot theme_cowplot
#' @importFrom patchwork plot_layout
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

  
  summarise_cast <- nowcast %>% 
    dplyr::filter(import_status %in% "local") %>% 
    EpiNow::summarise_cast(incubation_period = incubation_period)
  
  ## Drop nowcast
  rm(nowcast)
  
  ## Combine nowcast with observed cases by onset and report
  reported_cases <-
    dplyr::filter(cases, import_status %in% "local") %>% 
    dplyr::count(date, wt = confirm) %>%
    dplyr::select(date, median = n) %>%
    dplyr::mutate(type = "Observed by report date",
                  confidence = 1)
  
  
  ## Count cumulative cases
  all_cases <-
    dplyr::bind_rows(summarise_cast, reported_cases) %>%
    dplyr::group_by(type) %>%
    dplyr::ungroup()
  
  ## Save combined data
  saveRDS(all_cases,  paste0(target_folder, "/summarised_nowcast.rds"))
  
  rm(summarise_cast, reported_cases)
  
  ## Extract latest cases
  current_cases <-
    dplyr::filter(all_cases, type %in% "nowcast") %>%
    dplyr::filter(date == max(date)) %>%
    dplyr::mutate(range = purrr::pmap(
      list(mean, bottom, top),
      function(mean, bottom, top) {
        list(point = mean,
             lower = bottom, 
             upper = top,
             mid_lower = lower,
             mid_upper = upper)
      }))
  
  
  latest_date <- dplyr::pull(current_cases, date)
  
  saveRDS(latest_date,  paste0(target_folder, "/latest_date.rds"))
  
  current_cases <- dplyr::pull(current_cases, range)
  
  saveRDS(current_cases,  paste0(target_folder, "/current_cases.rds"))
  
  ## Plot comparison of cases
  plot_cases <-  
    dplyr::filter(all_cases, !type %in% "from_delay",
                  date >= min_plot_date) %>%
    dplyr::mutate(median = ifelse(type == "nowcast", NA, median)) %>%
    EpiNow::plot_confidence(legend = ifelse(report_forecast, "bottom", "none")) +
    ggplot2::labs(y = "Daily cases", x = "Date") +
    ggplot2::geom_vline(xintercept = as.Date(target_date), linetype = 2)
  
  
  if (report_forecast) {
    
    case_forecast <- case_forecast %>% 
      dplyr::mutate(date = date - lubridate::days(incubation_period))
    
    plot_cases <- 
      EpiNow::plot_forecast(plot =  plot_cases, 
                            forecast = case_forecast)
    
    rm(case_forecast)
  }
  
  if (save_plots) {
    suppressWarnings(
      suppressMessages(
        ggplot2::ggsave(paste0(target_folder, "/cases_plot.png"),
                        plot_cases,
                        width = 12,
                        height = 3,
                        dpi = 320)
      ))
    
  }
  
  saveRDS(plot_cases,  paste0(target_folder, "/plot_cases.rds"))  
  
# Munge time-varying ------------------------------------------------------

  
  ## Pull out R estimates
  bigr_estimates <- dplyr::filter(reff_estimates,
                                  rt_type %in% "nowcast")
  
  
  bigr_estimates <- dplyr::left_join(bigr_estimates, 
                                     dplyr::select(all_cases, type, confidence, date_onset),
                                     by = c("type", "date" = "date_onset")
  ) %>%
    dplyr::filter(!is.na(confidence)) %>% 
    dplyr::mutate(date_onset = date) %>%
    dplyr::mutate(date = date - incubation_period)
  
  saveRDS(bigr_estimates,
          paste0(target_folder, "/bigr_estimates.rds"))
  
  # Pull out and plot big R -------------------------------------------------
  
  extract_bigr_values <- function(max_var, sel_var) {
    max_var <- dplyr::enquo(max_var)
    sel_var <- dplyr::enquo(sel_var)
    
    out <- EpiNow::pull_max_var(bigr_estimates, !!max_var,
                                !!sel_var, type_selected = "nowcast")
    
    return(out)
  }
  
  ## Pull summary measures
  R_max_estimate <- extract_bigr_values(median, R0_range)
  
  
  saveRDS(R_max_estimate,
          paste0(target_folder, "/bigr_eff_max_estimate.rds"))
  
  R_latest <- extract_bigr_values(date, R0_range)
  
  saveRDS(R_latest,
          paste0(target_folder, "/bigr_eff_latest.rds"))
  
  ## Pull out probability of control
  prob_control <- extract_bigr_values(date, prob_control) %>%
    signif(2)
  
  saveRDS(prob_control,
          paste0(target_folder, "/prob_control_latest.rds"))
  
  ## Plot R estimates
  plot_bigr <- 
    dplyr::filter(bigr_estimates, 
                  type %in% "nowcast",
                  date >= min_plot_date) %>%
    EpiNow::plot_confidence(plot_median = FALSE, 
                            legend = ifelse(report_forecast, "bottom", "none")) +
    ggplot2::labs(y = "Effective Reproduction no.", x = "Date") +
    ggplot2::geom_hline(yintercept = 1, linetype = 2) +
    ggplot2::expand_limits(y = 0) +
    ggplot2::geom_vline(xintercept = as.Date(target_date), linetype = 2)
  
  if (report_forecast) {
    
    effr_forecast <-  dplyr::filter(reff_estimates,
                                    rt_type %in% "forecast") %>% 
      dplyr::mutate(date = date - lubridate::days(incubation_period))
      
    plot_bigr <- 
      EpiNow::plot_forecast(plot =  plot_bigr, 
                            forecast = effr_forecast)
  }
  
  if (save_plots) {
    ## Save plot
    suppressWarnings(
      suppressMessages(
        ggplot2::ggsave(paste0(target_folder, "/bigr_eff_plot.png"),
                        plot_bigr,
                        width = 12,
                        height = 6,
                        dpi = 320)
      ))
  }
  
  saveRDS(plot_bigr,
          paste0(target_folder, "/bigr_eff_plot.rds"))
  
  rm(reff_estimates, bigr_estimates)
  # Pull out and plot little R ----------------------------------------------
  
  littler_estimates$time_varying_r[[1]] <- 
    dplyr::left_join( littler_estimates$time_varying_r[[1]],
                      dplyr::select(all_cases, type, confidence, date_onset) %>% 
                        dplyr::filter(type %in% "nowcast"),
                      by = c("date" = "date_onset")
    ) %>%
    dplyr::filter(!is.na(confidence)) %>% 
    dplyr::mutate(date_onset = date) %>%
    dplyr::mutate(date = date - incubation_period) %>% 
    dplyr::select(-type)
  
  saveRDS(littler_estimates,
          paste0(target_folder, "/rate_spread_estimates.rds"))
  
  ## get overall estimates
  report_overall <-
    dplyr::mutate(littler_estimates,
                   report_overall = purrr::map(overall_little_r,
                                               ~ purrr::map_dfr(., function(estimate) {
                                                 paste0(
                                                   signif(estimate$mean, 2), " (",
                                                   signif(estimate$bottom, 2), " -- ", signif(estimate$top, 2),
                                                   ")")
                                               }))) %>%
    tidyr::unnest("report_overall") %>%
    dplyr::select(Data = type,
                  `Rate of growth` = little_r,
                  `Doubling/halving time (days)` = doubling_time,
                  `Adjusted R-squared` = goodness_of_fit
    )
  
  
  saveRDS(report_overall,
          paste0(target_folder, "/rate_spread_overall_summary.rds"))
  
  clean_double <- function(var, type) {
    var <- signif(var, 2)
    
    return(var)
  }
  
  ## get latest estimates
  report_latest <-  littler_estimates %>%
    dplyr::mutate(report_latest = purrr::map(time_varying_r, function(estimate) {
      estimate <- dplyr::filter(estimate, date == max(date))
      
      estimate$bottom <- clean_double(estimate$bottom, type = estimate$vars[1])
      estimate$top <- clean_double(estimate$top, type = estimate$vars[1])
      estimate$mean <- clean_double(estimate$mean, type = estimate$vars[1])
      
      out <- tibble::tibble(
        vars = estimate$vars,
        range = paste0(estimate$mean, " (",
                       estimate$bottom, " -- ", estimate$top,
                       ")")
      )
      
      return(out)
    })) %>%
    tidyr::unnest("report_latest") %>%
    dplyr::select(type, vars, range) %>%
    tidyr::spread(key = "vars", value = "range") %>%
    dplyr::select(Data = type,
                  `Rate of growth` = little_r,
                  `Doubling/halving time (days)` = doubling_time,
                  `Adjusted R-squared` = goodness_of_fit
    )
  
  
  saveRDS(report_latest,
          paste0(target_folder, "/rate_spread_latest_summary.rds"))
  
  
  ## Get individual estimates
  rate_spread_latest <- report_latest %>%
    dplyr::filter(Data == "nowcast") %>%
    dplyr::pull(`Rate of growth`)
  
  
  saveRDS(rate_spread_latest,
          paste0(target_folder, "/rate_spread_latest.rds"))
  
  doubling_time_latest <- report_latest %>%
    dplyr::filter(Data == "nowcast") %>%
    dplyr::pull(`Doubling/halving time (days)`)
  
  saveRDS(doubling_time_latest,
          paste0(target_folder, "/doubling_time_latest.rds"))
  
  adjusted_r_latest <- report_latest %>%
    dplyr::filter(Data == "nowcast") %>%
    dplyr::pull(`Adjusted R-squared`)
  
  saveRDS(adjusted_r_latest,
          paste0(target_folder, "/adjusted_r_latest.rds"))
  
  ## Prepare data for plotting
  plot_littler_data <-
    tidyr::unnest(littler_estimates, "time_varying_r") %>%
    dplyr::filter(date >= min_plot_date) %>% 
    dplyr::select(-overall_little_r) %>%
    dplyr::mutate(vars = vars %>%
                    factor(levels = c("little_r", "doubling_time", "goodness_of_fit"),
                           labels = c("Rate of growth",
                                      "Doubling/halving time (days)",
                                      "Adjusted R-squared")
                    ))
  
  ## Define generic plotting function
  plot_littler_fn <- function(littler_df, plot_var = "Rate of growth") {
    plot_littler <- littler_df %>%
      dplyr::filter(vars %in% plot_var) %>%
      EpiNow::plot_confidence(plot_median = FALSE) +
      ggplot2::geom_hline(yintercept = 0, linetype = 2) +
      ggplot2::theme(legend.position = "none") +
      ggplot2::labs(y = "", x = "Date")
    
    return(plot_littler)
  }
  
  ## Plot each measure
  plot_littler <- plot_littler_data %>%
    plot_littler_fn(plot_var = "Rate of growth") +
    ggplot2::coord_cartesian(ylim = c(-0.5, 0.5)) +
    ggplot2::labs(tag = "A")
  
  plot_doublingtime <- plot_littler_data %>%
    plot_littler_fn(plot_var = "Doubling/halving time (days)") +
    ggplot2::coord_cartesian(ylim=c(-40, 40)) +
    ggplot2::labs(tag = "B")
  
  plot_fit <- plot_littler_data %>%
    plot_littler_fn(plot_var = "Adjusted R-squared") +
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
        ggplot2::ggsave(paste0(target_folder, "/rate_spread_plot.png"),
                        plot_littler_summary,
                        width = 12,
                        height = 14,
                        dpi = 320)
      ))
    
    
  }
  
  saveRDS(plot_littler_summary,
          paste0(target_folder, "/rate_spread_plot.rds"))
  
  rm(littler_estimates, plot_littler_summary)
  
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
        ggplot2::ggsave(paste0(target_folder, "/rt_cases_plot.png"),
                        rt_cases_plot,
                        width = 12,
                        height = 8,
                        dpi = 320)
      ))
  }
  
  saveRDS(rt_cases_plot,
          paste0(target_folder, "/rt_cases_plot.rds"))
  
  
  ## Regional summary
  region_summary <- tibble::tibble(
    measure = c("New confirmed cases by infection date",
                "Expected change in daily cases",
                "Effective reproduction no.",
                "Doubling/halving time (days)",
                "Adjusted R-squared"),
    estimate = c(
      current_cases %>% 
        EpiNow::make_conf(),
      prob_control %>% 
        EpiNow::map_prob_change() %>% 
        as.character(),
      R_latest %>% 
        EpiNow::make_conf(digits = 1),
      doubling_time_latest,
      adjusted_r_latest
    )
  )
  
  saveRDS(region_summary, paste0(target_folder, '/region_summary.rds'))
  
  rm(list = ls())
  
return(invisible(NULL)) 
}