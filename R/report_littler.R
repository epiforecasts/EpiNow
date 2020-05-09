


report_littler <- function(littler_estimates, summarised_nowcast,
                           target_folder) {
  
  
  ## Data.table of confidence estimates
  case_confidence <- data.table::copy(summarised_nowcast)[, .(type, confidence, date = date_onset)]
  case_confidence <- case_confidence[type %in% "nowcast"]
  
  ## Merge in case confidence
  littler_estimates$time_varying_r[[1]] <- 
    case_confidence[littler_estimates$time_varying_r[[1]], on = "date"][
      !is.na(confidence)][, type := NULL]
  
  saveRDS(littler_estimates,
          paste0(target_folder, "/rate_spread_estimates.rds"))
  
  ## get overall estimates
  report_overall <- data.table::copy(littler_estimates)[,
                      .(report_overall = purrr::map(overall_little_r,
                                                    ~ purrr::map_dfr(., function(estimate) {
                                                      paste0(
                                                        signif(estimate$mean, 2), " (",
                                                        signif(estimate$bottom, 2), " -- ", 
                                                        signif(estimate$top, 2),")")})), type)][, 
                      .(data.table::as.data.table(type), data.table::rbindlist(report_overall))]
  
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
                                            
                                            estimate$bottom <- clean_double(estimate$bottom, 
                                                                            type = estimate$vars[1])
                                            estimate$top <- clean_double(estimate$top, 
                                                                         type = estimate$vars[1])
                                            estimate$mean <- clean_double(estimate$mean,
                                                                          type = estimate$vars[1])
                                            
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
  
  
  ## Tidy time-varying little R
  tidy_littler <- littler_estimates[type %in% "nowcast"][,
                    .(data.table::as.data.table(type),
                      data.table::rbindlist(time_varying_r))][,
                    var := factor(var, levels = c("little_r", "doubling_time", "goodness_of_fit"),
                                  labels = c("Rate of growth",
                                             "Doubling/halving time (days)",
                                             "Adjusted R-squared"))]
  
  return(tidy_littler)
}