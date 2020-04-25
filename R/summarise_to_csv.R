
#' Summarise rt and cases as a csv
#'
#' @param results_dir Character string indicating the directory from which to extract results
#' @param summary_dir Character string the directory into which to save results
#' @param type Character string, the region identifier to apply
#' @param incubation_shift Numeric, the incubation shift to apply to the estimates, defaults to 5 days.
#' @inheritParams get_timeseries
#' @return Nothing is returned
#' @export
#' @importFrom data.table as.data.table setnames
#' @importFrom readr write_csv
#' @examples
#' 
#' 
summarise_to_csv <- function(results_dir = NULL, summary_dir = NULL, 
                             type = "country", incubation_shift = 5, 
                             date = NULL) {
  
  
  
  timeseries <- EpiNow::get_timeseries(results_dir, date = NULL, summarised = TRUE)
  
  ## Clean and save Rt estimates
  rt <- data.table::as.data.table(timeseries$rt)[type %in% "nowcast", 
                                                 .(region, date = date - lubridate::days(incubation_shift),
                                                   type = rt_type, median = round(median, 1),
                                                   lower_90 = round(bottom, 1), upper_90 = round(top, 1),
                                                   lower_50 = round(lower, 1), upper_50 = round(upper, 1), 
                                                   prob_control = round(prob_control, 2))]
  
  data.table::setnames(rt, "region", type)
  
  
  readr::write_csv(rt, paste0(summary_dir, "/rt.csv"))
  
  ## Clean and save case estimates
  cases <- data.table::as.data.table(timeseries$incidence)[type %in% "nowcast", 
                                                 .(region, date = date_onset - lubridate::days(incubation_shift),
                                                   median = round(median, 1),
                                                   lower_90 = round(bottom, 0), upper_90 = round(top, 0),
                                                   lower_50 = round(lower, 0), upper_50 = round(upper, 0), 
                                                   confidence = round(confidence, 2))]
  
  data.table::setnames(cases, "region", type)
  
  
  readr::write_csv(cases, paste0(summary_dir, "/cases.csv"))
  
  return(invisible(NULL))
}
