#' Report Effective Reproduction Number Estimates
#'
#' @param reff_estimates Dataframe of effective R estimates. As produced by `epi_measures_pipeline`.
#' @param summarised_nowcast A data.table of summarised nowcast cases as produced by `report_nowcast`.
#' @inheritParams report_nowcast
#' @return A data.table of summarised effective Reproduction number estimates
#' @export
#' @importFrom data.table copy 
#' @examples
report_reff <- function(reff_estimates, summarised_nowcast,
                        target_folder) {
  
  ## Pull out R estimates
  bigr_estimates <- reff_estimates[rt_type %in% "nowcast"]
  
  ## Data.table of confidence estimates
  case_confidence <- data.table::copy(summarised_nowcast)[, .(type, confidence, date = date_onset)]
  
  ## Join confidence onto R estimates
  bigr_estimates <- case_confidence[bigr_estimates, on = c("type", "date")][
    !is.na(confidence)]
  
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
  
  return(bigr_estimates)
}