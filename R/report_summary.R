


#' Provid Summary Statistics on a Rt Pipeline
#'
#' @return
#' @export
#' @inheritParams summarise_cast
#' @importFrom data.table data.table
#' @examples
summarise_pipeline <- function(target_folder) { 
  
  current_cases <- readRDS(paste0(target_folder, "/current_cases.rds"))
  prob_control <- readRDS(paste0(target_folder, "/prob_control_latest.rds"))
  R_latest <- readRDS(paste0(target_folder, "/bigr_eff_latest.rds"))
  doubling_time_latest <- readRDS(paste0(target_folder, "/doubling_time_latest.rds"))
  adjusted_r_latest <- readRDS(paste0(target_folder, "/adjusted_r_latest.rds"))
  
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