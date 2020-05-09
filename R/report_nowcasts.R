#' Report Case Nowcast Estimates
#'
#' @description Returns a summarised nowcast as well as saving key information to the
#' results folder.
#' @param nowcast 
#' @param reported_cases 
#'
#' @return A data.table of all cases summarised using HDI intervals
#' @export
#' @importFRom data.table rbindlist
#' @examples
#' 
#' 
report_nowcasts <- function(nowcast, reported_cases) {
  
  
  summarised_cast <- 
    EpiNow::summarise_cast(nowcast[import_status %in% "local"])
  
  
  
  ## Combine nowcast with observed cases by onset and report
  reported_cases <- cases[import_status %in% "local",
                          .(median = sum(confirm), 
                            type = "Observed by report date",
                            confidence = 1), by = "date"]
  
  
  ## Count cumulative cases
  all_cases <- data.table::rbindlist(list(summarised_cast, 
                                          reported_cases), fill = TRUE)
  
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
  
  return(all_cases)
}