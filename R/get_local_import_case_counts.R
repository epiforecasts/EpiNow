#' Combine total and imported case counts
#'
#' @param total_cases Dataframe with following variables: `date` and `cases`.
#' @param linelist Dataframe with at least the following variables: `date_confirm`, `import_status`
#' @param cases_from A character string containing a date in the format `"yyyy-mm-dd"`. Applies a 
#' filter to returned cases.
#' @importFrom dplyr rename mutate count filter select full_join lag
#' @importFrom tidyr complete gather
#' @importFrom lubridate ymd
#' @return A tibble containing cases by date locally and imported
#' @export
#'
#' @examples
#'
get_local_import_case_counts <- function(total_cases, linelist = NULL, cases_from = NULL) {

  total_cases <- total_cases %>%
    dplyr::rename(total = cases) %>%
    dplyr::mutate(date = as.Date(date))

  imported_cases <- linelist %>%
    tidyr::drop_na(date_confirm) %>%
    dplyr::count(date_confirm, import_status) %>%
    dplyr::filter(import_status %in% "imported") %>%
    dplyr::select(date = date_confirm, imported = n)

  cases <- total_cases %>%
    dplyr::full_join(imported_cases, by = "date") %>%
    tidyr::complete(date = seq(min(.$date), max(.$date), by = "day"),
                    fill = list(imported = 0, total = 0)) %>%
    dplyr::mutate(local = ifelse(total >= imported, total - imported, 0),
                  overflow = ifelse(total < imported, imported - total, 0))


  ## Deal with imported cases but no overall cases by reducing later number of reported cases
  for(index in 1:nrow(cases)) {

    overflow <- cases$overflow[index]

    j <- index

    while(overflow > 0 | j > nrow(cases)) {
      j <- j + 1

      cases$local[j] <- cases$total[j] - overflow
      if (cases$local[j] < 0) {
        overflow <- -cases$local[j]
        cases$local[j] <- 0
      }else{
        overflow <- 0
      }

    }
  }

  cases <- cases  %>%
    dplyr::select(date, local, imported) %>%
    tidyr::gather(key = "import_status", value = "cases", local, imported)
  
  if (!is.null(cases_from)) {
    cases <- cases %>% 
      dplyr::filter(date >= lubridate::ymd(cases_from))
  }

  return(cases)
}
