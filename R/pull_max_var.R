#' Pull out maximum variables values based on a variable
#'
#' @param df Datatable with the following variables: `type` and `si_dist`
#' @param var Unquoted variable name to pull out the maximum R estimate for.
#' @param type_selected The nowcast type to extract.
#' @importFrom data.table as.data.table
#' @return A character string containing the maximum variable
#' @export
#'
#' @examples
#'
#' df <- data.table::data.table(type = c("nowcast", "other"),
#'                              var = c(1:10),
#'                              sel = "test")
#'                              
#' pull_max_var(df, max_var = "var", sel_var = "var", type_selected = "nowcast")
pull_max_var <- function(df, max_var = NULL,
                         sel_var = NULL, type_selected = NULL) {


  df <- data.table::as.data.table(df)[type %in% type_selected]
  df <- df[df[[max_var]] == max(df[[max_var]])][[sel_var]]
    
  return(df)
}
