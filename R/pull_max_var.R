#' Pull out maximum variables values based on a variable
#'
#' @param df Dataframe with the following variables: `type` and `si_dist`
#' @param var Unquoted variable name to pull out the maximum R estimate for.
#' @param type_selected The nowcast type to extract.
#' @importFrom dplyr enquo filter pull
#' @importFrom rlang !!
#' @return A character string containing the maximum variable
#' @export
#'
#' @examples
#'
pull_max_var <- function(df, max_var = NULL,
                         sel_var = NULL, type_selected = NULL) {

  max_var <- dplyr::enquo(max_var)
  sel_var <- dplyr::enquo(sel_var)

  df %>%
    dplyr::filter(type %in% type_selected) %>%
    dplyr::filter(!!max_var == max(!!max_var)) %>%
    dplyr::pull(!!sel_var)
}
