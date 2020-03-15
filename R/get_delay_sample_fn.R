##' Get a sampling function for reporting delays
##'
##' @param linelist Dataframe with a delay_confirmation date variable
##' @param verbose Logical, defaults to `FALSE`. Should progress messages be printed
##' @param resample Logical, defaults to `FALSE`. Should the supplied delays be resampled.
##' @return A list of function that takes one parameter, `n`, the number of reporting
##'   delays to randomly sample
##' @importFrom dplyr filter
##' @importFrom purrr map2
##' @importFrom rstan extract
##' @author Sebastian Funk <sebastian.funk@lshtm.ac.uk>
##'
##' @export
##'
##' @examples
##'
##' ## Example
##' \dontrun{
##' get_delay_sample_fn()
##' }
##'
##' ## Code
##' get_delay_sample_fn
get_delay_sample_fn <- function(linelist, verbose = FALSE, samples = 1) {

  ## Confirmation delays
  confirmation_delays <- linelist %>%
    .$delay_confirmation %>%
    as.integer() %>%
    .[!is.na(.)] %>%
    ## Check confirmation delay is above 0 if not drop
    .[. >= 0]


  ## Maximum allowed delay
  max_delay <- max(confirmation_delays) + 1

  fit <- EpiNow::dist_fit(confirmation_delays, samples = samples, dist = "exp")

  # extract fitted exponential distribution parameter for correct number of samples
  delay_rate <- sample(rstan::extract(fit)$lambda, samples)


  sample_functions <- delay_rate %>%
    purrr::map(function(par) {
      sample_function <- function(n, dist = FALSE, max_delay = NULL){
        if(!dist) {
          rexp(n, par)
        }else{
          if (length(n) > max_delay) {
            n <- 1:max_delay
          }
          pexp(n, par)
        }
        }
      })

  truncated_sample_functions <- sample_functions %>%
    purrr::map( function(sample_function) {
      truncated_sample_function <- function(n, dist = FALSE) {
      n <- sample_function(n, dist, get("max_delay"))

      if (!dist) {
        while(any(!is.na(n) & n >= get("max_delay"))) {
          n <- ifelse(n >= get("max_delay"), sample_function(n), n)
        }

        n <- as.integer(n)
      }

      return(n)
    }})

  return(truncated_sample_functions)
}
