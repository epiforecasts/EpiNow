##' Get a sampling function for reporting delays
##'
##' @param linelist Dataframe with a delay_confirmation date variable
##' @param verbose Logical, defaults to `FALSE`. Should progress messages be printed
##' @param resample Logical, defaults to `FALSE`. Should the supplied delays be resampled.
##' @return A list of function that takes one parameter, `n`, the number of reporting
##'   delays to randomly sample
##' @importFrom dplyr filter
##' @importFrom purrr map map2
##' @importFrom rstan extract
##' @importFrom loo loo relative_eff extract_log_lik
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

  # Fit gamma and exponential models
  fit_exp <- EpiNow::dist_fit(confirmation_delays, samples = samples, dist = "exp")
  
  # If there is enough data, try fitting a gamma  
  if(nrow(confirmation_delays) >= 30) {
    
    fit_gam <- EpiNow::dist_fit(confirmation_delays, samples = samples, dist = "gamma")
    # Extract log likelihoods
    log_lik_exp <- loo::extract_log_lik(fit_exp, merge_chains = FALSE)
    log_lik_gam <- loo::extract_log_lik(fit_gam, merge_chains = FALSE)
    # Calculate relative efficiencies
    rel_exp <- loo::relative_eff(exp(log_lik_exp))
    rel_gam <- loo::relative_eff(exp(log_lik_gam))
    # Estimate looic
    loo_exp <- loo::loo(log_lik_exp, r_eff = rel_exp)
    loo_gam <- loo::loo(log_lik_gam, r_eff = rel_gam)
    # Choose best model
    best_model <- ifelse(loo_exp$estimates[3,1] < loo_gam$estimates[3,1], "exp", "gamma")
    
  }else{
    best_model <- "exp"
  }
  
  if(best_model == "exp"){
    
    delay_rate <- sample(rstan::extract(fit_exp)$lambda, samples)
    
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
    
  }else if(best_model == "gamma"){
    
    delay_alpha <- sample(rstan::extract(fit_gam)$alpha, samples)
    delay_beta <- sample(rstan::extract(fit_gam)$beta, samples)
    
    sample_functions <- purrr::map2(delay_alpha, delay_beta, function(alpha, beta){
      sample_function <- function(n, dist = FALSE, max_delay = NULL){
        if(!dist) {
          rgamma(n, alpha, beta)
        }else{
          if (length(n) > max_delay) {
            n <- 1:max_delay
          }
          pgamma(n, alpha, beta)
        }
      }
    })
    
  }
  
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
