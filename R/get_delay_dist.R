#' Get a Parameters that Defne a Discrete Delay
#'
#' @param delays Numeric vector of integer delays.
#' @param verbose Logical, defaults to `FALSE`. Should progress messages be printed
#' @param bootstraps Numeric, defaults to 1. The number of bootstrap samples (with replacement)
#'  of the delay distribution to take.
#' @param bootstrap_samples Numeric, defaults to 100. The number of samples to take in each boostrap.
#' When the sample size of the supplied delay distribution is less than 100 this is used instead.
#' @return A data.table of delay distributions and the parameters that define them.
#' @importFrom purrr map map2 flatten
#' @importFrom future.apply future_lapply
#' @importFrom rstan extract
#' @importFrom data.table data.table rbindlist
#' @importFrom loo loo relative_eff extract_log_lik
#' @author Sebastian Funk <sebastian.funk@lshtm.ac.uk>
#'
#' @export
#'
#' @examples
#'
#' ## Example with exponential and a small smaple
#' delays <- rexp(20, 1)
#'
#' get_delay_dist(delays, samples = 10, verbose = TRUE)
#'
#'
#' ## Example with gamma and a larger sample
#' delays <- rgamma(100, 4, 1)
#'
#' out <- get_delay_dist(delays, samples = 2, bootstraps = 2)
#'
#' ## Inspect
#' out
#' 
#' ## Inspect one parameter
#' out$params[[1]]
#' 
#' 
#' ## Load into delay skeleton and sample with truncation
#' EpiNow::delay_dist_skel(10, model = out$model[[1]],
#'                         params = out$params[[1]],
#'                         max_delay = out$max_delay[[1]])
get_delay_dist <- function(delays, verbose = FALSE, samples = 1,
                                bootstraps = 1, bootstrap_samples = 250) {

  if (samples < bootstraps) {
    samples <- bootstraps
  }
  ## Make delays integer if not
  delays <- as.integer(delays)
  ## Remove NA delays
  delays <- delays[!is.na(delays)]
  ## Filter out negative delays
  delays <- delays[delays >= 0]
  
  get_single_delay <- function(delays = NULL, samples = 1) {
    
    ## Delay structure
    out <- data.table::data.table(
      max_delay = max(delays) + 1
    )
    
    # Fit gamma and exponential models
    fit_exp <- EpiNow::dist_fit(delays, samples = samples, dist = "exp")
    
    # If there is enough data, try fitting a gamma  
    if(length(delays) >= 30) {
      
      fit_gam <- EpiNow::dist_fit(delays, samples = samples, dist = "gamma")
      
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
    
    ## Add model definition to output
    out <- out[, model := best_model]
    
    if(best_model == "exp"){
      if (verbose) {
        message("Exponential selected as the best fit for the delay distribution")
      }
      delay_rate <- sample(rstan::extract(fit_exp)$lambda, samples)
      
      ## Add parameters to output
      out <- out[, params := list(purrr::map(delay_rate, ~ list(rate = .)))]
      
    }else if(best_model == "gamma"){
      if (verbose) {
        message("Gamma selected as the best fit for the delay distribution")
      }

      delay_alpha <- sample(rstan::extract(fit_gam)$alpha, samples)
      delay_beta <- sample(rstan::extract(fit_gam)$beta, samples)
      
      out <- out[, params := list(purrr::map2(delay_alpha, delay_beta,
                                         ~ list(alpha = .x, beta = .y)))]
    } 
    
    ## Unnest parameter lists
    if (samples != 1) {
      out <- out[, .(params = purrr::flatten(params)),
                 by = c("model", "max_delay")]
    }
   
 return(out)
  }

  
  if (bootstraps == 1) {
    delay_defs <- get_single_delay(delays, samples = samples)
  }else{
    ## Fit each sub sample
    delay_defs <- future.apply::future_lapply(1:bootstraps,
                   function(boot){get_single_delay(sample(delays, 
                                                             min(length(delays), bootstrap_samples),
                                                             replace = TRUE),
                                                      samples = ceiling(samples / bootstraps))},
                   future.scheduling = 10)
           
    ## Bind distributions together               
    delay_defs <- data.table::rbindlist(delay_defs)
    
    ## Resample without replacement to force the correct number of samples.
    indexes <- sample(1:nrow(delay_defs), samples, replace = FALSE)
    
    delay_defs <- delay_defs[indexes]
  }

  return(delay_defs)
}
