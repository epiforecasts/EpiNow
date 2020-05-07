#' Delay Distribution Skeleton
#'
#' @description This function acts as a skeleton of for a truncated defined by 
#' model type, maximum delay and model parameters. It is designed to be used with the
#' output from `get_delay_dist`.
#' @param n Numeric vector, number of samples to take (or days for the probability density).
#' @param dist Logical, defaults to `FALSE`. Should the probability density be returned rather
#' than a number of samples. 
#' @param cum Logical, defaults to `TRUE`. If `dist = TRUE` should the returned distribution be 
#' cumulative.
#' @param model Character string, defining the model to be used. Supported options are exponential 
#' ("exp") and gamma ("gamma").
#' @param params A list of parameters values (by name) required for each model. For the exponential model
#' this is a rate parameter and for the gamma model this is alpha and beta.
#' @param max_delay Numeric, the maximum delay to allow. Defaults to 120. Samples outside 
#' of this range are resampled.
#'
#' @return A vector of samples or a probability distribution.
#' @export
#'
#' @examples
#' 
#' ## Exponential model
#' 
#' ## Sample
#' delay_dist_skel(10, model = "exp", params = list(rate = 1))
#' 
#' ## Cumulative prob density
#' delay_dist_skel(1:10, model = "exp", dist = TRUE, params = list(rate = 1))
#' 
#' ## Probability density
#' delay_dist_skel(1:10, model = "exp", dist = TRUE, 
#'                 cum = FALSE, params = list(rate = 1))
#' 
#' ## Gamma model
#' 
#' delay_dist_skel(10, model = "gamma", params = list(alpha = 1, beta = 2))
#' 
#' ## Cumulative prob density
#' delay_dist_skel(0:10, model = "gamma", dist = TRUE,
#'                 params = list(alpha = 1, beta = 2))
#' 
#' ## Probability density
#' delay_dist_skel(0:10, model = "gamma", dist = TRUE, 
#'                 cum = FALSE, params = list(alpha = 2, beta = 2))
#'     

delay_dist_skel <- function(n, dist = FALSE, cum = TRUE, model,
                            params, max_delay = 120) {
  
  if (model %in% "exp") {
    ## Define support functions for exponential dist
    rdist <- function(n) {rexp(n, params$rate)}
    pdist <- function(n) {pexp(n, params$rate)}
    ddist <- function(n) {pexp(n + 0.5, params$rate) -
        pexp(n - 0.4999, params$rate)}
  }else if (model %in% "gamma") {
    rdist <- function(n) {rgamma(n, params$alpha, params$beta)}
    pdist <- function(n) {pgamma(n, params$alpha, params$beta)}
    ddist <- function(n) {
      pgamma(n + 0.5, params$alpha, params$beta) -
        pgamma(n - 0.4999, params$alpha, params$beta)}
  }
  
  ## Define internal sampling function
  inner_skel <- function(n, dist = FALSE, cum = TRUE, max_delay = NULL){
    if(!dist) {
      rdist(n)
    }else{
      if (length(n) > max_delay) {
        n <- 1:max_delay
      }
      if (cum) {
        pdist(n)
      }else{
        ddist(n)
      }
    }
  }

  ## Define truncation wrapper
  truncated_skel <- function(n, dist, cum, max_delay) {
    n <- inner_skel(n, dist, cum, max_delay)
    
    if (!dist) {
      while(any(!is.na(n) & n >= max_delay)) {
        n <- ifelse(n >= max_delay, inner_skel(n), n)
      }
      
      n <- as.integer(n)
    }
    
    return(n)
  }
  
  ## Call function
  sample <- truncated_skel(n, dist = dist, cum = cum, max_delay = max_delay)
  
  return(sample)
}