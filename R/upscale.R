#' Draw with an offset from a negative binomial distribution
#'
#' @description Samples size (the number of trials) of a binomial distribution
#'  copied from https://github.com/sbfnk/bpmodels/blob/master/R/utils.r
#' @param n Numeric, number of samples to draw
#' @param x Numeric, offset.
#' @param prob Numeric, probability of successful trial
#' @param max_upscale Numeric, maximum upscaling of cases allowed at each time point
#' @export
#' @examples
#' x <- c(0:10)
#' prob <- pgamma(1:11, 4, 2)
#'
#' rbinom_size(length(x), x, prob, max_upscale = 10)
#'
rbinom_size <- function(n, x, prob, max_upscale) {
  y <- ifelse(is.na(x), 0, x + stats::rnbinom(n, x + 1, prob))
  
  if (!missing(max_upscale)) {
    y <- ifelse(y > max_upscale * (x + 1),  max_upscale * (x + 1), y)
  }
  
  return(y)
}


#' Adjust Case Counts for Truncation
#'
#' @param cases Numeric vector of cases
#' @param cum_freq Numeric vector of cumulative frequencies
#' @param confidence_adjustment Numeric vector of frequencies used to adjust confidence
#' @param dates Character vector of dates
#' @param samples Numeric, number of samples to take
#' @return
#' @export
#' @inheritParams rbinom_size
#' @importFrom purrr map
#' @importFrom data.table data.table
#' @examples
#'
#'
adjust_for_truncation <- function(cases, cum_freq, dates, 
                                  confidence_adjustment = NULL,
                                  samples, max_upscale) {
  
  
  out <- purrr::map(seq_len(samples), function(sample) {
    
    ## Sample cases
    x_cases <- cases
    x_cases[length(cases):(length(cases) - (length(cum_freq) - 1))] <-
      EpiNow::rbinom_size(
        length(cum_freq),
        cases[length(cases):(length(cases) - (length(cum_freq) - 1))],
        cum_freq, max_upscale = max_upscale)
    
    ## Add confidence based on the cumulative frequency
    confidence <- rep(1, length(x_cases))
    
    conf_meas <- cum_freq
    
    if (!is.null(confidence_adjustment)){
      if (length(conf_meas) > length(confidence_adjustment)){
        confidence_adjustment <- c(confidence_adjustment,
                                   rep(1, (length(conf_meas) - length(confidence_adjustment))))
      }
      
      if (length(conf_meas) < length(confidence_adjustment)){
        conf_meas <- c(conf_meas, 
                       rep(1, length(confidence_adjustment) - length(conf_meas)))
      }
      conf_meas <- conf_meas * confidence_adjustment
    } 
    
    confidence[length(confidence):max(1, ((length(confidence) - (length(conf_meas) - 1))))] <- 
      conf_meas[1:min(length(confidence), length(conf_meas))]
    
    return(data.table::data.table(date = dates,
                                  cases = x_cases,
                                  confidence = confidence))
  })
  
  return(out)
}