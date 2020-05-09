##' Draw with an offset from a negative binomial distribution
##'
##' @description Samples size (the number of trials) of a binomial distribution
##'  copied from https://github.com/sbfnk/bpmodels/blob/master/R/utils.r
##' @param n Numeric, number of samples to draw
##' @param x Numeric, offset.
##' @param prob Numeric, probability of successful trial
##' @export
rbinom_size <- function(n, x, prob) {
  x <- ifelse(is.na(x), 0, x + stats::rnbinom(n, x + 1, prob))
  
  return(x)
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
#' @importFrom purrr map
#' @importFrom data.table data.table
#' @examples
#'
#'
adjust_for_truncation <- function(cases, cum_freq, dates, 
                                  confidence_adjustment = NULL,
                                  samples) {
  
  
  out <- purrr::map(seq_len(samples), function(sample) {
    
    ## Sample cases
    x_cases <- cases
    x_cases[length(cases):(length(cases) - (length(cum_freq) - 1))] <-
      EpiNow::rbinom_size(
        length(cum_freq),
        cases[length(cases):(length(cases) - (length(cum_freq) - 1))],
        cum_freq)
    
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
    
    confidence[length(confidence):(length(confidence) - (length(cum_freq) - 1))] <- conf_meas
    
    return(data.table::data.table(date = dates,
                                  cases = x_cases,
                                  confidence = confidence))
  })
  
  return(out)
}