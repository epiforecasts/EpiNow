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
                                  confidence_adjustment, samples) {


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

    if (!missing(confidence_adjustment)){
      if (length(conf_meas) > length(confidence_adjustment)){
        confidence_adjustment <- c(confidence_adjustment,
                                   rep(1, length(conf_meas) - length(confidence_adjustment)))
      }
      
      if (length(conf_meas) < length(confidence_adjustment)){
        conf_meas <- c(conf_meas, 
                       rep(1, length(confidence_adjustment) - length(conf_meas)))
      }
      conf_meas <- conf_meas * confidence_adjusmtent
    }
    
    confidence[length(confidence):(length(confidence) - (length(cum_freq) - 1))] <- conf_meas

    return(data.table::data.table(date = dates,
                                  cases = x_cases,
                                  confidence = confidence))
  })

 return(out)
}
