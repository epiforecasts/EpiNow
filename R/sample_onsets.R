#' Sample onset times
#'
#' @param onsets Numeric vector of onset times
#' @param cum_freq Numeric vector of cumulative frequencies
#' @param dates Character vector of dates
#' @param samples Numeric, number of samples to take
#' @param report_delay Numeric, delay between the last data point and the date of the report
#' @return
#' @export
#' @importFrom purrr map
#' @importFrom tibble tibble
#' @examples
#'
#'
sample_onsets <- function(onsets = NULL, cum_freq = NULL, dates = NULL, samples = NULL, report_delay = NULL) {


  out <- purrr::map(seq_len(samples), function(sample) {

    ## Sample onsets
    x_onsets <- onsets
    x_onsets[length(onsets):(length(onsets) - (length(cum_freq) - 1))] <-
      EpiNow::rbinom_size(
        length(cum_freq),
        onsets[length(onsets):(length(onsets) - (length(cum_freq) - 1))],
        cum_freq)

    ## Add confidence based on the cumulative frequency
    confidence <- rep(1, length(x_onsets))
    confidence[length(confidence):(length(confidence) - (length(cum_freq) - 1))] <- cum_freq

    return(tibble::tibble(date = dates,
                          cases = x_onsets,
                          confidence = confidence))
  })

 return(out)
}
