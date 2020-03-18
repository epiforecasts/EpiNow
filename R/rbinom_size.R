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
