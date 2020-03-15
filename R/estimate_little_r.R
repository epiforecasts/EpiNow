#' Estimate r
#'
#' @param sample A dataframe containing a numeric cases variable.
#' @param min_time Numeric, minimum time to use to fit the model.
#' @param max_time Numeric, maximum time to use to fit the model.
#'
#' @return A dataframe containing an estimate of r, its standard deviation and a
#' measure of the goodness of fit.
#' @export
#' @importFrom dplyr filter mutate select
#' @importFrom tibble tibble
#' @examples
#'
#'
estimate_little_r <- function(sample, min_time = NULL, max_time = NULL) {
  ## Replace for 0 values
  zero_replace <- log(1e-10)
  ## Add time var
  sample <- sample %>%
    dplyr::mutate(time = 1:length(cases)) %>%
    dplyr::mutate(cases = ifelse(cases == 0, zero_replace, log(cases)))

  ## Add all data if time windows are not given
  if (is.null(min_time)) {
    min_time <- min(sample$time, na.rm = TRUE)
  }

  if (is.null(max_time)) {
    max_time <- max(sample$time, na.rm = TRUE)
  }

  ## Limit data based on time window supplied
  sample <- sample %>%
    dplyr::filter(time >= min_time, time <= max_time)

  ## Fit log model
  model <- lm(sample$cases ~ sample$time)

  model_sum <- summary(model)

  ## Extract little r and summary measures
  result <- tibble::tibble(r = model_sum$coefficients[2, 1],
                           sd = model_sum$coefficients[2, 2],
                           fit_meas = model_sum$adj.r.squared)

  return(result)
}
