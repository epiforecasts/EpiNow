#' Estimate r
#'
#' @param sample A datatable containing a numeric cases variable.
#' @param min_time Numeric, minimum time to use to fit the model.
#' @param max_time Numeric, maximum time to use to fit the model.
#'
#' @return A datatable containing an estimate of r, its standard deviation and a
#' measure of the goodness of fit.
#' @export
#' @examples
#'
#' cases <- data.table::setDT(EpiSoon::example_obs_cases)[, 
#'                           cases := as.integer(cases)]
#'
#' estimate_little_r(cases)
estimate_little_r <- function(sample, min_time = NULL, max_time = NULL) {
  ## Add time var
  sample <- sample[, time := 1:length(cases)]

  ## Add all data if time windows are not given
  if (is.null(min_time)) {
    min_time <- min(sample$time, na.rm = TRUE)
  }

  if (is.null(max_time)) {
    max_time <- max(sample$time, na.rm = TRUE)
  }

  ## Limit data based on time window supplied
  sample <- sample[time >= min_time][time <= max_time]

  ## Fit log model - adapted from the R0 package
  model <- glm(cases ~ time, family = poisson(), data = sample)

  model_sum <- summary(model)

  ## Extract little r and summary measures
  result <- tibble::tibble(r = model_sum$coefficients[2, 1],
                           sd = model_sum$coefficients[2, 2],
                           fit_meas = (model$null.deviance - model$deviance) /
                             (model$null.deviance))
  return(result)
}
