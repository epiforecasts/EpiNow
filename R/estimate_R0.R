
#' Estimate the time varying R0 - using EpiEstim
#'
#' @param cases A dataframe containing a list of local cases with the following variables: `date`, `cases`, and `import_status`
#' @param serial_intervals A matrix with columns representing samples and rows representing the probability of the serial intervel being on
#' that day.
#' @param rt_prior A list defining the reproduction number prior containing the mean (`mean_prior`) and standard deviation (`std_prior`)
#' @param window Numeric, the window over which to estimate time-varying R
#' @param si_samples Numeric, the number of samples to take from the serial intervals supplied
#' @param rt_samples Numeric, the number of samples to take from the estimated R distribution for each time point.
#' @inheritParams add_dates
#' @return A tibble containing the date and summarised R estimte.
#' @export
#' @importFrom EpiEstim estimate_R make_config
#' @importFrom tidyr drop_na complete spread
#' @importFrom dplyr rename full_join
#' @importFrom tibble tibble
#' @importFrom purrr map2
#' @examples
#'
#'
estimate_R0 <- function(cases = NULL, serial_intervals = NULL,
                        rt_prior = NULL, window = NULL, si_samples = 100,
                        rt_samples = 100) {


  ## Adjust input based on the presence of imported cases
  if (length(unique(cases$import_status)) > 1) {
    incid <- cases %>%
      tidyr::spread(key = "import_status", value = "cases") %>%
      tidyr::complete(date = seq(min(date), max(date), by = "day"),
                      fill = list(local = 0, imported = 0)) %>%
      dplyr::select(date, local, imported)
  }else{
    incid <- cases %>%
      dplyr::rename(I = cases) %>%
      tidyr::complete(date = seq(min(date), max(date), by = "day"),
                      fill = list(I = 0)) %>%
      dplyr::select(date, I)
  }

  serial_intervals_index <- sample(1:ncol(serial_intervals),
                             si_samples,
                             replace = ncol(serial_intervals) < si_samples)

  est_r <- purrr::map(serial_intervals_index, function(index) {
    ## estimate R
    R <- suppressWarnings(
      EpiEstim::estimate_R(incid,
                              method = "si_from_sample",
                              si_sample = serial_intervals[, index],
                              config = do.call(EpiEstim::make_config,
                                               c(rt_prior, list(t_start = seq(window, nrow(incid) - (window - 1)),
                                                                t_end = seq(window * 2 -1, nrow(incid)))
                                               )))$R
    )

    ## Filter out NA values
    R <- tidyr::drop_na(R, `Mean(R)`)

    ## Take samples from the assumed gamma distribution
    R_samples <- purrr::map2(R$`Mean(R)`, R$`Std(R)`,
                             function(mean, sd) {
                               theta <- sd^2/mean
                               k <- mean/theta
                               stats::rgamma(rt_samples, shape = k, scale = theta)
                             })


  })


  est_r <- purrr::transpose(est_r)
  est_r <- purrr::map(est_r, unlist)

  out <- tibble::tibble(date = EpiNow::add_dates(incid$date, length(est_r)),
                              R = est_r)

  return(out)
}
