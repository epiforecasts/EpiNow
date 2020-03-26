
#' Estimate the time varying R0 - using EpiEstim
#'
#' @param cases A dataframe containing a list of local cases with the following variables: `date`, `cases`, and `import_status`
#' @param serial_intervals A matrix with columns representing samples and rows representing the probability of the serial intervel being on
#' that day.
#' @param rt_prior A list defining the reproduction number prior containing the mean (`mean_prior`) and standard deviation (`std_prior`)
#' @param windows Numeric vector, windows over which to estimate time-varying R. The best performing window will be 
#' selected per serial interval sample by default (based on which window best forecasts current cases). 
#' @param return_best Logical defaults to `TRUE`. Should the estimates for the best performing window be returned or estimates 
#' for all windows.
#' @param si_samples Numeric, the number of samples to take from the serial intervals supplied
#' @param rt_samples Numeric, the number of samples to take from the estimated R distribution for each time point.
#' @param min_est_date Date to begin estimation.
#' @inheritParams add_dates
#' @return A tibble containing the date and summarised R estimte.
#' @export
#' @importFrom EpiEstim estimate_R make_config
#' @importFrom tidyr drop_na complete spread
#' @importFrom dplyr rename full_join pull
#' @importFrom tibble tibble
#' @importFrom purrr map2
#' @examples
#'
#' cases <- data.frame(date = seq(as.Date("2020-01-01"),
#'                                as.Date("2020-01-30"),
#'                                by = "days"),
#'                     cases = 1:30)
#'                     
#'                     
#' serial_intervals <- as.matrix(EpiNow::covid_serial_intervals[,1])
#' rt_prior <- list(mean_prior = 2.6, std_prior = 2) 
#' windows <- c(1, 3, 7)
#' 
#' estimate_R0(cases, serial_intervals, 
#'             rt_prior = rt_prior, windows = windows,
#'             rt_samples = 10, si_samples = 2, return_best = FALSE)
estimate_R0 <- function(cases = NULL, serial_intervals = NULL,
                        rt_prior = NULL, windows = NULL, 
                        si_samples = 100, rt_samples = 100,
                        return_best = TRUE, min_est_date = NULL) {
 
 
  ## Adjust input based on the presence of imported cases
  if (length(unique(cases$import_status)) > 1) {
    incid <- cases %>%
      dplyr::select(date, cases, import_status) %>% 
      tidyr::spread(key = "import_status", value = "cases") %>%
      tidyr::complete(date = seq(min(date), max(date), by = "day"),
                      fill = list(local = 0, imported = 0)) 
    
    ## Predict cases forward in time using just local cases
    summed_cases <- incid %>% 
      dplyr::rename(cases = local) %>% 
      dplyr::select(-imported)
  
  }else{
    incid <- cases %>%
      dplyr::select(date, cases) %>% 
      dplyr::rename(I = cases) %>%
      tidyr::complete(date = seq(min(date), max(date), by = "day"),
                      fill = list(I = 0))
    
    summed_cases <- incid %>% 
      dplyr::rename(cases = I)
  }

  ## Calculate when to start the window estimation of Rt
  min_case_date <- summed_cases %>% 
    dplyr::filter(cases > 0) %>% 
    dplyr::pull(date) %>% 
    min()
  
  wait_time <- as.numeric(min_est_date - min_case_date) + 1
  
  if (wait_time > nrow(incid)){
    wait_time <- nrow(incid)
  }
  
  if (wait_time <= 2) {
    wait_time <- 2
  }
  

  ## Sample serial intervals
  serial_intervals_index <- sample(1:ncol(serial_intervals),
                             si_samples,
                             replace = ncol(serial_intervals) < si_samples)

   
  ### Estimate R across serial interval samples
  est_r <- purrr::map_dfr(serial_intervals_index, function(index) {
    
    ### Estimate and score R over multiple windows
    est_r <- purrr::map_dfr(windows, 
                        function(window) {
                          
                          window_start <- seq(wait_time - window,
                                              nrow(incid) - (window - 1))
                          window_end <- window_start + window - 1
                          
                          
                          ## estimate R
                          R <- suppressWarnings(
                            EpiEstim::estimate_R(incid,
                                                 method = "si_from_sample",
                                                 si_sample = serial_intervals[, index],
                                                 config = do.call(EpiEstim::make_config,
                                                                  c(rt_prior, 
                                                                    list(t_start = window_start,
                                                                         t_end = window_end)
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
                          
                          
                          ## Put into data frame by date
                          out <- tibble::tibble(
                            date = EpiNow::add_dates(incid$date, length(R_samples)),
                            R = purrr::map(R_samples,
                                           ~ tibble::tibble(R = ., sample = 1:length(.)))
                          ) %>% 
                            tidyr::unnest(R)
                          
                          ## Make current case predictions from past cases and current Rt values
                          preds <- out %>% 
                            dplyr::rename(rt = R) %>% 
                            dplyr::group_split(sample) %>% 
                            purrr::map_dfr(
                              ~ EpiSoon::predict_current_cases(
                                rts = dplyr::select(., -sample), 
                                cases = summed_cases,
                                serial_interval = serial_intervals[, index]
                              ), .id = "sample") %>% 
                            dplyr::mutate(sample = as.numeric(sample), 
                                          horizon = 0) %>% 
                            dplyr::select(date, cases, sample, horizon)
                          
                          ## Score the forecast
                          scores <- EpiSoon::score_case_forecast(preds, summed_cases)
                          
                          ## Evaluate the window using the mean CRPS across all time points and samples
                          summarised_score <- scores %>% 
                            dplyr::summarise(mean = mean(crps, na.rm = TRUE),
                                             sd = sd(crps, na.rm = TRUE))
                          
                          out <- out %>% 
                            dplyr::mutate(
                              score = summarised_score$mean,
                              score_sd = summarised_score$sd,
                              window = window
                            )
                          
                          return(out)
                        })
    
    ## Return the best performing window
    if (return_best) {
      est_r <- est_r %>% 
        dplyr::filter(score == min(score)) %>% 
        dplyr::filter(window == min(window)) %>% 
        dplyr::select(-score, -score_sd)
    }
    
   return(est_r) 
  }, .id = "si_sample")
  

  if (si_samples == 1) {
    return(est_r)
  }else{
    est_r <- dplyr::mutate(est_r, sample = sample * as.numeric(si_sample)) %>% 
      dplyr::select(-si_sample)
    
    return(est_r)
}

}



