
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
#' cases <- data.frame(date = seq(as.Date("2020-01-01"),
#'                                as.Date("2020-01-30"),
#'                                by = "days"),
#'                     cases = 1:30)
#'                     
#'                     
#' serial_intervals <- as.matrix(EpiNow::covid_serial_intervals[,1])
#' rt_prior <- list(mean_prior = 2.6, std_prior = 2) 
#' window <- c(1, 3, 7)
#' 
#' estimate_R0(cases, serial_intervals, 
#'             rt_prior = rt_prior, windows = windows,
#'             rt_samples = 10, si_samples = 1)
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

  
  ## Summarise cases
  summed_cases <- cases %>% 
    dplyr::group_by(date) %>% 
    dplyr::summarise(cases = sum(cases, na.rm = TRUE)) %>% 
    dplyr::ungroup()
  
  ## Sample serial intervals
  serial_intervals_index <- sample(1:ncol(serial_intervals),
                             si_samples,
                             replace = ncol(serial_intervals) < si_samples)

  windows <- c(1, 3, 7)
  return_best <- TRUE
  
  ### Estimate and score R over multiple windows
  est_r <- purrr::map(windows, 
                      function(window) {
                        
                        window_start <- seq(2, nrow(incid) - (window - 1))
                        window_end <- window_start + window - 1
                          
                        ### Estimate R across serial interval samples
                        est_r <- purrr::map(serial_intervals_index, function(index) {
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
                          
                          
                        })
                        
                        
                        ## Fix list structure
                        est_r <- purrr::transpose(est_r)
                        est_r <- purrr::map(est_r, unlist)
                        
                        ## Put into data frame by date
                        out <- tibble::tibble(
                          date = EpiNow::add_dates(incid$date, length(est_r)),
                          R = purrr::map(est_r,
                                         ~ tibble::tibble(R = ., sample = 1:length(.)))
                        )
                        
                        
                        ## Unnest dataframe
                        out <- out %>% 
                          tidyr::unnest(R)
                        
                        ## Forecast
                        preds <- out %>% 
                          dplyr::rename(rt = R) %>% 
                          dplyr::group_split(sample) %>% 
                          purrr::map_dfr(
                            ~iterative_case_forecast(rts = dplyr::select(., -sample), 
                                                     cases = summed_cases,
                                                     serial_interval = serial_intervals[, 1], 
                                                     horizon = 1),
                                         .id = "sample") %>% 
                          dplyr::mutate(sample = as.numeric(sample)) %>% 
                          dplyr::select(date, cases, horizon, sample)
                        
                        
                        ## Score the one day ahead iterative forecast
                        ## We don't need to loop over forecast_date here as in EpiSoon::evaluate_model
                        ## as we know we are only doing one-day lookaheads here (as not using forecast rts)
                        scores <- score_case_forecast(preds, summed_cases)
                        
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
  
 est_r <- dplyr::bind_rows(est_r)
 
 
 if (return_best) {
   est_r <- est_r %>% 
     dplyr::filter(score == min(score)) %>% 
     dplyr::filter(window == min(window)) %>% 
     dplyr::select(-score, -score_sd, -window)
 }

  return(out)
}



# Dev functions for EpiSoon -----------------------------------------------


iterative_case_forecast <- function(rts = NULL, cases = NULL, 
                                    serial_interval = NULL, horizon = NULL) {
  
  predictions <- purrr::map_dfr(rts$date - lubridate::days(1), function(target_date) {
    EpiSoon::predict_cases(cases, rts, serial_interval = serial_interval,
                           horizon = horizon, forecast_date = target_date) %>% 
      dplyr::mutate(forecast_date = target_date) %>% 
      dplyr::select(forecast_date, date, cases) %>% 
      dplyr::mutate(horizon = as.numeric(date - forecast_date))})
  
  return(predictions)
}


score_case_forecast <- function(pred_cases, obs_cases) {
  pred_cases <- pred_cases %>% 
    dplyr::rename(rt = cases)
  
  obs_cases <- obs_cases %>% 
    dplyr::rename(rt = cases)
  
  scores <- EpiSoon::score_forecast(pred_cases, obs_cases)
  
  return(scores)
}


rts <- out %>% 
  tidyr::unnest(R) %>% 
  dplyr::group_by(date) %>% 
  dplyr::mutate(sample = 1:dplyr::n()) %>% 
  dplyr::ungroup() %>% 
  dplyr::rename(rt = R) 

preds <- rts %>% 
  dplyr::group_split(sample) %>% 
  purrr::map_dfr(~iterative_case_forecast(rts = dplyr::select(., -sample), 
                                          cases = cases,
                                          serial_interval = serial_intervals[, 1], 
                                          horizon = 1),
                 .id = "sample") %>% 
  dplyr::mutate(sample = as.numeric(sample)) %>% 
  dplyr::select(forecast_date, date, cases, horizon, sample)


scores <- preds %>% 
  dplyr::group_split(forecast_date) %>% 
  setNames(unique(preds$forecast_date)) %>% 
  purrr::map_dfr(~score_case_forecast(dplyr::select(., -forecast_date),
                                      cases), .id = "forecast_date")

summarised_score <- scores %>% 
  dplyr::summarise(crps = mean(crps, na.rm = TRUE))