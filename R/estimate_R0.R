
#' Estimate the time varying R0 - using EpiEstim
#'
#' @param cases A dataframe containing a list of local cases with the following variables: `date`, `cases`, and `import_status`
#' @param serial_intervals A matrix with columns representing samples and rows representing the probability of the serial intervel being on
#' that day.
#' @param rt_prior A list defining the reproduction number prior containing the mean (`mean_prior`) and standard deviation (`std_prior`)
#' @param windows Numeric vector, windows over which to estimate time-varying R. The best performing window will be 
#' selected per serial interval sample by default (based on which window best forecasts current cases). 
#' @param si_samples Numeric, the number of samples to take from the serial intervals supplied
#' @param rt_samples Numeric, the number of samples to take from the estimated R distribution for each time point.
#' @param min_est_date Date to begin estimation.
#' @param forecast_model An uninitialised bsts model passed to `EpiSoon::forecast_rt` to be used for forecasting
#' future Rt values. An example of the required structure is: `function(ss, y){bsts::AddSemilocalLinearTrend(ss, y = y)}`.
#' @param horizon Numeric, defaults to 0. The horizon over which to forecast Rts and cases.
#' @inheritParams add_dates
#' @return A tibble containing the date and summarised R estimte.
#' @export
#' @importFrom EpiEstim estimate_R make_config
#' @importFrom tidyr drop_na complete spread
#' @importFrom dplyr rename full_join pull
#' @importFrom tibble tibble
#' @importFrom purrr map2 safely compact map
#' @importFrom EpiSoon predict_current_cases forecast_rt forecast_cases score_case_forecast
#' @examples
#'
#' ## Nowcast Rts                  
#' estimates <- estimate_R0(cases = EpiSoon::example_obs_cases, 
#'                          serial_intervals = as.matrix(EpiNow::covid_serial_intervals[,1]), 
#'                          rt_prior = list(mean_prior = 2.6, std_prior = 2),
#'                          windows = c(1, 3, 7), rt_samples = 10, si_samples = 2,
#'                          min_est_date =  as.Date("2020-02-18"))
#'                          
#'                          
#' estimates$rts
#'  
#'## Nowcast Rts, forecast Rts and the forecast cases
#' estimates <- estimate_R0(cases = EpiSoon::example_obs_cases, 
#'                          serial_intervals = as.matrix(EpiNow::covid_serial_intervals[,1]), 
#'                          rt_prior = list(mean_prior = 2.6, std_prior = 2),
#'                          windows = c(1, 3, 7), rt_samples = 10, si_samples = 2,
#'                          min_est_date =  as.Date("2020-02-18"),
#'                          forecast_model = function(ss, y){bsts::AddSemilocalLinearTrend(ss, y = y)},
#'                          horizon = 7)
#'                                            
#'## Rt estimates and forecasts
#' estimates$rts
#' 
#' 
#' 
#' ## Case forecasts
#' estimates$cases
estimate_R0 <- function(cases = NULL, serial_intervals = NULL,
                        rt_prior = NULL, windows = NULL, 
                        si_samples = 100, rt_samples = 100,
                        min_est_date = NULL, forecast_model = NULL, 
                        horizon = 0) {
  
 
  ## Adjust input based on the presence of imported cases
  if (suppressWarnings(length(unique(cases$import_status)) > 1)) {
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
  
  
  ## Sample serial intervals
  serial_intervals_index <- sample(1:ncol(serial_intervals),
                             si_samples,
                             replace = ncol(serial_intervals) < si_samples)

   
  ### Estimate R across serial interval samples
  ### Forecast ahead if given a model and horizon for each sample
  estimates <- purrr::map(serial_intervals_index, function(index) {
    
    ### Estimate and score R over multiple windows
    est_r <- purrr::map_dfr(windows, 
                        function(window) {
                          
                          
                          window_start <- seq(max(wait_time - window, 2),
                                              nrow(incid) - window)
                          window_end <- window_start + window
                          
                          
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
    
    ## Select the best performing window
      est_r <- est_r %>% 
        dplyr::filter(score == min(score)) %>% 
        dplyr::filter(window == min(window)) %>% 
        dplyr::select(-score, -score_sd)
      
      
      
      if (horizon > 0 & !is.null(forecast_model)) {
        
        safe_forecast <- purrr::safely(EpiSoon::forecast_rt)
        
        ## Forecast Rts
        rt_forecasts <- est_r %>% 
          dplyr::select(date, rt = R, sample) %>% 
          dplyr::group_split(sample) %>% 
          purrr::map(
            ~ safe_forecast(.,
                            model = forecast_model, 
                             horizon = horizon,
                             samples = 1)[[1]])
        
        ## Get rid of null forecasts due to `bsts` errors and drop sample
        rt_forecasts <- purrr::compact(rt_forecasts)
        rt_forecasts <- purrr::map(rt_forecasts, ~ dplyr::select(., -sample))
        
        ## Bind Rts forecasts together
        rt_forecasts <- dplyr::bind_rows(rt_forecasts, .id = "sample") %>% 
          dplyr::mutate(sample = as.numeric(sample))
        
        ## Forecast cases
        case_forecasts <-   
          EpiSoon::forecast_cases(
            cases = summed_cases,
            fit_samples = rt_forecasts,
            rdist = rpois,
            serial_interval = serial_intervals[, index]
          ) %>% 
          dplyr::mutate(rt_type = "forecast")
        
        
        ## Join Rt estimates and forecasts
        est_r <- est_r %>% 
          dplyr::mutate(rt_type = "nowcast") %>% 
          dplyr::bind_rows(rt_forecasts %>% 
                             dplyr::mutate(rt_type = "forecast") %>% 
                             dplyr::select(date, R = rt, sample,  horizon, rt_type))
        
        return(list(rts = est_r, cases = case_forecasts))
        
      }else{
        
        ## Return just nowcast if no forecast has been run
        est_r <- est_r %>% 
          dplyr::mutate(rt_type = "nowcast", horizon = NA)
        
        return(list(rts = est_r))
      }
      
  })
  
    ## Reorganise list output
    estimates <- purrr::transpose(estimates)
  
  ## Organise returns to have a unique sample ID
  if (si_samples == 1) {
    return(estimates)
  }else{
    
    ## Function to bind si sample outputs
    join_si_samples <- function(df) {
      dplyr::bind_rows(df, .id = "si_sample") %>% 
        dplyr::mutate(sample = sample * as.numeric(si_sample)) %>% 
        dplyr::select(-si_sample)
    }
    
    ## Make sample unique for Rts
    estimates$rts <- join_si_samples(estimates$rts)
    
    ## If forecast has been run do the same for cases
   if (horizon > 0 & !is.null(forecast_model)) {
      estimates$cases <- join_si_samples(estimates$cases)
    }
     
    
    return(estimates)
  }

}



