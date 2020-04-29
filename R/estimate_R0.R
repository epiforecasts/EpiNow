
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
#' @return A tibble containing the date and summarised R estimte.
#' @export
#' @importFrom EpiEstim estimate_R make_config
#' @importFrom tidyr drop_na complete spread
#' @importFrom dplyr rename full_join pull bind_rows left_join
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
#'                          forecast_model = function(...){EpiSoon::fable_model(model = fable::ETS(y ~ trend("A")), ...)},
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
  

  ##Generic mean gamma sampler
  mean_rgamma <- function(samples, mean, sd) {
      theta <- sd^2/mean
      k <- mean/theta
      samples <- stats::rgamma(samples, shape = k, scale = theta)
      
      return(samples)
  }
  
  ## Adjust input based on the presence of imported cases
  if (suppressWarnings(length(unique(cases$import_status)) > 1)) {
    incid <-  
      dplyr::select(cases, 
                    date, cases, import_status) %>% 
      tidyr::spread(key = "import_status", value = "cases") %>%
      tidyr::complete(date = seq(min(date), max(date), by = "day"),
                      fill = list(local = 0, imported = 0)) 
    
    ## Predict cases forward in time using just local cases
    summed_cases <- 
      dplyr::rename(incid,
                    cases = local) %>% 
      dplyr::select(-imported)
  
  }else{
    incid <-
      dplyr::select(cases,
                    date, cases) %>% 
      dplyr::rename(I = cases) %>%
      tidyr::complete(date = seq(min(date), max(date), by = "day"),
                      fill = list(I = 0))
    
    summed_cases <- dplyr::rename(incid, cases = I)
  }
 
  ## Calculate when to start the window estimation of Rt
  min_case_date <- dplyr::filter(summed_cases, cases > 0)
  min_case_date <- dplyr::pull(min_case_date, date)
  min_case_date <- min(min_case_date)
  
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
    est_r <- purrr::map(windows, 
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
                                                    ~ mean_rgamma(rt_samples, .x, .y) %>% 
                                                     sort())
                          
                           
                          ## Put into data frame by date
                          out <- tibble::tibble(
                            date = EpiNow::add_dates(incid$date, length(R_samples)),
                            mean_R = R$`Mean(R)`,
                            sd_R = R$`Std(R)`,
                            R = purrr::map(R_samples,
                                           ~ tibble::tibble(R = ., sample = 1:length(.)))
                          )
                          
                          out <- tidyr::unnest(out, R)
                          
                          ## Make current case predictions from past cases and current Rt values
                          preds <-  
                            dplyr::rename(out, rt = R) %>% 
                            dplyr::group_split(sample)
                          
                          preds <- 
                            purrr::map(
                              preds, 
                              ~ EpiSoon::predict_current_cases(
                                rts = dplyr::select(., -sample), 
                                cases = summed_cases,
                                serial_interval = serial_intervals[, index]
                              ))
                          
                          preds <- dplyr::bind_rows(preds, .id  = "sample") %>% 
                            dplyr::mutate(sample = as.numeric(sample), 
                                          horizon = 0) 
                          
                          preds <- dplyr::select(preds, date, cases, sample, horizon)
                          
                          ## Score the forecast
                          scores <- EpiSoon::score_case_forecast(preds, summed_cases, 
                                                                 scores = "crps")
                          
                          ## Evaluate the window using the median CRPS across all time points and samples
                          summarised_score <-  
                            dplyr::summarise(scores,
                                             median = median(crps, na.rm = TRUE))
                          
                          out_list <-
                            list(
                              rt = dplyr::mutate(
                                out,
                                window = window
                              ),
                             score = summarised_score$median
                            )

                          
                          return(out_list)
                        }) 
    
    ## Extract scores, detect which is smallest and extract largest window with smallest score
    min_score_index <- purrr::map_dbl(est_r, 
                                  ~ .$score)
    min_score_index <- which(min_score_index == min(min_score_index, na.rm = TRUE))
    min_score_index <- min_score_index[length(min_score_index)]
    
    ##Extract the estimates with the lowest score
    est_r <- est_r[[min_score_index]]$r
      
      
      ## Check to see how many Rt data points have been returned
      ## If fewer than 3 then turn off forecasting
      if (length(unique(est_r$date)) < 3) {
        horizon <- 0
      }
      
      
      if (horizon > 0 & !is.null(forecast_model)) {
        
        safe_forecast <- purrr::safely(EpiSoon::forecast_rt)
        
        ## Forecast Rts using the mean estimate
        rt_forecasts <-
          dplyr::select(est_r, date, rt = mean_R) %>%
          unique() %>%
          safe_forecast(model = forecast_model, 
                        horizon = horizon,
                        samples = rt_samples)

        
        ##Forecast the variance using the same model structure
        sd_forecasts <- 
          dplyr::select(est_r, date, rt = sd_R) %>% 
          unique() %>% 
          safe_forecast(model = forecast_model, 
                        horizon = horizon,
                        samples = rt_samples)
        
        
        ## Drop the error catching element
        rt_forecasts <- rt_forecasts[[1]]
        sd_forecasts <- sd_forecasts[[1]]
        
        rt_forecasts <- dplyr::left_join(
          rt_forecasts,
          sd_forecasts, by = c("date", "sample", "horizon")
        )
        
        ## Add gamma noise  and drop mean and sd forecasts
        ## Assume that the minumum allowed gamma noise is 1e-4
        ## Zero sd will result in rgamma draws that are also 0 and so must be avoided
        rt_forecasts <- dplyr::mutate(rt_forecasts, 
                                      rt.y = ifelse(rt.y <= 0, 1e-4, rt.y),
                                      rt = purrr::map2_dbl(rt.x, rt.y, ~
                                                            mean_rgamma(1, mean = .x,
                                                                       sd = .y)))
        
        rt_forecasts <- dplyr::select(rt_forecasts,
                                      sample, date, horizon, rt)
        
        ## Forecast cases
        case_forecasts <-   
          EpiSoon::forecast_cases(
            cases = summed_cases,
            fit_samples = rt_forecasts,
            rdist = rpois,
            serial_interval = serial_intervals[, index]
          ) 
        
        case_forecasts <- 
          dplyr::mutate(case_forecasts, rt_type = "forecast")
        
        
        ## Join Rt estimates and forecasts
        est_r <- dplyr::mutate(est_r, rt_type = "nowcast") %>% 
          dplyr::select(-mean_R, -sd_R) %>% 
          dplyr::bind_rows(rt_forecasts %>% 
                             dplyr::mutate(rt_type = "forecast") %>% 
                             dplyr::select(date, R = rt, sample,  horizon, rt_type))
        
        return(list(rts = est_r, cases = case_forecasts))
        
      }else{
        
        ## Return just nowcast if no forecast has been run
        est_r <- dplyr::mutate(est_r, rt_type = "nowcast", horizon = NA)
        
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



