
eenb_fit <- function(country) {
  
  country <- "Austria"
  nowcast <- readRDS(here::here("/covid/national",country,"/latest/nowcast.rds"))
  processed_nowcast <- nowcast %>%
    dplyr::group_by(sample) %>%
    tidyr::complete(date = seq.Date(to = max(nowcast$date),
                                    from = min(nowcast$date),
                                    by = "day"),
                    fill = list(cases = 0)) %>%
    dplyr::ungroup()
  
  dat <- list(t = length(unique(nowcast$date)),
              k = max(as.numeric(processed_nowcast$sample)))
  
  
  w <- EpiNow::covid_serial_intervals[,1:dat$k]
  dat$n <- dim(w)[1]
  
  obs_local_mat <- matrix(processed_nowcast$cases,
                          byrow = FALSE,
                          nrow = dat$t,
                          ncol = dat$k)
  
  dat$obs_local <- processed_nowcast$cases
  
  obs_imported_mat <- matrix(0, nrow = dat$t, ncol = dat$k)
  dat$obs_imported <- rep(0, dat$t * dat$k)
  
  dat$tau <- 5
  
  dat$w <- w
  
  dat$q <- 10
  
  infectiousness_mat <- matrix(0, nrow = dat$t, ncol = dat$k)
  # Calculate infectiousness at each timestep
  for(j in 1:dat$k) {
    for (s in 2:dat$t){
      for (i in 1:(s - 1)){
        ind = s - i;
        infectiousness_mat[s, j] = infectiousness_mat[s, j] + 
          ifelse(ind > dat$n, 0, (obs_imported_mat[i, j] + obs_local_mat[i, j]) * w[ind, j])
      }
    }
  }
  dat$infectiousness <- as.vector(infectiousness_mat)
  dat$infectiousness <- ifelse(dat$infectiousness == 0, 1E-06, dat$infectiousness)
  
  
  init_fun <- function() {list(R = rgamma(n = dat$t, shape = 1, scale = 5), 
                               phi = runif(1, 0, 1))}
  
  mod <- stanmodels$ee_negbinom
  
  fit <- rstan::sampling(mod,
                         data = dat,
                         init = init_fun,
                         chains = 4,
                         iter = 2000, 
                         cores = 4,
                         refresh = 50)
  
  res <- rstan::extract(fit)
  
  
  return(res)
}