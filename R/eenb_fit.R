#' Fit the negative binomial version of EpiEstim
#'
#' @param nowcast_dir The directory of the nowcast to fit to
#'
#' @return
#' @export
#' @importFrom dplyr group_by ungroup
#' @importFrom tidyr complete
#' @importFrom rstan sampling extract
#'
#' @examples
#' 
eenb_fit <- function(nowcast_dir) {
  
  message("Reading nowcast...")
  nowcast <- readRDS(nowcast_dir)
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
  
  # dat$obs_local <- processed_nowcast$cases # For sharded versions
  dat$obs_local <- obs_local_mat
  
  obs_imported_mat <- matrix(0, nrow = dat$t, ncol = dat$k)
  # dat$obs_imported <- rep(0, dat$t * dat$k) # For sharded versions
  # dat$obs_imported <- obs_imported_mat
  
  dat$tau <- 5
  
  dat$w <- w
  
  dat$q <- 10
  
  message("Creating infectiousness matrix...")
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
  # dat$infectiousness <- as.vector(infectiousness_mat) # for sharding
  dat$infectiousness <- infectiousness_mat
  
  for(i in 1:nrow(dat$infectiousness)) {
    for(j in 1:ncol(dat$infectiousness)) {
      dat$infectiousness[i, j] <- ifelse(dat$infectiousness[i, j] == 0, 1E-06, dat$infectiousness[i, j])
    }
  }
  
  
  init_fun <- function() {list(R = rgamma(n = dat$t, shape = 1, scale = 5), 
                               phi = runif(1, 0, 1))}
  
  mod <- stanmodels$ee_negbinom
  
  message(paste0("Running for ",dat$k," SI & nowcast samples"))
  message(paste0("and ",dat$t," time steps..."))
  
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