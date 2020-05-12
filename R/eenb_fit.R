#' Fit the negative binomial version of EpiEstim
#'
#' @param nowcast_dir The directory of the nowcast to fit to
#'
#' @return
#' @export
#' @importFrom rstan sampling extract
#'
#' @examples
#' 
eenb_fit <- function(nowcast_dir, geration_times) {
  
  message("Reading nowcast...")
  nowcast <- readRDS(nowcast_dir)[type %in% "infection_upscaled"][, type := NULL]
  nowcast <- nowcast[, sample := as.numeric(sample)]
  ## Make sure there are no missing dates
  nowcast_grid <- data.table::copy(nowcast)[,
       .(date = seq(min(date), max(date), by = "days"),
         sample = list(1:max(sample)), 
         import_status = list(list("local", "imported")))][,
       .(sample = unlist(sample), import_status), by = c("date")][,
       .(import_status = unlist(import_status)), by = c("date", "sample")]
  
  nowcast <-  merge(nowcast, nowcast_grid, 
                    by = c("date", "sample", "import_status"))
  
  nowcast <-  nowcast[is.na(cases), cases := 0 ][,
                      .(sample = as.numeric(sample), date = date, 
                        cases, import_status)]
  nowcast <- data.table::setorder(nowcast, import_status, sample, date)
  
  ## Split into local and imported cases
  local_cases <- nowcast[import_status == "local"][, import_status := NULL]
  imported_cases <- nowcast[import_status == "imported"][, import_status := NULL]
  
  dat <- list(t = length(unique(local_cases$date)),
              k = length(unique(local_cases$sample)))
  
  
  w <- EpiNow::covid_generation_times[,1:dat$k]
  dat$n <- dim(w)[1]
  
  obs_local_mat <- matrix(local_cases$cases,
                          byrow = FALSE,
                          nrow = dat$t,
                          ncol = dat$k)

  dat$obs_local <- obs_local_mat
  
  obs_imported_mat <- matrix(0, nrow = dat$t, ncol = dat$k)
  
  
  dat$tau <- 5
  
  dat$w <- w
  
  dat$q <- 10
  
  message("\nCreating infectiousness matrix...")
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
  
  mod <- stanmodels$epistim
  
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