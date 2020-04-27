#' Run the model
#'
#' @param String defining country to run model on (default = South Korea) 
#'
#' @return List of two plots to show model fit
#' @export
#' @useDynLib EpiNow, .registration=TRUE
#' @importFrom NCoVUtils get_ecdc_cases
#' @importFrom dplyr filter 
#' @importFrom rstan sampling extract
#' @importFrom ggplot2 ggplot geom_line geom_ribbon geom_hline xlab ylab scale_x_date coord_cartesian geom_bar
#' @importFrom cowplot theme_cowplot
#'
#' @examples
#' 
run_epimodel <- function(country = "South_Korea", trunc = 7) {

sk <- NCoVUtils::get_ecdc_cases(countries = country)

sk <- sk %>% 
  dplyr::filter(date > "2020-01-31") 

dat_test <- list(t = nrow(sk), 
                 tau = 7,
                 si_loc = 1.3780732,
                 si_scale = 0.6184616,
                 inc_loc = 1.621,
                 inc_scale = 0.418,
                 delay_alpha = 1.3218,
                 delay_beta  = 0.2359536,
                 obs_local = sk$cases,
                 obs_imported = rep(0, nrow(sk)))

model <- stanmodels$epimodel

fit <- rstan::sampling(model, 
                       iter = 2000, 
                       data = dat_test, 
                       chains = 4,
                       control = list(adapt_delta = 0.8),
                       include = FALSE,
                       pars = c("delaymat","conv_delay_mat","inc_conv_delay",
                                "delayvec", "conv_delay", "ci"))

res <- rstan::extract(fit)

res_full <- data.frame(meda = apply(res$inf_R,MARGIN = 2, median),
                       UQa = apply(res$inf_R, MARGIN = 2, 
                                   FUN = function(x){quantile(x, prob=0.975)}),
                       LQa = apply(res$inf_R, MARGIN = 2, 
                                   FUN = function(x){quantile(x, prob=0.025)}),
                       med = apply(res$R,MARGIN = 2, median),
                       UQ = apply(res$R, MARGIN = 2, 
                                  FUN = function(x){quantile(x, prob=0.975)}),
                       LQ = apply(res$R, MARGIN = 2, 
                                  FUN = function(x){quantile(x, prob=0.025)}),
                       date = sk$date,
                       model = "stan_full",
                       
                       confirm = c(apply(res$inf_cases,MARGIN = 2, median),rep(0,7))
)

min_date <- sk$date[which(sk$cases > 10)][1]

p2 <- res_full %>%
  dplyr::filter(date < (max(sk$date) - 7), date >= min_date) %>%
  ggplot2::ggplot(ggplot2::aes(x=date)) + 
  ggplot2::geom_line(ggplot2::aes(y = meda), col = "blue") +
  ggplot2::geom_ribbon(ggplot2::aes(ymin = LQa, ymax = UQa), alpha = 0.1, fill= "blue") + 
  cowplot::theme_cowplot() + 
  ggplot2::geom_hline(yintercept = 1, lty = 2) +
  ggplot2::ylab("Reproduction number") + 
  ggplot2::xlab("") +
  ggplot2::scale_x_date(date_labels = "%b %d", date_breaks = "2 weeks", 
                        limits = c(min_date,max(sk$date))) +
  ggplot2::coord_cartesian(ylim = c(0,NA))


p1 <- data.frame(date = sk$date, confirm = sk$cases) %>%
  dplyr::filter(date >= min_date) %>%
  ggplot2::ggplot(ggplot2::aes(x = date, y = confirm)) + 
  ggplot2::geom_bar(stat="identity") +
  ggplot2::geom_bar(data = subset(res_full,date < (max(sk$date) - 7) & date >= min_date) , 
                    stat = "identity", fill = "red2", alpha = 0.5) +
  ggplot2::scale_x_date(date_labels = "%b %d", date_breaks = "2 weeks") +
  cowplot::theme_cowplot() +
  ggplot2::ylab("Daily cases by infection/confirmation date")



return(list(p1, p2))
}