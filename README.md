
# EpiNow: Estimate realtime case counts and time-varying epidemiological parameters

[![R-CMD-check](https://github.com/epiforecasts/EpiNow/workflows/R-CMD-check/badge.svg)](https://github.com/epiforecasts/EpiNow/actions)
[![Codecov test
coverage](https://codecov.io/gh/epiforecasts/EpiNow/branch/master/graph/badge.svg)](https://codecov.io/gh/epiforecasts/EpiNow?branch=master)
[![DOI](https://zenodo.org/badge/247464257.svg)](https://zenodo.org/badge/latestdoi/247464257)

This packages estimates the time-varying reproduction number, rate of
spread and doubling time using a range of open-source tools and current
best practices. It aims to help users avoid some of the limitations of
naive implementations in a framework that is informed by community
feedback and is under active development. It assumes that only limited
data is available on cases by date of onset and instead uses cases by
date of report. These are then imputed to case counts by date of
infection using an uncertain reporting delay and incubation period.
Right truncation of cases is dealt with internally by `{EpiNow}`, as is
propogating uncertainty from all inputs into the final parameter
estimates (helping to mitigate spurious findings). Time-varying
estimates of the reproduction number are estimated using the
[`{EpiEstim}`](https://github.com/annecori/EpiEstim) package by date of
infection with a generation time estimate that includes uncertainty.
Time-varying estimates of the rate of growth are derived using a
quasipoisson GLM with a sliding window, which are then used to estimate
the doubling time. Optimal windows are chosen by using one day ahead
case prediction. Optionally, the time-varying reproduction number can be
forecast forwards in time using an integration with the
[`{EpiSoon}`](https://epiforecasts.io/EpiSoon) package and converted to
a case forecast using a branching process. See the
[methods](https://epiforecasts.io/covid/methods.html) section of our
Covid-19 site for a detailed discussion of the approach.

## Installation

Install the stable version of the package using
[`{drat}`](https://epiforecasts.io/drat/):

``` r
install.packages("drat")
drat:::add("epiforecasts")
install.packages("EpiNow")
```

Install the development version of the package with:

``` r
remotes::install_github("epiforecasts/EpiNow")
```

For simple deployment/development a prebuilt docker image is also
available
[here](https://github.com/epiforecasts/EpiNow/packages/246723).

## Quick start

`{EpiNow}` is designed to be used at scale with few changes to the
defaults and a single function call or to be used in an ad-hoc fashion
via individual function calls. In the following section we give an
overview of the simple use case. For more on using each function see the
[function
documentation](https://epiforecasts.io/EpiNow/reference/index.html) and
[introductory
vignette](https://epiforecasts.io/EpiNow/articles/getting-started.html).
A working implementation for COVID-19 can be found
[here](https://github.com/epiforecasts/covid-global/blob/master/update_nowcasts.R).
This quick start requires the following packages:

``` r
library(EpiNow)
library(EpiSoon)
library(data.table)
```

### Reporting delays

Reporting delays can either be fitted using package functionality or
determined elsewhere and then defined with uncertainty for use in
`{EpiNow}`. When data is supplied an interval censored gamma and
exponential distribution will be fit and then compared using the `{loo}`
package. Note that in this example a single bootstrap is used (i.e no
bootstrap) but in real scenarios multiple bootstraps should be used to
represent the uncertainty in the reported distribution (and to
approximate stochastic change over time). The commented code (requires
the `{future}` package) can be used to parallelise long running sections
of code.

``` r
# future::plan("multiprocess")
example_delays <- rexp(25, 1/10)

delay_dist <- EpiNow::get_dist_def(example_delays, 
                                   samples = 5, bootstraps = 1)

delay_dist
#>    model max_value params
#> 1:   exp        39 <list>
#> 2:   exp        39 <list>
#> 3:   exp        39 <list>
#> 4:   exp        39 <list>
#> 5:   exp        39 <list>
```

Alternatively an uncertain distribution can be defined (for example
based on literature estimates). Currently supported distributions are
the log normal and gamma
distributions.

``` r
delay_dist <- EpiNow::lognorm_dist_def(mean = 5, mean_sd = 1, sd = 3, sd_sd = 1,
                                       max_value = 30, samples = 5, to_log = TRUE)

delay_dist
#>      model params max_value
#> 1: lognorm <list>        30
#> 2: lognorm <list>        30
#> 3: lognorm <list>        30
#> 4: lognorm <list>        30
#> 5: lognorm <list>        30
```

### [Rt pipeline](https://epiforecasts.io/EpiNow/reference/rt_pipeline.html)

This wraps the core functionality of the package and includes results
reporting. It requires a data frame of cases by date of report and a
`dist_skel` compatible data frame of reporting delay distributions (as
produced by `get_dist_def` or `lognorm_dist_def` etc.). Internally
current best estimates of the incubation period, generation time and
reproduction number are then used but these can also be manually
specified (see
[here](https://github.com/epiforecasts/EpiNow/tree/master/data-raw) for
the code that generates these estimates). Whilst defaults are likely to
work for most users [the
documentation](https://epiforecasts.io/EpiNow/reference/rt_pipeline.html)
provides additional options. For regions with high cases loads users
should consider using approximate sampling (`approx_delay`). Forecasting
is supported via `EpiSoon` and companion packages (see documentation for
an example). If data by date of onset is available this can be passed to
`linelist` and used rather than sampling dates of onsets - note this is
currently only partially supported functionality. Please open an issue
if this functionality is needed for your use case.

Save everything to a temporary directory - change this (`rt_pipeline`
will create the directory for you) to inspect the results

``` r
target_dir <- file.path(tempdir(), "test")
```

Load example case data from `{EpiSoon}` and convert to have the required
variable names (note imported cases are supported and should be
delinated with `import_status = "imported"`).

``` r
cases <- data.table::setDT(EpiSoon::example_obs_cases)
cases <- cases[, `:=`(confirm = as.integer(cases), import_status = "local")][,
                   cases := NULL]

tail(cases)
#>          date confirm import_status
#> 1: 2020-03-17     296         local
#> 2: 2020-03-18     343         local
#> 3: 2020-03-19     399         local
#> 4: 2020-03-20     454         local
#> 5: 2020-03-21     605         local
#> 6: 2020-03-22     367         local
```

Run the complete pipeline that includes nowcasting cases, estimating the
time-varying reproduction number, the rate of growth and the doubling
time. See the documentation for an example that incorporates
forecasting.

``` r
# future::plan("multiprocess")
rt_pipeline(cases = cases,
            delay_defs = delay_dist,
            target_date = max(cases$date),
            target_folder = target_dir)
```

List available output files.

``` r
list.files(target_dir, recursive = TRUE)
#>  [1] "2020-03-22/adjusted_r_latest.rds"          
#>  [2] "2020-03-22/bigr_eff_latest.rds"            
#>  [3] "2020-03-22/bigr_eff_max_estimate.rds"      
#>  [4] "2020-03-22/bigr_eff_plot.png"              
#>  [5] "2020-03-22/bigr_eff_plot.rds"              
#>  [6] "2020-03-22/bigr_estimates.rds"             
#>  [7] "2020-03-22/case_forecast.rds"              
#>  [8] "2020-03-22/cases_by_report.rds"            
#>  [9] "2020-03-22/cases_plot.png"                 
#> [10] "2020-03-22/current_cases.rds"              
#> [11] "2020-03-22/delays.rds"                     
#> [12] "2020-03-22/doubling_time_latest.rds"       
#> [13] "2020-03-22/incubation.rds"                 
#> [14] "2020-03-22/latest_date.rds"                
#> [15] "2020-03-22/nowcast.rds"                    
#> [16] "2020-03-22/plot_cases.rds"                 
#> [17] "2020-03-22/prob_control_latest.rds"        
#> [18] "2020-03-22/rate_spread_estimates.rds"      
#> [19] "2020-03-22/rate_spread_latest_summary.rds" 
#> [20] "2020-03-22/rate_spread_latest.rds"         
#> [21] "2020-03-22/rate_spread_overall_summary.rds"
#> [22] "2020-03-22/rate_spread_plot.png"           
#> [23] "2020-03-22/rate_spread_plot.rds"           
#> [24] "2020-03-22/region_summary.rds"             
#> [25] "2020-03-22/rt_cases_plot.png"              
#> [26] "2020-03-22/rt_cases_plot.rds"              
#> [27] "2020-03-22/summarised_littler.rds"         
#> [28] "2020-03-22/summarised_nowcast.rds"         
#> [29] "2020-03-22/summarised_reff.rds"            
#> [30] "2020-03-22/time_varying_littler.rds"       
#> [31] "2020-03-22/time_varying_params.rds"        
#> [32] "latest/adjusted_r_latest.rds"              
#> [33] "latest/bigr_eff_latest.rds"                
#> [34] "latest/bigr_eff_max_estimate.rds"          
#> [35] "latest/bigr_eff_plot.png"                  
#> [36] "latest/bigr_eff_plot.rds"                  
#> [37] "latest/bigr_estimates.rds"                 
#> [38] "latest/case_forecast.rds"                  
#> [39] "latest/cases_by_report.rds"                
#> [40] "latest/cases_plot.png"                     
#> [41] "latest/current_cases.rds"                  
#> [42] "latest/delays.rds"                         
#> [43] "latest/doubling_time_latest.rds"           
#> [44] "latest/incubation.rds"                     
#> [45] "latest/latest_date.rds"                    
#> [46] "latest/nowcast.rds"                        
#> [47] "latest/plot_cases.rds"                     
#> [48] "latest/prob_control_latest.rds"            
#> [49] "latest/rate_spread_estimates.rds"          
#> [50] "latest/rate_spread_latest_summary.rds"     
#> [51] "latest/rate_spread_latest.rds"             
#> [52] "latest/rate_spread_overall_summary.rds"    
#> [53] "latest/rate_spread_plot.png"               
#> [54] "latest/rate_spread_plot.rds"               
#> [55] "latest/region_summary.rds"                 
#> [56] "latest/rt_cases_plot.png"                  
#> [57] "latest/rt_cases_plot.rds"                  
#> [58] "latest/summarised_littler.rds"             
#> [59] "latest/summarised_nowcast.rds"             
#> [60] "latest/summarised_reff.rds"                
#> [61] "latest/time_varying_littler.rds"           
#> [62] "latest/time_varying_params.rds"
```

Read in and examine the output nowcast
cases.

``` r
summarised_nowcast <- readRDS(paste0(target_dir, "/latest/summarised_nowcast.rds"))

tail(summarised_nowcast)
#>          date                    type bottom top lower upper median mean
#> 1: 2020-03-17 Observed by report date     NA  NA    NA    NA    296   NA
#> 2: 2020-03-18 Observed by report date     NA  NA    NA    NA    343   NA
#> 3: 2020-03-19 Observed by report date     NA  NA    NA    NA    399   NA
#> 4: 2020-03-20 Observed by report date     NA  NA    NA    NA    454   NA
#> 5: 2020-03-21 Observed by report date     NA  NA    NA    NA    605   NA
#> 6: 2020-03-22 Observed by report date     NA  NA    NA    NA    367   NA
#>    confidence
#> 1:          1
#> 2:          1
#> 3:          1
#> 4:          1
#> 5:          1
#> 6:          1
```

Read in and examine the output time-varying
estimates.

``` r
time_varying_params <- readRDS(paste0(target_dir, "/latest/time_varying_params.rds"))

names(time_varying_params)
#> [1] "R0"             "rate_of_spread" "raw_R0"
```

Examine the summarised Rt estimates.

``` r
tail(time_varying_params$R0)
#>       type       date rt_type   bottom      top    lower    upper   median
#> 1: nowcast 2020-03-09 nowcast 1.217312 1.528710 1.305830 1.415065 1.389882
#> 2: nowcast 2020-03-10 nowcast 1.276451 1.591787 1.348404 1.484948 1.392524
#> 3: nowcast 2020-03-11 nowcast 1.245211 1.503579 1.268110 1.369768 1.359855
#> 4: nowcast 2020-03-12 nowcast 1.170007 1.517092 1.275032 1.393599 1.338198
#> 5: nowcast 2020-03-13 nowcast 1.187054 1.513738 1.275118 1.365197 1.301014
#> 6: nowcast 2020-03-14 nowcast 1.210802 1.431523 1.250140 1.337106 1.317296
#>        mean        std prob_control mean_window sd_window mean_crps   sd_crps
#> 1: 1.395579 0.10464534            0         2.6  1.658312     7.888 5.7534569
#> 2: 1.399404 0.10035833            0         2.6  1.384437     6.512 3.1815510
#> 3: 1.356776 0.08949395            0         4.8  2.179449     6.296 0.9877922
#> 4: 1.326924 0.10777089            0         2.4  1.527525     8.848 2.6814673
#> 5: 1.319871 0.10492874            0         2.6  1.658312     6.184 1.7803745
#> 6: 1.312543 0.07091372            0         3.6  2.000000     5.480 3.8674712
#>    R0_range
#> 1:   <list>
#> 2:   <list>
#> 3:   <list>
#> 4:   <list>
#> 5:   <list>
#> 6:   <list>
```

Examine the Rt and case
plot.

``` r
knitr::include_graphics(paste0(target_dir, "/latest/rt_cases_plot.png"))
```

### [Regional Rt pipeline](https://epiforecasts.io/EpiNow/reference/regional_rt_pipeline.html)

This function provides a wrapper to `{rt_pipeline}` that allows it to be
run on multiple regions at once with the same assumed report delay.

Define a new target directory.

``` r
target_dir <- file.path(tempdir(), "test-regional")
```

Define cases in multiple regions delineated by the region variable.

``` r
cases <- data.table::rbindlist(list(
   data.table::copy(cases)[, region := "testland"],
   cases[, region := "realland"]))
```

Run the pipeline on each region in turn. The commented code (requires
the `{future}` package) can be used to run regions in parallel (see
[here](https://github.com/epiforecasts/covid-global/blob/master/update_nowcasts.R)
for an optimised nested example).

``` r
# future::plan("multiprocess")
regional_rt_pipeline(cases = cases,
             delay_defs = delay_dist,
             target_folder = target_dir)
```

List output folders.

``` r
list.files(target_dir, recursive = FALSE)
#> [1] "realland" "testland"
```

Summarise the results accross all
regions.

``` r
EpiNow::regional_summary(results_dir = file.path(tempdir(), "test-regional"),
                         summary_dir = file.path(tempdir(), "test-summary"),
                         target_date = "latest",
                         region_scale = "Country",
                         csv_region_label = "country",
                         log_cases = TRUE)
```

An example of the summary output can be seen
[here](https://github.com/epiforecasts/covid-uk/tree/master/nowcast/regional-summary).

### Reporting templates

Rmarkdown templates are provided in the package (`templates`) for
semi-automated reporting of the estimates. These are currently
undocumented but an example integration can be seen
[here](https://github.com/epiforecasts/covid/blob/master/_posts/national/united-kingdom/united-kingdom.Rmd).
If using these templates to report your results please highlight our
[limitations](https://epiforecasts.io/covid/) as these are key to
understanding our results.

## Contributing

File an issue [here](https://github.com/epiforecasts/EpiNow/issues) if
you have identified an issue with the package. Please note that due to
operational constraints priority will be given to users informing
government policy or offering methodological insights. We welcome all
contributions, in particular those that improve the approach or the
robustness of the code base.
