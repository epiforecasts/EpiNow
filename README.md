
# EpiNow: Estimate realtime case counts and time-varying epidemiological parameters 

![R-CMD-check](https://github.com/epiforecasts/EpiNow/workflows/R-CMD-check/badge.svg)
[![DOI](https://zenodo.org/badge/247464257.svg)](https://zenodo.org/badge/latestdoi/247464257)


This packages estimates the time-varying reproduction number, rate of spread and doubling time using a range of open-source tools and current best practices. It aims to help users avoid some of the limitations of naive implementations in a framework that is informed by community feedback and is under active development. It assumes that only limited data is available on cases by date of onset and instead uses cases by date of report. These are then imputed to case counts by date of infection using an uncertain reporting delay and incubation period. Right truncation of cases is dealt with internally by `EpiNow`, as is propogating uncertainty from all inputs into the final parameter estimates (helping to mitigate spurious findings). Time-varying estimates of the reproduction number are estimated using the [`EpiEstim`](https://github.com/annecori/EpiEstim) package by date of infection with a generation time estimate that includes uncertainty. Time-varying estimates of the rate of growth are derived using a quasipoisson GLM with a sliding window, which are then used to estimate the doubling time. Optimal windows are chosen by using one day ahead case prediction. Optionally, the time-varying reproduction number can be forecast forwards in time using an integration with the [`EpiSoon`](https://epiforecasts.io/EpiSoon) package and converted to a case forecast using a branching process. See the [methods](https://epiforecasts.io/covid/methods.html) section of our Covid-19 site for a detailed discussion of the approach. 


## Installation

Install the analysis and all dependencies with: 

```r
remotes::install_github("epiforecasts/EpiNow", dependencies = TRUE)
```

## Quick start


*Work in progress: please see `?rt_pipeline` and `?regional_rt_pipeline` for examples of implementing the tooling. A working implementation can also be found [here](https://github.com/epiforecasts/covid-global/blob/master/update_nowcasts.R).*

## Contributing

File an issue [here](https://github.com/epiforecasts/EpiNow/issues) if you have identified an issue with the package. Please note that due to operational constraints priority will be given to users informing government policy or offering methodological insights. We welcome all contributions, in particular those that improve the approach or the robustness of the code base.


## Docker

This package was developed in a docker container based on the `rocker/geospatial` docker image. 

To build the docker image run (from the `EpiNow` directory):

```bash
docker build . -t epinow
```

To run the docker image run:

```bash
docker run -d -p 8787:8787 --name epinow -e USER=epinow -e PASSWORD=epinow time_vary
```

The rstudio client can be found on port :8787 at your local machines ip. The default username:password is epinow:epinow, set the user with -e USER=username, and the password with - e PASSWORD=newpasswordhere. The default is to save the analysis files into the user directory.

To mount a folder (from your current working directory - here assumed to be `tmp`) in the docker container to your local system use the following in the above docker run command (as given mounts the whole `epinow` directory to `tmp`).

```{bash, eval = FALSE}
--mount type=bind,source=$(pwd)/tmp,target=/home/time_vary
```

To access the command line run the following:

```{bash, eval = FALSE}
docker exec -ti epinow bash
```
