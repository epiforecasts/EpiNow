
# EpiNow

[![DOI](https://zenodo.org/badge/247464257.svg)](https://zenodo.org/badge/latestdoi/247464257)

*Warning: This package is a work in progress and is currently developed solely with the COVID-19 outbreak in mind. Breaking changes may occur and the authors cannot guarantee support.*

**Aim:** To identify changes in the reproduction number, rate of spread, and doubling time during the course of outbreaks whilst accounting for potential biases due to delays in case reporting.

## Installation

Install the analysis and all dependencies with: 

```r
remotes::install_github("epiforecasts/EpiNow", dependencies = TRUE)
```

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
