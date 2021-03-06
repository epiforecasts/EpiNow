% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/report.R
\name{report_nowcast}
\alias{report_nowcast}
\title{Report Case Nowcast Estimates}
\usage{
report_nowcast(nowcast, cases, target, target_folder)
}
\arguments{
\item{nowcast}{A dataframe as produced by \code{nowcast_pipeline}}

\item{cases}{A dataframe of cases (in date order) with the following variables:
\code{date} and \code{cases}.}

\item{target}{Character string indicting the data type to use as the "nowcast".}

\item{target_folder}{Character string indicating the folder into which to save results.
Also used to extract previously generated results.}
}
\description{
Returns a summarised nowcast as well as saving key information to the
results folder.
}
