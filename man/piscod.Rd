% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/piscod.R
\name{piscod}
\alias{piscod}
\title{values extraction of PISCO daily data}
\usage{
piscod(x, start = NULL, end = NULL)
}
\arguments{
\item{x}{a dataframe with PISCO file name (in netCDF format), longitude and latitude of station.}

\item{start}{OPTIONAL, start date of dataset, default value is 1981-01-01.}

\item{end}{OPTIONAL, end date of dataset, default value is 2016-12-31.}
}
\description{
function for extract values of PISCO daily data, PISCO is Peruvian Interpolated Data of the Senamhi’s Climatological and Hydrologycal Observations.
}
\author{
Geomar Perales Apaico
}
