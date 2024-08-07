% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/senamhi.find.R
\name{senamhi.find}
\alias{senamhi.find}
\title{function for search SENAMHI stations information by coordinates and maximum distance}
\usage{
senamhi.find(x, senamhi, dist_max)
}
\arguments{
\item{x}{numeric; numeric vector of coordinates (latitude and longitude)}

\item{senamhi}{stations information of SENAMHI web scraping}

\item{dist_max}{numeric, value of maximum distance from stations to point (kms)}
}
\description{
function for search SENAMHI stations information by coordinates (latitude and longitude) and maximum distance inside SENAMHI data web.
}
\examples{

x <- c(-13.18, -74.23)
dist_max <- 50
stations <- senamhi()

senamhi.find(x, senamhi(), dist_max)

}
\author{
Geomar Perales Apaico
}
