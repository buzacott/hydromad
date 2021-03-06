% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Corin.R
\docType{data}
\name{Corin}
\alias{Corin}
\title{Daily dataset for the Corin catchment.}
\format{
A \code{zoo} object of class \verb{"zooreg", "zoo"}. It is a regular time series
indexed by days, in \code{Date} format.
There are four columns:
\itemize{
\item \code{P} (precipitation, mm/day)
\item \code{Q} (streamflow, mm/day)
\item \code{E} (potential evapotranspiration, mm/day)
\item \code{T} (daily average temperature, ºC)
}
}
\source{
Data obtained through the Australian Bureau of Meteorology Water Data web services API.
Data owner: ACT - Icon Water Limited.
}
\usage{
data("Corin")
}
\description{
A daily dataset containing precipitation, streamflow, potential
evapotranspiration and average temperature for the Corin catchment (147 km²),
ACT, Australia. The Corin catchment is the headwaters of the greater Cotter
River catchment, which is a tributary to the Murrumbidgee River.
}
\details{
\itemize{
\item Daily rainfall at Cotter Hut (ID: 570946).
\item Daily mean streamflow at Cotter River at Gingera (ID: 410730).
\item Daily Morton's potential ET calculated using Corin Dam (ID: 570947) weather
station data.
\item Daily average temperature as measured at Corin Dam weather station
(ID: 570947).
}
}
\examples{
data(Corin)
summary(Corin)
xyplot(Corin)
}
\keyword{datasets}
