% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plotting.R
\name{plt_fnc_jd}
\alias{plt_fnc_jd}
\title{Plot a function usin Julian Day as x coordinate}
\usage{
plt_fnc_jd(fnc, from, to, interval, tick_interval)
}
\arguments{
\item{fnc}{Function to plot}

\item{from}{Initial julian day}

\item{to}{Final julian day}

\item{interval}{sample interval in time}

\item{tick_interval}{Tick interval}
}
\value{
Plot
}
\description{
Plot a function usin Julian Day as x coordinate
}
\examples{
fnc<-function(x) apparent_equatorial_position(x,"sun")$declination
plt_fnc_jd(fnc, str2jd("1972-03-01"),str2jd("1973-03-01"),10,60)

}
