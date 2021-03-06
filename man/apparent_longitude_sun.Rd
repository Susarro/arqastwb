% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sun.R
\name{apparent_longitude_sun}
\alias{apparent_longitude_sun}
\title{Apparent geometric longitude referred to the mean equinox of the date}
\usage{
apparent_longitude_sun(jd)
}
\arguments{
\item{jd}{Julian day}
}
\value{
Sun's Apparent geometric longitude in degrees
}
\description{
Apparent geometric longitude referred to the mean equinox of the date
}
\examples{
jd<-julian_day(13.19,11,2028)
apparent_longitude_sun(jd)

}
