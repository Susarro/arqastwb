% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sun.R
\name{geometric_mean_longitude_sun}
\alias{geometric_mean_longitude_sun}
\title{Geometric mean longitude of the sun referred to the mean equinox of the date}
\usage{
geometric_mean_longitude_sun(jd)
}
\arguments{
\item{jd}{Julian day}
}
\value{
Sun's geometric mean longitude in degrees
}
\description{
Geometric mean longitude of the sun referred to the mean equinox of the date
}
\examples{
jd<-julian_day(13.19,11,2028)
geometric_mean_longitude_sun(jd)

}
