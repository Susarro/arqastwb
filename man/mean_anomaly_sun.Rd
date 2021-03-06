% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sun.R
\name{mean_anomaly_sun}
\alias{mean_anomaly_sun}
\title{Mean anomaly of the sun}
\usage{
mean_anomaly_sun(jd)
}
\arguments{
\item{jd}{Julian day}
}
\value{
Sun's mean anomaly in degrees
}
\description{
Mean anomaly of the sun
}
\examples{
jd<-julian_day(13.19,11,2028)
mean_anomaly_sun(jd)

}
