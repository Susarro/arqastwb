% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/coordinates.R
\name{atmospheric_refraction_correction}
\alias{atmospheric_refraction_correction}
\title{Altitude correction due to atmospheric refraction}
\usage{
atmospheric_refraction_correction(altitude, apparent = TRUE)
}
\arguments{
\item{altitude}{Altitude indegrees}

\item{apparent}{If True apparent altitude to true altitude. If false, true altitude to apparent altitude}
}
\value{
Corrected altitude
}
\description{
Altitude correction due to atmospheric refraction
}
\examples{
degrees2str(atmospheric_refraction_correction(0.5))
degrees2str(atmospheric_refraction_correction(str2degrees("00Âº01'14.777\\u0022"),apparent=FALSE))

}
