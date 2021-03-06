% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/moon.R
\name{apparent_equatorial_position_moon}
\alias{apparent_equatorial_position_moon}
\title{Apparent equatorial coordinates of sun}
\usage{
apparent_equatorial_position_moon(jd)
}
\arguments{
\item{jd}{Julian day}
}
\value{
Equatorial coordinates
\itemize{
  \item declination Declination in degrees
  \item right_ascension Right ascension in hour angle
  \item distance Distance to Earth
  \item illuminated_fraction Illuminated fraction of the disk of the Moon
  \item apparent_magnitude apparent magnitude
}#'
}
\description{
Apparent equatorial coordinates of sun
}
\examples{
jd<-julian_day(13.19,11,2028)
apparent_longitude_sun(jd)

}
