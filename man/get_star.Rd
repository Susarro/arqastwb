% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/general.R
\name{get_star}
\alias{get_star}
\title{Returns the characteristics of a star}
\usage{
get_star(name)
}
\arguments{
\item{name}{Name of the star. It should be include in the list get_stars()}
}
\value{
\itemize{
  \item former_declination Declination in degrees of the star referred to former_jd
  \item former_right_ascension Right ascension in hour angle of the star referred to former_jd
  \item proper_motion_declination declination of proper motion in degrees
  \item proper_motion_right_ascension right ascension of proper motion in hours
  \item former_jd Epoch of former_declination and former_right_ascension
  \item apparent_magnitude Apparent magnitude of the star
}'
}
\description{
Returns the characteristics of a star
}
\examples{
get_star("alpha aquilae")

}
