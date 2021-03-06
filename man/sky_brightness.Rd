% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/general.R
\name{sky_brightness}
\alias{sky_brightness}
\title{Visual magnitude of non-standard sky brightness}
\usage{
sky_brightness(angular_dist, sun_altitude, star_zenith_dist,
  visual_extinction_coefficient = 0.2)
}
\arguments{
\item{angular_dist}{Angular distance sun-star in degrees}

\item{sun_altitude}{Sun's altitude in degrees}

\item{star_zenith_dist}{Star's zenith in degrees}

\item{visual_extinction_coefficient}{Visual extinction coefficient}
}
\value{
Visual magnitude of non-standard sky brightness
}
\description{
Visual magnitude of non-standard sky brightness
}
\examples{
sky_brightness(degrees2rad(150),degrees2rad(-10),degrees2rad(88))

}
