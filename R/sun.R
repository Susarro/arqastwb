#' Apparent equatorial coordinates of sun
#'
#' @param jd Julian day
#' @param precise Precise calculation
#' @return Equatorial coordinates
#' \itemize{
#'   \item declination Declination in degrees
#'   \item right_ascension Right ascension in hour angle
#' }#'
#' @export
#' @examples
#' jd<-julian_day(13.19,11,2028)
#' apparent_longitude_sun(jd)
#'
apparent_equatorial_position_sun<-function(jd,precise=TRUE){
  if(isFALSE(precise))
  {
    T <- centuries_since_2000(jd)
    L<-reduce_degrees(apparent_longitude_sun(jd),signed=TRUE)
    omega <- 125.04 - 1934.136 * T
    obliquity<-true_obliquity_ecliptic(jd,precise)+0.00256*cos(degrees2rad(omega))
    right_ascension<-rad2hours(atan2(cos(degrees2rad(obliquity))*sin(degrees2rad(L)),cos(degrees2rad(L))))
    declination<-rad2degrees(asin(sin(degrees2rad(obliquity))*sin(degrees2rad(L))))
    return (list(declination=reduce_degrees(declination,signed=TRUE),right_ascension=reduce_hours(right_ascension)))
  }
  else
  {
    T <- centuries_since_2000(jd)
    L <- heliocentric_longitude(jd,"earth")
    B <- heliocentric_latitude(jd,"earth")
    sun_longitude <- L+ 180
    sun_latitude <- -B
    R <- heliocentric_radius(jd,"earth")
    #Correction to de FK5 system
    corr <- sun_longitude - 1.397 * T + 0.00031 * T^2
    sun_longitude <- sun_longitude -0.09033 / 3600
    sun_latitude = sun_latitude + 0.03916 * (cos(corr*pi/180) - sin(corr*pi/180)) / 3600
    sun_longitude = sun_longitude + nutation_in_longitude(jd)
    sun_longitude = sun_longitude + -20.4898 / (R * 3600)
    equ<-ecliptic2equatorial(longitude=sun_longitude,latitude=sun_latitude,jd=jd,obliquity=NULL)

    return (list(declination=reduce_degrees(equ$declination,signed=TRUE),right_ascension=reduce_hours(equ$right_ascension)))
  }
}


#' Apparent geometric longitude referred to the mean equinox of the date
#'
#' @param jd Julian day
#' @return Sun's Apparent geometric longitude in degrees
#' @examples
#' jd<-julian_day(13.19,11,2028)
#' apparent_longitude_sun(jd)
#'
apparent_longitude_sun <- function(jd) {
  T <- centuries_since_2000(jd)
  omega <- 125.04 - 1934.136 * T
  return (true_longitude_sun(jd)- (0.00569 + 0.00478 * sin(degrees2rad(omega))))
}

#' True geometric longitude referred to the mean equinox of the date
#'
#' @param jd Julian day
#' @return Sun's true geometric longitude in degrees
#' @examples
#' jd<-julian_day(13.19,11,2028)
#' true_longitude_sun(jd)
#'
true_longitude_sun <- function(jd) {
  T <- centuries_since_2000(jd)
  M = mean_anomaly_sun(jd)
  center <- (1.914600 - 0.004817 * T - 0.000014 * T^2) * sin(degrees2rad(M)) + (0.019993 - 0.000101 * T) * sin(degrees2rad(M*2)) + 0.000290 * sin(degrees2rad(M*3))
  return (geometric_mean_longitude_sun(jd) + center)
}

#' Mean anomaly of the sun
#'
#' @param jd Julian day
#' @return Sun's mean anomaly in degrees
#' @examples
#' jd<-julian_day(13.19,11,2028)
#' mean_anomaly_sun(jd)
#'
mean_anomaly_sun <- function(jd) {
  T <- centuries_since_2000(jd)
  return (357.52910 + 35999.05030 * T + 0.000155 * T^2 - 0.00000048 * T^3)
}

#' Geometric mean longitude of the sun referred to the mean equinox of the date
#'
#' @param jd Julian day
#' @return Sun's geometric mean longitude in degrees
#' @examples
#' jd<-julian_day(13.19,11,2028)
#' geometric_mean_longitude_sun(jd)
#'
geometric_mean_longitude_sun <- function(jd) {
  T <- centuries_since_2000(jd)
  return (280.46645 + 36000.76983 * T + 0.0003032 * T^2)
}
