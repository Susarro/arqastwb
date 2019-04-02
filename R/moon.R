#' Geometric mean longitude of the moon referred to the mean equinox of the date
#'
#' @param jd Julian day
#' @return Geometric mean longitude of teh moon in degrees
#' @examples
#' jd<-julian_day(13.19,11,2028)
#' mean_longitude_moon(jd)
#'
mean_longitude_moon <- function(jd) {
  T <- centuries_since_2000(jd)
  return (218.3164591 + 481267.88134236 * T - 0.0013268 * T^2 + T^3 / 538841 - T^4 / 65194000)
}

#' Mean elongation of the moon referred to the mean equinox of the date
#'
#' @param jd Julian day
#' @return Mean elongation of the moon in degrees
#' @examples
#' jd<-julian_day(13.19,11,2028)
#' mean_elongation_moon(jd)
#'
mean_elongation_moon <- function(jd) {
  T <- centuries_since_2000(jd)
  return (297.8502042 + 445267.1115168 * T - 0.0016300 * T^2 + T^3 / 545868 - T^4 / 113065000)
}

#' Mean anomaly of the moon referred to the mean equinox of the date
#'
#' @param jd Julian day
#' @return Mean anomaly of the moon in degrees
#' @examples
#' jd<-julian_day(13.19,11,2028)
#' mean_elongation_moon(jd)
#'
mean_anomaly_moon <- function(jd) {
  T <- centuries_since_2000(jd)
  return (134.9634114 + 477198.8676313 * T + 0.0089970 * T^2 + T^3 / 69699 - T^4 / 14712000)
}

#' Moon`s argument of latitude (mean distance of the Moon from its ascending node) referred to the mean equinox of the date
#'
#' @param jd Julian day
#' @return Moon`s argument of latitude in degrees
#' @examples
#' jd<-julian_day(13.19,11,2028)
#' argument_of_latitude_moon(jd)
#'
argument_of_latitude_moon <- function(jd) {
  T <- centuries_since_2000(jd)
  return (93.2720993 + 483202.0175273 * T - 0.0034029 * T^2 - T^3 / 3526000 + T^4 / 863310000)
}

#' Distance to Earth
#'
#' @param jd Julian day
#' @return Distance to Earth
#' @export
#' @examples
#' jd<-julian_day(13.19,11,2028)
#' distance_to_earth_moon(jd)
#'

distance_to_earth_moon <- function(jd) {
  T <- centuries_since_2000(jd)
  LM <- reduce_degrees(218.3164591 + 481267.88134236 * T - 0.0013268 * T^2 + T^3 / 538841 - T^4 / 65194000)
  D <- reduce_degrees(297.8502042 + 445267.1115168 * T - 0.0016300 * T^2 + T^3 / 545868 - T^4 / 113065000)
  M <- reduce_degrees(357.5291092 + 35999.0502909 * T - 0.0001536 * T^2 + T^3/24490000)
  MM <- reduce_degrees(134.9634114 + 477198.8676313 * T + 0.0089970 * T^2 + T^3 / 69699 - T^4 / 14712000)
  F <- reduce_degrees(93.2720993 + 483202.0175273 * T - 0.0034029 * T^2 - T^3 / 3526000 + T^4 / 863310000)
  A1 <- reduce_degrees(119.75 + 131.849 * T)
  A2 <- reduce_degrees(53.09 + 479264.290 * T)
  A3 <- reduce_degrees(313.45 + 481266.484 * T)
  E <- 1 - 0.002516 * T - 0.0000074 * T^2

  R<- colSums(cos(rbind(c(0, 0, 1, 0),
                    c(2, 0, -1, 0),
                    c(2, 0, 0, 0),
                    c(0, 0, 2, 0),
                    c(0, 1, 0, 0),
                    c(0, 0, 0, 2),
                    c(2, 0, -2, 0),
                    c(2, -1, -1, 0),
                    c(2, 0, 1, 0),
                    c(2, -1, 0, 0),
                    c(0, 1, -1, 0),
                    c(1, 0, 0, 0),
                    c(0, 1, 1, 0),
                    c(2, 0, 0, -2),
                    c(0, 0, 1, 2),
                    c(0, 0, 1, -2),
                    c(4, 0, -1, 0),
                    c(0, 0, 3, 0),
                    c(4, 0, -2, 0),
                    c(2, 1, -1, 0),
                    c(2, 1, 0, 0),
                    c(1, 0, -1, 0),
                    c(1, 1, 0, 0),
                    c(2, -1, 1, 0),
                    c(2, 0, 2, 0),
                    c(4, 0, 0, 0),
                    c(2, 0, -3, 0),
                    c(0, 1, -2, 0),
                    c(2, 0, -1, 2),
                    c(2, -1, -2, 0),
                    c(1, 0, 1, 0),
                    c(2, -2, 0, 0),
                    c(0, 1, 2, 0),
                    c(0, 2, 0, 0),
                    c(2, -2, -1, 0),
                    c(2, 0, 1, -2),
                    c(2, 0, 0, 2),
                    c(4, -1, -1, 0),
                    c(0, 0, 2, 2),
                    c(3, 0, -1, 0),
                    c(2, 1, 1, 0),
                    c(4, -1, -2, 0),
                    c(0, 2, -1, 0),
                    c(2, 2, -1, 0),
                    c(2, 1, -2, 0),
                    c(2, -1, 0, -2),
                    c(4, 0, 1, 0),
                    c(0, 0, 4, 0),
                    c(4, -1, 0, 0),
                    c(1, 0, -2, 0),
                    c(2, 1, 0, -2),
                    c(0, 0, 2, -2),
                    c(1, 1, 1, 0),
                    c(3, 0, -2, 0),
                    c(4, 0, -3, 0),
                    c(2, -1, 2, 0),
                    c(0, 2, 1, 0),
                    c(1, 1, -1, 0),
                    c(2, 0, 3, 0),
                    c(2, 0, -1, -2)) %*% rbind(D, M, MM, F)*pi/180) *
            rbind(-20905355, -3699111, -2955968, -569925, 48888 * E, -3149, 246158, -152138 * E, -170733, -204586 * E, -129620 * E, 108743, 104755 * E, 10321, 0, 79661, -34782, -23210, -21636,
                            24208 * E, 30824 * E, -8379, -16675 * E, -12831 * E, -10445, -11650, 14403, -7003 * E, 0, 10056 * E, 6322, -9884 * E ^2, 5751 * E, 0 * E^2, -4950 * E^2, 4130, 0, -3958 * E,
                            0, 3258, 2616 * E, -1897 * E, -2117 * E^2, 2354 * E^2, 0 * E, 0 * E, -1423, -1117, -1571 * E, -1739, 0 * E, -4421, 0 * E, 0, 0, 0 * E, 1165 * E^2, 0 * E, 0, 8752))

  return ((385000.56 + R / 1000) / 1.4960e8)
}


#' Apparent equatorial coordinates of sun
#'
#' @param jd Julian day
#' @return Equatorial coordinates
#' \itemize{
#'   \item declination Declination in degrees
#'   \item right_ascension Right ascension in hour angle
#'   \item distance Distance to Earth
#'   \item illuminated_fraction Illuminated fraction of the disk of the Moon
#'   \item apparent_magnitude apparent magnitude
#' }#'
#' @export
#' @examples
#' jd<-julian_day(13.19,11,2028)
#' apparent_longitude_sun(jd)
#'
apparent_equatorial_position_moon<-function(jd){

  T <- centuries_since_2000(jd)
  LM <- reduce_degrees(218.3164591 + 481267.88134236 * T - 0.0013268 * T^2 + T^3 / 538841 - T^4 / 65194000)
  D <- reduce_degrees(297.8502042 + 445267.1115168 * T - 0.0016300 * T^2 + T^3 / 545868 - T^4 / 113065000)
  M <- reduce_degrees(357.5291092 + 35999.0502909 * T - 0.0001536 * T^2 + T^3/24490000)
  MM <- reduce_degrees(134.9634114 + 477198.8676313 * T + 0.0089970 * T^2 + T^3 / 69699 - T^4 / 14712000)
  F <- reduce_degrees(93.2720993 + 483202.0175273 * T - 0.0034029 * T^2 - T^3 / 3526000 + T^4 / 863310000)
  A1 <- reduce_degrees(119.75 + 131.849 * T)
  A2 <- reduce_degrees(53.09 + 479264.290 * T)
  A3 <- reduce_degrees(313.45 + 481266.484 * T)
  E <- 1 - 0.002516 * T - 0.0000074 * T^2

  L<- colSums(sin(rbind(c(0, 0, 1, 0),
                    c(2, 0, -1, 0),
                    c(2, 0, 0, 0),
                    c(0, 0, 2, 0),
                    c(0, 1, 0, 0),
                    c(0, 0, 0, 2),
                    c(2, 0, -2, 0),
                    c(2, -1, -1, 0),
                    c(2, 0, 1, 0),
                    c(2, -1, 0, 0),
                    c(0, 1, -1, 0),
                    c(1, 0, 0, 0),
                    c(0, 1, 1, 0),
                    c(2, 0, 0, -2),
                    c(0, 0, 1, 2),
                    c(0, 0, 1, -2),
                    c(4, 0, -1, 0),
                    c(0, 0, 3, 0),
                    c(4, 0, -2, 0),
                    c(2, 1, -1, 0),
                    c(2, 1, 0, 0),
                    c(1, 0, -1, 0),
                    c(1, 1, 0, 0),
                    c(2, -1, 1, 0),
                    c(2, 0, 2, 0),
                    c(4, 0, 0, 0),
                    c(2, 0, -3, 0),
                    c(0, 1, -2, 0),
                    c(2, 0, -1, 2),
                    c(2, -1, -2, 0),
                    c(1, 0, 1, 0),
                    c(2, -2, 0, 0),
                    c(0, 1, 2, 0),
                    c(0, 2, 0, 0),
                    c(2, -2, -1, 0),
                    c(2, 0, 1, -2),
                    c(2, 0, 0, 2),
                    c(4, -1, -1, 0),
                    c(0, 0, 2, 2),
                    c(3, 0, -1, 0),
                    c(2, 1, 1, 0),
                    c(4, -1, -2, 0),
                    c(0, 2, -1, 0),
                    c(2, 2, -1, 0),
                    c(2, 1, -2, 0),
                    c(2, -1, 0, -2),
                    c(4, 0, 1, 0),
                    c(0, 0, 4, 0),
                    c(4, -1, 0, 0),
                    c(1, 0, -2, 0),
                    c(2, 1, 0, -2),
                    c(0, 0, 2, -2),
                    c(1, 1, 1, 0),
                    c(3, 0, -2, 0),
                    c(4, 0, -3, 0),
                    c(2, -1, 2, 0),
                    c(0, 2, 1, 0),
                    c(1, 1, -1, 0),
                    c(2, 0, 3, 0),
                    c(2, 0, -1, -2)) %*% rbind(D, M, MM, F)*pi/180) *
            rbind(6288774, 1274027, 658314, 213618, -185116 * E, -114332, 58793, 57066 * E, 53322,  45758 * E, -40923 * E, -34720, -30383 * E, 15327, -12528, 10980, 10675, 10034, 8548, -7888 * E,
                            -6766 * E,  -5163, 4987 * E, 4036 * E, 3994, 3861, 3665, -2689 * E, -2602, 2390 * E, -2348, 2236 * E^2, -2120 * E, -2069 * E * E, 2048 * E^2, -1773, -1595, 1215 * E, -1110,
                            -892, -810 * E, 759 * E, -713 * E^2, -700 * E^2, 691 * E, 596 * E, 549, 537, 520 * E, -487, -399 * E, -381, 351 * E, -340, 330, 327 * E, -323 * E^2, 299 * E, 294, 0))

  L<- L+ 3958 * sin(degrees2rad(A1)) + 1962 * sin(degrees2rad(LM - F)) + 318 * sin(degrees2rad(A2))

  moon_longitude = LM + L / 1000000 + nutation_in_longitude(jd)

  B<- colSums(sin(rbind(c(0, 0, 0, 1),
                        c(0, 0, 1, 1),
                        c(0, 0, 1, -1),
                        c(2, 0, 0, -1),
                        c(2, 0, -1, 1),
                        c(2, 0, -1, -1),
                        c(2, 0, 0, 1),
                        c(0, 0, 2, 1),
                        c(2, 0, 1, -1),
                        c(0, 0, 2, -1),
                        c(2, -1, 0, -1),
                        c(2, 0, -2, -1),
                        c(2, 0, 1, 1),
                        c(2, 1, 0, -1),
                        c(2, -1, -1, 1),
                        c(2, -1, 0, 1),
                        c(2, -1, -1, -1),
                        c(0, 1, -1, -1),
                        c(4, 0, -1, -1),
                        c(0, 1, 0, 1),
                        c(0, 0, 0, 3),
                        c(0, 1, -1, 1),
                        c(1, 0, 0, 1),
                        c(0, 1, 1, 1),
                        c(0, 1, 1, -1),
                        c(0, 1, 0, -1),
                        c(1, 0, 0, -1),
                        c(0, 0, 3, 1),
                        c(4, 0, 0, -1),
                        c(4, 0, -1, 1),
                        c(0, 0, 1, -3),
                        c(4, 0, -2, 1),
                        c(2, 0, 0, -3),
                        c(2, 0, 2, -1),
                        c(2, -1, 1, -1),
                        c(2, 0, -2, 1),
                        c(0, 0, 3, -1),
                        c(2, 0, 2, 1),
                        c(2, 0, -3, -1),
                        c(2, 1, -1, 1),
                        c(2, 1, 0, 1),
                        c(4, 0, 0, 1),
                        c(2, -1, 1, 1),
                        c(2, -2, 0, -1),
                        c(0, 0, 1, 3),
                        c(2, 1, 1, -1),
                        c(1, 1, 0, -1),
                        c(1, 1, 0, 1),
                        c(0, 1, -2, -1),
                        c(2, 1, -1, -1),
                        c(1, 0, 1, 1),
                        c(2, -1, -2, -1),
                        c(0, 1, 2, 1),
                        c(4, 0, -2, -1),
                        c(4, -1, -1, -1),
                        c(1, 0, 1, -1),
                        c(4, 0, 1, -1),
                        c(1, 0, -1, -1),
                        c(4, -1, 0, -1),
                        c(2, -2, 0, 1)) %*% rbind(D, M, MM, F)*pi/180) *
                rbind(5128122, 280602, 277693, 173237, 55413, 46271, 32573, 17198, 9266, 8822, 8216 * E, 4324, 4200, -3359 * E, 2463 * E, 2211 * E, 2065 * E, -1870 * E, 1828, -1794 * E, -1749,
                                -1565 * E, -1491, -1475 * E, -1410 * E, -1344 * E, -1335, 1107, 1021, 833, 777, 671, 607, 596, 491 * E, -451, 439, 422, 421, -366 * E, -351 * E, 331, 315 * E, 302 * E^2,
                                -283, -229 * E, 223 * E, 223 * E, -220 * E, -220 * E, -185, 181 * E, -177 * E, 176, 166 * E, -164, 132, -119, 115 * E, 107 * E ^2))

 B <- B -2235 * sin(degrees2rad(LM)) + 382 * sin(degrees2rad(A3)) + 175 * sin(degrees2rad(A1 - F)) + 175 * sin(degrees2rad(A1 + F)) + 127 * sin(degrees2rad(LM - MM)) - 115 * sin(degrees2rad(LM + MM))

 moon_latitude <- B/1000000

 equ<-ecliptic2equatorial(longitude=moon_longitude,latitude=moon_latitude,jd=jd,obliquity=NULL)

 distance<-distance_to_earth_moon(jd)

 sun_longitude <- heliocentric_longitude(jd,"earth") + 180
 geocentric_elongation <- reduce_degrees(rad2degrees(acos(cos(degrees2rad(moon_latitude)) * cos(degrees2rad(moon_longitude - sun_longitude)))))

 R <- heliocentric_radius(jd,"earth")
 phase_angle = reduce_degrees(rad2degrees(atan2(R * sin(degrees2rad(geocentric_elongation)), distance - R * cos(degrees2rad(geocentric_elongation)))))
 k<-((1 + cos(degrees2rad(phase_angle))) / 2)

 m<--12.73+0.026*phase_angle+4e-9*phase_angle^4

 return (list(declination=reduce_degrees(equ$declination,signed=TRUE),right_ascension=reduce_hours(equ$right_ascension),distance=distance,illuminated_fraction=k,apparent_magnitude=round(m,1)))
}
