
#' Angular separation between two celestial bodies
#'
#' @param declination1 Declination of first celestial body in degrees
#' @param right_ascension1 Right ascension of first celestial body in hour angle
#' @param declination2 Declination of second celestial body in degrees
#' @param right_ascension2 Right ascension of second celestial body in hour angle
#' @return Angular separation
#' @export
#' @examples
#' declination1<-str2degrees("19º10'57\u0022")
#' right_ascension1<-str2hours("14:15:39.7")
#' declination2<-str2degrees("-11º09'41\u0022")
#' right_ascension2<-str2hours("13:25:11.6")
#' angular_separation(declination1,right_ascension1, declination2, right_ascension2)
#'
angular_separation <- function(declination1,right_ascension1, declination2, right_ascension2){
  return(rad2degrees(acos(sin(degrees2rad(declination1))*sin(degrees2rad(declination2))+cos(degrees2rad(declination1))*cos(degrees2rad(declination2))*cos(hours2rad(right_ascension1-right_ascension2)))))
}

#' Equatorial coordinates for a stat due to precession
#'
#' @param jd Julian day
#' @param former_jd Julian day for reference (usually J2000)
#' @param former_declination Reference declination in degrees
#' @param former_right_ascension Reference right ascension in hour angle
#' @param proper_motion_declination Proper motion declination in degrees
#' @param proper_motion_right_ascension Proper motion right ascension in hour angle
#' @return Equatorial coordinates
#' \itemize{
#'   \item declination Declination in degrees
#'   \item right_ascension Right ascension in hour angle
#' }
#' @examples
#' fra<-str2hours("02:44:11.986")
#' fd<-str2degrees("49º13'42.48\u0022")
#' former_jd=str2jd("2000-1-1T12:00")
#' jd<-julian_day(13.19,11,2028)
#' pmra<-0.03425/3600
#' pmd<--0.0895/3600
#' precession_star(jd, str2jd("2000-01-01T12:00"),fd,fra, pmd, pmra)
#'
precession_star<- function(jd, former_jd=str2jd("2000-01-01T12:00"),former_declination,former_right_ascension, proper_motion_declination, proper_motion_right_ascension){

  T <- centuries_since_2000(former_jd)
  t <- (jd - former_jd) / 36525

  former_declination <- former_declination + proper_motion_declination * t * 100
  former_right_ascension <- former_right_ascension + proper_motion_right_ascension * t * 100

  a1 <- (2306.2181 + 1.39656 * T - 0.000139 * T^2) * t + (0.30188 - 0.000344 * T) * t^2 + 0.017998 * t^3 #seconds
  a2 <- (2306.2181 + 1.39656 * T - 0.000139 * T^2) * t + (1.09468 + 0.000066 * T) * t^2 + 0.018203 * t^3 #seconds
  a3 <- (2004.3109 - 0.85330 * T - 0.000217 * T^2) * t - (0.42665 + 0.000217 * T) * t^2 - 0.041833 * t^3 #seconds

  A1 = degrees2hours(a1 / 3600)
  A2 = degrees2hours(a2 / 3600)
  A3 = degrees2hours(a3 / 3600)

  A = cos(degrees2rad(former_declination)) * sin(hours2rad(former_right_ascension + A1))
  B = cos(hours2rad(A3)) * cos(degrees2rad(former_declination)) * cos(hours2rad(former_right_ascension + A1)) - sin(hours2rad(A3)) * sin(degrees2rad(former_declination))
  C = sin(hours2rad(A3)) * cos(degrees2rad(former_declination)) * cos(hours2rad(former_right_ascension + A1)) + cos(hours2rad(A3)) * sin(degrees2rad(former_declination))

  right_ascension <- reduce_hours(rad2hours(atan2(A, B))+A2)
  declination <- reduce_degrees(rad2degrees(asin(C)))
  return (list(declination=declination, right_ascension=right_ascension))
}


#' Equatorial coordinates for a star
#'
#' @param jd Julian day
#' @param former_jd Julian day for reference (usually J2000)
#' @param former_declination Reference declination in degrees
#' @param former_right_ascension Reference right ascension in hour angle
#' @param proper_motion_declination Proper motion declination in degrees
#' @param proper_motion_right_ascension Proper motion right ascension in hour angle
#' @param precise if precise calculation
#' @return Equatorial coordinates
#' \itemize{
#'   \item declination Declination in degrees
#'   \item right_ascension Right ascension in hour angle
#' }
#' @export
#' @examples
#' jd<-julian_day(13.19,11,2028)
#' former_jd<-str2jd("2000-01-01T12:00")
#' fd<-str2degrees("49º13'42.48\u0022")
#' fra<-str2hours("02:44:11.986")
#' apparent_equatorial_position_star(jd,former_jd,fd,fra, -0.0895/3600, 0.03425/3600)
#'
apparent_equatorial_position_star<- function(jd, former_jd=str2jd("2000-01-01T12:00"),former_declination,former_right_ascension, proper_motion_declination, proper_motion_right_ascension,precise=TRUE){

    T <- centuries_since_2000(former_jd)
    t <- (jd - former_jd) / 36525

    former_declination <- former_declination + proper_motion_declination * t * 100
    former_right_ascension <- former_right_ascension + proper_motion_right_ascension * t * 100

    correction_aberration<- aberration_correction_star(jd,declination=former_declination,right_ascension=former_right_ascension,precise=precise)

    former_declination <- former_declination+correction_aberration$declination
    former_right_ascension <- former_right_ascension + correction_aberration$right_ascension

    a1 <- (2306.2181 + 1.39656 * T - 0.000139 * T^2) * t + (0.30188 - 0.000344 * T) * t^2 + 0.017998 * t^3 #seconds
    a2 <- (2306.2181 + 1.39656 * T - 0.000139 * T^2) * t + (1.09468 + 0.000066 * T) * t^2 + 0.018203 * t^3 #seconds
    a3 <- (2004.3109 - 0.85330 * T - 0.000217 * T^2) * t - (0.42665 + 0.000217 * T) * t^2 - 0.041833 * t^3 #seconds

    A1 = degrees2hours(a1 / 3600)
    A2 = degrees2hours(a2 / 3600)
    A3 = degrees2hours(a3 / 3600)

    A = cos(degrees2rad(former_declination)) * sin(hours2rad(former_right_ascension + A1))
    B = cos(hours2rad(A3)) * cos(degrees2rad(former_declination)) * cos(hours2rad(former_right_ascension + A1)) - sin(hours2rad(A3)) * sin(degrees2rad(former_declination))
    C = sin(hours2rad(A3)) * cos(degrees2rad(former_declination)) * cos(hours2rad(former_right_ascension + A1)) + cos(hours2rad(A3)) * sin(degrees2rad(former_declination))

    right_ascension <- reduce_hours(rad2hours(atan2(A, B))+A2)
    declination <- reduce_degrees(rad2degrees(asin(C)))

    correction_nutation<- nutation_correction_star(jd,declination=declination,right_ascension=right_ascension,precise=precise)

    d<-declination+correction_nutation$declination
    ra<-right_ascension+correction_nutation$right_ascension

    return (list(declination=reduce_degrees(d), right_ascension=reduce_hours(ra)))
}


#' Nutation correction
#'
#' @param jd Julian day
#' @param declination Declination in degrees
#' @param right_ascension Right ascension in hour angle
#' @param precise if precise calculation
#' @return
#' \itemize{
#'   \item declination correction in declination due to nutation
#'   \item right_ascension correction in right ascension due to nutation
#' }
#' @examples
#' declination<-str2degrees("19º10'57\u0022")
#' right_ascension<-str2hours("14:15:39.7")
#' jd<-julian_day(13.19,11,2028)
#' nutation_correction_star(jd,declination,right_ascension)
#'

nutation_correction_star<- function(jd, declination, right_ascension, precise=T){
  T <- centuries_since_2000(jd)
  obliquity <- true_obliquity_ecliptic(jd,precise)
  nutation_longitude<-nutation_in_longitude(jd)
  nutation_obliquity<-nutation_in_obliquity(jd)

  ra<-degrees2hours((cos(degrees2rad(obliquity))+sin(degrees2rad(obliquity))*sin(hours2rad(right_ascension))*tan(degrees2rad(declination)))*nutation_longitude-(cos(hours2rad(right_ascension))*tan(degrees2rad(declination)))*nutation_obliquity)
  d<-(sin(degrees2rad(obliquity))*cos(hours2rad(right_ascension)))*nutation_longitude+sin(hours2rad(right_ascension))*nutation_obliquity
  return (list(declination=reduce_degrees(d,signed=T), right_ascension=reduce_hours(ra)))
}

#' Annual aberration correction
#'
#' @param jd Julian day
#' @param declination Declination in degrees
#' @param right_ascension Right ascension in hour angle
#' @param precise Precise calculation
#' @return
#' \itemize{
#'   \item declination correction in declination due to annual aberration
#'   \item right_ascension correction in right ascension due to annual aberration
#' }
#' @examples
#' declination<-str2degrees("19º10'57\u0022")
#' right_ascension<-str2hours("14:15:39.7")
#' jd<-julian_day(13.19,11,2028)
#' aberration_correction_star(jd,declination,right_ascension)
#'

aberration_correction_star<- function(jd, declination, right_ascension, precise=TRUE){
  T <- centuries_since_2000(jd)
  if(!isTRUE(precise))
  {
    sun_longitude<-true_longitude_sun(jd)
    T <- centuries_since_2000(jd)
    e<-0.016708617 - 0.000042037 * T - 0.0000001236 * T^2
    perihelion_longitude<-102.93735 + 1.71953 * T + 0.00046 * T^2
    a<-20.49552/3600
    obliquity<-true_obliquity_ecliptic(jd,precise=FALSE)
    ra<-degrees2hours(-a*((cos(hours2rad(right_ascension))*cos(degrees2rad(sun_longitude))*cos(degrees2rad(obliquity))+sin(degrees2rad(hours2rad(right_ascension))*sin(degrees2rad(sun_longitude)))/cos(degrees2rad(declination)))+
      e*a*((cos(hours2rad(right_ascension))*cos(degrees2rad(perihelion_longitude))*cos(degrees2rad(obliquity))+sin(hours2rad(right_ascension))*sin(degrees2rad(pi)))/cos(degrees2rad(declination)))))
    obliquity<-true_obliquity_ecliptic(jd)

    d<--a*(cos(degrees2rad(sun_longitude))*cos(degrees2rad(obliquity))*(tan(degrees2rad(obliquity))*cos(degrees2rad(declination))-sin(hours2rad(right_ascension))*sin(degrees2rad(declination)))+cos(hours2rad(right_ascension))*sin(degrees2rad(declination))*sin(degrees2rad(sun_longitude)))+
      e*a*(cos(degrees2rad(perihelion_longitude))*cos(degrees2rad(obliquity))*(tan(degrees2rad(obliquity))*cos(degrees2rad(declination))-sin(hours2rad(right_ascension))*sin(degrees2rad(declination)))+cos(hours2rad(right_ascension))*sin(degrees2rad(declination))*sin(degrees2rad(perihelion_longitude)))

    return (list(declination=reduce_degrees(d,signed=T), right_ascension=reduce_hours(ra)))
  }
  else
  {
   L2 <- 3.1761467 + 1021.3285546 * T #mean longitude of Venus referred to the mean equinox of J2000 in radians
   L3 <- 1.7534703 + 628.3075849 * T
   L4 <- 6.2034809 + 334.0612431 * T
   L5 <- 0.5995465 + 52.9690965 * T
   L6 <- 0.8740168 + 21.3299095 * T
   L7 <- 5.4812939 + 7.4781599 * T
   L8 <- 5.3118863 + 3.8133036 * T #mean longitude of Neptune referred to the mean equinox of J2000 in radians
   l <- 3.8103444 + 8399.6847337 * T #mean longitude of the Moon referred to the mean equinox of J2000 in radians
   D <- 5.19844667 + 7771.3771486 * T
   m <- 2.3555559 + 8328.6914289 * T
   F <- 1.6279052 + 8433.4661601 * T

   xs<-colSums(rbind(
     (-1719914 - 2 * T) * sin(L3),
     (6434 + 141 * T) * sin(2 * L3),
     (715) * sin(L5),
     (715) * sin(l),
     (486 - 5 * T) * sin(3 * L3),
     (159) * sin(L6),
     (0) * sin(F),
     (39) * sin(l + m),
     (33) * sin(2 * L5),
     (31) * sin(2 * L3 - L5),
     (8) * sin(3 * L3 - 8 * L4 + 3 * L5),
     (8) * sin(5 * L3 - 8 * L4 + 3 * L5),
     (21) * sin(2 * L2 - L3),
     (-19) * sin(L2),
     (17) * sin(L7),
     (16) * sin(L3 - 2 * L5),
     (16) * sin(L8),
     (11) * sin(L3 + L5),
     (0) * sin(2 * L2 - 2 * L3),
     (-11) * sin(L3 - L5),
     (-7) * sin(4 * L3),
     (-10) * sin(3 * L3 - 2 * L5),
     (-9) * sin(L2 - 2 * L3),
     (-9) * sin(2 * L2 - 3 * L3),
     (0) * sin(2 * L6),
     (0) * sin(2 * L2 - 4 * L3),
     (8) * sin(3 * L3 - 2 * L4),
     (8) * sin(l + 2 * D - m),
     (-4) * sin(8 * L2 - 12 * L3),
     (-4) * sin(8 * L2 - 14 * L3),
     (-6) * sin(2 * L4),
     (-1) * sin(3 * L2 - 4 * L3),
     (4) * sin(2 * L3 - 2 * L5),
     (0) * sin(3 * L2 - 3 * L3),
     (5) * sin(2 * L3 - 2 * L4),
     (5) * sin(l - 2 * D)
   ))


   xc<-colSums(rbind(
     (-25) * cos(L3),
     (28007 - 107 * T) * cos(2 * L3),
     (0) * cos(L5),
     (0) * cos(l),
     (-236 - 4 * T) * cos(3 * L3),
     (0) * cos(L6),
     (0) * cos(F),
     (0) * cos(l + m),
     (-10) * cos(2 * L5),
     (1) * cos(2 * L3 - L5),
     (-28) * cos(3 * L3 - 8 * L4 + 3 * L5),
     (-28) * cos(5 * L3 - 8 * L4 + 3 * L5),
     (0) * cos(2 * L2 - L3),
     (0) * cos(L2),
     (0) * cos(L7),
     (0) * cos(L3 - 2 * L5),
     (0) * cos(L8),
     (-1) * cos(L3 + L5),
     (-11) * cos(2 * L2 - 2 * L3),
     (-2) * cos(L3 - L5),
     (-8) * cos(4 * L3),
     (0) * cos(3 * L3 - 2 * L5),
     (0) * cos(L2 - 2 * L3),
     (0) * cos(2 * L2 - 3 * L3),
     (-9) * cos(2 * L6),
     (-9) * cos(2 * L2 - 4 * L3),
     (0) * cos(3 * L3 - 2 * L4),
     (0) * cos(l + 2 * D - m),
     (-7) * cos(8 * L2 - 12 * L3),
     (-7) * cos(8 * L2 - 14 * L3),
     (-5) * cos(2 * L4),
     (-1) * cos(3 * L2 - 4 * L3),
     (-6) * cos(2 * L3 - 2 * L5),
     (-7) * cos(3 * L2 - 3 * L3),
     (-5) * cos(2 * L3 - 2 * L4),
     (0) * cos(l - 2 * D)
   ))

   ys<-colSums(rbind(
     (25 - 13 * T) * sin(L3),
     (25697 - 95 * T) * sin(2 * L3),
     (6) * sin(L5),
     (0) * sin(l),
     (-216 - 4 * T) * sin(3 * L3),
     (2) * sin(L6),
     (0) * sin(F),
     (0) * sin(l + m),
     (-9) * sin(2 * L5),
     (1) * sin(2 * L3 - L5),
     (25) * sin(3 * L3 - 8 * L4 + 3 * L5),
     (-25) * sin(5 * L3 - 8 * L4 + 3 * L5),
     (0) * sin(2 * L2 - L3),
     (0) * sin(L2),
     (0) * sin(L7),
     (0) * sin(L3 - 2 * L5),
     (1) * sin(L8),
     (-1) * sin(L3 + L5),
     (-10) * sin(2 * L2 - 2 * L3),
     (-2) * sin(L3 - L5),
     (-8) * sin(4 * L3),
     (0) * sin(3 * L3 - 2 * L5),
     (0) * sin(L2 - 2 * L3),
     (0) * sin(2 * L2 - 3 * L3),
     (-8) * sin(2 * L6),
     (8) * sin(2 * L2 - 4 * L3),
     (0) * sin(3 * L3 - 2 * L4),
     (0) * sin(l + 2 * D - m),
     (-6) * sin(8 * L2 - 12 * L3),
     (6) * sin(8 * L2 - 14 * L3),
     (-4) * sin(2 * L4),
     (-2) * sin(3 * L2 - 4 * L3),
     (-5) * sin(2 * L3 - 2 * L5),
     (-6) * sin(3 * L2 - 3 * L3),
     (-4) * sin(2 * L3 - 2 * L4),
     (0) * sin(l - 2 * D)
   ))

   yc<-colSums(rbind(
     (1578089 + 156 * T) * cos(L3),
     (-5904 - 130 * T) * cos(2 * L3),
     (-657) * cos(L5),
     (-656) * cos(l),
     (-446 + 5 * T) * cos(3 * L3),
     (-147) * cos(L6),
     (26) * cos(F),
     (-36) * cos(l + m),
     (-30) * cos(2 * L5),
     (-28) * cos(2 * L3 - L5),
     (8) * cos(3 * L3 - 8 * L4 + 3 * L5),
     (-8) * cos(5 * L3 - 8 * L4 + 3 * L5),
     (-19) * cos(2 * L2 - L3),
     (17) * cos(L2),
     (-16) * cos(L7),
     (15) * cos(L3 - 2 * L5),
     (-15) * cos(L8),
     (-10) * cos(L3 + L5),
     (0) * cos(2 * L2 - 2 * L3),
     (9) * cos(L3 - L5),
     (6) * cos(4 * L3),
     (9) * cos(3 * L3 - 2 * L5),
     (-9) * cos(L2 - 2 * L3),
     (-8) * cos(2 * L2 - 3 * L3),
     (0) * cos(2 * L6),
     (0) * cos(2 * L2 - 4 * L3),
     (-8) * cos(3 * L3 - 2 * L4),
     (-7) * cos(l + 2 * D - m),
     (4) * cos(8 * L2 - 12 * L3),
     (-4) * cos(8 * L2 - 14 * L3),
     (5) * cos(2 * L4),
     (-7) * cos(3 * L2 - 4 * L3),
     (-4) * cos(2 * L3 - 2 * L5),
     (0) * cos(3 * L2 - 3 * L3),
     (-5) * cos(2 * L3 - 2 * L4),
     (-5) * cos(l - 2 * D)
   ))

   zs<-colSums(rbind(
     (10 + 32 * T) * sin(L3),
     (11141 - 48 * T) * sin(2 * L3),
     (-15) * sin(L5),
     (0) * sin(l),
     (-94) * sin(3 * L3),
     (-6) * sin(L6),
     (0) * sin(F),
     (0) * sin(l + m),
     (-5) * sin(2 * L5),
     (0) * sin(2 * L3 - L5),
     (11) * sin(3 * L3 - 8 * L4 + 3 * L5),
     (-11) * sin(5 * L3 - 8 * L4 + 3 * L5),
     (0) * sin(2 * L2 - L3),
     (0) * sin(L2),
     (0) * sin(L7),
     (1) * sin(L3 - 2 * L5),
     (-3) * sin(L8),
     (-1) * sin(L3 + L5),
     (-4) * sin(2 * L2 - 2 * L3),
     (-1) * sin(L3 - L5),
     (-3) * sin(4 * L3),
     (0) * sin(3 * L3 - 2 * L5),
     (0) * sin(L2 - 2 * L3),
     (0) * sin(2 * L2 - 3 * L3),
     (-3) * sin(2 * L6),
     (3) * sin(2 * L2 - 4 * L3),
     (0) * sin(3 * L3 - 2 * L4),
     (0) * sin(l + 2 * D - m),
     (-3) * sin(8 * L2 - 12 * L3),
     (3) * sin(8 * L2 - 14 * L3),
     (-2) * sin(2 * L4),
     (1) * sin(3 * L2 - 4 * L3),
     (-2) * sin(2 * L3 - 2 * L5),
     (-3) * sin(3 * L2 - 3 * L3),
     (-2) * sin(2 * L3 - 2 * L4),
     (0) * sin(l - 2 * D)
   ))

   zc<-colSums(rbind(
     (684185 - 358 * T) * cos(L3),
     (-2559 - 55 * T) * cos(2 * L3),
     (-282) * cos(L5),
     (-285) * cos(l),
     (-193) * cos(3 * L3),
     (-61) * cos(L6),
     (-59) * cos(F),
     (-16) * cos(l + m),
     (-13) * cos(2 * L5),
     (-12) * cos(2 * L3 - L5),
     (3) * cos(3 * L3 - 8 * L4 + 3 * L5),
     (-3) * cos(5 * L3 - 8 * L4 + 3 * L5),
     (-8) * cos(2 * L2 - L3),
     (8) * cos(L2),
     (-7) * cos(L7),
     (7) * cos(L3 - 2 * L5),
     (-6) * cos(L8),
     (-5) * cos(L3 + L5),
     (0) * cos(2 * L2 - 2 * L3),
     (4) * cos(L3 - L5),
     (3) * cos(4 * L3),
     (4) * cos(3 * L3 - 2 * L5),
     (-4) * cos(L2 - 2 * L3),
     (-4) * cos(2 * L2 - 3 * L3),
     (0) * cos(2 * L6),
     (0) * cos(2 * L2 - 4 * L3),
     (-3) * cos(3 * L3 - 2 * L4),
     (-3) * cos(l + 2 * D - m),
     (2) * cos(8 * L2 - 12 * L3),
     (-2) * cos(8 * L2 - 14 * L3),
     (2) * cos(2 * L4),
     (-4) * cos(3 * L2 - 4 * L3),
     (-2) * cos(2 * L3 - 2 * L5),
     (0) * cos(3 * L2 - 3 * L3),
     (-2) * cos(2 * L3 - 2 * L4),
     (-2) * cos(l - 2 * D)
   ))


   X = xs + xc
   Y = ys + yc
   Z = zs + zc
   c = 17314463350.0 #velocidad de la luz



   ar <- rad2hours(Y * cos(hours2rad(right_ascension)) - X * sin(hours2rad(right_ascension))) / (c * cos(degrees2rad(declination)))
   d <-rad2degrees(-((X*cos(hours2rad(right_ascension))+Y*sin(hours2rad(right_ascension)))*sin(degrees2rad(declination))-Z*cos(degrees2rad(declination)))/c)

   return (list(declination=d, right_ascension=ar))
 }
}
