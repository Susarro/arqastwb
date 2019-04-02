#' Return a list of the stars included in the catalogue
#'
#' @return list of the stars included in the catalogue
#' @examples
#' get_stars()
#'
get_stars<- function(){
  return(c("alpha aquilae","alpha aurigae","alpha bootis","alpha canis majoris","alpha canis minoris","alpha cassiopeia", "alpha centauri", "alpha cygni", "alpha coronae", "alpha crucis", "alpha geminorum", "alpha lyrae", "alpha orionis", "alpha piscis austrini", "alpha scorpii", "alpha tauri", "alpha virginis", "beta centauri", "beta crucis", "beta geminorum", "beta orionis", "delta crucis", "delta orionis", "epsilon orionis", "eta tauri", "eta ursae majoris", "gamma crucis", "pleiades", "zeta orionis"))
}

#' Return the list of planets that can be seen withe the naked eye
#'
#' @return list of the stars included in the catalogue
#' @examples
#' get_stars()
#'
get_planets<- function(){
  return(c("mercury","venus","mars","jupyter","saturn"))
}


#' Returns the characteristics of a star
#'
#' @param name Name of the star. It should be include in the list get_stars()
#' @return
#' \itemize{
#'   \item former_declination Declination in degrees of the star referred to former_jd
#'   \item former_right_ascension Right ascension in hour angle of the star referred to former_jd
#'   \item proper_motion_declination declination of proper motion in degrees
#'   \item proper_motion_right_ascension right ascension of proper motion in hours
#'   \item former_jd Epoch of former_declination and former_right_ascension
#'   \item apparent_magnitude Apparent magnitude of the star
#' }'
#' @export
#' @examples
#' get_star("alpha aquilae")
#'

get_star<- function(name){

  if(name=="alpha aquilae")
  {
    return (list(former_declination=str2degrees("8\u00b052'05.9563\u0022"), former_right_ascension=str2hours("19:50:46.99855"),
                 proper_motion_declination=385.29 / 3600000, proper_motion_right_ascension=536.23 / (3600000 * 15),
                 former_jd=str2jd("2000-1-1T12:00"),
                 apparent_magnitude=0.77))

  }
  if(name=="alpha aurigae")
  {
    return (list(former_declination=str2degrees("45\u00b059'52.768\u0022"), former_right_ascension=str2hours("5:16:41.3591"),
                 proper_motion_declination=-427.11 / 3600000, proper_motion_right_ascension=75.52 / (3600000 * 15),
                 former_jd=str2jd("2000-1-1T12:00"),
                 apparent_magnitude=0.91))
  }
  if(name=="alpha bootis")
  {
    return (list(former_declination=str2degrees("19\u00b010'56.7\u0022"), former_right_ascension=str2hours("14:15:39.67"),
                 proper_motion_declination=-1999.4 / 3600000, proper_motion_right_ascension=-1093.45 / (3600000 * 15),
                 former_jd=str2jd("2000-1-1T12:00"),
                 apparent_magnitude=-0.04))
  }
  if(name=="alpha canis majoris")
  {
    return (list(former_declination=str2degrees("-16\u00b042'58.017\u0022"), former_right_ascension=str2hours("06:45:09.9173"),
                 proper_motion_declination=-1223.14 / 3600000, proper_motion_right_ascension=-546.05 / (3600000 * 15),
                 former_jd=str2jd("2000-1-1T12:00"),
                 apparent_magnitude=-1.47))
  }
  if(name=="alpha canis minoris")
  {
    return (list(former_declination=str2degrees("5\u00b013'29.9552\u0022"), former_right_ascension=str2hours("7:39:18.11950"),
                 proper_motion_declination=-1036.8 / 3600000, proper_motion_right_ascension=-714.590 / (3600000 * 15),
                 former_jd=str2jd("2000-1-1T12:00"),
                 apparent_magnitude=0.34))
  }
  if(name=="alpha cassiopeia")
  {
    return (list(former_declination=str2degrees("56\u00b032'14.392\u0022"), former_right_ascension=str2hours("00:40:30.4405"),
                 proper_motion_declination=-32.77 / 3600000, proper_motion_right_ascension=50.36 / (3600000 * 15),
                 former_jd=str2jd("2000-1-1T12:00"),
                 apparent_magnitude=2.24))
  }
  if(name=="alpha centauri")
  {
    return (list(former_declination=str2degrees("-60\u00b050'02.308\u0022"), former_right_ascension=str2hours("14:39:36.4951"),
                 proper_motion_declination=699 / 3600000, proper_motion_right_ascension=-3642 / (3600000 * 15),
                 former_jd=str2jd("2000-1-1T12:00"),
                 apparent_magnitude=-0.01))
  }
  if(name=="alpha cygni")
  {
    return (list(former_declination=str2degrees("45\u00b016'49\u0022"), former_right_ascension=str2hours("20:41:25.9"),
                 proper_motion_declination=1.95 / 3600000, proper_motion_right_ascension=1.99 / (3600000 * 15),
                 former_jd=str2jd("2000-1-1T12:00"),
                 apparent_magnitude=1.25))
  }
  if(name=="alpha coronae")
  {
    return (list(former_declination=str2degrees("26\u00b042'52.89\u0022"), former_right_ascension=str2hours("15:34:41.268"),
                 proper_motion_declination=-89.58 / 3600000, proper_motion_right_ascension=-120.27 / (3600000 * 15),
                 former_jd=str2jd("2000-1-1T12:00"),
                 apparent_magnitude=2.23))
  }
  if(name=="alpha crucis")
  {
    return (list(former_declination=str2degrees("-63\u00b005'56.7343\u0022"), former_right_ascension=str2hours("12:26:35.89522"),
                 proper_motion_declination=-14.86 / 3600000, proper_motion_right_ascension=-35.83 / (3600000 * 15),
                 former_jd=str2jd("2000-1-1T12:00"),
                 apparent_magnitude=0.77))
  }
  if(name=="alpha geminorum")
  {
    return (list(former_declination=str2degrees("31\u00b053'17.8160\u0022"), former_right_ascension=str2hours("07:34:35.87319"),
                 proper_motion_declination=-145.19 / 3600000, proper_motion_right_ascension=-191.45 / (3600000 * 15),
                 former_jd=str2jd("2000-1-1T12:00"),
                 apparent_magnitude=1.93))
  }
  if(name=="alpha lyrae")
  {
    return (list(former_declination=str2degrees("38\u00b047'01.2802\u0022"), former_right_ascension=str2hours("18:36:56.33635"),
                 proper_motion_declination=286.23 / 3600000, proper_motion_right_ascension=200.94 / (3600000 * 15),
                 former_jd=str2jd("2000-1-1T12:00"),
                 apparent_magnitude=0.03))
  }
  if(name=="alpha orionis")
  {
    return (list(former_declination=str2degrees("7\u00b024'25.426\u0022"), former_right_ascension=str2hours("5:55:10.30530"),
                 proper_motion_declination=9.56 / 3600000, proper_motion_right_ascension=24.95 / (3600000 * 15),
                 former_jd=str2jd("2000-1-1T12:00"),
                 apparent_magnitude=0.42))
  }
  if(name=="alpha piscis austrini")
  {
    return (list(former_declination=str2degrees("-29\u00b037'20.050\u0022"), former_right_ascension=str2hours("22:57:39.0465"),
                 proper_motion_declination=-158.98 / 3600000, proper_motion_right_ascension=-331.11 / (3600000 * 15),
                 former_jd=str2jd("2000-1-1T12:00"),
                 apparent_magnitude=1.16))
  }
  if(name=="alpha scorpii")
  {
    return (list(former_declination=str2degrees("-26\u00b025'55.2094\u0022"), former_right_ascension=str2hours("16:29:24.45970"),
                 proper_motion_declination=-23.30 / 3600000, proper_motion_right_ascension=-12.11 / (3600000 * 15),
                 former_jd=str2jd("2000-1-1T12:00"),
                 apparent_magnitude=0.6))
  }
  if(name=="alpha tauri")
  {
    return (list(former_declination=str2degrees("16\u00b030'33.49\u0022"), former_right_ascension=str2hours("04:35:55.239"),
                 proper_motion_declination=-189.35 / 3600000, proper_motion_right_ascension=62.78 / (3600000 * 15),
                 former_jd=str2jd("2000-1-1T12:00"),
                 apparent_magnitude=0.75))
  }
  if(name=="alpha virginis")
  {
    return (list(former_declination=str2degrees("-11\u00b009'40.75\u0022"), former_right_ascension=str2hours("13:25:11.579"),
                 proper_motion_declination=-30.65 / 3600000, proper_motion_right_ascension=-42.35 / (3600000 * 15),
                 former_jd=str2jd("2000-1-1T12:00"),
                 apparent_magnitude=1.04))
  }
  if(name=="beta centauri")
  {
    return (list(former_declination=str2degrees("-60\u00b022'22.9266\u0022"), former_right_ascension=str2hours("14:03:49.40535"),
                 proper_motion_declination=-23.16 / 3600000, proper_motion_right_ascension=-33.27 / (3600000 * 15),
                 former_jd=str2jd("2000-1-1T12:00"),
                 apparent_magnitude=0.61))
  }
  if(name=="beta crucis")
  {
    return (list(former_declination=str2degrees("-59\u00b041'19.5792\u0022"), former_right_ascension=str2hours("12:47:43.26877"),
                 proper_motion_declination=-16.185 / 3600000, proper_motion_right_ascension=-42.97 / (3600000 * 15),
                 former_jd=str2jd("2000-1-1T12:00"),
                 apparent_magnitude=1.25))
  }
  if(name=="beta geminorum")
  {
    return (list(former_declination=str2degrees("28\u00b001'34.3160\u0022"), former_right_ascension=str2hours("07:45:18.94987"),
                 proper_motion_declination=-45.8 / 3600000, proper_motion_right_ascension=-626.55 / (3600000 * 15),
                 former_jd=str2jd("2000-1-1T12:00"),
                 apparent_magnitude=1.14))
  }
  if(name=="beta orionis")
  {
    return (list(former_declination=str2degrees("-8\u00b012'05.8981\u0022"), former_right_ascension=str2hours("05:14:32.27210"),
                 proper_motion_declination=0.5 / 3600000, proper_motion_right_ascension=1.31 / (3600000 * 15),
                 former_jd=str2jd("2000-1-1T12:00"),
                 apparent_magnitude=0.13))
  }
  if(name=="delta crucis")
  {
    return (list(former_declination=str2degrees("-58\u00b044'56.1369\u0022"), former_right_ascension=str2hours("12:15:08.71673"),
                 proper_motion_declination=-10.36 / 3600000, proper_motion_right_ascension=-35.81 / (3600000 * 15),
                 former_jd=str2jd("2000-1-1T12:00"),
                 apparent_magnitude=2.79))
  }
  if(name=="delta orionis")
  {
    return (list(former_declination=str2degrees("-0\u00b017'56.7424\u0022"), former_right_ascension=str2hours("05:32:00.40009"),
                 proper_motion_declination=-0.69 / 3600000, proper_motion_right_ascension=0.64 / (3600000 * 15),
                 former_jd=str2jd("2000-1-1T12:00"),
                 apparent_magnitude=2.23))
  }
  if(name=="epsilon orionis")
  {
    return (list(former_declination=str2degrees("-0.1\u00b012'06.90\u0022"), former_right_ascension=str2hours("05:36:12.81"),
                 proper_motion_declination=-1.06 / 3600000, proper_motion_right_ascension=1.49 / (3600000 * 15),
                 former_jd=str2jd("2000-1-1T12:00"),
                 apparent_magnitude=1.70))
  }
  if(name=="eta tauri")
  {
    return (list(former_declination=str2degrees("24\u00b06'18.49\u0022"), former_right_ascension=str2hours("03:47:29.077"),
                 proper_motion_declination=-43.67 / 3600000, proper_motion_right_ascension=19.34 / (3600000 * 15),
                 former_jd=str2jd("2000-1-1T12:00"),
                 apparent_magnitude=2.873))
  }
  if(name=="eta ursae majoris")
  {
    return (list(former_declination=str2degrees("49\u00b018'48\u0022"), former_right_ascension=str2hours("13:47:32.4"),
                 proper_motion_declination=14.91 / 3600000, proper_motion_right_ascension=121.17 / (3600000 * 15),
                 former_jd=str2jd("2000-1-1T12:00"),
                 apparent_magnitude=1.85))
  }
  if(name=="gamma crucis")
  {
    return (list(former_declination=str2degrees("-57\u00b006'47.5684\u0022"), former_right_ascension=str2hours("12:31:09.95961"),
                 proper_motion_declination=-265.08 / 3600000, proper_motion_right_ascension=-28.23 / (3600000 * 15),
                 former_jd=str2jd("2000-1-1T12:00"),
                 apparent_magnitude=1.63))
  }
  if(name=="pleiades")
  {
    return (list(former_declination=str2degrees("24\u00b06'18.49\u0022"), former_right_ascension=str2hours("03:47:29.077"),
                 proper_motion_declination=-43.67 / 3600000, proper_motion_right_ascension=19.34 / (3600000 * 15),
                 former_jd=str2jd("2000-1-1T12:00"),
                 apparent_magnitude=1.6))
  }
  if(name=="zeta orionis")
  {
    return (list(former_declination=str2degrees("-1\u00b056'34.2649\u0022"), former_right_ascension=str2hours("05:40:45.52666"),
                 proper_motion_declination=2.03 / 3600000, proper_motion_right_ascension=3.19 / (3600000 * 15),
                 former_jd=str2jd("2000-1-1T12:00"),
                 apparent_magnitude=1.77))
  }
  stop("The star required is not in the catalogue\n")
}

#' Distance to Earth with planetary aberration correction (effect of light-time and Earth's motion)
#'
#' @param jd Julian day
#' @param celestial_body celestial_body name as string: mercury, venus, mars, jupyter, saturn, moon, sun...
#' @return distance to Earth
#' @examples
#' distance_to_earth(julian_day(13.19,11,2028),"venus")
#'
distance_to_earth <- function(jd, celestial_body){
  distance<-NA
  if(celestial_body=="moon")
  {
    distance<-distance_to_earth_moon(jd)
  }
  else if(celestial_body=="sun")
  {
    distance<-heliocentric_radius(jd,"earth")
  }
  else if(celestial_body %in% get_planets())
  {
    distance<-aberration_corrected_coordinates_planet(jd, celestial_body)$distance
  }
  else
  {
    distance<-Inf
  }
  return(distance)
}

#' Apparent equatorial coordinates of a celestial body
#'
#' @param jd Julian day
#' @param celestial_body celestial_body name as string: mercury, venus, mars, jupyter, saturn, moon, sun...
#' @return Equatorial coordinates
#' \itemize{
#'   \item declination Declination in degrees
#'   \item right_ascension Right ascension in hour angle
#'   \item distance Distance to Earth (planet or moon)
#'   \item illuminated_fraction Illuminated fraction of the disk of a planet  or moon (planet or moon)
#'   \item apparent_magnitude apparent magnitude (planet or moon)
#' }#'
#' @export
#' @examples
#' jd<-julian_day(13.19,11,2028)
#' apparent_equatorial_position_planet(jd,"venus")
#'
apparent_equatorial_position<- function(jd, celestial_body){
  if(celestial_body=="moon")
  {
    return (apparent_equatorial_position_moon(jd))
  }
  else if(celestial_body=="sun")
  {
    return (apparent_equatorial_position_sun(jd))
  }
  else if(celestial_body %in% get_planets())
  {
    return (apparent_equatorial_position_planet(jd,celestial_body))
  }
  else if(celestial_body %in% get_stars())
  {
    ch<-get_star(celestial_body)
    return (apparent_equatorial_position_star(jd=jd,
                                              former_jd = ch$former_jd,
                                              former_declination=ch$former_declination,
                                              former_right_ascension=ch$former_right_ascension,
                                              proper_motion_declination=ch$proper_motion_declination,
                                              proper_motion_right_ascension=ch$proper_motion_right_ascension))
  }
}

#' Dates of solstices, equinoxes and cross-quarter days in an interval
#'
#' @param from Initial value of the interval to be searched
#' @param to Final value of the interval to be searched
#' @param silent If false a text progress bar is shown
#' @return a data frame with two columns: festival identifying the festival and date the corresponding Julian date
#' @export
#' @examples
#' from <- str2jd("2014-03-08")
#' to <- str2jd("2018-03-08")
#' festivals(from,to)
#'
festivals<-function(from,to, silent=FALSE){
  if(isFALSE(silent)) cat("\n","winter solstices","\n")
  roots_winter<-crossing_zero_up(function(x) derivative(function(x) apparent_equatorial_position(x,"sun")$declination, x),
                                 from=from-365.25, to=to+365.25, period=365.25, search_interval=5,silent=silent)
  if(isFALSE(silent)) cat("\n","summer solstices","\n")
  roots_summer<-crossing_zero_up(function(x)-derivative(function(x) apparent_equatorial_position(x,"sun")$declination, x),
                                 from=from-365.25, to=to+365.25, period=365.25, search_interval=5,silent=silent)

  solstices<-data.frame(date=roots_winter,festival="winter solstice") %>%
    rbind(data.frame(date=roots_summer,festival="summer solstice")) %>%
    arrange(date)

  if(isFALSE(silent)) cat("\n","equinoxes","\n")
  equinoxes<-solstices %>%
    mutate(festival=case_when(
      paste(lag(festival,1), festival,sep="-")=="summer solstice-winter solstice"~"autumn equinox",
      paste(lag(festival,1), festival,sep="-")=="winter solstice-summer solstice"~"spring equinox")) %>%
    mutate(date=(date+lag(date,1))/2) %>%
    stats::na.omit() %>%
    arrange(date)

  if(isFALSE(silent)) cat("\n","cross-quarter days","\n")
  cross_quarter_days<-solstices %>%
    rbind(equinoxes) %>%
    arrange(date) %>%
    mutate(festival=case_when(
      paste(lag(festival,1),festival,sep="-")=="summer solstice-autumn equinox"~"lughnasa",
      paste(lag(festival,1),festival,sep="-")=="autumn equinox-winter solstice"~"samhain",
      paste(lag(festival,1),festival,sep="-")=="winter solstice-spring equinox"~"imbolc",
      paste(lag(festival,1),festival,sep="-")=="spring equinox-summer solstice"~"beltaine")) %>%
    mutate(date=(date+lag(date,1))/2) %>%
    stats::na.omit() %>%
    arrange(date)

  festivals<- solstices %>% rbind(equinoxes) %>%
    rbind(cross_quarter_days) %>%
    arrange(date) %>%
    filter(date>from & date<to)

}

#' Dates of first visibility of the lunar crescent just after a solar festival (solstice, equinox or cross-quarter day)
#'
#' @param from Initial value of the interval to be searched
#' @param to Final value of the interval to be searched
#' @param festivities Festivities data frame obtained with festivals() function
#' @param festival_reference Solar festival (solstice, equinox or cross-quarter day) as reference for the first new moon
#' @param geographical_longitude Geographical longitude in degrees
#' @param geographical_latitude Geographical latitude in degrees
#' @param geographical_altitude Geographical altitude in meters
#' @param silent If false a text progress bar is shown
#' @return Dates of first visibility of the lunar crescent
#' @export
#' @examples
#' from <- str2jd("2014-03-08")
#' to <- str2jd("2018-03-08")
#' geographical_latitude=str2degrees("42°25'27\"N")
#' geographical_longitude=str2degrees("6°03'00\"W")
#' geographical_altitude=782
#' first_new_moon(from,to, festival_reference="spring equinox",
#'                geographical_latitude=geographical_latitude,
#'                geographical_longitude=geographical_longitude,
#'                geographical_altitude=geographical_altitude)
#'
first_new_moon<-function(from,to,festivities=NULL, festival_reference="winter solstice", geographical_latitude, geographical_longitude,geographical_altitude,silent=FALSE){

  parms_sun=c("sun",degrees2str(geographical_latitude), degrees2str(geographical_longitude),as.character(geographical_altitude),degrees2str(0))
  parms_moon=c("moon",degrees2str(geographical_latitude), degrees2str(geographical_longitude),as.character(geographical_altitude),degrees2str(0))

  if(is.null(festivities))
  {
    festivities<-festivals(from=from,to=to,silent=silent)
  }

  festivities<- festivities %>% filter(.data$festival==festival_reference) %>% filter(date>from & date<to)

  lunar_visibility<-vector()


  if(isFALSE(silent))
  {
    pb<-utils::txtProgressBar(min = 1, max = nrow(festivities), initial = 0, char = "=",
                              width = NA, style = 1)
  }

  for(i in 1:nrow(festivities))
  {

    from<-festivities$date[i]
    to<-festivities$date[i]+30

    rr<-rootSolve::uniroot.all(function(x) derivative(function(x) apparent_equatorial_position_moon(x)$illuminated_fraction, x),c(from, to))
    new_moon_jd<-first(rr[apparent_equatorial_position_moon(rr)$illuminated_fraction<0.5])

    from<-new_moon_jd
    to<-from+5
    sun_settting<-crossing_zero_up(function(x) -apparent_elevation(x,parms_sun),
                                   from=from, to=to, period=1, search_interval=0.1,silent=TRUE)

    moon_settting<-crossing_zero_up(function(x) -apparent_elevation(x,parms_moon),
                                    from=sun_settting[1], to=sun_settting[1]+to-from, period=1.1, search_interval=0.2,silent=TRUE)


    moon_visibility_df<-data.frame(sun=sun_settting[sapply(moon_settting,function(m) max(which((m-sun_settting)>=0 )))],
                                   moon=moon_settting,y=0) %>%
      mutate(moon_observation=.data$sun+(4/9)*(.data$moon-.data$sun)) %>%
      mutate(sun_elevation=apparent_elevation(.data$moon_observation,parms_sun)) %>%
      mutate(moon_elevation=apparent_elevation(.data$moon_observation,parms_moon)) %>%
      mutate(arcus_visionis=.data$moon_elevation-.data$sun_elevation) %>%
      mutate(W=apparent_equatorial_position_moon(.data$moon_observation)$illuminated_fraction*32) %>%
      mutate(lunar_visibility_threshold=5.65 -0.1018*.data$W^3+0.7319*.data$W^2-6.3226*.data$W+7.1651)


    mv<-(moon_visibility_df
         %>% filter(.data$moon_observation>new_moon_jd & .data$arcus_visionis>.data$lunar_visibility_threshold)
         %>% slice(1))$moon_observation

    #cat(jd2str(mv),"\n")
    lunar_visibility<-c(lunar_visibility,mv)

    if(isFALSE(silent)) utils::setTxtProgressBar(pb, i)

  }
  return(lunar_visibility)
}


#' Visual magnitude of non-standard sky brightness
#'
#' @param angular_dist Angular distance sun-star in degrees
#' @param sun_altitude Sun's altitude in degrees
#' @param star_zenith_dist Star's zenith in degrees
#' @param visual_extinction_coefficient Visual extinction coefficient
#' @return Visual magnitude of non-standard sky brightness
#' @export
#' @examples
#' sky_brightness(degrees2rad(150),degrees2rad(-10),degrees2rad(88))
#'
sky_brightness<-function(angular_dist,sun_altitude,star_zenith_dist,visual_extinction_coefficient=0.2){
  angular_dist<-angular_dist*pi/180
  star_zenith_dist<-star_zenith_dist*pi/180
  sun_altitude<-sun_altitude*pi/180
  standard_sky_brightness<-10^(4.75-angular_dist*star_zenith_dist/3+sun_altitude*(12+8.21*star_zenith_dist)+2.86*star_zenith_dist)
  dark_sky_zenith_limiting_magnitude<-6

  K<-10^-1.9
  C<-10^-9.80

  Y<--0.2*(dark_sky_zenith_limiting_magnitude+16.57+visual_extinction_coefficient+2.5*log10(C))
  non_standard_sky_brightness<-(10^Y-1)^2/K+visual_extinction_coefficient/0.2*(standard_sky_brightness-117.8785)
  K<-ifelse(log10(non_standard_sky_brightness)<3.17,10^-1.9,10^-5.9)
  C<-ifelse(log10(non_standard_sky_brightness)<3.17,10^-9.8,10^-8.35)
  Y<--0.2*(dark_sky_zenith_limiting_magnitude+16.5+visual_extinction_coefficient+2.5*log10(C))
  non_standard_sky_brightness<-(10^Y-1)^2/K+visual_extinction_coefficient/0.2*(standard_sky_brightness-117.8785)
  illuminance<-C*(1+sqrt(K*non_standard_sky_brightness))^2
  extinction_air_mass<-1/(cos(star_zenith_dist)+0.025*exp(-11*cos(star_zenith_dist)))
  sky_visual_magnitude<--16.57-2.5*log10(illuminance)-visual_extinction_coefficient*extinction_air_mass

  return(sky_visual_magnitude)
}

#' Extinction zenith angle
#'
#' @param star_visual_magnitude Visual magnitude of a star
#' @param visual_extinction_coefficient Visual extinction coefficient
#' @return Extinction angle, in degrees
#' @export
#' @examples
#' sky_brightness(degrees2rad(150),degrees2rad(-10),degrees2rad(88))
#'
extinction_angle<-function(star_visual_magnitude,visual_extinction_coefficient=0.2){
  dark_sky_zenith_limiting_magnitude<-6
  visual_extinction_coefficient<-0.2

  K<-10^-1.9
  C<-10^-9.8

  Y<--0.2*(dark_sky_zenith_limiting_magnitude+16.5+visual_extinction_coefficient+2.5*log10(C))
  non_standard_sky_brightness<-(10^Y-1)^2/K#+visual_extinction_coefficient*236/0.2
  K<-ifelse(log10(non_standard_sky_brightness)<3.17,10^-1.9,10^-5.9)
  C<-ifelse(log10(non_standard_sky_brightness)<3.17,10^-9.8,10^-8.35)
  Y<--0.2*(dark_sky_zenith_limiting_magnitude+16.5+visual_extinction_coefficient+2.5*log10(C))
  non_standard_sky_brightness<-(10^Y-1)^2/K#+visual_extinction_coefficient*236/0.2
  illuminance<-C*(1+sqrt(K*non_standard_sky_brightness))^2
  extinction_air_mass<-(-16.57-2.5*log10(illuminance)-star_visual_magnitude)/visual_extinction_coefficient
  star_extinction_angle<-vector()
  for(eam in extinction_air_mass)
  {
    fZ<-function(Z){eam-1/(cos(degrees2rad(Z))+0.025*exp(-11*cos(degrees2rad(Z))))}
    r1<-rootSolve::uniroot.all(fZ,c(0, 90))
    s<-first(r1[derivative(fZ,r1,0.01)<0])
    star_extinction_angle<-c(star_extinction_angle,s)
  }

  return(star_extinction_angle)
}


#' Star visibility information when certain star sets or rises at a given horizontal coordinate value
#'
#' @param x Julian day
#' @param parms Vector of strings containing the name of celestial body, geographic longitude, geographic latitude, geographic altitude and reference horizontal coordinate value
#' @param horizontal_coordinate Horizontal coordinate (azimuth or elevation) to compare to a reference coordinate value or threshold
#' @param horizontal_star_time "setting" or "rising". Only required if horizontal_coordinate=="elevation"
#' @return Star visibility information
#' \itemize{
#'   \item date_star Julian day when the star reaches the specified horizontal coordinate value when rising or setting
#'   \item sky_visual_magnitude Visual magnitude of the sky at the specified horizontal coordinate value when the star is rising or setting
#'   \item azimuth_sun Azimuth of the Sun when the star reaches the specified horizontal coordinate value when rising or setting
#'   \item azimuth_star Azimuth of the star when it reaches the specified horizontal  coordinate value when rising or setting
#'   \item altitude_sun Altitude of the Sun when the star reaches the specified horizontal coordinate value when rising or setting
#'   \item altitude_star Altitude of the star when it reaches the specified horizontal  coordinate value when rising or setting
#'   \item star_visual_magnitude Visual magnitude of the star
#' }#'
#' @export
#' @examples
#' years<--2000:-1999
#' from<-str2jd(paste(years[1],"01","01",sep="-"))
#' to<-str2jd(paste(years[length(years)],"01","01",sep="-"))
#' star_name<-"epsilon orionis"
#' ee<-star_visibility(seq(from,to,1),
#'                     parms=c(star_name,
#'                             "42\u00b036'25\u0022N",
#'                             "5\u00b033'58\u0022W",
#'                             "782",
#'                             "2\u00b00'0\u0022"),
#'                     horizontal_coordinate="elevation", horizontal_star_time="setting")
#'
#' ee<-star_visibility(seq(from,to,1),
#'                     parms=c(star_name,
#'                             "42\u00b036'25\u0022N",
#'                             "5\u00b033'58\u0022W",
#'                             "782",
#'                             "2\u00b00'0\u0022"),
#'                     horizontal_coordinate="elevation", horizontal_star_time="rising")
#'
#' ee<-star_visibility(seq(from,to,1),
#'                     parms=c(star_name,
#'                             "42\u00b036'25\u0022N",
#'                             "5\u00b033'58\u0022W",
#'                             "782",
#'                             "-107\u00b022'33.866\u0022"),
#'                     horizontal_coordinate="azimuth")
#'
#' ee<-star_visibility(seq(from,to,1),
#'                     parms=c(star_name,
#'                             "42\u00b036'25\u0022N",
#'                             "5\u00b033'58\u0022W",
#'                             "782",
#'                             "107\u00b022'33.866\u0022"),
#'                     horizontal_coordinate="azimuth")
#'
star_visibility<-function(x, parms,horizontal_coordinate="elevation", horizontal_star_time="setting"){
  if(horizontal_coordinate=="elevation")
  {
    fnc<-function(x) true_elevation(x, parms=parms)
  }
  else if(horizontal_coordinate=="azimuth")
  {
    fnc<-function(x) azimuth(x, parms=parms)
  }
  else
  {
    stop("required elevation or azimuth in horizontal_coordinate")
  }
  x<-sapply(x,function(x0) {
    r1<-rootSolve::uniroot.all(fnc,c(x0, x0+1))
    if(length(r1)>0)
    {
      if(horizontal_coordinate=="elevation")
      {
        if(horizontal_star_time=="setting")
        {
          r2<-first(r1[derivative(fnc,r1)<0])
        }
        else if(horizontal_star_time=="rising")
        {
          r2<-first(r1[derivative(fnc,r1)>0])
        }
        else
        {
          stop("required setting or rising in horizontal_star_time")
        }
      }else
      {
        if(str2degrees(parms[5])<0)
        {
          r2<-first(r1[derivative(function(x) apparent_elevation(x, parms=parms),r1)<0])
        }
        else
        {
          r2<-first(r1[derivative(function(x) apparent_elevation(x, parms=parms),r1)>0])
        }
      }
    }
    else
    {
      r2<-NA
    }

    return(r2)
  })


  geographic_latitude=str2degrees(parms[2])
  geographic_longitude=str2degrees(parms[3])
  geographic_altitude =as.numeric(parms[4])

  e<-apparent_equatorial_position(x,parms[1])
  ea<-geocentric2topocentric(e$declination, e$right_ascension, jd=x, geographic_longitude, geographic_latitude,geographic_altitude, parms[1])
  #ea<-e
  h<-equatorial2horizontal(geographic_longitude,geographic_latitude,ea$declination, ea$right_ascension,x)
  #a<-atmospheric_refraction_correction(h$altitude,apparent=FALSE)
  a<-h$altitude

  e0<-apparent_equatorial_position(x,"sun")
  ea0<-geocentric2topocentric(e0$declination, e0$right_ascension, jd=x, geographic_longitude, geographic_latitude,geographic_altitude, "sun")
  #ea0<-e0
  h0<-equatorial2horizontal(geographic_longitude,geographic_latitude,ea0$declination, ea0$right_ascension,x)
  #a0<-atmospheric_refraction_correction(h0$altitude,apparent=FALSE)
  a0<-h0$altitude

  sb<-sky_brightness(angular_dist=abs(reduce_degrees(h$azimuth-h0$azimuth,signed=TRUE)),sun_altitude=reduce_degrees(a0,signed=TRUE),star_zenith_dist=90-reduce_degrees(a,signed=TRUE))

  return(list(date_star=x,
              sky_visual_magnitude=sb,
              azimuth_sun=reduce_degrees(h0$azimuth,signed=TRUE),
              azimuth_star=reduce_degrees(h$azimuth,signed=TRUE),
              altitude_sun=reduce_degrees(a0,signed=TRUE),
              altitude_star=reduce_degrees(a,signed=TRUE),
              star_visual_magnitude=get_star(parms[1])$apparent_magnitude))
}



#' Julian day for star's appearance or disappearance
#'
#' @param from Initial value of the interval to be searched
#' @param to Final value of the interval to be searched
#' @param star_name Name of the star
#' @param geographic_longitude Geographical longitude in degrees
#' @param geographic_latitude Geographical latitude in degrees
#' @param geographic_altitude Geographical altitude in meters
#' @param horizontal_coordinate Horizontal coordinate (azimuth or elevation) to compare to a reference coordinate value or threshold
#' @param horizontal_star_time "setting" or "rising". Only required if horizontal_coordinate=="elevation"
#' @param coordinate_threshold Horizontal coordinate value used as a reference or threshold for the specified horizonal coordinate of the star during its setting or rising
#' @param visibility_type "appearance" (visual magnitude of the sky increasing as days go by) or "disappearance" (visual magnitude of the sky decreasing as days go by)
#' @param silent If false a text progress bar is shown
#' @return Time of the star's appearance or disappearance  during its setting or rising at twilight time
#' @export
#' @examples
#' years<--2000:-1999
#' from<-str2jd(paste(years[1],"01","01",sep="-"))
#' to<-str2jd(paste(years[length(years)],"01","01",sep="-"))
#' star_name<-"epsilon orionis"
#' stellar_phenomena(from=from,to=to, star_name=star_name,
#'                   geographic_latitude=str2degrees("42\u00b036'25\u0022N"),
#'                   geographic_longitude=str2degrees("5\u00b033'58\u0022W"),
#'                   geographic_altitude=782,
#'                   horizontal_coordinate="elevation", horizontal_star_time="setting",
#'                   coordinate_threshold=2, visibility_type="disappearance", silent=FALSE)
stellar_phenomena<-function(from,to, star_name,
                            geographic_latitude, geographic_longitude, geographic_altitude,
                            horizontal_coordinate, horizontal_star_time, coordinate_threshold,
                            visibility_type="appearance", silent=FALSE){
  visibility_slope<-1
  if(visibility_type=="disappearance")
  {
    visibility_slope<--1
  }
  return(crossing_zero_up(
    function(x){
      ee<-star_visibility(x,parms=c(star_name,
                                    degrees2str(geographic_latitude),
                                    degrees2str(geographic_longitude),
                                    as.character(geographic_altitude),
                                    degrees2str(coordinate_threshold)),
                          horizontal_coordinate=horizontal_coordinate, horizontal_star_time=horizontal_star_time)
      return (visibility_slope*(ee$sky_visual_magnitude-ee$star_visual_magnitude))
    },
    from=from, to=to, period=365.25, search_interval=20,incrx=1,silent=silent))
}

#' Calculate azimuth and altitude from geographical coordinates of origin and foresight
#'
#' @param geographic_latitude.origin Geographical latitude of the origin in degrees
#' @param geographic_longitude.origin Geographical longitude of the origin in degrees
#' @param geographic_altitude.origin Geographical altitude of the origin in meters
#' @param geographic_latitude.foresight Geographical latitude of the foresight in degrees
#' @param geographic_longitude.foresight Geographical longitude of the foresight in degrees
#' @param geographic_altitude.foresight Geographical altitude of the foresight in meters
#' @return Horizontal coordinates
#' \itemize{
#'   \item azimuth Azimuth in degrees
#'   \item altitude Altitude in hour angle
#' }
#' @export
#' @examples
#' geographic_latitude.origin<-c(str2degrees("42\u00b026'58.00\u0022N"),
#'                               str2degrees("42\u00b026'58.00\u0022N"))
#' geographic_longitude.origin<-c(str2degrees("06\u00b003'54.00\u0022W"),
#'                                str2degrees("06\u00b003'54.00\u0022W"))
#' geographic_altitude.origin<-c(850,850)
#' geographic_latitude.foresight<-c(str2degrees("42\u00b019'43.50\u0022N"),
#'                                  str2degrees("42\u00b020'45.00\u0022N"))
#' geographic_longitude.foresight<-c(str2degrees("06\u00b021'51.00\u0022W"),
#'                                   str2degrees("06\u00b023'37.00\u0022W"))
#' geographic_altitude.foresight<-c(2047,2183)
#'
#' do.call(alignment2equatorial,
#'         data.frame(geographic_latitude.origin,
#'            geographic_longitude.origin,
#'            geographic_altitude.origin,
#'            geographic_latitude.foresight,
#'            geographic_longitude.foresight,
#'            geographic_altitude.foresight))

alignment2equatorial<-function(geographic_latitude.origin, geographic_longitude.origin, geographic_altitude.origin,
                               geographic_latitude.foresight, geographic_longitude.foresight, geographic_altitude.foresight){
  d<-rad2degrees(acos(sin(degrees2rad(geographic_latitude.origin))*sin(degrees2rad(geographic_latitude.foresight))+cos(degrees2rad(geographic_latitude.origin))*cos(degrees2rad(geographic_latitude.foresight))*cos(degrees2rad(geographic_longitude.foresight-geographic_longitude.origin))))
  a0<-rad2degrees(asin(cos(degrees2rad(geographic_latitude.foresight))*sin(degrees2rad(geographic_longitude.foresight-geographic_longitude.origin))/sin(degrees2rad(d))))
  azimuth<-ifelse(geographic_longitude.origin==geographic_longitude.foresight,
                  ifelse(geographic_latitude.foresight>=geographic_latitude.origin,0,180),
                  ifelse(geographic_longitude.foresight>=geographic_longitude.origin,a0,180-a0))
  h<-rad2degrees(atan2(((6378135+geographic_altitude.foresight)*cos(degrees2rad(d))-(6378135+geographic_altitude.origin+1.7)),((6378135+geographic_altitude.origin)*sin(degrees2rad(d)))))
  h0<-atmospheric_refraction_correction(h,apparent=TRUE)
  return(list(azimuth=azimuth,altitude=h0))
}

#' Get error in azimuth given the horizontal coordinates of an alignment in reference to a theorical declination
#'
#' @param azimuth Apparent azimuth in degrees of an alignment
#' @param altitude Apparent elevation in degrees of an alignment
#' @param celestial_body celestial body as string. It can be empty
#' @param jd Julian day. -1000-1-1 as default
#' @param disk It can be "upper" for upper limb, "lower" for lower limb or "center"
#' @param declination_reference Theorical declination as reference
#' @param geographic_latitude Geographical latitude of the origin in degrees
#' @return Error in azimuth
#' @export
#' @examples
#' df<- data.frame(origin=c("Fuencalada","Fuencalada"), foresight=c("Los Cambitos","Teleno"),
#'                 azimuth=c(241.4563,246.9905),altitude=c(2.035853,2.192668),
#'                 geographic_latitude=c(42.44944,42.44944))
#' error=check_alignment(azimuth=df$azimuth,altitude=df$altitude,
#'                       celestial_body=c("moon","sun"),
#'                       disk=c("upper","upper"),
#'                       declination_reference=c(-18.81,-16.4),
#'                       geographic_latitude=df$geographic_latitude)


check_alignment<-function(azimuth, altitude, celestial_body, jd=julian_day(1,1,-1000), disk, declination_reference,geographic_latitude){
  parallax<-sapply(celestial_body,function(c) ifelse(c=="",0,rad2degrees(asin(sin(degrees2rad(8.794/3600))/distance_to_earth(jd,c)))))
  corrected_altitude<-altitude+rad2degrees(asin(sin(degrees2rad(parallax))*cos(degrees2rad(altitude))))
  d<-sapply(disk, function(d) switch(d,"upper"=-1,"lower"=1,"center"=0))
  p<-ifelse(celestial_body=="sun" | celestial_body=="moon",1,0)

  corrected_altitude<-corrected_altitude+960*d*p/3600
  A<-rad2degrees(acos(((sin(degrees2rad(declination_reference))-sin(degrees2rad(geographic_latitude))*sin(degrees2rad(corrected_altitude)))/(cos(degrees2rad(geographic_latitude))*cos(degrees2rad(corrected_altitude))))))
  error<-ifelse(azimuth>180,abs(360-A-azimuth),abs(A-azimuth))
  return(error)
}

#' Fortuitous probability for a number of coincidences for a total of alignments using Bernoulli process
#'
#' @param meaningful Number of meaningful directions. By default 18 are considered: equinoxes, cross-quarter days, solstices, lunar standstills (rising and setting)
#' @param coincidences Number of coincidences to within +- error of azimuth
#' @param total Number of horizon aligments
#' @param error Azimuth error
#' @param type "bernoulli" or "rosenfeldt"
#' @return Fortuitous probability
#' @export
#' @examples
#' probability_fortuitous_alignments.bernoulli(coincidences=3, total=10, error=1)
probability_fortuitous_alignments<-function(meaningful=18,coincidences, total, error=1, type="bernoulli"){
  result<-ifelse(type=="bernoulli",
                 probability_fortuitous_alignments.bernoulli(meaningful=meaningful,coincidences=coincidences, total=total, error=error),
                 ifelse(type=="rosenfeldt",
                        probability_fortuitous_alignments.rosenfeldt(meaningful=meaningful,coincidences=coincidences, total=total, error=error),
                        0))
  return(result)
}

#' Fortuitous probability for a number of coincidences for a total of alignments using Bernoulli process
#'
#' @param meaningful Number of meaningful directions. By default 18 are considered: equinoxes, cross-quarter days, solstices, lunar standstills (rising and setting)
#' @param coincidences Number of coincidences to within +- error of azimuth
#' @param total Number of horizon aligments
#' @param error Azimuth error
#' @return Fortuitous probability
#' @examples
#' probability_fortuitous_alignments.bernoulli(coincidences=3, total=10, error=1)
probability_fortuitous_alignments.bernoulli<-function(meaningful=18,coincidences, total, error=1){

  P <- meaningful
  N <- 360/(2*error) #error margin
  S <- total
  probability <- P/N

  result<-0

  for (a in coincidences:total)
  {
    M <- a

    num = factorial(total)*(probability^a)*(1-probability)^(total - a)
    den = factorial(M)*factorial(S-M)
    res = num/den
    result <- result+res
  }

  return (result)
}

#' Fortuitous probability for a number of coincidences for a total of alignments using Rosenfeldt's method
#'
#' @param meaningful Number of meaningful directions. By default 18 are considered: equinoxes, cross-quarter days, solstices, lunar standstills (rising and setting)
#' @param coincidences Number of coincidences to within +- error of azimuth
#' @param total Number of horizon aligments
#' @param error Azimuth error
#' @return Fortuitous probability
#' @examples
#' probability_fortuitous_alignments.rosenfeldt(coincidences=3, total=10, error=1)
probability_fortuitous_alignments.rosenfeldt<-function(meaningful=18,coincidences, total, error=1){

  P <- meaningful
  N <- 360/(2*error) #error margin
  S <- total

  result<-0

  for (a in coincidences:total)
  {
    M <- a
    num <- gamma(methods::as(P+1,"mpfr"))*gamma(methods::as(N-P+1,"mpfr"))*gamma(methods::as(S+1,"mpfr"))*gamma(methods::as(N-S+1,"mpfr"))
    den <- gamma(methods::as(M+1,"mpfr"))*gamma(methods::as(P-M+1,"mpfr"))*gamma(methods::as(S-M+1,"mpfr"))*gamma(methods::as(N+M-S-P+1,"mpfr"))*gamma(methods::as(N+1,"mpfr"))
    res <- num/den
    result <- result+res
  }

  return (as.numeric(result))
}
