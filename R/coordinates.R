#' Hour angle to radian transformation
#'
#' @param hour_angle Hour angle
#' @return Radians
#' @export
#' @examples
#' hours2rad(12)
#'
hours2rad <- function(hour_angle){
  return(hour_angle*15*pi/180)
}

#' Radian to hour angle
#'
#' @param radian Radian
#' @return Hour angle
#' @export
#' @examples
#' rad2hours(3.141593)
#'
rad2hours <- function(radian){
  return(radian*180/(pi*15))
}

#' Radian to degrees
#'
#' @param radian Radian
#' @return Degrees
#' @export
#' @examples
#' rad2degrees(3.141593)
#'
rad2degrees <- function(radian){
  return(radian*180/pi)
}

#' Degrees to radian transformation
#'
#' @param degrees Degrees
#' @return Radians
#' @export
#' @examples
#' degrees2rad(180)
#'
degrees2rad <- function(degrees){
  return(degrees*pi/180)
}

#' Degrees to hour angle transformation
#'
#' @param degrees Degrees
#' @return Hour angle
#' @export
#' @examples
#' degrees2hours(180)
#'
degrees2hours <- function(degrees){
  return(degrees/15)
}

#' Hour angle to degrees transformation
#'
#' @param hour_angle Hour angle
#' @return Degrees
#' @export
#' @examples
#' hours2degrees(12)
#'
hours2degrees <- function(hour_angle){
  return(hour_angle*15)
}

#' Sexagesimal degre components
#'
#' It retrieves the sexagesimal degree components, i.e. degrees, minutes and seconds
#'
#' @param degrees Degrees
#' @return sexagesimal degree components
#' \itemize{
#'   \item degrees
#'   \item minutes
#'   \item seconds
#' }
#' @export
#' @examples
#' dms(c(15.5,30.14))
#'
dms <- function(degrees) {
  sign<-ifelse(degrees>0,1,-1)
  value<-abs(degrees)
  degrees = as.integer(value)
  minutes = as.integer(((value - degrees) * 60.0))
  seconds = (value - degrees - minutes / 60.0) * 3600

  return(list(sign=sign,degrees=degrees,minutes=minutes,seconds=seconds))
}

#' Sexagesimal degree formatting to string
#'
#' It converts angle into string
#'
#' @param degrees Degrees
#' @return string d°m's" formatted
#' @export
#' @examples
#' degrees2str(c(15.5,30.14))
#'
degrees2str<- function(degrees) {
  ddmmss<-dms(degrees)
  return(paste(ifelse(ddmmss$sign>0,"","-"),sprintf("%02d", ddmmss$degrees),"\u00b0",sprintf("%02d", ddmmss$minutes),"'",sprintf("%06.3f", ddmmss$seconds),"\u0022",sep = ""))
}

#' Sexagesimal degree parsing from string
#'
#' It parses a string into sexagesimal degrees
#'
#' @param str string dºm's" formatted
#' @param separators Separator characters for degrees, minutes and seconds
#' @return Sexagesimal degree
#' @export
#' @examples
#' str2degrees(c("12\u00b030'30.25\u0022","0\u00b030'30\u0022"))
#'
str2degrees<- function(str, separators = c("\u00b0", "'", "\u0022")) {
  dms <- as.character(str)
  dms <- gsub(pattern = " ", replacement = "", x = dms)
  for (s in separators) dms <- gsub(pattern = s, replacement = "#", x = dms)

  splits <- strsplit(dms, split = "#")
  n <- length(dms)
  deg <- min <- sec <- hem <- vector("character", n)

  for (i in 1:n) {
    deg[i] <- splits[[i]][1]
    min[i] <- splits[[i]][2]
    sec[i] <- splits[[i]][3]
    hem[i] <- splits[[i]][4]
  }

  dec <- abs(as.numeric(deg)) + (as.numeric(min) / 60) + (as.numeric(sec) / 3600)
  sign <- ifelse (as.numeric(deg)< 0 | hem %in% c("S", "O", "W") ,-1, 1)
  dec <- sign * dec
  return(dec)
}

#' Reduce degrees to the interval [0,360), or (-180,180] if signed
#'
#' @param degrees Decimal degrees
#' @param signed Signed degrees in the interval (-180,180]
#' @return degrees value reduced to the interval [0,360), or (-180,180] if signed
#' @export
#' @examples
#' reduce_degrees(c(19.4+24*3,12-24*2))
#'
reduce_degrees<- function(degrees, signed=FALSE){
  degrees<-ifelse(degrees < 0,degrees + ceiling(-degrees / 360.0) * 360,ifelse(degrees >= 360,degrees - floor(degrees / 360.0) * 360,degrees))

  if(isTRUE(signed))
  {
    return(ifelse(degrees>=180,degrees-360,degrees))
  }
  else
  {
    return (degrees)
  }

  return(degrees)
}


#' Equatorial coordinates to Ecliptic coordinates transformation
#'
#' @param right_ascension Right ascension in hour angle
#' @param declination Declination in degrees
#' @param jd Julian day used to determine true obliquity of ecliptic
#' @param obliquity true obliquity of ecliptic. If null it uses jd to calculate it
#' @return Ecliptic coordinates
#' \itemize{
#'   \item latitude
#'   \item longitude longitude in degrees
#' }
#' @export
#' @examples
#' obl <- 23.4392911
#' decl<-str2degrees("28º01'34.26\"")
#' ra<-str2hours("07:45:18.946")
#' equatorial2ecliptic(declination = decl,right_ascension=ra, obliquity = obl)
#'
equatorial2ecliptic <- function(declination,right_ascension,jd=NULL,obliquity=NULL) {
  if(is.null(obliquity))
  {
    if(is.null(jd))
    {
      stop("Both jd and obliquity can't be null")
    }
    obliquity <- true_obliquity_ecliptic(jd)
  }

  longitude <- reduce_degrees(rad2degrees(atan2(sin(hours2rad(right_ascension)) * cos(degrees2rad(obliquity)) + tan(degrees2rad(declination)) * sin(degrees2rad(obliquity)), cos(hours2rad(right_ascension)))))
  latitude <-  reduce_degrees(rad2degrees(asin(sin(degrees2rad(declination)) * cos(degrees2rad(obliquity)) - cos(degrees2rad(declination)) * sin(degrees2rad(obliquity)) * sin(hours2rad(right_ascension)))))
  return(list(longitude = longitude,latitude=latitude))
}

#' Ecliptic coordinates to  Equatorial coordinates transformation
#'
#' @param latitude Latitude in degrees
#' @param longitude Longitude in degrees
#' @param jd Julian day used to determine true obliquity of ecliptic
#' @param obliquity true obliquity of ecliptic. If null it uses jd to calculate it
#' @return Equatorial coordinates
#' \itemize{
#'   \item declination Declination in degrees
#'   \item right_ascension Right ascension in hour angle
#' }
#' @export
#' @examples
#' ecliptic2equatorial(longitude = 113.2156292276,latitude=6.684170072045,obliquity=23.4392911)
#'
ecliptic2equatorial <- function(longitude,latitude,jd=NULL,obliquity=NULL) {
  if(is.null(obliquity))
  {
    if(is.null(jd))
    {
      stop("Both jd and obliquity can't be null")
    }
    obliquity <- true_obliquity_ecliptic(jd)
  }

  right_ascension <- reduce_hours(rad2hours(atan2(sin(degrees2rad(longitude)) * cos(degrees2rad(obliquity)) - tan(degrees2rad(latitude)) * sin(degrees2rad(obliquity)), cos(degrees2rad(longitude)))))
  declination <- reduce_degrees(rad2degrees(asin(sin(degrees2rad(latitude)) * cos(degrees2rad(obliquity)) + cos(degrees2rad(latitude)) * sin(degrees2rad(obliquity)) * sin(degrees2rad(longitude)))))
  return(list(declination = declination,right_ascension=right_ascension))
}

#' Equatorial coordinates to  Horizontal coordinates transformation
#'
#' @param geographic_longitude Geographical longitude in degrees
#' @param geographic_latitude Geographical latitude in degrees
#' @param declination Declination in degrees
#' @param right_ascension Right Ascension as hour angle
#' @param jd Julian day used to determine true obliquity of ecliptic
#' @param obliquity true obliquity of ecliptic. If null it uses jd to calculate it
#' @return Horizontal coordinates
#' \itemize{
#'   \item azimuth Azimuth in degrees
#'   \item altitude Altitude in hour angle
#' }
#' @export
#' @examples
#' geographic_longitude<-str2degrees("-77º03'55.5\u0022")
#' geographic_latitude<-str2degrees("38º55'17\u0022")
#' jd<-str2jd("1987-04-10T19:21:00")
#' declination<-str2degrees("-6º43'11.61\u0022")
#' right_ascension<-str2hours("23:09:16.641")
#' equatorial2horizontal(geographic_longitude,geographic_latitude,declination, right_ascension,jd)
equatorial2horizontal<- function(geographic_longitude,geographic_latitude,declination, right_ascension,jd=NULL,obliquity=NULL)
{
  if(is.null(obliquity))
  {
    if(is.null(jd))
    {
      stop("Both jd and obliquity can't be null")
    }
    obliquity <- true_obliquity_ecliptic(jd)
  }
  astG <- apparent_sidereal_time_at_Greenwich(jd)
  local_hour_angle<-astG+degrees2hours(geographic_longitude)-right_ascension
  azimuth <- reduce_degrees(rad2degrees(atan2(sin(hours2rad(local_hour_angle)), cos(hours2rad(local_hour_angle)) * sin(degrees2rad(geographic_latitude)) - tan(degrees2rad(declination)) * cos(degrees2rad(geographic_latitude)))))+180
  altitude <- reduce_degrees(rad2degrees(asin(sin(degrees2rad(geographic_latitude)) * sin(degrees2rad(declination)) + cos(degrees2rad(geographic_latitude)) * cos(degrees2rad(declination)) * cos(hours2rad(local_hour_angle)))))
  return(list(azimuth = azimuth,altitude=altitude))
}

#' Equatorial coordinates to  Horizontal coordinates transformation
#'
#' @param geographic_longitude Geographical longitude in degrees
#' @param geographic_latitude Geographical latitude in degrees
#' @param azimuth Azimuth in degrees
#' @param altitude Altitude in degrees
#' @param jd Julian day used to determine true obliquity of ecliptic
#' @param obliquity true obliquity of ecliptic. If null it uses jd to calculate it
#' @return Horizontal coordinates
#' \itemize{
#'   \item azimuth Azimuth in degrees
#'   \item altitude Altitude in hour angle
#' }
#' @export
#' @examples
#' geographic_longitude<-str2degrees("-77º03'55.5\u0022")
#' geographic_latitude<-str2degrees("38º55'17\u0022")
#' jd<-str2jd("1987-04-10T19:21:00")
#' azimuth<-248.0336941301
#' altitude<-15.1248736234
#' horizontal2equatorial(geographic_longitude,geographic_latitude,azimuth, altitude,jd)
#'
horizontal2equatorial<- function(geographic_longitude,geographic_latitude,azimuth, altitude,jd=NULL,obliquity=NULL)
{
  if(is.null(obliquity))
  {
    if(is.null(jd))
    {
      stop("Both jd and obliquity can't be null")
    }
    obliquity <- true_obliquity_ecliptic(jd)
  }
  astG <- apparent_sidereal_time_at_Greenwich(jd)

  local_hour_angle <- reduce_hours(rad2hours(atan2(sin(degrees2rad(azimuth-180)), cos(degrees2rad(azimuth-180)) * sin(degrees2rad(geographic_latitude)) + tan(degrees2rad(altitude)) * cos(degrees2rad(geographic_latitude)))))

  right_ascension <- reduce_hours(astG + degrees2hours(geographic_longitude) - local_hour_angle)
  declination <- reduce_degrees(rad2degrees(asin(sin(degrees2rad(geographic_latitude)) * sin(degrees2rad(altitude)) + cos(degrees2rad(geographic_latitude)) * cos(degrees2rad(altitude)) * cos(degrees2rad(azimuth)))),signed=T)
  return(list(declination = declination,right_ascension=right_ascension))
}

#' Altitude correction due to atmospheric refraction
#'
#' @param altitude Altitude indegrees
#' @param apparent If True apparent altitude to true altitude. If false, true altitude to apparent altitude
#' @return Corrected altitude
#' @export
#' @examples
#' degrees2str(atmospheric_refraction_correction(0.5))
#' degrees2str(atmospheric_refraction_correction(str2degrees("00º01'14.777\u0022"),apparent=FALSE))
#'
atmospheric_refraction_correction<- function(altitude,apparent=TRUE){
  if(isTRUE(apparent))
  {
    v <- altitude + 7.31 / (altitude + 4.4)
    R <- (1 / tan(degrees2rad(v))) + 0.00135152167375625415468205231256
    real_altitude<-altitude - R / 60
    return (ifelse(altitude>=0,real_altitude,altitude))
  }
  else
  {
    v = reduce_degrees(altitude,signed=T) + 10.3 / (reduce_degrees(altitude,signed=T) + 5.11)
    R = (1.02 / tan(degrees2rad(v))) + 0.00196535584287727557399420258844
    apparent_elevation<-altitude + R / 60
    return (ifelse(apparent_elevation>=0,apparent_elevation,altitude))
  }
}

#' List formatting to string according to the names of its elements
#'
#' @param coord list of coordinates
#' @return list formatted as string
#' @export
#' @examples
#' coord<-list(declination=c(89.454275227064002,89.5),right_ascension=c(3.8046089729542945,3.9))
#' str(coord)
#
str<-function(coord)
{
  output<-list()

  for(i in 1:length(names(coord)))
  {
    output[[i]]<-switch(names(coord)[i],"declination"=degrees2str(coord[[i]]),"right_ascension"=hours2str(coord[[i]]),"latitude"=degrees2str(coord[[i]]),"longitude"=degrees2str(coord[[i]]),"azimuth"=degrees2str(coord[[i]]),"altitude"=degrees2str(coord[[i]]))
  }
  names(output)<-names(coord)
  return(output)
}

#' Geocentric to topocentric equatorial coordinates transformation
#'
#' @param geographic_longitude Geographical longitude in degrees
#' @param geographic_latitude Geographical latitude in degrees
#' @param geographic_altitude Geographical altitude in meters
#' @param declination Geocentric declination in degrees
#' @param right_ascension Geocentric right Ascension in hour angle
#' @param jd Julian day used to determine true obliquity of ecliptic
#' @param celestial_body Celestial body name as string: mercury, venus,mars, jupyter, saturn, sun or moon
#' @return Horizontal coordinates
#' \itemize{
#'   \item declination Topocentric declination in degrees
#'   \item right_ascension Topocentric right ascension in hour angle
#' }
#' @export
#' @examples
#' jd<-str2jd("2003-08-28T03:17:00")
#' geocentric2topocentric(-15.771083,-22.63534722222,jd,-116.8625,-33.356111,1706,"mars")
#'
geocentric2topocentric<- function(declination, right_ascension, jd, geographic_longitude, geographic_latitude,geographic_altitude, celestial_body)
{
  a<-6378.14
  f<-1/298.257
  e<-0.08181922
  b<-a*(1-f)
  H<-geographic_altitude
  u<-rad2degrees(atan(b*tan(degrees2rad(geographic_latitude))/a))
  rosingeolat<-b*sin(degrees2rad(u))/a+H*sin(degrees2rad(geographic_latitude))/6378140
  rocosgeolat<-cos(degrees2rad(u))+H*cos(degrees2rad(geographic_latitude))/6378140
  geocentric_latitude<-rad2degrees(atan(rosingeolat/rocosgeolat))
  geocentric_radius<-rosingeolat/sin(degrees2rad(geocentric_latitude))
  d<-distance_to_earth(jd,celestial_body)
  parallax<-rad2degrees(asin(sin(degrees2rad(8.794/3600))/d))
  astG <- apparent_sidereal_time_at_Greenwich(jd)
  local_hour_angle<-astG+degrees2hours(geographic_longitude)-right_ascension
  incrra<-rad2hours(atan2(-(rocosgeolat*sin(degrees2rad(parallax))*sin(hours2rad(local_hour_angle))),(cos(degrees2rad(declination))-rocosgeolat*sin(degrees2rad(parallax))*cos(hours2rad(local_hour_angle)))))
  geocentric_declination<-rad2degrees(atan2((sin(degrees2rad(declination))-rosingeolat*sin(degrees2rad(parallax)))*cos(hours2rad(incrra)),(cos(degrees2rad(declination))-rocosgeolat*sin(degrees2rad(parallax))*cos(hours2rad(local_hour_angle)))))
  geocentric_right_ascension<-right_ascension+incrra
  return(list(declination = geocentric_declination,right_ascension=geocentric_right_ascension))
}
