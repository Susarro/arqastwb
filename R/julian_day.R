#' Julian day parsed from an IS8601 formatted string
#'
#' @param str IS8601 formatted string
#' @return Julian day
#' @export
#' @examples
#' str2jd(c("1957-10-04T19:26:024.000","333-01-27T12:00:000.000"))
#'
str2jd<- function(str){
  str<-gsub(" ", "T", str)
  tt<-strsplit(str,"T")
  sapply(tt,function(t){


    if(startsWith(t[1], "-"))
    {
      sign=-1
      f<-substring(t[1], 2, nchar(t[1]))
    }
    else
    {
      sign<-1;
      f <- t[1]
    }

    if(length(t)==2)
    {
      h = t[2]
    }
    else
    {
      h = "00:00:00"
    }
    ff = unlist(strsplit(f,"-"))
    if (length(ff) == 3)
    {
      day = as.integer(ff[3])
      month = as.integer(ff[2])
      year = sign*as.integer(ff[1])
    }
    else
    {
      stop(cat("JulianDay: Format error in ",str,"\n"))
    }

    hours=str2hours(h)
    return(julian_day(day, month, year, hours))
  })
}




#' Julian day formatted as IS8601 string
#'
#' @param jd Julian day
#' @param short Short string
#' @return Julian day formatted as IS8601 string
#' @export
#' @examples
#' julian_day(day=26, month=4, year=1977, hours=9.6)
#'
jd2str<- function(jd, short=FALSE){
  components<-dmyh(jd)
  if(isTRUE(short))
  {return(paste(sprintf("%d", components$year),"-",sprintf("%02d", components$month),"-",sprintf("%02d", components$day),sep=""))
  }
  else
  {
   return(paste(sprintf("%d", components$year),"-",sprintf("%02d", components$month),"-",sprintf("%02d", components$day),"T",hours2str(components$hour),sep=""))
  }
}

#' Julian day components: day, month, year and hour
#'
#' @param jd Julian day
#' @return Julian day components
#' \itemize{
#'   \item day
#'   \item month
#'   \item year
#'   \item hour from 0 to 24
#' }
#' @export
#' @examples
#' dmyh(2443259.9)
#'
dmyh<- function(jd){
  iZ <- as.integer(jd + 0.5)
  iF <- jd + 0.5 - iZ
  temp <- as.integer((iZ - 1867216.25) / 36524.25)
  iA <- ifelse(iZ < 2299161,iZ,iZ + 1 + temp - as.integer(temp / 4))

  iB <- iA + 1524
  iC <- as.integer((iB - 122.1) / 365.25)
  iD <- as.integer(365.25 * iC)
  iE <- as.integer((iB - iD) / 30.6001)
  day <- as.integer(iB - iD - as.integer(30.6001 * iE) + iF)

  month <- ifelse(iE < 14,iE - 1,iE - 13)
  year <- ifelse(month > 2,iC - 4716,iC - 4715)
  hour<-((jd + 0.5 - as.integer(jd + 0.5)) * 24.0)

  return (list(day=day,month=month,year=year,hour=hour))
}

#' Hour components: hours, minutes, seconds
#'
#' @param hour_value Decimal number of hours
#' @return Hour components
#' \itemize{
#'   \item hours
#'   \item minutes
#'   \item seconds
#' }
#' @export
#' @examples
#' hms(c(19.4,12))
#'
hms<- function(hour_value){
  hour_value<-reduce_hours(hour_value)
  hours = as.integer(hour_value+0.0005/3600)
  minutes = as.integer(((hour_value - hours) * 60+0.0005/3600))
  ss = (hour_value - hours - minutes / 60.0) * 3600
  seconds<-ifelse(ss>0,ss,0)
  return(list(hours=hours,minutes=minutes,seconds=seconds))
}

#' Reduce hour value to the interval [0,24)
#'
#' @param hour_value Decimal number of hours
#' @return Hour value reduced to the interval [0,24)
#' @export
#' @examples
#' reduce_hours(c(19.4+24*3,12-24*2))
#'
reduce_hours<- function(hour_value){
  ifelse(hour_value < 0,hour_value + ceiling(-hour_value / 24.0) * 24,ifelse(hour_value > 24,hour_value - floor(hour_value / 24.0) * 24,hour_value))
}

#' Hour value formatted as hh:mm:ss string
#'
#' @param hour_value Decimal number of hours
#' @return Hour value formatted as hh:mm:ss string
#' @export
#' @examples
#' reduce_hours(c(19.4,12))
#'
hours2str<- function(hour_value){
  hhmmss<-hms(hour_value)
  return(paste(sprintf("%02d", hhmmss$hours),sprintf("%02d", hhmmss$minutes),sprintf("%06.3f", abs(hhmmss$seconds)),sep = ":"))
}

#' Hour parsed from an hh:mm:ss formatted string
#'
#' @param str IS8601 formatted string
#' @return Hour angle
#' @export
#' @examples
#' str2hours(c("20:15:30","12:10:05"))
#'
str2hours<- function(str){
  sapply(strsplit(gsub("[^0-9:.]", "", str) , ":"),function(h){
    if(length(h)==1)
    {
      return (as.numeric(h[1]))
    }
    else if(length(h)==2)
    {
      return (as.numeric(h[1])+as.numeric(h[2])/60)
    }
    else if(length(h)==3)
    {
      return (as.numeric(h[1])+as.numeric(h[2])/60+as.numeric(h[3])/3600)
    }
    else
    {
      stop("Argument format incorrect. Should be hh:mm:ss.SSS\n")
    }
  })
}

#' Julian day from day, month, year and hour angle
#'
#' @param day Day
#' @param month Month
#' @param year Year
#' @param hours Hour angle
#' @return Julian day
#' @export
#' @examples
#' julian_day(day=c(4,27),month=c(10,1),year=c(1957,333),hours=c(19.44,12))
#'
julian_day<- function(day, month, year, hours=0){

  year <- ifelse(month == 1 | month == 2, year-1, year)
  month <- ifelse(month == 1 | month == 2, month + 12, month)

  a <- as.integer(year / 100.0)
  b <- ifelse(year > 1582 | (year == 1582 & month>10),2 - a + as.integer(a / 4),0)

  d <- as.integer(365.25 * (year + 4716)) + as.integer(30.6001 * (month + 1)) + day + b - 1524.5

  return(d + hours / 24.0)
}
