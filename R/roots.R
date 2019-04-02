#' First derivative for the function and value given
#'
#' @param fnc Function
#' @param x Value in which the derivative should be calculated
#' @param incrx increment in x to calculate the derivative
#' @return derivative value at the value given
#' @export
#' @examples
#' fnc<-function(x) apparent_equatorial_position(x,"sun")$declination
#' x <- str2jd("2014-03-08")
#' derivative(fnc,x)
#'
derivative<-function(fnc, x, incrx=0.01){
  return((fnc(x+incrx)-fnc(x))/incrx)
}

#' Zero crossing of a periodical function
#'
#' @param fnc Function
#' @param from Initial value of the interval to be searched
#' @param to Final value of the interval to be searched
#' @param period Period of the function
#' @param search_interval Search interval for each root
#' @param incrx increment in x to calculate the derivative used to check the slope in the point of crossing
#' @param silent If true a text progress bar is shown
#' @param trace If true prints a trace
#' @return vector of roots of the function
#' @export
#' @examples
#' fnc<-function(x) apparent_equatorial_position(x,"sun")$declination
#' from <- str2jd("2014-03-08")
#' to <- str2jd("2018-03-08")
#' crossing_zero_up(fnc,from,to,period=365.25,search_interval=10)
#'
crossing_zero_up <- function(fnc, from, to, period, search_interval=0.005, incrx=0.01, silent=FALSE, trace=TRUE){
  if(isFALSE(silent))
  {
   pb<-utils::txtProgressBar(min = from, max = to, initial = 0, char = "=",
                 width = NA, style = 1, file = "")
  }
  r<-vector()
  periods<-vector()

  repeat{
   r1<-rootSolve::uniroot.all(fnc,c(from, from+period*2))
   if(length(r1)>0)
   {
    r2<-first(r1[derivative(fnc,r1,incrx)>0])
    if(is.na(r2))
    {
      from<-from+period
    }
    else{
     r<-c(r,r2)
     from<-r2
     if(isTRUE(trace))
     {
      cat(jd2str(r2),"\n")
     }
     break;
    }
   }
   else{
     if(isTRUE(trace))
     {
      cat("No",jd2str(from),"\n")
     }
     from<-from+period
   }

  }

  while(from<to)
  {
    fr0<-from+period-search_interval/2
    fr1<-from+period+search_interval/2
    r1<-tryCatch({stats::uniroot(fnc, c(fr0,fr1))},
      error=function(e) return(NULL),
      warning=function(w) return(NULL)
    )

    if(!is.null(r1) && r1$root<to)
    {
     r<-c(r,r1$root)
     if(isTRUE(trace))
     {
      cat(jd2str(r1$root),period,"\n")
     }
     p<-r1$root-from
     if(abs(p-period)/period <0.2)
     {
       periods<-c(p,periods)
       if(length(periods)>20)
       {
         periods<-periods[1:20]
       }
     }
     period<-stats::median(periods)
     from<-r1$root
    }
    else
    {
      break
    }
    if(isFALSE(silent)) utils::setTxtProgressBar(pb, from)
  }
  return(r)
}

#' Function for getting the difference between the azimuth coordinate according to some parameters and an reference azimuth
#'
#' @param x Julian day
#' @param parms Vector of strings containing the name of celestial body, geographic longitude, geographic latitude, geographic altitude  and reference azimuth
#' @return Difference in azimuth
#' @export
#' @examples
#' from <- str2jd("2014-03-08")
#' to <- str2jd("2015-03-08")
#' azimuth(seq(from,to,1),parms=c("sun","42°36'25\"N","5°33'58\"W","782", "20°0'0\""))
#'
azimuth<-function(x, parms){
  e<-apparent_equatorial_position(x,celestial_body = parms[1])
  h<-equatorial2horizontal(geographic_longitude=str2degrees(parms[3]),geographic_latitude=str2degrees(parms[2]),e$declination, e$right_ascension,x)
  return(reduce_degrees(h$azimuth,signed = TRUE)-str2degrees(parms[5]))
}

#' Function for getting the difference between the true elevation coordinate according to some parameters and an reference elevation
#'
#' @param x Julian day
#' @param parms Vector of strings containing the name of celestial body, geographic longitude, geographic latitude, geographic altitude and reference elevation
#' @return Difference in elevation
#' @export
#' @examples
#' from <- str2jd("2014-03-08")
#' to <- str2jd("2015-03-08")
#' true_elevation(seq(from,to,1),parms=c("sun","42°36'25\"N","5°33'58\"W","782","20°0'0\""))
#'
true_elevation<-function(x, parms){
  celestial_body = parms[1]
  geographic_latitude=str2degrees(parms[2])
  geographic_longitude=str2degrees(parms[3])
  geographic_altitude =as.numeric(parms[4])

  e<-apparent_equatorial_position(x,celestial_body)
  h<-equatorial2horizontal(geographic_longitude,geographic_latitude,e$declination, e$right_ascension,x)
  return(reduce_degrees(h$altitude,signed = TRUE)-str2degrees(parms[5]))
}

#' Function for getting the difference between the apparent elevation coordinate according to some parameters and an reference elevation
#'
#' @param x Julian day
#' @param parms Vector of strings containing the name of celestial body, geographic longitude, geographic latitude, geographic altitude and reference elevation
#' @return Difference in elevation
#' @export
#' @examples
#' from <- str2jd("2014-03-08")
#' to <- str2jd("2015-03-08")
#' apparent_elevation(seq(from,to,1),parms=c("saturn,","42°36'25\"N","5°33'58\"W","782"))
#'
apparent_elevation<-function(x, parms){
  celestial_body = parms[1]
  geographic_latitude=str2degrees(parms[2])
  geographic_longitude=str2degrees(parms[3])
  geographic_altitude =as.numeric(parms[4])

  e<-apparent_equatorial_position(x,celestial_body)
  ea<-geocentric2topocentric(e$declination, e$right_ascension, jd=x, geographic_longitude, geographic_latitude,geographic_altitude, celestial_body)
  h<-equatorial2horizontal(geographic_longitude,geographic_latitude,ea$declination, ea$right_ascension,x)
  return(reduce_degrees(atmospheric_refraction_correction(reduce_degrees(h$altitude,signed=TRUE),apparent=FALSE),signed = TRUE)-str2degrees(parms[5]))
}







