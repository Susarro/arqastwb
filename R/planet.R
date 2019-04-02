#' Orbital elements for the mean equinox of the date
#'
#' @param jd Julian day
#' @param planet planet name as string: mercury, venus,earth, mars, jupyter, saturn
#' @return
#' \itemize{
#'   \item mean_longitude Mean longitude of the planet in degrees
#'   \item semimajor_axis Semimajor axis of the orbit
#'   \item eccentricity Eccentricity of the orbit
#'   \item inclination Inclination on the plane of the ecliptic in degrees
#'   \item ascending_node_longitude Longitude of the ascending node in degrees
#'   \item perihelion_longitude Longitude of the perihelion in degrees
#' }
#' @examples
#' planetary_orbit_elements(julian_day(13.19,11,2028),"venus")
#'
planetary_orbit_elements <- function(jd,planet){
  T <- centuries_since_2000(jd)

  if(planet=="mercury")
  {
    mean_longitude = 252.250906 + 149474.0722491 * T + 0.00030397 * T^2 + 0.000000018 * T^3
    semimajor_axis = 0.387098310
    eccentricity = 0.20563175 + 0.000020406 * T - 0.0000000284 * T^2 - 0.00000000017 * T^3
    inclination = 7.004986 + 0.0018215 * T - 0.00001809 * T^2 + 0.000000053 * T^3
    ascending_node_longitude = 48.330893 + 1.1861890 * T + 0.00017587 * T^2 + 0.000000211 * T^3
    perihelion_longitude = 77.456119 + 1.5564775 * T + 0.00029589 * T^2 + 0.000000056 * T^3
    return (list(mean_longitude=reduce_degrees(mean_longitude),
                 semimajor_axis=semimajor_axis,
                 eccentricity=eccentricity,
                 inclination=reduce_degrees(inclination),
                 ascending_node_longitude=reduce_degrees(ascending_node_longitude),
                 perihelion_longitude=reduce_degrees(perihelion_longitude)
    ))
  }
  if(planet=="venus")
  {
    mean_longitude = 181.979801 + 58519.2130302 * T + 0.00031060 * T^2 + 0.000000015 * T^3
    semimajor_axis = 0.723329820
    eccentricity = 0.00677188 - 0.000047766 * T + 0.0000000975 * T^2 + 0.00000000044 * T^3
    inclination = 3.394662 + 0.0010037 * T - 0.00000088 * T^2 - 0.000000007 * T^3
    ascending_node_longitude = 76.679920 + 0.9011190 * T + 0.00040665 * T^2 - 0.000000080 * T^3
    perihelion_longitude = 131.563707 + 1.4022188 * T - 0.00107337 * T^2 - 0.000005315 * T^3
    return (list(mean_longitude=reduce_degrees(mean_longitude),
                 semimajor_axis=semimajor_axis,
                 eccentricity=eccentricity,
                 inclination=reduce_degrees(inclination),
                 ascending_node_longitude=reduce_degrees(ascending_node_longitude),
                 perihelion_longitude=reduce_degrees(perihelion_longitude)
    ))
  }
  if(planet=="earth")
  {
    mean_longitude <- 100.466449 + 36000.7698231 * T + 0.00030368 * T^2 + 0.000000021 * T^3
    semimajor_axis <- 1.000001018
    eccentricity <- 0.01670862 - 0.000042037 * T - 0.0000001236 * T^2 + 0.00000000004 * T^3
    inclination <- 0
    ascending_node_longitude <- NULL
    perihelion_longitude <- 102.937348 + 1.7195269 * T + 0.00045962 * T^2 + 0.000000499 * T^3
    return (list(mean_longitude=mean_longitude,
                 semimajor_axis=semimajor_axis,
                 eccentricity=eccentricity,
                 inclination=inclination,
                 ascending_node_longitude=ascending_node_longitude,
                 perihelion_longitude=perihelion_longitude
    ))
  }
  if(planet=="mars")
  {
    mean_longitude <- 355.433275 + 19141.6964746 * T + 0.00031097 * T^2 + 0.000000015 * T^3
    semimajor_axis <- 1.523679342
    eccentricity <- 0.09340062 + 0.000090483 * T - 0.0000000806 * T^2 - 0.00000000035 * T^3
    inclination <- 1.849726 - 0.0006010 * T + 0.00001276 * T^2 - 0.000000006 * T^3
    ascending_node_longitude <- 49.558093 + 0.7720923 * T + 0.00001605 * T^2 + 0.000002325 * T^3
    perihelion_longitude <- 336.060234 + 1.8410331 * T + 0.00013515 * T^2 + 0.000000318 * T^3
    return (list(mean_longitude=reduce_degrees(mean_longitude),
                 semimajor_axis=semimajor_axis,
                 eccentricity=eccentricity,
                 inclination=reduce_degrees(inclination),
                 ascending_node_longitude=reduce_degrees(ascending_node_longitude),
                 perihelion_longitude=reduce_degrees(perihelion_longitude)
    ))
  }
  if(planet=="jupyter")
  {
    mean_longitude <- 34.351484 + 3036.3027889 * T + 0.00022374 * T^2 + 0.000000025 * T^3
    semimajor_axis <- 5.202603191 + 0.0000001913 * T
    eccentricity <- 0.04849485 + 0.000163244 * T - 0.0000004719 * T^2 - 0.00000000197 * T^3
    inclination <- 1.3032270 - 0.0054966 * T + 0.00000465 * T^2 - 0.000000004 * T^3
    ascending_node_longitude <- 100.464441 + 1.0209550 * T + 0.00040117 * T^2 + 0.000000569 * T^3
    perihelion_longitude <- 14.331309 + 1.6126668 * T + 0.00103127 * T^2 - 0.000004569 * T^3
    return (list(mean_longitude=mean_longitude,
                 semimajor_axis=semimajor_axis,
                 eccentricity=eccentricity,
                 inclination=inclination,
                 ascending_node_longitude=ascending_node_longitude,
                 perihelion_longitude=perihelion_longitude
    ))
  }
  if(planet=="saturn")
  {
    mean_longitude <- 50.077471 + 1223.5110141 * T + 0.00051952 * T^2 - 0.000000003 * T^3
    semimajor_axis <- 9.554909596 - 0.0000021389 * T
    eccentricity <- 0.05550862 - 0.000346818 * T - 0.0000006456 * T^2 + 0.00000000338 * T^3
    inclination <- 2.488878 - 0.0037363 * T - 0.00001516 * T^2 + 0.000000089 * T^3
    ascending_node_longitude <- 113.665524 + 0.8770979 * T - 0.00012067 * T^2 - 0.000002380 * T^3
    perihelion_longitude <- 93.056787 + 1.9637694 * T + 0.00083757 * T^2 + 0.000004899 * T^3
    return (list(mean_longitude=reduce_degrees(mean_longitude),
                 semimajor_axis=semimajor_axis,
                 eccentricity=eccentricity,
                 inclination=reduce_degrees(inclination),
                 ascending_node_longitude=reduce_degrees(ascending_node_longitude),
                 perihelion_longitude=reduce_degrees(perihelion_longitude)
    ))
  }
}

#' Planetary aberration correction (effect of light-time and Earth's motion)
#'
#' @param jd Julian day
#' @param planet planet name as string: mercury, venus,earth, mars, jupyter, saturn
#' @return
#' \itemize{
#'   \item d Corrected distance to Earth
#'   \item x Corrected rectangular geocentric coordinate of the planet x
#'   \item y Corrected rectangular geocentric coordinate y
#'   \item z Corrected rectangular geocentric coordinate z
#'   \item L Corrected heliocentric longitude of the planet
#'   \item B Corrected heliocentric latitude of the planet
#'   \item R Corrected heliocentric radius of the planet
#'
#' }
#' @examples
#' aberration_corrected_coordinates_planet(julian_day(13.19,11,2028),"venus")
#'
aberration_corrected_coordinates_planet <- function(jd, planet){

  vd<-vector()
  vx<-vector()
  vy<-vector()
  vz<-vector()
  vL<-vector()
  vB<-vector()
  vR<-vector()

  for(j in jd)
  {
    error <- 1e-1

    L0 <- heliocentric_longitude(j,celestial_body="earth")
    B0 <- heliocentric_latitude(j,celestial_body="earth")
    R0 <- heliocentric_radius(j,celestial_body="earth")
    distance<-0
    L<-0
    B<-0
    R<-0

    for (i in 1:1000000)
    {
      L_ <- heliocentric_longitude(j, celestial_body=planet)
      B_ <- heliocentric_latitude(j, celestial_body=planet)
      R_ <- heliocentric_radius(j, celestial_body=planet)

      x <- R_ * cos(degrees2rad(B_)) * cos(degrees2rad(L_)) - R0 * cos(degrees2rad(B0)) * cos(degrees2rad(L0))
      y <- R_ * cos(degrees2rad(B_)) * sin(degrees2rad(L_)) - R0 * cos(degrees2rad(B0)) * sin(degrees2rad(L0))
      z <- R_ * sin(degrees2rad(B_)) - R0 * sin(degrees2rad(B0))

      distance <- sqrt(x^2 + y^2 + z^2)
      light_time <- 0.0057755183 * distance

      if (L != 0 && B != 0)
      {
        e = sqrt((L - L_)^2 + (B - B)^2 + (R - R_)^2)
        if (e <= error)
        {
          break
        }
      }
      L <- L_
      B <- B_
      R <- R_
      j = j - light_time
    }
    vd<-c(vd,distance)
    vx<-c(vx,x)
    vy<-c(vy,y)
    vz<-c(vz,z)
    vL<-c(vL,L)
    vB<-c(vB,B)
    vR<-c(vR,R)
  }
  return(list(distance=vd,x=vx,y=vy,z=vz,L=vL,B=vB,R=vR))
}

#' Apparent equatorial coordinates of a planet
#'
#' @param jd Julian day
#' @param planet planet name as string: mercury, venus,earth, mars, jupyter, saturn
#' @return Equatorial coordinates
#' \itemize{
#'   \item declination Declination in degrees
#'   \item right_ascension Right ascension in hour angle
#'   \item distance Distance to Earth
#'   \item illuminated_fraction Illuminated fraction of the disk of a planet
#'   \item apparent_magnitude apparent magnitude
#' }#'
#' @export
#' @examples
#' jd<-julian_day(13.19,11,2028)
#' apparent_equatorial_position_planet(jd,"venus")
#'
apparent_equatorial_position_planet <- function(jd,planet){

  d<-aberration_corrected_coordinates_planet(jd,planet)
  geocentric_longitude <- rad2degrees(atan2(d$y, d$x))
  geocentric_latitude <- rad2degrees(atan2(d$z, sqrt(d$x^2 + d$y^2)))

  T <- centuries_since_2000(jd)

  eo <- planetary_orbit_elements(jd,"earth")
  ex <- eo$eccentricity

  sun_longitude = heliocentric_longitude(jd,"earth") + 180

  K <- 20.49552
  incr_longitude <- (-K * cos(degrees2rad(sun_longitude - geocentric_longitude)) + ex * K * cos(degrees2rad(eo$perihelion_longitude - geocentric_longitude))) / cos(degrees2rad(geocentric_latitude)) #seconds
  incr_latitude <- -K * sin(degrees2rad(geocentric_latitude)) * sin(degrees2rad(sun_longitude - geocentric_longitude)) - ex * sin(degrees2rad(eo$perihelion_longitude - geocentric_longitude))

  L2 <- geocentric_longitude - 1.397 * T - 0.00031 * T^2
  incr_longitude2 <- -0.09033 + 0.03916 * (cos(degrees2rad(L2)) + sin(degrees2rad(L2))) * tan(degrees2rad(geocentric_latitude))#segundos
  incr_latitude2 <- 0.03916 * (cos(degrees2rad(L2)) - sin(degrees2rad(L2)))

  geocentric_longitude <- geocentric_longitude + incr_longitude / 3600
  geocentric_latitude <- geocentric_latitude + incr_latitude / 3600

  geocentric_longitude = geocentric_longitude + incr_longitude2 / 3600
  geocentric_latitude = geocentric_latitude + incr_latitude2 / 3600

  geocentric_longitude = geocentric_longitude + nutation_in_longitude(jd)

  equ<-ecliptic2equatorial(longitude=geocentric_longitude,latitude=geocentric_latitude,jd = jd)

  R0 <- heliocentric_radius(jd,"earth");
  L0 <- heliocentric_longitude(jd,"earth");
  phase_angle = rad2degrees(acos(min(1,(d$R-R0*cos(degrees2rad(d$B))*cos(degrees2rad(d$L-L0)))/d$distance)))
  k <- (1+cos(degrees2rad(phase_angle)))/2
  temp<-0
  if(planet=="saturn")
  {
    T <- centuries_since_2000(jd)
    i<- 28.075216 - 0.012998*T+0.000004*T^2
    omega<-169.508470+1.394681*T+0.000412*T^2
    landa<-rad2degrees(atan2(d$y,d$x))
    beta<- rad2degrees(atan2(d$z,(sqrt(d$x^2+d$y^2))))
    B<-rad2degrees(asin(sin(degrees2rad(i))*cos(degrees2rad(beta))*sin(degrees2rad(landa-omega))-cos(degrees2rad(i))*sin(degrees2rad(beta))))
    a<-375.35/(d$distance*3600)
    b<-a*sin(degrees2rad(abs(B)))
    N<-113.6655+0.8771*T
    l<-heliocentric_longitude(jd,"saturn")
    b<-heliocentric_latitude(jd,"saturn")
    r<-heliocentric_radius(jd,"saturn")
    l_<-l-0.01759/r
    b_<-b-0.000764*cos(degrees2rad(l-N))/r
    U1<-rad2degrees(atan2((sin(degrees2rad(i))*sin(degrees2rad(b_))+cos(degrees2rad(i))*cos(degrees2rad(b_))*sin(degrees2rad(l_-omega))),(cos(degrees2rad(b_))*cos(degrees2rad(l_-omega)))))
    U2<-rad2degrees(atan2((sin(degrees2rad(i))*sin(degrees2rad(beta))+cos(degrees2rad(i))*cos(degrees2rad(beta))*sin(degrees2rad(landa-omega))),(cos(degrees2rad(beta))*cos(degrees2rad(landa-omega)))))
    incrU<-abs(U1-U2)
    temp<-0.044*incrU-2.60*sin(degrees2rad(abs(B)))+1.25*sin(degrees2rad(B))^2
  }
  m <- switch(planet,
              "mercury"=1.16+5*log10(d$R*d$distance)+0.02838*(phase_angle-50)+0.0001023*(phase_angle-50)^2,
              "venus"=-4.0+5*log10(d$R*d$distance)+0.01322*phase_angle+0.0000004247*phase_angle^3,
              "mars"=-1.30+5*log10(d$R*d$distance)+0.01486*phase_angle,
              "jupyter"=-8.93+5*log10(d$R*d$distance),
              "saturn"=-8.68+5*log10(d$R*d$distance)+temp)


  return (list(declination=reduce_degrees(equ$declination,signed=TRUE),right_ascension=reduce_hours(equ$right_ascension),distance=d$distance, illuminated_fraction=k,apparent_magnitude=round(m,1)))
}

