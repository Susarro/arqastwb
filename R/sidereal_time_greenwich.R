
#' Number of centuries since 2000-01-01
#'
#' @param jd Julian day

#' @return Number of centuries since 2000-01-01
#' @export
#' @examples
#' centuries_since_2000(c(2436116.31, 1842713.00))
#'
centuries_since_2000 <- function(jd) {
  return ((jd - 2451545) / 36525)
}

#' Mean sidereal time at Greenwich at 00:00 UT
#'
#' @param jd Julian day

#' @return Mean sidereal time at Greenwich at 00:00 UT as hour angle
#' @export
#' @examples
#' mean_sidereal_time_at_Greenwich_at_0_UT(c(2436116.31, 1842713.00))
#'
mean_sidereal_time_at_Greenwich_at_0_UT <- function(jd) {
  hour<-((jd + 0.5 - as.integer(jd + 0.5)) * 24.0)
  T <- centuries_since_2000(jd - hour / 24)
  mst = str2hours("6:41:50.54841")
  ss <- 8640184.812866 * T + 0.093104 * T^2 - 0.0000062 * T^3
  return (mst + ss / 3600)
}

#' Mean sidereal time at Greenwich
#'
#' @param jd Julian day

#' @return Mean sidereal time at Greenwich as hour angle
#' @export
#' @examples
#' mean_sidereal_time_at_Greenwich(c(2436116.31, 1842713.00))
#'
mean_sidereal_time_at_Greenwich <- function(jd) {
  hour<-((jd + 0.5 - as.integer(jd + 0.5)) * 24.0)
  return (mean_sidereal_time_at_Greenwich_at_0_UT(jd) + hour*1.00273790935)
}

#' Apparent sidereal time at Greenwich
#'
#' @param jd Julian day

#' @return Julian day
#' @export Mean sidereal time at Greenwich as hour angle
#' @examples
#' mean_sidereal_time_at_Greenwich(c(2436116.31, 1842713.00))
#'
apparent_sidereal_time_at_Greenwich <- function(jd) {
  obliquity<-true_obliquity_ecliptic(jd)
  hour<-((jd + 0.5 - as.integer(jd + 0.5)) * 24.0)
  hs <- hour * 1.00273790935 + mean_sidereal_time_at_Greenwich_at_0_UT(jd)
  corr <- nutation_in_longitude(jd)
  corr2<-ifelse(corr > 180,-(360 - corr),corr)
  return (hs + corr2 * cos(obliquity*pi/180) / 15)
}
