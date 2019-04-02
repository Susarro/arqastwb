#' Nutation in longitude
#'
#' @param jd Julian day

#' @return Nutation in longitude
#' @export
#' @examples
#' nutation_in_longitude(c(2436116.31, 1842713.00))
#'
nutation_in_longitude <- function(jd) {
  T <- centuries_since_2000(jd)
  #Longitude of the ascending node of the Moon's mean orbit on the ecliptic, measured from the mean equinox of the date
  omega <- 125.04452 - 1934.136261 * T + 0.0020708 * T^2 + T^3 / 45000
  #Mean longitude of the Sun
  LS <- 280.4665 + 36000.7698 * T
  #Mean longitude of the Moon
  LM <- 218.3165 + 481267.8813 * T
  #Mean elongation of the Moon from the Sun
  D <- 297.85036 + 445267.111480 * T - 0.001914 * T^2 + T^3 / 189474
  #Mean anomaly of the Sun (Earth)
  MS <- 357.52772 + 35999.050340 * T - 0.0001603 * T^2 - T^3 / 300000
  #Mean anomaly of the Moon
  MM <- 134.96298 + 477198.867398 * T + 0.0086972 * T^2 + T^3 / 56250
  #Moon's argument of latitude
  F <- 93.27191 + 483202.017538 * T - 0.0036825 * T^2 + T^3 / 327270

  nl <- colSums(sin(rbind(
    c(0, 0, 0, 0, 1),
    c(-2, 0, 0, 2, 2),
    c(0, 0, 0, 2, 2),
    c(0, 0, 0, 0, 2),
    c(0, 1, 0, 0, 0),
    c(0, 0, 1, 0, 0),
    c(-2, 1, 0, 2, 2),
    c(0, 0, 0, 2, 1),
    c(0, 0, 1, 2, 2),
    c(-2, -1, 0, 2, 2),
    c(-2, 0, 1, 0, 0),
    c(-2, 0, 0, 2, 1),
    c(0, 0, -1, 2, 2),
    c(2, 0, 0, 0, 0),
    c(0, 0, 1, 0, 1),
    c(2, 0, -1, 2, 2),
    c(0, 0, -1, 0, 1),
    c(0, 0, 1, 2, 1),
    c(-2, 0, 2, 0, 0),
    c(0, 0, -2, 2, 1),
    c(2, 0, 0, 2, 2),
    c(0, 0, 2, 2, 2),
    c(0, 0, 2, 0, 0),
    c(-2, 0, 1, 2, 2),
    c(0, 0, 0, 2, 0),
    c(-2, 0, 0, 2, 0),
    c(0, 0, -1, 2, 1),
    c(0, 2, 0, 0, 0),
    c(2, 0, -1, 0, 1),
    c(-2, 2, 0, 2, 2),
    c(0, 1, 0, 0, 1),
    c(-2, 0, 1, 0, 1),
    c(0, -1, 0, 0, 1),
    c(0, 0, 2, -2, 0),
    c(2, 0, -1, 2, 1),
    c(2, 0, 1, 2, 2),
    c(0, 1, 0, 2, 2),
    c(-2, 1, 1, 0, 0),
    c(0, -1, 0, 2, 2),
    c(2, 0, 0, 2, 1),
    c(2, 0, 1, 0, 0),
    c(-2, 0, 2, 2, 2),
    c(-2, 0, 1, 2, 1),
    c(2, 0, -2, 0, 1),
    c(2, 0, 0, 0, 1),
    c(0, -1, 1, 0, 0),
    c(-2, -1, 0, 2, 1),
    c(-2, 0, 0, 0, 1),
    c(0, 0, 2, 2, 1),
    c(-2, 0, 2, 0, 1),
    c(-2, 1, 0, 2, 1),
    c(0, 0, 1, -2, 0),
    c(-1, 0, 1, 0, 0),
    c(-2, 1, 0, 0, 0),
    c(1, 0, 0, 0, 0),
    c(0, 0, 1, 2, 0),
    c(0, 0, -2, 2, 2),
    c(-1, -1, 1, 0, 0),
    c(0, 1, 1, 0, 0),
    c(0, -1, 1, 2, 2),
    c(2, -1, -1, 2, 2),
    c(0, 0, 3, 2, 2),
    c(2, -1, 0, 2, 2)) %*% rbind(D, MS, MM, F, omega)*pi/180) *
      rbind(-171996 - 174.2 * T,-13187 - 1.6 * T,
            -2274 - 0.2 * T,2062 + 0.2 * T,1426 - 3.4 * T,712 + 0.1 * T,-517 + 1.2 * T,-386 - 0.4 * T,-301,217 - 0.5 * T,
            -158,129 + 0.1 * T,123,63,63 + 0.1 * T,-59,-58 - 0.1 * T,-51,48,46,-38,-31,29,29,26,-22,21,17 - 0.1 * T,16,-16 + 0.1 * T,
            -15,-13,-12,11,-10,-8,7,-7,-7,-7,6,6,6,-6,-6,5,-5,-5,-5,4,4,4,-4,-4,-4,3,-3,-3,-3,-3,-3,-3,-3)) / 10000

  return (nl / 3600)
}

#' Nutation in obliquity
#'
#' @param jd Julian day

#' @return Nutation in obliquity
#' @export
#' @examples
#' nutation_in_obliquity(c(2436116.31, 1842713.00))
#'
nutation_in_obliquity <- function(jd) {
  T = centuries_since_2000(jd)
  #Longitude of the ascending node of the Moon's mean orbit on the ecliptic, measured from the mean equinox of the date
  omega <- 125.04452 - 1934.136261 * T + 0.0020708 * T^2 + T^3 / 450000
  #Mean longitude of the Sun
  LS <- 280.4665 + 36000.7698 * T
  #Mean longitude of the Moon
  LM <- 218.3165 + 481267.8813 * T
  #Mean elongation of the Moon from the Sun
  D <- 297.85036 + 445267.111480 * T - 0.001914 * T^2 + T^3 / 189474
  #Mean anomaly of the Sun (Earth)
  MS <- 357.52772 + 35999.050340 * T - 0.0001603 * T^2 - T^3 / 300000
  #Mean anomaly of the Moon
  MM <- 134.96298 + 477198.867398 * T + 0.0086972 * T^2 + T^3 / 56250
  #Moon's argument of latitude
  F <- 93.27191 + 483202.017538 * T - 0.0036825 * T^2 + T^3 / 327270

  nl <- colSums(cos(rbind(c(0, 0, 0, 0, 1),
                          c(-2, 0, 0, 2, 2),
                          c(0, 0, 0, 2, 2),
                          c(0, 0, 0, 0, 2),
                          c(0, 1, 0, 0, 0),
                          c(0, 0, 1, 0, 0),
                          c(-2, 1, 0, 2, 2),
                          c(0, 0, 0, 2, 1),
                          c(0, 0, 1, 2, 2),
                          c(-2, -1, 0, 2, 2),
                          c(-2, 0, 0, 2, 1),
                          c(0, 0, -1, 2, 2),
                          c(0, 0, 1, 0, 1),
                          c(2, 0, -1, 2, 2),
                          c(0, 0, -1, 0, 1),
                          c(0, 0, 1, 2, 1),
                          c(0, 0, -2, 2, 1),
                          c(2, 0, 0, 2, 2),
                          c(0, 0, 2, 2, 2),
                          c(-2, 0, 1, 2, 2),
                          c(0, 0, -1, 2, 1),
                          c(2, 0, -1, 0, 1),
                          c(-2, 2, 0, 2, 2),
                          c(0, 1, 0, 0, 1),
                          c(-2, 0, 1, 0, 1),
                          c(0, -1, 0, 0, 1),
                          c(2, 0, -1, 2, 1),
                          c(2, 0, 1, 2, 2),
                          c(0, 1, 0, 2, 2),
                          c(0, -1, 0, 2, 2),
                          c(2, 0, 0, 2, 1),
                          c(-2, 0, 2, 2, 2),
                          c(-2, 0, 1, 2, 1),
                          c(2, 0, -2, 0, 1),
                          c(2, 0, 0, 0, 1),
                          c(-2, -1, 0, 2, 1),
                          c(-2, 0, 0, 0, 1),
                          c(0, 0, 2, 2, 1)) %*% rbind(D, MS, MM, F, omega)*pi/180) *
                  rbind(92025 + 8.9 * T,5736 - 3.1 * T,977 - 0.5 * T,-895 + 0.5 * T,54 - 0.1 * T,-7,224 - 0.6 * T,200,129 - 0.1 * T,
                        -95 + 0.3 * T,-70,-53,-33,26,32,27,-24,16,13,-12,-10,-8,7,9,7,6,5,3,-3,3,3,-3,-3,3,3,3,3,3)) / 10000
  return (nl / 3600)
}

#' Mean obliquity of the ecliptic
#'
#' @param jd Julian day
#' @param precise Precise calculation
#' @return Mean obliquity of the ecliptic
#' @export
#' @examples
#' mean_obliquity_ecliptic(c(2436116.31, 1842713.00))
#'
mean_obliquity_ecliptic <- function(jd, precise=F) {
  T <- centuries_since_2000(jd)
  U <- T / 100
  a <-ifelse(precise,
             -4680.93 * U - 1.55 * U^2 + 1999.25 * U^3 - 51.38 * U^4 - 249.67 * U^5 - 39.05 * U^6 + 7.12 * U^7 + 27.87 * U^8 + 5.79 * U^9 + 2.45 * U^10,
             -46.8150 * T - 0.00059 * T^2 + 0.001813 * T^3)

  return (str2degrees("23\u00b026'21.448\u0022")+a / 3600)
}

#' True obliquity of the ecliptic
#'
#' @param jd Julian day
#' @param precise Precise calculation
#' @return True obliquity of the ecliptic
#' @export
#' @examples
#' mean_obliquity_ecliptic(c(2436116.31, 1842713.00))
#'
true_obliquity_ecliptic <- function(jd, precise=F) {
  oe <- mean_obliquity_ecliptic(jd,precise)
  noe <- nutation_in_obliquity(jd)
  return (oe+noe)
}
