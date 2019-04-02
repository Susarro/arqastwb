#' Plot a function usin Julian Day as x coordinate
#'
#' @param fnc Function to plot
#' @param from Initial julian day
#' @param to Final julian day
#' @param interval sample interval in time
#' @param tick_interval Tick interval
#' @return Plot
#' @export
#' @examples
#  @importFrom rlang .data
#' fnc<-function(x) apparent_equatorial_position(x,"sun")$declination
#' plt_fnc_jd(fnc, str2jd("1972-03-01"),str2jd("1973-03-01"),10,60)
#'
plt_fnc_jd<-function(fnc, from, to, interval, tick_interval){
  return(data.frame(time=seq(from, to,interval)) %>%
    mutate(f=fnc(.data$time)) %>%
    ggplot2::ggplot(ggplot2::aes(x=.data$time,y=.data$f)) +
      ggplot2::geom_line(colour = "blue")+
      ggplot2::scale_x_continuous(breaks=seq(from,to,by=tick_interval),labels=jd2str(seq(from,to,by=tick_interval),short=TRUE)))
}
