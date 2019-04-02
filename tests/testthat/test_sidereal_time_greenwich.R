context("Sidereal time at Greenwich")

test_that("Sidereal time at Greenwich", {
  jd<-julian_day(day=10,month=4,year=1987,hours=0)
  expect_equal(jd, 2446895.5)
  expect_equal(hours2str(mean_sidereal_time_at_Greenwich_at_0_UT(jd)),"13:10:46.367")
  expect_equal(hours2str(mean_sidereal_time_at_Greenwich(jd)),"13:10:46.367")
  expect_equal(hours2str(apparent_sidereal_time_at_Greenwich(jd)),"13:10:46.135")
  jd2<-str2jd("1987-04-10T19:21")
  expect_equal(hours2str(mean_sidereal_time_at_Greenwich_at_0_UT(jd2)),"13:10:46.367")
  expect_equal(hours2str(mean_sidereal_time_at_Greenwich(jd2)),"08:34:57.090")
})
