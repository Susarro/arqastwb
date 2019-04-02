context("Julian day testing")

test_that("Julian day manipulation", {
  expect_equal(julian_day(day=c(4,27),month=c(10,1),year=c(1957,333),hours=c(19.44,12)), c(2436116.31, 1842713.00))
  expect_equal(reduce_hours(c(19.4+24*3,12-24*2)), c(19.4,12.0))
  expect_equal(hours2str(c(19.4,12)),c("19:24:00.000","12:00:00.000"))
  expect_equal(c(19.4,12.0),str2hours(c("19:24:00.000","12:00:00.000")))
  expect_equal(jd2str(c(2436116.31, 1842713.00)),c("1957-10-04T19:26:24.000","333-01-27T12:00:00.000"))
  expect_equal(c(2436116.31, 1842713.00),str2jd(c("1957-10-04T19:26:24.000","333-01-27T12:00:00.000")))
})
