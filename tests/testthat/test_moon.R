context("Moon")

test_that("Position and distance", {

  expect_equal(apparent_equatorial_position_moon(str2jd("1992-04-12")),
               list(declination=13.76836485972,
                    right_ascension=8.979231572563,
                    distance=0.002462631581053,
                    illuminated_fraction=0.6785589017075,
                    apparent_magnitude=-10.8))

})
