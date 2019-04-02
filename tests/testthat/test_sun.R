context("Sun")

test_that("Sun position calculation", {
 jd<-str2jd("1992-10-13")
 expect_equal(apparent_equatorial_position_sun(jd,precise=FALSE),list(declination= -7.785040137049,right_ascension=13.22538863968))
 expect_equal(apparent_equatorial_position_sun(jd,precise=TRUE),list(declination= -7.783870593711,right_ascension=13.22521186891))

})
