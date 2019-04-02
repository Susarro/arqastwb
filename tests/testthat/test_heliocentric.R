context("Heliocentric")

test_that("Heliocentric coordinates of planets", {

  jd<-str2jd("1992-12-20")

  expect_equal(heliocentric_longitude(jd,"venus"),26.11428357192)
  expect_equal(heliocentric_latitude(jd,"venus"),357.3792981952)
  expect_equal(heliocentric_radius(jd,"venus"),0.7246029367167)

  heliocentric_longitude(jd,"mercury")
  heliocentric_latitude(jd,"mercury")
  heliocentric_radius(jd,"mercury")

  heliocentric_longitude(jd,"mars")
  heliocentric_latitude(jd,"mars")
  heliocentric_radius(jd,"mars")

  heliocentric_longitude(jd,"jupyter")
  heliocentric_latitude(jd,"jupyter")
  heliocentric_radius(jd,"jupyter")

  heliocentric_longitude(jd,"saturn")
  heliocentric_latitude(jd,"saturn")
  heliocentric_radius(jd,"saturn")

  heliocentric_longitude(jd,"earth")
  heliocentric_latitude(jd,"earth")
  heliocentric_radius(jd,"earth")

})
