context("Nutation and Obliquity")

test_that("Nutation and Obliquity", {

  jd<-str2jd("1987-04-10")
  expect_equal(jd, 2446895.5)
  expect_equal(centuries_since_2000(jd),-0.1272963723477)
  expect_equal(degrees2str(mean_obliquity_ecliptic(jd)),"23\u00b026'27.407\u0022")
  expect_equal(degrees2str(nutation_in_longitude(jd)),"-00\u00b000'03.788\u0022")
  expect_equal(degrees2str(nutation_in_obliquity(jd)),"00\u00b000'09.443\u0022")
  expect_equal(degrees2str(true_obliquity_ecliptic(jd)),"23\u00b026'36.850\u0022")


})
