context("Star")

test_that("Star calculation", {
  declination1<-str2degrees("19\u00b010'57\u0022")
  right_ascension1<-str2hours("14:15:39.7")
  declination2<-str2degrees("-11\u00b009'41\u0022")
  right_ascension2<-str2hours("13:25:11.6")
  expect_equal(angular_separation(declination1,right_ascension1, declination2, right_ascension2),32.79301033514)

  former_right_ascension<-str2hours("02:31:48.704")
  former_declination<-str2degrees("89\u00b015'50.72\u0022")
  proper_motion_right_ascension<-0.19877/3600
  proper_motion_declination<--0.0152/3600


  right_ascension<-str2hours("02:44:11.986")
  declination<-str2degrees("49\u00b013'42.48\u0022")
  jd<-julian_day(13.19,11,2028)
  expect_equal(aberration_correction_star(jd, declination, right_ascension, precise=FALSE),list(declination= 0.0018964166540335796,right_ascension=0.00016977867311333087))
  expect_equal(nutation_correction_star(jd, declination, right_ascension, precise=FALSE),list(declination= 0.0017315380586425148,right_ascension=0.00029205399013315425))

  former_right_ascension<-str2hours("02:44:11.986")
  former_declination<-str2degrees("49\u00b013'42.48''")
  former_jd=str2jd("2000-1-1T12:00")
  jd<-julian_day(13.19,11,2028)
  proper_motion_right_ascension<-0.03425/3600
  proper_motion_declination<--0.0895/3600

  expect_equal(apparent_equatorial_position_star(jd=jd, former_jd=str2jd("2000-01-01T12:00"),former_declination=former_declination,former_right_ascension=former_right_ascension, proper_motion_declination=proper_motion_declination, proper_motion_right_ascension=proper_motion_right_ascension,precise=TRUE),
  list(declination= 49.352068524100005,right_ascension=2.7706643067283916))

})
