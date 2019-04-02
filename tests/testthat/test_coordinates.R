context("Coordinates")

test_that("Hour angle and Sexagesimal degrees", {

  hour_angle=15.5
  hour_angle2<-degrees2hours(hours2degrees(hour_angle))
  hour_angle3<-rad2hours(hours2rad(hour_angle))
  hour_angle4<-degrees2hours(rad2degrees(hours2rad(hour_angle)))

  dd<-c(15.5,30.14)
  str<-degrees2str(dd)
  dd2<-str2degrees(str)


  expect_equal(hour_angle, hour_angle2)
  expect_equal(hour_angle, hour_angle3)
  expect_equal(hour_angle, hour_angle4)
  expect_equal(str, c("15\u00b030'00.000\"","30\u00b008'24.000\""))
  expect_equal(dd2, dd)
  expect_equal(reduce_degrees(c(15,15+360*4,15-360*2,360)),c(15,15,15,0))
  expect_equal(reduce_degrees(c(120,240,359,181),signed=T),c(120,-120,-1,-179))
})

test_that("Coordinates transformation", {
  obl <- 23.4392911
  decl<-str2degrees("28\u00b001'34.26\"")
  ra<-str2hours("07:45:18.946")
  ecl<-equatorial2ecliptic(declination = decl,right_ascension=ra, obliquity = obl)
  expect_equal(ecl,list(longitude = 113.2156292276,latitude=6.684170072045))
  expect_error(equatorial2ecliptic(declination = decl,right_ascension=ra),"Both jd and obliquity can't be null")
  equ<-ecliptic2equatorial(longitude=ecl$longitude,latitude=ecl$latitude,obliquity=obl)
  expect_equal(equ,list(declination = decl,right_ascension=ra))
  expect_error(ecliptic2equatorial(longitude=ecl$longitude,latitude=ecl$latitude),"Both jd and obliquity can't be null")

  jd<-str2jd("1987-04-10T19:21:00")
  geographic_longitude<-str2degrees("-77\u00b003'55.5\"")
  geographic_latitude<-str2degrees("38\u00b055'17\"")
  declination<-str2degrees("-6\u00b043'11.61\"")
  right_ascension<-str2hours("23:09:16.641")
  hor<-equatorial2horizontal(geographic_longitude,geographic_latitude,declination, right_ascension,jd)
  expect_equal(hor,list(azimuth = 248.0336941301,altitude=15.1248736234))
  eq<-horizontal2equatorial(geographic_longitude,geographic_latitude,hor$azimuth, hor$altitude,jd)
  expect_equal(eq,list(declination = declination,right_ascension=right_ascension))

  jd<-str2jd("2003-08-28T03:17:00")
  geographic_longitude<--116.8625
  geographic_latitude<-33.356111
  geographic_altitude<-1706
  declination<--15.771083
  right_ascension<-22.63534722222
  celestial_body<-"mars"

  expect_equal(geocentric2topocentric(declination,right_ascension,jd,geographic_longitude,geographic_latitude,geographic_altitude,celestial_body),list(declination=-15.77501187815,right_ascension=22.63570667377
))
})

test_that("Coordinates formatting", {
  coord<-list(declination=c(89.454275227064002,89.5),right_ascension=c(3.8046089729542945,3.9))
  expect_equal(str(coord),list(declination=c( "89\u00b027'15.391\u0022","89\u00b030'00.000\u0022"),right_ascension=c("03:48:16.592","03:54:00.000")))
})




