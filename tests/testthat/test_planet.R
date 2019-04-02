context("Planets")

test_that("Orbital elements", {

 expect_equal(planetary_orbit_elements(str2jd("2065-06-24.0"),"mercury"),list(mean_longitude=203.4947015771,semimajor_axis=0.38709831,eccentricity=0.2056450990275,inclination=7.006170924088,ascending_node_longitude=49.10765026683,perihelion_longitude=78.47538174105))
})

test_that("Distance and position", {
  expect_equal(aberration_corrected_coordinates_planet(str2jd("1992-12-20"),"venus"),list(distance=0.9109467907118,x=0.6217932798082,y=-0.6649052841321,z=-0.03313813251595, L=26.11428357192, B=357.3792981952, R=0.7246029367167))
  expect_equal(apparent_equatorial_position_planet(str2jd("1992-12-20"),"venus"),
               list(declination=-18.88802984917,
                    right_ascension=21.07819118387,
                    distance=0.9109467907118,
                    illuminated_fraction=0.6464899740349,
                    apparent_magnitude=-3.8))
})


