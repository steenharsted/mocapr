test_that("add_frontal_plane_knee_angle works", {
 df <- data.frame(
                  LA_APR = c(10,5,0,-5,-10),
                  LK_APR = c(0,0,0,0,0),
                  LA_APU = c(0,0,0,0,0),
                  LK_APU = c(10,10,10,10,10),
                  RA_APR = c(20,15,10,5,0),
                  RK_APR = c(10,10,10,10,10),
                  RA_APU = c(0,0,0,0,0),
                  RK_APU = c(10,10,10,10,10))

 df <- add_frontal_plane_knee_angle(df)

 expect_equal(round(df$LFPKA, 5), c( 45.00000,  26.56505,  0.00000, -26.56505, -45.00000))
 expect_equal(round(df$RFPKA, 5), c(-45.00000, -26.56505,  0.00000,  26.56505,  45.00000))

})


# Frontal Plane Projection Angle
test_that("add_frontal_plane_projection_angle", {
  df <- data.frame(
                   LH_APR = c(10,10,10, 0,10,20),
                   LK_APR = c(10,10,10,10,10,10),
                   LA_APR = c( 0,10,20,10,10,10),
                   LH_APU = c(30,30,30,30,30,30),
                   LK_APU = c(10,10,10,10,10,10),
                   LA_APU = c( 0, 0, 0, 0, 0, 0),
                   RH_APR = c(10,10,10, 0,10,20),
                   RK_APR = c(10,10,10,10,10,10),
                   RA_APR = c( 0,10,20,10,10,10),
                   RH_APU = c(30,30,30,30,30,30),
                   RK_APU = c(10,10,10,10,10,10),
                   RA_APU = c( 0, 0, 0, 0, 0, 0))

  df <- add_frontal_plane_projection_angle(df)

  expect_equal(round(df$LFPPA, 5), c(-45.00000,   0.00000, 45.00000, -26.56505,  0.00000,  26.56505))
  expect_equal(round(df$RFPPA, 5), c( 45.00000,   0.00000,-45.00000,  26.56505,  0.00000, -26.56505))

})


# Frontal Plane Knee Deviation

test_that("add_frontal_plane_knee_deviation works", {
 df <- data.frame(
                  LH_APR = c( 10, 10, 10,  0, 10, 20),
                  LK_APR = c( 10, 10, 10, 10, 10, 10),
                  LA_APR = c(  0, 10, 20, 10, 10, 10),
                  LH_APU = c( 30, 30, 30, 30, 30, 30),
                  LK_APU = c( 10, 10, 10, 10, 10, 10),
                  LA_APU = c(  0,  0,  0,  0,  0,  0),
                  RH_APR = c( 20, 20, 20, 10, 20, 30),
                  RK_APR = c( 20, 20, 20, 20, 20, 20),
                  RA_APR = c( 10, 20, 30, 50, 50, 60),
                  RH_APU = c( 30, 30, 30, 30, 30, 30),
                  RK_APU = c( 10, 10, 10, 10, 10, 10),
                  RA_APU = c(  0,  0,  0,  0,  0,  0))

 df <- add_knee_anke_hip_ratios(df)

  expect_equal(df$KHR, c(1, 1, 1, 1, 1, 1))
  expect_equal(df$AHR, c(1, 1, 1, 4, 4, 5))
  expect_equal(df$KAR, c(1, 1, 1, 0.25, 0.25, 0.2))

})



# add_knee_ankle_hip_ratios

test_that("add_knee_ankle_hip_ratios", {
  df <- data.frame(
    LH_APR = c(10,10,10, 0,10,20),
    LK_APR = c(10,10,10,10,10,10),
    LA_APR = c( 0,10,20,10,10,10),
    LH_APU = c(30,30,30,30,30,30),
    LK_APU = c(10,10,10,10,10,10),
    LA_APU = c( 0, 0, 0, 0, 0, 0),
    RH_APR = c(10,10,10, 0,10,20),
    RK_APR = c(10,10,10,10,10,10),
    RA_APR = c( 0,10,20,10,10,10),
    RH_APU = c(30,30,30,30,30,30),
    RK_APU = c(10,10,10,10,10,10),
    RA_APU = c( 0, 0, 0, 0, 0, 0))

  df <- add_frontal_plane_knee_deviation(df)

  expect_equal(round(df$LFPKD, 6), c(-6.324555,  0.000000,  6.324555, -3.162278,  0.000000,  3.162278))
  expect_equal(round(df$RFPKD, 6), c( 6.324555,  0.000000, -6.324555,  3.162278,  0.000000, -3.162278))

})
