test_that("add_squat_events_works", {

  # Prepare data

  df <- data.frame(
    frame = seq(1:11),
    LHY = c(100, 95, 90, 85, 80, 75, 70, 65, 60, 80, 100),
    RHY = c(100, 95, 90, 85, 80, 75, 70, 65, 60, 80, 100))
  df <- add_squat_events(df)

  expect_equal(unique(df$phase), c(1,2,3))
  expect_equal(df[[9,5]], 2)

})
