test_that("add_phases_jump work", {
  # Test input
  my_test_df <- data.frame(frame = 1:15)
  expect_error(add_phases_jump(my_test_df))

  my_test_df$marks <- NA
  expect_error(add_phases_jump(my_test_df))

  my_test_df$marks[4] <- "TOB"
  expect_error(add_phases_jump(my_test_df))

  my_test_df$marks[8] <- "FFB"
  expect_error(add_phases_jump(my_test_df))


  df <- tibble::tibble(
    frame = c(1:11),
    marks = c( NA, NA, NA,"TOB",  NA,  NA,  NA, "FFB", NA,  NA,  NA),
    LHY =   c(100, 50, 75,  100, 125, 150, 125,   100, 75,  50, 100),
    RHY =   c(100, 50, 75,  100, 125, 150, 125,   100, 75,  50, 100)
  )

  df2 <- tibble::tibble(
    frame = c(1:15),
    marks = c( NA, NA, NA, "TOL", "TOR",  NA,  NA,  NA,  NA, "FFL", "FFR",  NA,  NA, NA,  NA),
    LHY =   c(100, 50, 50,    75,  100,  125, 150, 150, 125,   100,    75,  70,  50, 50, 100),
    RHY =   c(100, 50, 50,    75,  100,  125, 150, 150, 125,   100,    75,  70,  50, 50, 100)
  )

  df <- add_phases_jump(df)
  df2 <- add_phases_jump(df2)

  expect_equal(df$events_b, c(NA, "DP", NA,  "TO", NA,   "MH", NA,   "FF", NA,   "DL", NA ))
  expect_equal(df2$phase_b, c(1,  2,  3, NA,  4,  5,  6,  7,  7, NA,  8,  9,  9, 10, 11))

  # Test problematic data (gave error earlier)
  # This data has uneven landing

  df3 <- data.frame(
    frame = seq(1:15),
    marks = c(NA, NA, NA,"TOR", "TOL",  NA,  NA,  NA,"FFR",  NA, NA, "FFL", NA, NA, NA),
    LHY   = c(10,  7,  5,    7,    10,  12,  15,  12,   11, 10,  10,  9, 10, 11, 11),
    RHY   = c(10,  7,  5,    7,    10,  12,  15,  12,   11, 10,  10,  9, 10, 11, 11))

  df3 <- add_phases_jump(df3)

  expect_equal(df3$phase_b, c(1,1,2,NA,4,5,6,7,NA,NA,NA,8,10,11,11))

  df4 <- data.frame(
    frame = seq(1:15),
    marks = c(NA, NA, NA,"TOR", "TOL",  NA,  NA,  NA,"FFR",  NA, NA, "FFL", NA, NA, NA),
    LHY   = c(10,  7,  5,    7,    15,  12,  14,  12,   11, 10,  10,  9, 10, 11, 11),
    RHY   = c(10,  7,  5,    7,    15,  12,  14,  12,   11, 10,  10,  9, 10, 11, 11))

  df4 <- add_phases_jump(df4)
  expect_equal(df4$phase_b, c(1, 1, 2, NA, 4, 5, 6, 7, NA, NA, NA, 8, 10, 11, 11))
})


test_that("add_jump_events works", {
  df <- tibble::tibble(
    frame = c(1:9),
    marks = c( NA, NA, NA,"TOB",  NA, "FFB", NA,  NA,  NA),
    LHY =   c(100, 50, 75,  100, 150,   100, 75,  50, 100),
    RHY =   c(100, 50, 75,  100, 150,   100, 75,  50, 100)
  )

  df2 <- tibble::tibble(
    frame = c(1:9),
    marks = c( NA, NA, "TOL", "TOR",  NA, "FFL", "FFR",  NA,  NA),
    LHY =   c(100, 50, 75,  100, 150,   100, 75,  50, 100),
    RHY =   c(100, 50, 75,  100, 150,   100, 75,  50, 100)
  )

  df <- add_jump_events(df)
  df2 <- add_jump_events(df2)

  expect_equal(unique(df$phase), c(1,2,3,4,5,6,7,8,9))
  expect_equal(unique(df2$phase), c(1,2,3,4,5,6,7,8,9))
  expect_equal(df$jump_events, c(NA, "deepest_prep", NA, "toe_off", "highest_pos", "flat_foot", NA, "deepest_land", NA))
  expect_equal(df2$jump_events, c(NA, "deepest_prep", "first_toe_off", "toe_off", "highest_pos", "flat_foot", "last_flat_foot", "deepest_land", NA))

  # Expect errors
  df1 <- tibble::tibble(
    frame = c(1:9),
    marks = c( NA, NA, NA,"TOB",  NA, "FFB", "FFL",  NA,  NA),
    LHY =   c(100, 50, 75,  100, 150,   100, 75,  50, 100),
    RHY =   c(100, 50, 75,  100, 150,   100, 75,  50, 100)
  )

  df2 <- tibble::tibble(
    frame = c(1:9),
    marks = c( NA, NA, NA, NA,  NA, "FFB", NA,  NA,  NA),
    LHY =   c(100, 50, 75,  100, 150,   100, 75,  50, 100),
    RHY =   c(100, 50, 75,  100, 150,   100, 75,  50, 100)
  )

  expect_error(add_jump_events(df1))
  expect_error(add_jump_events(df2))

})


test_that("add_jump_length_and_jump_height works with recommended method", {

  # Test method argument
  my_test_df <- data.frame(frame = seq(1:10))
  expect_error(add_jump_length_and_height(my_test_df, method = "bla"))

  # Test input
  expect_error(add_jump_length_and_height(my_test_df))

  my_test_df$events_b <- NA
  expect_error(add_jump_length_and_height(my_test_df))

  my_test_df$events_b <- c(NA, NA, NA, "TO", NA, NA, "FF", NA, NA, NA)
  expect_error(add_jump_length_and_height(my_test_df))

  my_test_df$LA_MPF <- c(0, 0, 0, 0, 100, 150, 200, 200, 200, 200)
  my_test_df$RA_MPF <- c(0, 0, 0, 0, 100, 150, 200, 200, 200, 200)
  expect_error(add_jump_length_and_height(my_test_df))

  my_test_df$LHY <- c(100, 75, 50, 100, 125, 150, 100, 75, 50, 100)
  my_test_df$RHY <- c(100, 75, 50, 100, 125, 150, 100, 75, 50, 100)
  expect_error(add_jump_length_and_height(my_test_df))

  my_test_df$marks <- c(NA, NA, NA, "TOB", NA, NA, "FFB", NA, NA, NA)
  my_test_df$phase_b <- c(1, 1,  2,     4,  5,  6,     7,  8,  9, 10)
  my_test_df <- add_jump_length_and_height(my_test_df)

  expect_equal(my_test_df$jump_length[1], 20)
  expect_equal(my_test_df$jump_height[1], 5)


  # Create a jump with bilateral take off and landing
  df <- dplyr::filter(mocapr_data, movement_nr == 1)
  df <- mocapr::project_full_body_to_MP(df)
  df <- add_phases_jump(df)


  # Create a jump with unilateral take off and landing
  df2 <- dplyr::filter(mocapr_data, movement_nr == 2)
  df2 <- df2 %>%
    dplyr::mutate(
      marks = NA,
      marks = ifelse(frame == 59, "TOL", marks),
      marks = ifelse(frame == 61, "TOR", marks),
      marks = ifelse(frame == 85, "FFL", marks),
      marks = ifelse(frame == 87, "FFR", marks))
  df2 <- mocapr::project_full_body_to_MP(df2)
  df2 <- add_phases_jump(df2)

  add_jump_length_and_height(df)
  add_jump_length_and_height(df2)


  })


test_that("add_jump_length_and_jump_height works with global method", {

  # Take of and landing with both feet
  df <- tibble::tibble(
    frame = c(1:9),
    marks = c( NA, NA, NA,"TOB",  NA, "FFB",  NA,  NA,  NA),
    LHY =   c(100, 50, 75,  100, 150,   100,  75,  50, 100),
    RHY =   c(100, 50, 75,  100, 150,   100,  75,  50, 100),
    LAX =   c(  0,  0,  0,    0,  50,   100, 100, 100, 100),
    RAX =   c(  0,  0,  0,    0,  50,   100, 100, 100, 100),
    LAZ =   rep(-10,9),
    RAZ =   rep( 10,9))

  # Test that the function fails if add_jump_events() is not run before
  expect_error(add_jump_length_and_height(df, method = "global"))

  # Test that jump_events contains the needed values if it exists
  df_test1 <- df_test2 <- df
  df_test1$jump_events <- "BAD"
  df_test1$jump_events <- NA
  expect_error((add_jump_length_and_height(df_test1, method = "global")))
  expect_error((add_jump_length_and_height(df_test2, method = "global")))

  # Run add_jump_events
  df <- add_jump_events(df)
  df <- add_jump_length_and_height(df, method = "global")

  expect_equal(unique(df$jump_length), 10)
  expect_equal(unique(df$jump_height), 5)

  # Single leg take off and landing
  df <- tibble::tibble(
    frame = c(1:9),
    marks = c( NA, NA, "TOL","TOR",  NA, "FFL",  "FFR",  NA,  NA),
    LHY =   c(100, 50, 75,  100, 150,   100,  75,  50, 100),
    RHY =   c(100, 50, 75,  100, 150,   100,  75,  50, 100),
    LAX =   c(  0,  0,  0,    0,  50,   100, 100, 100, 100),
    RAX =   c(  0,  0,  0,    0,  50,   100, 100, 100, 100),
    LAZ =   rep(-10,9),
    RAZ =   rep( 10,9))

  df <- add_jump_events(df)
  df <- add_jump_length_and_height(df, method = "global")

  expect_equal(unique(df$jump_length), 10.2)
  expect_equal(unique(df$jump_height), 5)


  df <- tibble::tibble(
    frame = c(1:9),
    marks = c( NA, NA, "TOR","TOL",  NA, "FFR",  "FFL",  NA,  NA),
    LHY =   c(100, 50, 75,  100, 150,   100,  75,  50, 100),
    RHY =   c(100, 50, 75,  100, 150,   100,  75,  50, 100),
    LAX =   c(  0,  0,  0,    0,  50,   100, 100, 100, 100),
    RAX =   c(  0,  0,  0,    0,  50,   100, 100, 100, 100),
    LAZ =   rep(-10,9),
    RAZ =   rep( 10,9))

  df <- add_jump_events(df)
  df <- add_jump_length_and_height(df, method = "global")

  expect_equal(unique(df$jump_length), 10.2)
  expect_equal(unique(df$jump_height), 5)
  })



