test_that("add_jump_events works", {
  df <- tibble::tibble(
    frame = c(1:9),
    marks = c( NA, NA, NA,"TOB",  NA, "FFB", NA,  NA,  NA),
    LHY =   c(100, 50, 75,  100, 150,   100, 75,  50, 100),
    RHY =   c(100, 50, 75,  100, 150,   100, 75,  50, 100)
  )

  df <- add_jump_events(df)

  expect_equal(unique(df$phase), c(1,2,3,4,5,6,7,8,9))
  expect_equal(df$jump_events, c(NA, "deepest_prep", NA, "toe_off", "highest_pos", "flat_foot", NA, "deepest_land", NA))

})



test_that("add_jump_length_and_jump_height works", {

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

  df <- add_jump_events(df)
  df <- add_jump_length_and_height(df)

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
  df <- add_jump_length_and_height(df)

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
  df <- add_jump_length_and_height(df)

  expect_equal(unique(df$jump_length), 10.2)
  expect_equal(unique(df$jump_height), 5)

})

