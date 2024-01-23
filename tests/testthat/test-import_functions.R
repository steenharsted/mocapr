test_that("import_captury_csv works", {
  path <- system.file("examples", "SBJ_new_version.csv", package = "mocapr")
  df <- import_captury_csv(path)
  expect_equal(nrow(df), 137)
  expect_equal(ncol(df), 106)
  expect_equal(df[[1,1]], "Captury")
  expect_equal(round(df[[1,5]], 3), -1433.694)
})

test_that("import_captury", {
  path <- system.file("examples", "vertical_jump_old_cap_version.csv", package = "mocapr")
  df <- suppressMessages(import_captury(path))
  expect_equal(nrow(df), 143)
  expect_equal(ncol(df), 70)
  expect_equal(df[[1,1]], "Captury")
  expect_equal(round(df[[1,5]], 4), -128.8398)
})



test_that("import_optitrack", {
  path <- system.file("examples", "optitrack.csv", package = "mocapr")
  df <- suppressMessages(import_optitrack_csv(path))
  expect_equal(nrow(df), 765)
  expect_equal(ncol(df), 150)
  expect_equal(df[[1,1]], "Optitrack")
  expect_equal(round(df[[1,5]], 3), 0.006)


  df <- suppressMessages(import_optitrack_csv(path, keep_rotations = FALSE))
  expect_equal(nrow(df), 765)
  expect_equal(ncol(df), 66)

  df <- suppressMessages(import_optitrack_csv(path, keep_rotations = FALSE, keep_finger_coords = TRUE))
  expect_equal(nrow(df), 765)
  expect_equal(ncol(df), 156)

  df <- suppressMessages(import_optitrack_csv(path, keep_rotations = FALSE, keep_marker_coords = TRUE))
  expect_equal(nrow(df), 765)
  expect_equal(ncol(df), 156)


})

test_that("import_freemocap_csv", {
  path <- system.file("examples", "freemocap_sit_to_stand.csv", package = "mocapr")
  df <- suppressMessages(import_freemocap_csv(path))
  expect_equal(nrow(df), 361)
  expect_equal(ncol(df), 50)
  expect_equal(df[[1,1]], "FreeMoCap")
  expect_equal(round(df[[1,5]], 3), 2862.018)

  df <- suppressMessages(import_freemocap_csv(path, keep_face_coords = TRUE, keep_finger_coords = TRUE))
  expect_equal(nrow(df), 361)
  expect_equal(ncol(df), 101)
  expect_equal(df[[1,1]], "FreeMoCap")
})
