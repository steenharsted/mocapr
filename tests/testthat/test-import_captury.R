test_that("import_captury", {
  path <- system.file("examples", "vertical_jump_old_cap_version.csv", package = "mocapr")
  df <- import_captury(path)
  expect_equal(nrow(df), 143)
  expect_equal(df[[1,1]], "Captury")
  expect_equal(round(df[[1,5]], 4), -128.8398)
})
