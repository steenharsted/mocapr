test_that("project_single_joint_to_MP fails when .method is not correct", {
  df <- dplyr::filter(mocapr::mocapr_data, movement_nr == 11)
  expect_error(project_single_joint_to_MP(df, LSX, LSY, LSZ, "LS", .method = 1))
  expect_error(project_single_joint_to_MP(df, LSX, LSY, LSZ, "LS", .method = NULL))
  expect_error(project_single_joint_to_MP(df, LSX, LSY, LSZ, "LS", .method = NA))
})

test_that("projecting to MP is stable when .method is 'first_last'", {
  df <- dplyr::filter(mocapr::mocapr_data, movement_nr == 11)

  #expect_known_value(project_single_joint_to_MP(df, LSX, LSY, LSZ, "LS", .method = "first_last"), here::here("tests", "testthat", "reference_files", "first_last"))

  expect_warning({

    df <- project_full_body_to_MP(df, .method = "first_last")
    numeric_columns <- names(dplyr::select_if(df,is.numeric))
    df <- tidyr::pivot_longer(df, cols = dplyr::one_of(numeric_columns))
    df <- dplyr::summarise(df, value = mean(value))
    expect_equal(round(dplyr::pull(df, value), 4), 160.5583)

    }, regexp = NULL)

})

test_that("project to MP is stable when .method is 'first_dist'", {
  df <- dplyr::filter(mocapr::mocapr_data, movement_nr == 11)

  #expect_known_value(project_single_joint_to_MP(df, LSX, LSY, LSZ, "LS", .method = "first_dist"), here::here("tests", "testthat", "reference_files", "first_dist"))

  df <- project_full_body_to_MP(df, .method = "first_dist")
  numeric_columns <- names(dplyr::select_if(df,is.numeric))
  df <- tidyr::pivot_longer(df, cols = dplyr::one_of(numeric_columns))
  df <- dplyr::summarise(df, value = mean(value))
  expect_equal(round(dplyr::pull(df, value), 4), 202.4854)

})
