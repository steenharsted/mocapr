test_that("animate_anatomical() returns data correctly", {
  df <- mocapr::mocapr_data %>%
    dplyr::filter(movement_nr == 1) %>%
    mocapr::project_full_body_to_AP() %>%
    dplyr::filter(frame %in% c(1)) %>%
    mocapr::animate_anatomical(
      return_data = TRUE,
      reduce_data = TRUE
    )

  expect_equal(round(df[[1,4]], 4), 80.2525)

})
