test_that("Animate movement returns data as useual", {
  df <- mocapr::mocapr_data %>%
    dplyr::filter(movement_nr == 1) %>%
    mocapr::project_full_body_to_MP() %>%
    dplyr::filter(frame %in% c(1)) %>%
    mocapr::animate_movement(
      return_data = TRUE,
      reduce_data = TRUE
    ) %>%
    dplyr::pull(U)

  expect_equal(round(mean(df), 2), 1045.58)


  df <- mocapr::mocapr_data %>%
    dplyr::filter(movement_nr == 1) %>%
    mocapr::project_full_body_to_MP() %>%
    dplyr::filter(frame %in% c(100)) %>%
    mocapr::animate_movement(
      return_data = TRUE,
      reduce_data = TRUE
    ) %>%
    dplyr::pull(value)

  expect_equal(round(mean(df), 4), 406.9594)
})
