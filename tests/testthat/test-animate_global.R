test_that("multiplication works", {
  df <- mocapr::mocapr_data %>%
    dplyr::filter(movement_nr == 1) %>%
    dplyr::filter(frame %in% c(1)) %>%
    mocapr::animate_global(
      return_data = TRUE,
      reduce_data = TRUE
    ) %>%
    dplyr::pull(Y)

  expect_equal(round(mean(df), 2), 1045.58)


  df <- mocapr::mocapr_data %>%
    dplyr::filter(movement_nr == 1) %>%
    dplyr::filter(frame %in% c(100)) %>%
    mocapr::animate_global(
      return_data = TRUE,
      reduce_data = TRUE
    ) %>%
    dplyr::pull(value)

  expect_equal(round(mean(df), 3), 433.394)

})
