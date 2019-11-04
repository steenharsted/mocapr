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


test_that("plots are stable between versions", {
  df <- mocapr::mocapr_data %>%
    dplyr::filter(movement_nr == 1) %>%
    mocapr::project_full_body_to_MP() %>%
    dplyr::filter(frame %in% c(1)) %>%
    mocapr::animate_movement(
      return_plot = TRUE)

  # Test layers and required aes()
  expect_identical(df$layers[[1]]$geom$required_aes, c("x", "y"))
  expect_identical(df$layers[[2]]$geom$required_aes, c("x", "y"))
  expect_identical(df$layers[[3]]$geom$required_aes, c("x", "y"))
  expect_error(df$layers[[4]]$geom$required_aes)

  # Test that size guide removed
  expect_identical(df$guides$size, FALSE)

  # Test Labels are stable
  expect_identical(df$labels, list(
    x = "(mm)",
    y = "Height (mm)",
    group = "Side_frame",
    size = "size_path_color",
    colour = "Side",
    fill = "Side"))

  # Test that ratio of axis is 1
  expect_equal(df$coordinates$ratio, 1)
})

