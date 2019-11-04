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


test_that("plots are stable between versions", {
  df <- mocapr::mocapr_data %>%
    dplyr::filter(movement_nr == 1) %>%
    dplyr::filter(frame %in% c(1)) %>%
    mocapr::project_full_body_to_AP() %>%
    mocapr::animate_anatomical(
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


test_that("Layers are different if arguments are FALSE", {
  df <- mocapr::mocapr_data %>%
    dplyr::filter(movement_nr == 1) %>%
    dplyr::filter(frame %in% c(1)) %>%
    mocapr::project_full_body_to_AP() %>%
    mocapr::animate_anatomical(
      point = FALSE,
      line_colored = FALSE,
      line_black = FALSE,
      return_plot = TRUE)

  # Test layers and required aes()
  expect_error(df$layers[[1]]$geom$required_aes)
})
