# tmp <- tempfile()
# tmp2 <- tempfile()

test_that("project_single_joint_to_MP fails when .method is not correct", {
  df <- dplyr::filter(mocapr::mocapr_data, movement_nr == 11)
  expect_error(project_single_joint_to_MP(df, LSX, LSY, LSZ, "LS", .method = 1))
  expect_error(project_single_joint_to_MP(df, LSX, LSY, LSZ, "LS", .method = NULL))
  expect_error(project_single_joint_to_MP(df, LSX, LSY, LSZ, "LS", .method = NA))
})

test_that("project_single_joint_to_MP works when .method is 'first_last'", {
  df <- dplyr::filter(mocapr::mocapr_data, movement_nr == 11)

  expect_known_value(project_single_joint_to_MP(df, LSX, LSY, LSZ, "LS", .method = "first_last"), tmp)
})

test_that("project_single_joint_to_MP works when .method is 'first_dist'", {
  df <- dplyr::filter(mocapr::mocapr_data, movement_nr == 11)
  expect_known_value(project_single_joint_to_MP(df, LSX, LSY, LSZ, "LS", .method = "first_dist"), tmp2)

})
