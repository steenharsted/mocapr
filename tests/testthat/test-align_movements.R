test_that("multiplication works", {
  # Generate data
  df <- tibble::tibble(
    frame = c(rep(seq(1:5), 2), 1, 2, 3, 4,
              101, 102, 103, 104, 105, 106,
              401, 402, 403),
    x = rnorm(n = 23),
    ID = c(rep(c(1,2), each = 5),
           3, 3, 3, 3, 4, 4, 4,
           4, 4, 4, 5, 5, 5),
    aligner = c(NA, "here", NA, NA, NA,
                NA, NA, "here", NA, NA,
                "here", NA, NA, NA, NA,
                NA, "here", NA, NA, NA,
                NA, NA, "here"))
  # Align movements
  df_aligned <- align_movements(df,
                  .group_var = ID,
                  event_var = aligner,
                  event_value = "here",
                  return_equal_length_groups = TRUE,
                  prolong_event = 1)

  expect_equal(nrow(df_aligned), 30)


  df_aligned2 <- align_movements(df,
                                .group_var = ID,
                                event_var = aligner,
                                event_value = "here",
                                return_equal_length_groups = FALSE,
                                prolong_event = 10)
  expect_equal(nrow(df_aligned2), 68)


  # Produce error
  expect_error(align_movements(df,
                               .group_var = ID,
                               event_var = aligner,
                               event_value = "here",
                               return_equal_length_groups = TRUE,
                               prolong_event = 0))

  expect_error(align_movements(df,
                               .group_var = ID,
                               event_var = aligner,
                               event_value = "here",
                               return_equal_length_groups = 3,
                               prolong_event = 0))


  df <- tibble::tibble(
    frame = c(rep(seq(1:5), 2), 1, 2, 3, 4,
              101, 102, 103, 104, 105, 106,
              401, 402, 403),
    x = rnorm(n = 23),
    ID = c(rep(c(1,2), each = 5),
           3, 3, 3, 3, 4, 4, 4,
           4, 4, 4, 5, 5, 5),
    aligner = c(NA, NA, NA, NA, NA,
                NA, NA, "here", NA, NA,
                "here", NA, NA, NA, NA,
                NA, "here", NA, NA, NA,
                NA, NA, "here"))

  expect_error(align_movements(df,
                  .group_var = ID,
                  event_var = aligner,
                  event_value = "here",
                  return_equal_length_groups = TRUE,
                  prolong_event = 1))
})
