## code to prepare `mocapr_data` dataset goes here
mocapr_data <- tibble::tibble(
  file = list.files(here::here("data-raw"), pattern = ".csv")) %>%
  dplyr::mutate(
    data = purrr::map(.x = file, .f = ~mocapr::import_captury(here::here("data-raw", .x)))) %>%
  dplyr::mutate(
    file = stringr::str_match(file,  pattern = "(?<=shots_).*(?=_unknown)"),
    movement_description = dplyr::case_when(
      file == "Capoeira_1" ~ "capoeira dance",
      file == "Gait_1_straight" ~ "normal gait in a straight line",
      file == "Gait_2_square" ~ "normal gait in a semi square",
      file == "Gait_3_drop_foot" ~ "gait with simulated drop foot",
      file == "Gait_4_int_rot" ~ "gait with simulated internal rotation",
      file == "SBJ_1" ~ "standing long jump for maximal performance",
      file == "SBJ_2" ~ "standing long jump with simulated poor landing technique of the right lower extremity",
      file == "VJ_1" ~ "vertical jump for maximal performance",
      TRUE ~ "No information"
    )) %>%
  dplyr::mutate(
    movement_nr = dplyr::case_when(
      file == "Capoeira_1" ~ 8,
      file == "Gait_1_straight" ~ 4,
      file == "Gait_2_square" ~ 5,
      file == "Gait_3_drop_foot" ~ 6,
      file == "Gait_4_int_rot" ~ 7,
      file == "SBJ_1" ~ 1,
      file == "SBJ_2" ~ 2,
      file == "VJ_1" ~ 3,
      TRUE ~ NA_real_
    )) %>%

  # Change marks columns to character vectors
  dplyr::mutate(
    data = purrr::map(data, ~dplyr::mutate(.x, marks = as.character(marks)))) %>%

  # select, arrange, and unnest
  dplyr::select(movement_nr, movement_description, data) %>%
  dplyr::arrange(movement_nr) %>%
  tidyr::unnest(cols = c(data))

usethis::use_data(mocapr_data, overwrite = TRUE)



