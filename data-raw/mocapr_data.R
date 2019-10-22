## code to prepare `mocapr_data` dataset goes here
mocapr_data <- tibble::tibble(
  file = list.files(here::here("data-raw"), pattern = ".csv")) %>%
  dplyr::mutate(
    data = purrr::map(.x = file, .f = ~mocapr::import_captury(here::here("data-raw", .x)))) %>%
  dplyr::mutate(
    file = stringr::str_match(file,  pattern = "(?<=shots_).*(?=_unknown)"),
    movement_description = dplyr::case_when(
      file == "Capoeira_1" ~ "capoeira dance",
      file == "Gait_1_straight" ~ "gait normal in a straight line",
      file == "Gait_2_square" ~ "gait normal in a semi square",
      file == "Gait_3_drop_foot" ~ "gait with simulated drop foot",
      file == "Gait_4_int_rot" ~ "gait with simulated internal rotation",
      file == "SBJ_1" ~ "standing long jump for maximal performance",
      file == "SBJ_2" ~ "standing long jump for maximal performance",
      file == "SBJ_3" ~ "standing long jump with simulated poor landing technique of the right lower extremity",
      file == "VJ_1" ~ "vertical jump for maximal performance",
      file == "Lunge_1" ~ "forward lunge normal",
      file == "Squat_1" ~ "squat normal",
      TRUE ~ "No information"
    )) %>%
  dplyr::mutate(
    movement_nr = dplyr::case_when(
      file == "Capoeira_1" ~ 9,
      file == "Gait_1_straight" ~ 5,
      file == "Gait_2_square" ~ 6,
      file == "Gait_3_drop_foot" ~ 7,
      file == "Gait_4_int_rot" ~ 8,
      file == "SBJ_1" ~ 1,
      file == "SBJ_2" ~ 2,
      file == "SBJ_3" ~ 3,
      file == "VJ_1" ~ 4,
      file == "Lunge_1" ~ 10,
      file == "Squat_1" ~ 11,
      TRUE ~ NA_real_
    )) %>%

  # Change marks columns to character vectors
  dplyr::mutate(
    data = purrr::map(data, ~dplyr::mutate(.x, marks = as.character(marks)))) %>%

  # select, arrange, and unnest
  dplyr::select(movement_nr, movement_description, data) %>%
  dplyr::arrange(movement_nr) %>%
  tidyr::unnest(cols = c(data)) %>%

  # remove the first part of movement_nr_2 as it is irrelevant
  dplyr::filter(!(movement_nr == 3 & frame < 60)) %>%

  # Correct Toe data in movement_nr 10 as it is 0 in all frame
  dplyr::mutate(
    LTX = dplyr::if_else(movement_nr == 10, LAX, LTX),
    LTY = dplyr::if_else(movement_nr == 10, LAY, LTY),
    LTZ = dplyr::if_else(movement_nr == 10, LAZ, LTZ),
    RTX = dplyr::if_else(movement_nr == 10, RAX, RTX),
    RTY = dplyr::if_else(movement_nr == 10, RAY, RTY),
    RTZ = dplyr::if_else(movement_nr == 10, RAZ, RTZ))

usethis::use_data(mocapr_data, overwrite = TRUE)



