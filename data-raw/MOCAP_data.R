## code to prepare `MOCAP_data` dataset goes here
MOCAP_data <- tibble::tibble(
  file = list.files(here::here("data-raw"), pattern = ".csv")) %>%
  dplyr::mutate(
    data = purrr::map(.x = file, .f = ~import_captury(here::here("data-raw", .x)))) %>%
  dplyr::mutate(
    movement_description = dplyr::case_when(
      file == "caipoera.csv" ~ "caipoera dance",
      file == "gait_1.csv" ~ "normal gait in a straight line",
      file == "gait_2.csv" ~ "normal gait in a semi square",
      file == "standing_long_jump_1.csv" ~ "standing long jump for maximal performance",
      file == "standing_long_jump_2.csv" ~ "standing long jump with simulated poor landing technique of the right lower extremity",
      file == "vertical_jump.csv" ~ "vertical jump for maximal performance",
      TRUE ~ "No information"
    )) %>%
  dplyr::mutate(
    movement_nr = dplyr::case_when(
      file == "caipoera.csv" ~ 6,
      file == "vertical_jump.csv" ~ 5,
      file == "gait_1.csv" ~ 3,
      file == "gait_2.csv" ~ 4,
      file == "standing_long_jump_1.csv" ~ 1,
      file == "standing_long_jump_2.csv" ~ 2,
      TRUE ~ NA_real_
    )) %>%
  dplyr::select(movement_nr, movement_description, data) %>%
  dplyr::arrange(movement_nr) %>%
  tidyr::unnest()

usethis::use_data(MOCAP_data, overwrite = TRUE)



