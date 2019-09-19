#' Time align two or more motion capture recordings containing the same event.
#'
#' \code{align_movements()} is usefull for preparing tibbles that contain multible movements for animations. All movements must contain the same event that are supplied with arguments \code{event_var} and \code{event_value}. The frame numbers in the movements
#' will be syncrhonised according to the supplied event, and the event will occur at the same framenumber for all movements.
#'
#' @param .data A tibble containing multible movements that are grouped by a variable (provided in group_var), and that all contain the same event (provided in event_var and event_value). )
#' @param .group_var The grouping variable of the movements. Mulitble variables should be given using unquoted variable names inside c(), e.g., c(group1, group2)
#' @param event_var The variable that you wish to align the movements by
#' @param event_value The value in even_var that you wish to align the movements by
#' @param return_equal_length_groups TRUE/FALSE. If the movements are of different lengths. Should the first and last rows of the shorter movements be duplicated? Defaults to TRUE.
#' @param prolong_event Must be a positive whole number. Should the event that you match the movements on be prolonged? It will essentially create a freeze for a given number of frames. Defaults to 1
#'
#' @return An ungrouped tibble.
#' @export
#'
#' @examples
#' # Generate data
#' df <- tibble::tibble(
#'     frame = c(rep(seq(1:5), 2), 1, 2, 3, 4, 101, 102, 103, 104, 105, 106, 401, 402, 403),
#'     x = rnorm(n = 23), ID = c(rep(c(1,2), each = 5), 3, 3, 3, 3, 4, 4, 4, 4, 4, 4, 5, 5, 5),
#'     aligner = c(NA, "here", NA, NA, NA, NA, NA, "here", NA, NA, "here", NA, NA, NA, NA, NA, "here", NA, NA, NA, NA, NA, "here"))
#'
#' align_movements(df, .group_var = ID, event_var = aligner, event_value = "here", return_equal_length_groups = TRUE, prolong_event = 1)
align_movements <- function(.data, .group_var, event_var, event_value, return_equal_length_groups = TRUE, prolong_event = 1){

  #Ensure prolong_event is a positive integer
  if(prolong_event %%1 != 0 | prolong_event <= 0 ) {
    stop("prolong_event must be a whole positive number")
  }

  #Ensure return_equal is logical
  if(!is.logical(return_equal_length_groups)) {
    stop("return_equal_length_groups must be a logical (TRUE or FALSE)")
  }

  df <- .data %>%
    dplyr::ungroup() %>%
    dplyr::group_by_at(dplyr::vars({{.group_var}})) %>%
    dplyr::mutate(
      dummy = dplyr::if_else({{event_var}} == {{event_value}}, 1, 0))

  #test if all groups contain only 1 match
  df_test  <- df %>%
    dplyr::summarise(matches_in_group = sum(.data$dummy, na.rm = TRUE))

  if(any(!df_test$matches_in_group == 1) ) {
    df_test <- df_test %>%
      dplyr::filter(.data$matches_in_group != 1)
    print(df_test)
    print("All groups must contain one and only one match. The above groups contains none or more than one match")
    print("returning error")
    stop()
  }

  # Align frame numbers so that match occurs at frame = 0
  # match occurs when dummy == 1
  df <- df %>%
    dplyr::mutate(
      frame_nr_at_dummy = max(.data$frame[.data$dummy == 1], na.rm = TRUE),
      frame = .data$frame - .data$frame_nr_at_dummy,
      min_frame_by_group = min(.data$frame),
      max_frame_by_group = max(.data$frame)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      min_frame_all = min(.data$min_frame_by_group),
      max_frame_all = max(.data$max_frame_by_group)) %>%
    dplyr::group_by_at(dplyr::vars({{.group_var}})) %>%
    dplyr::mutate(
      duplicate_row = dplyr::case_when(
        frame == min(.data$frame) ~ .data$min_frame_by_group - .data$min_frame_all + 1,
        frame == max(.data$frame) ~ .data$max_frame_all - .data$max_frame_by_group + 1,
        TRUE ~ 1))



  if(return_equal_length_groups){
    #Create duplicate rows of first and last rows to ensure equal length
    df <- df %>%
      dplyr::slice(rep(seq_len(dplyr::n()), .data$duplicate_row)) %>%

      #Prolong event
      dplyr::mutate(
        prolong = dplyr::if_else(.data$frame == 0 & dplyr::row_number() == max(dplyr::row_number()[.data$frame == 0]), prolong_event, 1)) %>%
      dplyr::slice(rep(seq_len(dplyr::n()), .data$prolong)) %>%

      #give new frame numbers
      dplyr::mutate(frame = dplyr::row_number()) %>%

      #remove helper vars and ungroup tibble
      dplyr::select(-.data$duplicate_row, -.data$dummy, -.data$frame_nr_at_dummy, -.data$min_frame_by_group, -.data$max_frame_by_group, -.data$min_frame_all, -.data$max_frame_all, -.data$prolong) %>%
      dplyr::ungroup()}
  else{
    df <- df %>%

      #Prolong event
      dplyr::mutate(
        prolong = dplyr::if_else(.data$frame == 0 & dplyr::row_number() == max(dplyr::row_number()[.data$frame == 0]), prolong_event, 1)) %>%
      dplyr::slice(rep(seq_len(dplyr::n()), .data$prolong)) %>%

      #give new frame numbers
      dplyr::mutate(
        frame = min(.data$frame) + abs(.data$min_frame_all) + dplyr::row_number()) %>%

      #remove helper vars and ungroup tibble
      dplyr::select(-.data$duplicate_row, -.data$dummy, -.data$frame_nr_at_dummy, -.data$min_frame_by_group, -.data$max_frame_by_group, -.data$min_frame_all, -.data$max_frame_all, -.data$prolong) %>%
      dplyr::ungroup()}
  df
}
