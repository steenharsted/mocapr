# add_jump_events----
#' add_jump_events()
#'
#' @param .data A tibble containing motioncapture data from a squat in the format used in the mocapr package.
#'
#' @return A tibble
#' @export
#'
#' @examples
add_jump_events <- function(.data){
  df <- .data

  #Create a small dataframe that only contains rows with toe off events (TO[L|R|B])
  TOE_OFF <- df %>%
    dplyr::filter(str_detect(marks, 'TO')) %>%
    dplyr::filter(marks == dplyr::last(marks) | marks == dplyr::first(marks)) %>%
    dplyr::select(frame, marks) %>%
    dplyr::arrange(frame)

  ## If toe off of the feet happens simultaneously, add a second row identical to the first row
  if(nrow(TOE_OFF) == 1){
    TOE_OFF <- TOE_OFF %>%
      dplyr::slice(rep(1:n(), each=2))
  }

  #Create a small dataframe that only contains rows with flat foot events (FF[L|R|B])
  FLAT_FOOT <- df %>%
    dplyr::filter(str_detect(marks, 'FF')) %>%
    dplyr::filter(marks == first(marks) | marks == last(marks)) %>%
    dplyr::select(frame, marks) %>%
    dplyr::arrange(frame)

  ## If flat foot contact of the feet happens simultaneously, add a second row identical to the first row
  if(nrow(FLAT_FOOT) == 1){
    FLAT_FOOT <- FLAT_FOOT %>%
      dplyr::slice(rep(1:n(), each=2))
  }

  ##create jump_events so that last TO[L|R|B] will be toe off and
  ##first FF[L|R|B] will be flat_foot
  df <- df %>%
    dplyr::mutate(
      jump_events = case_when(
        frame == TOE_OFF[[2,1]] ~ "toe_off",
        frame == TOE_OFF[[1,1]] ~ "first_toe_off",
        frame == FLAT_FOOT[[1,1]] ~ "flat_foot",
        frame == FLAT_FOOT[[2,1]] ~ "last_flat_foot",
        TRUE ~ marks))




  #Generate Jump Phases----
  df <- df %>%
    #Define 4 (toe_off) and 6 (flat_foot)
    dplyr::mutate(
      phase = case_when(
        jump_events == "toe_off" ~ 4,
        jump_events == "flat_foot" ~ 6,
        TRUE ~ 5)) %>%
    #Assign:
    #time before 4 as 3 and
    #time after 6 as 7
    dplyr::mutate(
      phase = case_when(
        frame < frame[phase == 4] ~ 3,
        frame > frame[phase == 6] ~ 7,
        TRUE ~ phase)) %>%
    #Find the lowest position in phase 3 and give it value 2
    #Find the lowest position in phase 7 and give it value 8
    mutate(
      #Generate HAY at
      HAY_during_TAKE_OFF = ifelse(phase == 3, HAY, 10000),
      HAY_during_LANDING = ifelse(phase == 7, HAY, 10000),
      phase = ifelse(HAY_during_TAKE_OFF == min(HAY_during_TAKE_OFF), 2, phase),
      phase = ifelse(HAY_during_LANDING == min(HAY_during_LANDING), 8, phase)) %>%
    #Assign:
    #time before 2 as 1
    #time after 8 as 9
    dplyr::mutate(
      phase = case_when(
        frame < frame[phase == 2] ~ 1,
        frame > frame[phase == 8] ~ 9,
        TRUE ~ phase)) %>%
    dplyr::select(-HAY_during_TAKE_OFF, -HAY_during_LANDING) %>%
    dplyr::mutate(
      jump_events = case_when(
        phase == 5 & HAY == max(HAY[phase ==5]) ~ "highest_pos",
        phase == 2 ~ "deepest_prep",
        phase == 8 ~ "deepest_land",
        TRUE ~ jump_events
      )
    )
  df
}

# add_jump_length_and_jump_height ----
#' add_jump_length_and_height()
#'
#' @param .data A tibble containing motioncapture data from a squat in the format used in the mocapr package.
#'
#' @return A tibble
#' @export
#'
#' @examples
add_jump_length_and_height <- function(.data){

  df_1 <- .data %>%
    dplyr::filter(jump_events == "toe_off" |  jump_events == "flat_foot")

  #Define start point
  # The value of df_1$marks[1] tells if left or right or both feet were used at toe off
  # The start value is set at the ankle joint of the foot with last floor contact
  # If both feet touch the ground at toe off a point between the two ankle joints is
  # used as starting point
  if(df_1$marks[1] == "TOB"){
    start <- df_1 %>%
      dplyr::summarise(
        start_X = (first(LAX) + first(RAX)) / 2,
        start_Z = (first(LAZ) + first(RAZ)) / 2)
  }
  if(df_1$marks[1] == "TOL"){
    start <- df_1 %>%
      dplyr::summarise(
        start_X = first(LAX),
        start_Z = first(LAZ))
  }
  if(df_1$marks[1] == "TOR"){
    start <- df_1 %>%
      dplyr::summarise(
        start_X = first(RAX),
        start_Z = first(RAZ))
  }
  #Define end point
  # The value df_1$marks[2] tells what foot or if both has flat-foot contact first when landing
  if(df_1$marks[2] == "FFB"){
    end <- df_1 %>%
      dplyr::summarise(
        end_X = (last(LAX) + last(RAX)) / 2,
        end_Z = (last(LAZ) + last(RAZ)) / 2)
  }
  if(df_1$marks[2] == "FFL"){
    end <- df_1 %>%
      dplyr::summarise(
        end_X = last(LAX),
        end_Z = last(LAZ))
  }
  if(df_1$marks[2] == "FFR"){
    end <- df_1 %>%
      dplyr::summarise(
        end_X = last(RAX),
        end_Z = last(RAZ))
  }

  #Calculate Jump Height Value
  hip_height_start <-
    .data %>% dplyr::filter(phase==4) %>%
    dplyr::summarise(
      value  = (LHY+ RHY)/2) %>%
    dplyr::pull()

  hip_height_max  <-  .data %>%
    dplyr::filter(phase==5) %>%
    dplyr::summarise(
      value  = max(( LHY + RHY)/2)) %>%
    dplyr::pull()

  .data %>%
    dplyr::mutate(
      jump_length = round(sqrt((start$start_X - end$end_X)^2 + (start$start_Z - end$end_Z)^2 )/10, 1),
      jump_height = round( (hip_height_max - hip_height_start)/10, 1))
}
