#' Divide a jump into phases
#'
#' \code{add_phases_jump} uses average hip joint-center height and supplied events to divide a jump into 11 phases.
#' This division is useful for further analyis of the individual phases in the jump.
#'
#' Three phase columns are generated \code{phase_l, phase_r, phase_b}, describing the phases of the left leg, right leg, and both extremities respectively.
#'
#' The numerical values in \code{phase} column correspond to:
#' 1. Descending phase of the preparation
#' 1. The deepest postion in the preparation phase (A single frame)
#' 1. The ascending phase of the preparation (push-off)
#' 1. Toe off - the last fase with contact between the feet and the ground (A single frame)
#' 1. The ascending phase of the hang time
#' 1. The highest position of the hang time (A single frame)
#' 1. The descending phase of the hang time
#' 1. Impact (A single frame)
#' 1. Descending phase of the landing.
#' 1. Deepest position of the landing (A single frame)
#' 1. Ascending phase of the landing.
#'
#' The values in the column \code{phase_b} will be identical to the values in \code{phase_l} and \code{phase_r} if the subject is taking off
#' and landing simultaneously with both extremities. If this is not the case \code{phase_b} will take the value \code{4} at the frame where
#' the last foot has contact with the ground, and the value \code{8} at the frame where the last foot has impact. \code{NA} values will be given to phases
#' where there is transition (e.g. left foot still has contact with ground, while right foot is in the air.)
#'
#' @param .data A tibble containing motioncapture data from a jump. The data must contain:\cr
#' * \code{LHY} and \code{RHY} columns containg global spatial joint-center positions of the left and right hip\cr
#' * A \code{frame} column\cr
#' * A \code{marks} column with values descring toe off events (TOL = toe off of left foot, TOR = toe off of right foot, TOB = toe off from both feet),
#' and impact events (FFL = flat foot contact with left foot, FFR = flat foot contact with right foot, FFB = flat foot contact with both feet).
#'
#' @return The tibble supplied in the \code{.data argument}, with the added columns \code{phase_l}, \code{phase_r}, \code{phase_b},
#'  \code{events_l}, \code{events_r}, \code{events_b}
#' @export
#'
#' @examples
#' # With synthetic data
#' df <- tibble::tibble(
#'                      frame = c(1:11),
#'                      marks = c( NA, NA, NA,"TOB",  NA,  NA,  NA, "FFB", NA,  NA,  NA),
#'                      LHY =   c(100, 50, 75,  100, 125, 150, 125,   100, 75,  50, 100),
#'                      RHY =   c(100, 50, 75,  100, 125, 150, 125,   100, 75,  50, 100))
#'
#' df2 <- tibble::tibble(
#'                       frame = c(1:15),
#'                       marks = c( NA, NA, NA, "TOL", "TOR",  NA,  NA,  NA,  NA, "FFL", "FFR",  NA,  NA, NA,  NA),
#'                       LHY =   c(100, 50, 50,    75,  100,  125, 150, 150, 125,   100,    75,  70,  50, 50, 100),
#'                       RHY =   c(100, 50, 50,    75,  100,  125, 150, 150, 125,   100,    75,  70,  50, 50, 100))
#' add_phases_jump(df)
#' add_phases_jump(df2)
#'
#' # With real mocap data
#'  df3 <- dplyr::filter(mocapr::mocapr_data, movement_nr == 1)
#'  df3 <- dplyr::select(df, frame, marks, LHY, RHY)
#'
#'  add_phases_jump(df3)
#'
#' # A plot displaying the phases
#' df4 <- dplyr::filter(mocapr::mocapr_data, movement_nr == 1)
#' df4 <- add_phases_jump(df4)
#'  mocapr::animate_global(df4,
#'                         planes = "X",
#'                         remove_facet_labels = FALSE,
#'                         return_plot = TRUE,
#'                         col_facets = phase_b)
add_phases_jump <- function(.data){

  # Check inputs
  # Test if marks contain the needed elements
  .test <-
    data.frame(
      marks = unique(.data$marks)
    ) %>%
    dplyr::mutate(
      toe_offs = dplyr::if_else(marks == "TOB", 2, 0, missing = 0),
      impacts = dplyr::if_else(marks == "FFB", 2, 0, missing =0 ),
      toe_offs = dplyr::if_else(marks %in% c("TOL", "TOR"), 1, toe_offs),
      impacts = dplyr::if_else(marks %in% c("FFL", "FFR"), 1, impacts)
    ) %>%
    dplyr::summarise_if(is.numeric, sum)

  if(.test[["toe_offs"]] != 2) {
    stop("add_jump_events() requires the column marks to contain one row with 'TOB', or one row with 'TOL' and one row with 'TOR'")
  }

  if(.test[["impacts"]] != 2) {
    stop("add_jump_events() requires the column marks to contain one row with 'FFB', or one row with 'FFL' and one row with 'FFR'")
  }


  # Function
  df <- .data %>%


    # The events from both extremities is needed in order to define the jump phases
    dplyr::mutate(
      events_l = dplyr::if_else(marks == "TOB" | marks == "TOL", "TO", "TBD"),
      events_r = dplyr::if_else(marks == "TOB" | marks == "TOR", "TO", "TBD"),
      events_l = dplyr::if_else(marks == "FFB" | marks == "FFL", "FF", events_l, "TBD"),
      events_r = dplyr::if_else(marks == "FFB" | marks == "FFR", "FF", events_r, "TBD")
      ) %>%

    # First create a rough phase grouping
    dplyr::mutate(
      phase_l = dplyr::case_when(
        frame < frame[events_l == "TO"] ~ 3,
        frame > frame[events_l == "FF"] ~ 9,
        frame > frame[events_l == "TO"] & frame < frame[events_l == "FF"] ~ 5,
        TRUE ~ NA_real_),
      phase_r = dplyr::case_when(
        frame < frame[events_r == "TO"] ~ 3,
        frame > frame[events_r == "FF"] ~ 9,
        frame > frame[events_r == "TO"] & frame < frame[events_r == "FF"] ~ 5,
        TRUE ~ NA_real_)
    )


    # Fine tune the preparation phase

  df_prep <- df %>%
    dplyr::mutate(
      .dummy_HA_prep = LHY+RHY,
      .dummy_HA_prep = dplyr::if_else(phase_l == 3 & phase_r == 3, .dummy_HA_prep, 10000, missing = 10000),
      events_l = ifelse(phase_l == 3 & (LHY+RHY) == min(.dummy_HA_prep), "DP", events_l),
      events_r = ifelse(phase_r == 3 & (LHY+RHY) == min(.dummy_HA_prep), "DP", events_r),

      # Remove possible duplicates
      events_l = replace(events_l, duplicated(events_l), "TBD"),
      events_r = replace(events_r, duplicated(events_r), "TBD"),

      phase_l = dplyr::case_when(
      events_l == "TO" ~ 4,
      events_l == "DP" ~ 2,
      frame < frame[events_l == "DP"] ~ 1,
      frame > frame[events_l == "DP"] & frame < frame[events_l == "TO"] ~ 3,
      TRUE ~ phase_l),
    phase_r = dplyr::case_when(
      events_r == "TO" ~ 4,
      events_r == "DP" ~ 2,
      frame < frame[events_r == "DP"] ~ 1,
      frame > frame[events_r == "DP"] & frame < frame[events_r == "TO"] ~ 3,
      TRUE ~ phase_r)
    ) %>%
    dplyr::select(-.dummy_HA_prep)

    # Fine tune the air phase
  df_air <- df_prep %>%
    dplyr::mutate(
      .dummy_HA_air = LHY+RHY,
      .dummy_HA_air = dplyr::if_else(phase_l == 5 & phase_r == 5, .dummy_HA_air, -10000, missing = -10000),
      events_l = ifelse(phase_l == 5 & (LHY+RHY) == max(.dummy_HA_air), "MH", events_l),
      events_r = ifelse(phase_r == 5 & (LHY+RHY) == max(.dummy_HA_air), "MH", events_r),

      # Remove possible duplicates
      events_l = replace(events_l, duplicated(events_l), "TBD"),
      events_r = replace(events_r, duplicated(events_r), "TBD"),

      phase_l = dplyr::case_when(
        events_l == "MH" ~ 6,
        events_l == "FF" ~ 8,
        frame < frame[events_l == "MH"] & phase_l == 5 ~ 5,
        frame > frame[events_l == "MH"] & phase_l == 5 ~ 7,
        TRUE ~ phase_l),
      phase_r = dplyr::case_when(
        events_r == "MH" ~ 6,
        events_r == "FF" ~ 8,
        frame < frame[events_r == "MH"] & phase_r == 5 ~ 5,
        frame > frame[events_r == "MH"] & phase_r == 5 ~ 7,
        TRUE ~ phase_r)
      ) %>%
    dplyr::select(-.dummy_HA_air)

  # Fine tune the landing phase
  df_land <- df_air %>%
    dplyr::mutate(
      .dummy_HA_land = LHY+RHY,
      .dummy_HA_land = dplyr::if_else(phase_l == 9 & phase_r == 9, .dummy_HA_land, 10000, missing = 10000),
      events_l = ifelse(phase_l == 9 & phase_r == 9 & (LHY+RHY) == min(.dummy_HA_land), "DL", events_l),
      events_r = ifelse(phase_l == 9 & phase_r == 9 & (LHY+RHY) == min(.dummy_HA_land), "DL", events_r),

      # Remove possible duplicates
      events_l = replace(events_l, duplicated(events_l, fromLast = TRUE), "TBD"),
      events_r = replace(events_r, duplicated(events_r, fromLast = TRUE), "TBD"),

      phase_l = dplyr::case_when(
        events_l == "DL" ~ 10,
        frame < frame[events_l == "DL"] & phase_l == 9 ~ 9,
        frame > frame[events_l == "DL"] & phase_l == 9 ~ 11,
        TRUE ~ phase_l),
      phase_r = dplyr::case_when(
        events_r == "DL" ~ 10,
        frame < frame[events_r == "DL"] & phase_r == 9 ~ 9,
        frame > frame[events_r == "DL"] & phase_r == 9 ~ 11,
        TRUE ~ phase_r)
      ) %>%
    dplyr::select(-.dummy_HA_land)

  # Create the bilateral phase and events using phase and events from both extremities
  df_land %>%
    dplyr::mutate(
      events_l = dplyr::recode(events_l, TBD = NA_character_),
      events_r = dplyr::recode(events_r, TBD = NA_character_),
      events_b = dplyr::case_when(
        events_l == events_r ~ events_l,
        !is.na(events_l) & is.na(events_r) ~ events_l,
        is.na(events_l) & !is.na(events_r) ~ events_r,
        is.na(events_l) & is.na(events_r) ~ NA_character_,
        TRUE ~ "WARNING"),
      phase_b = dplyr::case_when(
        phase_l == phase_r ~ phase_l,
        dplyr::row_number() == dplyr::last(which(events_b %in% "TO")) ~ 4,
        dplyr::row_number() == dplyr::last(which(events_b %in% "FF")) ~ 8,
        TRUE ~ NA_real_)
    )

}



# add_jump_events----
#' Divide a jump into phases
#'
#' \code{add_jump_events} uses average hip joint-center height and supplied events to divide a jump into 9 phases.
#' This division is useful for further analyis of the individual phases in the jump.
#' The numerical values in \code{phase} column correspond to:
#' 1. Descending phase of the preparation
#' 1. The deepest postion in the preparation phase
#' 1. The ascending phase of the preparation (push-off)
#' 1. Toe off - the last fase with contact between the feet and the ground
#' 1. Hang time.
#' 1. Impact
#' 1. Descending phase of the landing.
#' 1. Deepest position of the landing.
#' 1. Ascending phase of the landing.
#'
#' @param .data A tibble containing motioncapture data from a jump. The data must contain:\cr
#' * \code{LHY} and \code{RHY} columns containg global spatial joint-center positions of the left and right hip\cr
#' * A \code{frame} column\cr
#' * A \code{marks} column with values descring toe off events (TOL = toe off of left foot, TOR = toe off of right foot, TOB = toe off from both feet),
#' and impact events (FFL = flat foot contact with left foot, FFR = flat foot contact with right foot, FFB = flat foot contact with both feet).
#'
#' @return The tibble supplied in the \code{.data argument}, with the added columns \code{phase} and \code{jump_events}
#' @export
#'
#' @examples
#' # Prepare data
#' df <- dplyr::filter(mocapr::mocapr_data, movement_nr == 1)
#' df <- dplyr::select(df, frame, marks, LHY, RHY)
#'
#' add_jump_events(df)
#'
#' # A plot displaying the phases
#' df2 <- dplyr::filter(mocapr::mocapr_data, movement_nr == 1)
#' df2 <- add_jump_events(df2)
#'
#' mocapr::animate_global(df2,
#'                        planes = "X",
#'                        remove_facet_labels = FALSE,
#'                        return_plot = TRUE,
#'                        col_facets = phase)
add_jump_events <- function(.data){

  message("This function has been deprecated since mocapr version 1.9006 available on Github since 2019-12-01. Consider using add_phases_jump() instead")

  # Avoid "No visible binding for global variable ..." when performing check()
  LHY <- RHY <- marks <- frame <- phase <- .HAY <- NULL
  HAY_during_TAKE_OFF <- HAY_during_LANDING <- NULL
  toe_offs <- impacts <- NULL

  # Test if marks contain the needed elements
  .test <-
    data.frame(
      marks = unique(.data$marks)
      ) %>%
    dplyr::mutate(
      toe_offs = dplyr::if_else(marks == "TOB", 2, 0, missing = 0),
      impacts = dplyr::if_else(marks == "FFB", 2, 0, missing =0 ),
      toe_offs = dplyr::if_else(marks %in% c("TOL", "TOR"), 1, toe_offs),
      impacts = dplyr::if_else(marks %in% c("FFL", "FFR"), 1, impacts)
      ) %>%
    dplyr::summarise_if(is.numeric, sum)

  if(.test[["toe_offs"]] != 2) {
    stop("add_jump_events() requires the column marks to contain one row with 'TOB', or one row with 'TOL' and one row with 'TOR'")
  }

  if(.test[["impacts"]] != 2) {
    stop("add_jump_events() requires the column marks to contain one row with 'FFB', or one row with 'FFL' and one row with 'FFR'")
  }


  # Function
  df <- .data %>%
    # Generate the averaged hip height
    dplyr::mutate(.HAY = (LHY+RHY)/2)

  #Create a small dataframe that only contains rows with toe off events (TO[L|R|B])
  TOE_OFF <- df %>%
    dplyr::filter(stringr::str_detect(marks, 'TO')) %>%
    dplyr::filter(marks == dplyr::last(marks) | marks == dplyr::first(marks)) %>%
    dplyr::select(frame, marks) %>%
    dplyr::arrange(frame)


  ## If toe off of the feet happens simultaneously, add a second row identical to the first row
  if(nrow(TOE_OFF) == 1){
    TOE_OFF <- TOE_OFF %>%
      dplyr::slice(rep(1:dplyr::n(), each=2))
  }

  #Create a small dataframe that only contains rows with flat foot events (FF[L|R|B])
  FLAT_FOOT <- df %>%
    dplyr::filter(stringr::str_detect(marks, 'FF')) %>%
    dplyr::filter(marks == dplyr::first(marks) | marks == dplyr::last(marks)) %>%
    dplyr::select(frame, marks) %>%
    dplyr::arrange(frame)

  ## If flat foot contact of the feet happens simultaneously, add a second row identical to the first row
  if(nrow(FLAT_FOOT) == 1){
    FLAT_FOOT <- FLAT_FOOT %>%
      dplyr::slice(rep(1:dplyr::n(), each=2))
  }

  ##create jump_events so that last TO[L|R|B] will be toe off and
  ##first FF[L|R|B] will be flat_foot
  df <- df %>%
    dplyr::mutate(
      jump_events = dplyr::case_when(
        frame == TOE_OFF[[2,1]] ~ "toe_off",
        frame == TOE_OFF[[1,1]] ~ "first_toe_off",
        frame == FLAT_FOOT[[1,1]] ~ "flat_foot",
        frame == FLAT_FOOT[[2,1]] ~ "last_flat_foot",
        TRUE ~ marks))




  #Generate Jump Phases----
  df <- df %>%
    #Define 4 (toe_off) and 6 (flat_foot)
    dplyr::mutate(
      phase = dplyr::case_when(
        jump_events == "toe_off" ~ 4,
        jump_events == "flat_foot" ~ 6,
        TRUE ~ 5)) %>%
    #Assign:
    #time before 4 as 3 and
    #time after 6 as 7
    dplyr::mutate(
      phase = dplyr::case_when(
        frame < frame[phase == 4] ~ 3,
        frame > frame[phase == 6] ~ 7,
        TRUE ~ phase)) %>%
    #Find the lowest position in phase 3 and give it value 2
    #Find the lowest position in phase 7 and give it value 8
    dplyr::mutate(
      #Generate HAY at
      HAY_during_TAKE_OFF = ifelse(phase == 3, .HAY, 10000),
      HAY_during_LANDING = ifelse(phase == 7, .HAY, 10000),
      phase = ifelse(HAY_during_TAKE_OFF == min(HAY_during_TAKE_OFF), 2, phase),
      phase = ifelse(HAY_during_LANDING == min(HAY_during_LANDING), 8, phase)) %>%
    #Assign:
    #time before 2 as 1
    #time after 8 as 9
    dplyr::mutate(
      phase = dplyr::case_when(
        frame < frame[phase == 2] ~ 1,
        frame > frame[phase == 8] ~ 9,
        TRUE ~ phase)) %>%
    dplyr::select(-HAY_during_TAKE_OFF, -HAY_during_LANDING) %>%
    dplyr::mutate(
      jump_events = dplyr::case_when(
        phase == 5 & .HAY == max(.HAY[phase ==5]) ~ "highest_pos",
        phase == 2 ~ "deepest_prep",
        phase == 8 ~ "deepest_land",
        TRUE ~ jump_events
        ))
  df <- df %>% dplyr::select(-.HAY)

  df
}

# add_jump_length_and_jump_height ----
#' Calculate measures of jump length and jump height
#'
#' Adds the columns \code{jump_length} and \code{jump_height} to a tibble containg mocap data of a jump.
#' @param .data A tibble containing motion-capture data from a jump.\cr
#' The data must contain the following columns:
#' * \code{jump_events} You can create this column using \code{\link{add_jump_events}}
#' * \code{marks} A character column containg one or more of c("TOL", "TOR", "TOB"), AND one or more of c("FFL", "FFR", "FFB")
#' * Global spatial ankle joint center positions in the floor plane: \code{LAX} \code{LAZ} \code{RAX} \code{RAZ}
#' * Global spatial hip joint-center height positions: \code{LHY} \code{RHY}
#'
#' @return The tibble suplied in \code{.data} argument with the added columns \code{jump_length} and \code{jump_height} both measures are in cm.
#' @export
#'
#' @examples
#' # Prepare data
#' df <- dplyr::filter(mocapr::mocapr_data, movement_nr == 1)
#' df <- add_phases_jump(df)
#' df <- mocapr::project_full_body_to_MP(df)
#'
#' add_jump_length_and_height(df)
#'
add_jump_length_and_height <- function(.data, method = "movement_plane"){
  # Avoid "No visible binding for global variable ..." when running check()
  LAZ <- LAX <- RAZ <- RAX <- LHY <- RHY <- jump_events <- phase <- NULL

  # Check method argument
  if(!method %in% c("movement_plane", "global")){
    stop("the method argument must be either 'movement_plane' or 'global'.")
  }


  if(method == "movement_plane") {

    # Check inputs

    if(suppressWarnings(is.null(.data$events_b))) {
      stop(".data is missing the column 'events_b'. You probably need to run 'mocapr::add_phases_jump()' first.")
    }

    if(!any(.data$events_b == "TO" | .data$events_b == "FF", na.rm = TRUE)){
      stop("The column 'events_b' must contain the values 'TO' and 'FF' in order to calculate jump length and jump height")
    }

    if(suppressWarnings(any(is.null(.data$LA_MPF), is.null(.data$RA_MPF)))) {
      stop(".data must contain the columns 'LA_MPF' and 'RA_MPF'. You probably need to run 'mocapr::add_project_full_body_to_MP()' first.")
    }

    if(suppressWarnings(any(is.null(.data$LHY), is.null(.data$RHY)))) {
      stop(".data must contain the columns 'LHY' and 'RHY', these columns contain the height coordinates of the two hip-joints.")
    }


    # In order to calculate the jump length and height we need use the last frame with TO and the first frame with FF
    df <- .data %>%
      dplyr::filter(events_b == "TO" | events_b == "FF") %>%
      dplyr::group_by(events_b) %>%
      dplyr::mutate(
        dummy = dplyr::case_when(
          events_b == "TO" & frame == max(frame) ~ 1,
          events_b == "FF" & frame == min(frame) ~ 1,
          TRUE ~ 0
        )) %>%
      dplyr::ungroup() %>%
      dplyr::filter(dummy == 1)

    # Determine what positions needs to be used for start point and end point for jump.
    # The positions needs to be different based on wheter or not the subject has a bilateral or a unilateral take off and landing
    # finally divide by 10 because output should be in cm
    jump_length_value <- df %>%
      dplyr::mutate(
        position = dplyr::case_when(
          marks == "TOR" ~ RA_MPF,
          marks == "TOL" ~ LA_MPF,
          marks == "TOB" ~ (RA_MPF+LA_MPF)/2,
          marks == "FFR" ~ RA_MPF,
          marks == "FFL" ~ LA_MPF,
          marks == "FFB" ~ (RA_MPF+LA_MPF)/2)) %>%
      dplyr::mutate(
        jump_length = abs(dplyr::first(.$position)-dplyr::last(.$position))/10
        ) %>%
      dplyr::filter(dplyr::row_number() == 1) %>%
      dplyr::pull(jump_length)


    jump_height_value <- .data %>%
      dplyr::select(frame, marks, events_b, phase_b, LHY, RHY) %>%
      dplyr::filter(phase_b == 4 | phase_b == 6) %>%
      dplyr::arrange(frame) %>%
      dplyr::mutate(HAY = (LHY+RHY)/2) %>%
      dplyr::mutate(jump_height = (dplyr::last(.$HAY) - dplyr::first(.$HAY))/10) %>%
      dplyr::slice(1) %>%
      dplyr::pull(jump_height)


    df_return <- .data %>%
      dplyr::mutate(
        jump_length = round(jump_length_value, 1),
        jump_height = round(jump_height_value, 1)
      )

    return(df_return)
  }


  if(method == "global"){
    warning("You are using a method of calcularing jump distance that has been deprecated since mocapr version 1.9007")
    # Check inputs
    if(suppressWarnings(is.null(.data$jump_events))) {
      stop(".data is missing the column 'jump_events'. You probably need to run 'mocapr::add_jump_events()' first.")
    }

    if(!any(.data$jump_events == "toe_off" | .data$jump_events == "flat_foot", na.rm = TRUE)){
      stop("The column 'jump_events' must contain the values 'toe_off' and 'flat_foot' in order to calculate jump length and jump height")
    }

    # Function
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
          start_X = (dplyr::first(LAX) + dplyr::first(RAX)) / 2,
          start_Z = (dplyr::first(LAZ) + dplyr::first(RAZ)) / 2)
    }
    if(df_1$marks[1] == "TOL"){
      start <- df_1 %>%
        dplyr::summarise(
          start_X = dplyr::first(LAX),
          start_Z = dplyr::first(LAZ))
    }
    if(df_1$marks[1] == "TOR"){
      start <- df_1 %>%
        dplyr::summarise(
          start_X = dplyr::first(RAX),
          start_Z = dplyr::first(RAZ))
    }
    #Define end point
    # The value df_1$marks[2] tells what foot or if both has flat-foot contact first when landing
    if(df_1$marks[2] == "FFB"){
      end <- df_1 %>%
        dplyr::summarise(
          end_X = (dplyr::last(LAX) + dplyr::last(RAX)) / 2,
          end_Z = (dplyr::last(LAZ) + dplyr::last(RAZ)) / 2)
    }
    if(df_1$marks[2] == "FFL"){
      end <- df_1 %>%
        dplyr::summarise(
          end_X = dplyr::last(LAX),
          end_Z = dplyr::last(LAZ))
    }
    if(df_1$marks[2] == "FFR"){
      end <- df_1 %>%
        dplyr::summarise(
          end_X = dplyr::last(RAX),
          end_Z = dplyr::last(RAZ))
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
}
