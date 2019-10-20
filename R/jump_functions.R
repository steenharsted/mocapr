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

  # Avoid "No visible binding for global variable ..." when performing check()
  LHY <- RHY <- marks <- frame <- phase <- .HAY <- NULL
  HAY_during_TAKE_OFF <- HAY_during_LANDING <- NULL

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
    stop("add_jump_events() requires the column marks to contain one row with ´TOB´, or one row with `TOL` and one row with `TOR`")
  }

  if(.test[["impacts"]] != 2) {
    stop("add_jump_events() requires the column marks to contain one row with ´FFB´, or one row with `FFL` and one row with `FFR`")
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
#' df <- dplyr::select(df, frame, marks, LHY, RHY, LAX, LAZ, RAX, RAZ)
#' df <- add_jump_events(df)
#'
#' add_jump_length_and_height(df)
#'
add_jump_length_and_height <- function(.data){

  # Avoid "No visible binding for global variable ..." when running check()
  LAZ <- LAX <- RAZ <- RAX <- LHY <- RHY <- jump_events <- phase <- NULL

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
