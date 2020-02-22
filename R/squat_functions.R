# add_squat_events----

#' Divide a squat movement into phases and events
#'
#' \code{add_squat_events()} takes a tibble as the only argument, and returns the tible with three extra columns useful for further analysis.
#' The three extra columns are: "squat_events", "phase", and "squat_depth".
#' The tibble must contain motioncapture data with the columns \code{frame}, \code{LHY}, and \code{RHY}
#' and the captured motion must be a squat. Please note that the function will not fail if the movement is not a squat.
#'
#'
#' @param .data A tibble containing motioncapture data from a squat and the columns \code{frame}, \code{LHY}, and \code{RHY}.
#'
#' @return A tibble with three added columns. \code{squat_events}, \code{phase}, and \code{squat_depth}. \code{squat_depth} is given in cm.
#' @export
#'
#' @examples
#' # Prepare data
#' df <- dplyr::filter(mocapr::mocapr_data, movement_nr == 1, frame > 20, frame < 70)
#' df <- dplyr::select(df, frame, LHY, RHY)
#'
#' add_squat_events(df)
#'
#' df2 <- data.frame(
#'                   frame = seq(1:11),
#'                   LHY = c(100, 95, 90, 85, 80, 75, 70, 65, 60, 80, 100),
#'                   RHY = c(100, 95, 90, 85, 80, 75, 70, 65, 60, 80, 100))
#' add_squat_events(df2)
add_squat_events <- function(.data){

  #Avoid "No visible binding for global variable ..." when performing check
  LHY <- RHY <- frame <- NULL
  squat_events <- phase <- squat_depth <- squat_descent <- squat_descent_rel <- NULL


  #This function add three variabels to the data
  # squat_events ("mid_point", "deepest_point")
  # phase (1,2,3)
  # squat_depth (a numeric value)
  .data  %>% dplyr::mutate(
    #Define deepest point using hip height averaged over left and right side
    squat_events = dplyr::if_else( LHY+RHY == min(LHY+RHY), "deepest_point", NA_character_),
    #Keep only the first occurence of deepest position if more exist.
    squat_events = replace(squat_events, duplicated(squat_events), NA)) %>%

    #Define phases according to when in the movement the subject is compared to the deepest point
    dplyr::mutate(
      phase = dplyr::if_else( squat_events == "deepest_point", 2, 0, missing = 0)) %>%
    dplyr::mutate(
      phase = dplyr::case_when(
        frame < frame[phase == 2] ~ 1,
        frame > frame[phase == 2] ~ 3,
        TRUE ~ phase)) %>%

    #Calculate squat depth
    dplyr::mutate(
      squat_depth       = ((dplyr::first(RHY)+dplyr::first(LHY)) - min(LHY+RHY))/10,

      #find the mid_point in descent (the point where the child is halfway down)
      #this is the frame were squat_descent is half the size of squat_depth
      squat_descent     = dplyr::if_else(phase == 1, squat_depth - (((LHY+RHY) - min(LHY+RHY)))/10, 0),
      squat_descent_rel = squat_depth / squat_descent) %>%
    dplyr::mutate(
      #find the row were squat_descent is closest to being half the size of squat_depth and
      #mark squat_descent in that row as "mid_descent"
      squat_events = dplyr::if_else( abs(squat_descent_rel - 2) == min( abs( squat_descent_rel - 2) ), "mid_descent", squat_events),

      #Keep only the first occurence of "mid_descent" if more exist.
      squat_events = replace(squat_events, duplicated(squat_events), NA)) %>%

    #Remove the two variabels as they were just used to calculate squat_events
    dplyr::select(-squat_descent, -squat_descent_rel)
}
