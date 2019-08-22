#captury import function----
#' import_captury
#'
#' Import_Captury() takes the filepath and filename of a .csv file containg motion capture data captured and exported using the CapturyLive motion capture system. The
#' .csv file is then imported and cleaned and returned as a tibble. All joint angles and global joint center positions are in abreviated names (e.g. left knee flexion =
#' LKF, global Y coordinate of the right hip joint is RHY).\cr
#' Please see the GitHub README.me for a more detailed description.
#'
#' @param filename Path and filename of a .csv file containg motion capture data from the Captury system
#' @param frames_pr_second Recorded frames pr. second used in the setup when capuring the data. Defaults to 50.
#'
#' @return A tibble containg joint angles and global joint center positions of the: toes, ankles, knees, hips, center of gravity, shoulders, elbows, and wrists.
#' @export
#'
#' @examples \dontrun{}
import_captury <- function(filename, frames_pr_second = 50){
  print("This function imports captury data csv as it was exported prior to version 0.0.156, for newer exports use import_captury_csv()")

  #Name Variables----
  df <-
    suppressWarnings(
      readr::read_delim(
        paste0(filename), ";", escape_double = FALSE, col_names = FALSE, trim_ws = TRUE, skip = 6)) %>%
    #Captury exports containts camera positions on the last 10 rows of the first 13 columns
    #Remove rows with information on cameara positions
    dplyr::filter(dplyr::row_number() < dplyr::n()-10) %>%
    #Convert the remaing data in the columns into numeric
    dplyr::mutate_at(c("X1", "X4", "X5", "X6", "X7", "X8", "X9", "X10", "X11", "X12", "X13"), list(as.numeric)) %>%
    #Select the columns of interest
    dplyr::select(X1,      #Frame
                  X3,      #Annotations
                  X4 :X6,  #CG
                  X7 :X9,  #Wrist Positions - left
                  X13:X15, #Elbow positions - left
                  X19:X21, #Shoulder Positions - left
                  X25:X27, #Wrist Positions - right
                  X31:X33, #Elbow positions - right
                  X37:X39, #Shoulder Positions - right
                  X43:X45, #Toe positions - left
                  X49:X51, #Ankle Positions - left
                  X52:X54, #Ankle Angles - left
                  X55:X57, #Knee Positions - left
                  X58:X60, #Knee Angles - left
                  X61:X63, #Hip Positions - left
                  X64:X66, #Hip Angles - left
                  X67:X69, #Toe Positions - right
                  X73:X75, #Annkle Positions - right
                  X76:X78, #Ankle Angles - right
                  X79:X81, #Knee Positions - right
                  X82:X84, #Knee Angles - right
                  X85:X87, #Hip Positions - right
                  X88:X90  #Hip Angles - right
    ) %>%
    dplyr::rename(frame = X1, marks = X3,
                  CGX = X4,  CGY = X5,  CGZ = X6,  #Center of Gravity
                  LWX = X7,  LWY = X8,  LWZ = X9,  #Left Wrist
                  LEX = X13, LEY = X14, LEZ = X15, #Left Elbow
                  LSX = X19, LSY = X20, LSZ = X21, #Left Shoulder
                  RWX = X25, RWY = X26, RWZ = X27, #Right Wrist
                  REX = X31, REY = X32, REZ = X33, #Right Elbow
                  RSX = X37, RSY = X38, RSZ = X39, #Right Shoulder
                  LTX = X43, LTY = X44, LTZ = X45, #Left Toe
                  LAX = X49, LAY = X50, LAZ = X51, #Left Ankle
                  LADF = X52,                      #Left Anle Dorsi Flexion
                  LKX = X55, LKY = X56, LKZ = X57, #Left Knee
                  LKF = X58, LKVarus = X59, LKRot = X60, #Left knee Flexion, Varus, and Roation
                  LHX = X61, LHY = X62, LHZ = X63, #Left Hip
                  LHF = X64, LHA = X65, LHRot = X66, #Left Hip Flexion, Abduction, Rotation
                  RTX = X67, RTY = X68, RTZ = X69, #Right Toe
                  RAX = X73, RAY = X74, RAZ = X75, #Right Ankle
                  RADF = X76,                      #Right Ankle Dorsi Flexion
                  RKX = X79, RKY = X80, RKZ = X81, #Right Knee
                  RKF = X82, RKVarus = X83, RKRot = X84, #Right knee flexion, Varus, roation
                  RHX = X85, RHY = X86, RHZ = X87, #Right Hip
                  RHF = X88, RHA = X89, RHRot = X90) %>% #Right Hip flexion, abduction, rotation
    dplyr::mutate(
      #Add a collumn with the system name = "Captury" because this is the Import_Captury() function
      mocap_system = "Captury",
      #Captury exports knee extension - change this to flexion, just because....
      LKF = LKF*(-1),
      RKF = RKF*(-1),
      #Calculate time in seconds
      time_seconds = frame/frames_pr_second,
      #Create average postions of the Hips, we need these to create the jump direction later
      HAX = (LHX+RHX)/2,
      HAY = (LHY+RHY)/2,
      HAZ = (LHZ+RHZ)/2) %>%
    dplyr::select(mocap_system, frame, time_seconds, dplyr::everything())
  dplyr::as_tibble(df)
}
