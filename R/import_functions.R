# Captury import of new export format
#' Import csv files exported by the Captury Live system version 0.0.168 or later
#'
#' \code{import_captury_csv} takes the filepath and filename of a .csv file containg motion capture data captured and exported using the CapturyLive motion capture system. The
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
#' @examples
#' path <- system.file("examples", "SBJ_new_version.csv", package = "mocapr")
#' import_captury_csv(path)
import_captury_csv <- function(filename, frames_pr_second = 50){

  # Avoid "No visible binding for global variable ..." when performing check()
  X1 <- X3 <- X4 <- X5 <- X6 <- X7 <- X8 <- X9 <- NULL
  X13 <- X14 <- X15 <- X19 <- NULL
  X20 <- X21 <- X25 <- X26 <- X27 <- X31 <- X32 <- X33 <- X37 <- X38 <- X39 <- NULL
  X43 <- X44 <- X45 <- X49 <- X50 <- X51 <- X52 <- X54 <- X55 <- X56 <- X57 <- X58 <- X59 <-  NULL
  X60 <- X61 <- X62 <- X63 <- X64 <- X65 <- X66 <- X67 <- X68 <- X69 <- X73 <- X74 <- X75 <- X76 <- X78 <- X79 <- NULL
  X80 <- X81 <- X82 <- X83 <- X84 <- X85 <- X86 <- X87 <- X88 <- X89 <- X90 <- NULL
  X100 <- X101 <- X102 <- X103 <- X104 <- X105 <- X109 <- X110 <- X111 <- X112 <- X113 <- X114 <- X115 <- X116 <- NULL
  X117 <- X121 <- X122 <- X123 <- X127 <- X128 <- X129 <- X133 <- X134 <- X135 <- X139 <- X140 <- X141 <- X145 <- NULL
  X146 <- X147 <- X151 <- X152 <- X153 <- X91 <- X92 <- X93 <- X97 <- X98 <- X99 <- NULL


  df <- LKF <- RKF <- frame <- LHX <- RHX <- LHY <- RHY <- LHZ <- RHZ <- mocap_system <- time_seconds <- NULL

  # Test input


  # Function
  suppressMessages(suppressWarnings(readr::read_delim(
    paste0(filename), ";", escape_double = FALSE, col_names = FALSE, trim_ws = TRUE, skip = 5))) %>%
    #Captury exports containts camera positions on the last 10 rows of the first 13 columns
    #Remove rows with information on cameara positions
    dplyr::filter(dplyr::row_number() < dplyr::n()-9) %>%
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
                  X88:X90, #Hip Angles - right
                  # variables in the new format
                  X91:X93, #Clavicle positions - Left
                  X97:X99, #Finger positions - Left
                  X100:X102, #ToeEnd - Left
                  X103:X105, #Clavicle positions - right
                  X109:X111, #Finger positions - right
                  X112:X114, #ToeEnd positions - right,
                  X115:X117, #Spine1 positions
                  X121:X123, #Spine2 positions
                  X127:X129, #Spine3 positions
                  X133:X135, #Spine4 positions
                  X139:X141, #Spine5 positions
                  X145:X147, #Neck positions
                  X151:X153  #Head positions
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
                  RHF = X88, RHA = X89, RHRot = X90,
                  # variables in the new format
                  LCX = X91, LCY = X92, LCZ = X93, #Clavicle positions - Left
                  LFX = X97, LFY = X98, LFZ = X99, #Finger positions - Left
                  LTEX = X100, LTEY = X101, LTEZ = X102, #ToeEnd - Left
                  RCX = X103, RCY = X104, RCZ = X105, #Clavicle positions - right
                  RFX = X109, RFY = X110, RFZ = X111, #Finger positions - right
                  RTEX = X112, RTEY = X113, RTEZ = X114, #ToeEnd positions - right,
                  S1X = X115, S1Y = X116, S1Z = X117, #Spine1 positions
                  S2X = X121, S2Y = X122, S2Z = X123, #Spine2 positions
                  S3X = X127, S3Y = X128, S3Z = X129, #Spine3 positions
                  S4X = X133, S4Y = X134, S4Z = X135, #Spine4 positions
                  S5X = X139, S5Y = X140, S5Z = X141, #Spine5 positions
                  S6X = X145, S6Y = X146, S6Z = X147, #Neck positions
                  S7X = X151, S7Y = X152, S7Z = X153, #Head positions
    ) %>%
    dplyr::mutate(
      #Add a collumn with the system name = "Captury" because this is the import_captury_csv() function
      mocap_system = "Captury",
      #Captury exports knee extension - change this to flexion, just because....
      LKF = LKF*(-1),
      RKF = RKF*(-1),
      #Calculate time in seconds
      time_seconds = frame/frames_pr_second) %>%
    dplyr::select(mocap_system, frame, time_seconds, dplyr::everything()) %>%
    dplyr::as_tibble(df)
}

#captury import function----
#' import_captury
#'
#' import_captury() takes the file path and file name of a .csv file containing motion capture data captured and exported using the CapturyLive motion capture system. The
#' .csv file is then imported and cleaned and returned as a tibble. All joint angles and global joint center positions are in abbreviated names (e.g. left knee flexion =
#' LKF, global Y coordinate of the right hip joint is RHY).\cr
#' Please see the GitHub README.me for a more detailed description.
#' @section Note:
#' This function imports Captury .csv files as they are exported from CapturyLive version 0.0.168 and earlier. Newer versions have a different layout
#' and should be imported using \code{import_captury_csv()}.
#'
#' @param filename Path and file name of a .csv file containing motion capture data from the Captury system
#' @param frames_pr_second Recorded frames pr. second used in the setup when capturing the data. Defaults to 50.
#'
#' @return A tibble containing joint angles and global joint center positions of the: toes, ankles, knees, hips, center of gravity, shoulders, elbows, and wrists.
#' @export
#'
#' @examples
#' path <- system.file("examples", "vertical_jump_old_cap_version.csv", package = "mocapr")
#' suppressMessages(import_captury(path))
import_captury <- function(filename, frames_pr_second = 50){
  message("This function imports captury data csv as it was exported prior to version 0.0.168, for newer exports use import_captury_csv()")

  # Avoid "No visible binding for global variable ..." when performing check()
  X1 <- X3 <- X4 <- X5 <- X6 <- X7 <- X8 <- X9 <- NULL
  X13 <- X14 <- X15 <- X19 <- NULL
  X20 <- X21 <- X25 <- X26 <- X27 <- X31 <- X32 <- X33 <- X37 <- X38 <- X39 <- NULL
  X43 <- X44 <- X45 <- X49 <- X50 <- X51 <- X52 <- X54 <- X55 <- X56 <- X57 <- X58 <- X59 <-  NULL
  X60 <- X61 <- X62 <- X63 <- X64 <- X65 <- X66 <- X67 <- X68 <- X69 <- X73 <- X74 <- X75 <- X76 <- X78 <- X79 <- NULL
  X80 <- X81 <- X82 <- X83 <- X84 <- X85 <- X86 <- X87 <- X88 <- X89 <- X90 <- NULL
  LKF <- RKF <- frame <- LHX <- RHX <- LHY <- RHY <- LHZ <- RHZ <- mocap_system <- time_seconds <- NULL

  #Name Variables----
  df <-
    suppressMessages(
    suppressWarnings(
      readr::read_delim(
        paste0(filename), ";", escape_double = FALSE, col_names = FALSE, trim_ws = TRUE, skip = 6))) %>%
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


# DARI import function----

#' import_DARI_bvh
#'
#' import_DARI_bvh takes the file-path and file-name of a .bvh file containing motion capture data exported using the DARI motion capture system. The
#' .bvh file is then imported, cleaned, and returned as a tibble. All global joint center positions are in abbreviated names (e.g. global Y coordinate of the right hip joint is RHY).\cr
#' Please see the GitHub README.me for a more detailed description.
#' @section Note:
#' This function imports .bvh files as they are exported from the DARI motion capture software.
#'
#' @param filename Path and file-name of the .bvh file to be imported
#'
#' @return A tibble containing global joint center positions of the: toes, ankles, knees, hips, clavicles, shoulders, elbows, and wrists.
#' @export
#'
#' @examples
#' \dontrun{
#' path <- system.file("examples", "DARI.bvh", package = "mocapr")
#' suppressMessages(import_DARI_bvh(path))
#' }
import_DARI_bvh <- function(filename) {
  ## Avodi no global binding for....
  LeftArm.Dx <- LeftArm.Dy <- LeftArm.Dz <- LeftFoot.Dx <- LeftFoot.Dy <- NULL
  LeftFoot.Dz <- LeftForeArm.Dx <- LeftForeArm.Dy <- LeftForeArm.Dz <- NULL
  LeftHand.Dx <- LeftHand.Dy <- LeftHand.Dz <-  LeftLeg.Dx <- NULL
  LeftLeg.Dy <- LeftLeg.Dz <- LeftShoulder.Dx <- LeftShoulder.Dy <- NULL
  LeftShoulder.Dz <- LeftToeBase.Dx <- LeftToeBase.Dy <- LeftToeBase.Dz <- NULL
  LeftUpLeg.Dx <- LeftUpLeg.Dy <- LeftUpLeg.Dz <- RightArm.Dx <- NULL
  RightArm.Dy <- RightArm.Dz <- RightFoot.Dx <- RightFoot.Dy <- NULL
  RightFoot.Dz <- RightForeArm.Dx <- RightForeArm.Dy <-  RightForeArm.Dz <- NULL
  RightHand.Dx <- RightHand.Dy <- RightHand.Dz <- RightLeg.Dx <- RightLeg.Dy <- NULL
  RightLeg.Dz <- RightShoulder.Dx <- RightShoulder.Dy <- RightShoulder.Dz <- NULL
  RightToeBase.Dx <- RightToeBase.Dy <- RightToeBase.Dz <- RightUpLeg.Dx <- NULL
  RightUpLeg.Dy <- RightUpLeg.Dz <- NULL

  if (!requireNamespace("RMoCap")){
    stop(
      "\nTo run the import_DARI_bvh() function you first need to install the RMoCap package from https://github.com/browarsoftware/RMoCap\nThe RMoCap package is made by Tomasz Hachaj and Marek R. Ogiela\n
Please consider citing their paper https://doi.org/10.1007/s00530-019-00633-9\n
To install the RMoCap package you can copy and run the following code:\n
            if (!require('RMoCap'))\n
               {\n
               if (!require('devtools'))\n
               {\n
                install.packages('devtools') # if you have not installed 'devtools' package\n
               }\n
               devtools::install_github('browarsoftware/RMoCap')\n
            }\n")
  }

  joint_data <-
    RMoCap::read.bvh(filename)

  joint_data <- joint_data %>%
    RMoCap::bvh.to.df()

  jd_renamed <- joint_data %>% dplyr::rename(
    #Left Upper Extremity
    LCX = LeftShoulder.Dx, #Clavicle x
    LCY = LeftShoulder.Dy, #Clavicle y
    LCZ = LeftShoulder.Dz, #Clavicle z
    LSX = LeftArm.Dx, # Shoulder x
    LSY = LeftArm.Dy, # Shoulder y
    LSZ = LeftArm.Dz, # Shoulder z
    LEX = LeftForeArm.Dx, # Elbow x
    LEY = LeftForeArm.Dy, # Elbow y
    LEZ = LeftForeArm.Dz, # Elbow z
    LWX = LeftHand.Dx, # Wrist x
    LWY = LeftHand.Dy, # Wrist y
    LWZ = LeftHand.Dz, # Wrist z
    #Left Lower Extremity
    LHX = LeftUpLeg.Dx, # Hip x
    LHY = LeftUpLeg.Dy, # Hip y
    LHZ = LeftUpLeg.Dz, # Hip z
    LKX = LeftLeg.Dx, # Knee X
    LKY = LeftLeg.Dy, # Knee y
    LKZ = LeftLeg.Dz, # Knee z
    LAX = LeftFoot.Dx, # Ankle x
    LAY = LeftFoot.Dy, # Ankle y
    LAZ = LeftFoot.Dz, # Ankle z
    LTX = LeftToeBase.Dx, #  Toe x
    LTY = LeftToeBase.Dy, #  Toe y
    LTZ = LeftToeBase.Dz, #  Toe z

    #Right Upper Extremity
    RCX = RightShoulder.Dx, #Clavicle x
    RCY = RightShoulder.Dy, #Clavicle y
    RCZ = RightShoulder.Dz, #Clavicle z
    RSX = RightArm.Dx, # Shoulder x
    RSY = RightArm.Dy, # Shoulder y
    RSZ = RightArm.Dz, # Shoulder z
    REX = RightForeArm.Dx, # Elbow x
    REY = RightForeArm.Dy, # Elbow y
    REZ = RightForeArm.Dz, # Elbow z
    RWX = RightHand.Dx, # Wrist x
    RWY = RightHand.Dy, # Wrist y
    RWZ = RightHand.Dz, # Wrist z
    #Right lower Extremity
    RHX = RightUpLeg.Dx, # Hip x
    RHY = RightUpLeg.Dy, # Hip y
    RHZ = RightUpLeg.Dz, # Hip z
    RKX = RightLeg.Dx, # Knee X
    RKY = RightLeg.Dy, # Knee y
    RKZ = RightLeg.Dz, # Knee z
    RAX = RightFoot.Dx, # Ankle x
    RAY = RightFoot.Dy, # Ankle y
    RAZ = RightFoot.Dz, # Ankle z
    RTX = RightToeBase.Dx, #  Toe x
    RTY = RightToeBase.Dy, #  Toe y
    RTZ = RightToeBase.Dz, #  Toe z
  ) %>%
    dplyr::mutate(
      frame = dplyr::row_number()
    )
  jd_renamed
}
