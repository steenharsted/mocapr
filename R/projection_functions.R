#project single joint to the Anatomical Planes(AP)----
#' project_single_joint_to_AP()
#'
#'project_single_joint_to_AP() takes the global 3D coordinates of a single joint and project these coordinates onto the anatomical planes of the subject (the frontal and the sagital plane).
#'The frontal plane is defined as the plane between the two hip joint centers that is perpendicular to the floor. The sagital plane is perpendicular to the frontal
#'and floor plane.\cr
#'As the subject moves the anatomical planes will change with each frame as the pose of the subject changes. This is different to the movement planes created
#'by the project_single_joint_to_MP() function where the planes stay the same througout the movement.
#'Please see the GitHub README.me for a more detailed description.
#'
#' @param .data A tibble containing the global 3D positions of the joints given in the parameters X, Y, Z and the 3D  positions of both hip joints.
#' @param Y The name of the global Y coordinate column (up direction) of the joint you wish to project to the frontal plane
#' @param X The name of the global X coordinate column of the joint you wish to project to the frontal plane
#' @param Z The name of the global Z coordinate column of the joint you wish to project to the frontal plane
#' @param New_Name The abreviated name of the new joint, the name of the returned variables will start with the value given in New_Name
#'
#' @return A tibble containing two columns with coordinates in the right and up direction. The variables are named '"New_Name"_FPR' and '"New_Name"_FPU'
#' @importFrom rlang :=
#' @export
#'
#' @examples
#' df <- dplyr::filter(mocapr::mocapr_data, movement_nr == 1)
#' project_single_joint_to_AP(df, Y=LSY, X=LSX, Z=LSZ, New_Name = "LS")
project_single_joint_to_AP<- function(.data, Y, X, Z, New_Name ="New"){

  # Avoid "No visible binding for global variable ..." when performing check()
  RHX <- RHZ <- LHX <- LHZ <- HAX <- HAY <- HAZ <- NULL
  AP_U_Y <- AP_U_Z <- AP_U_X <- NULL
  AP_R_Z <- AP_R_Y <- AP_R_X <- NULL
  FX <- FY <- FZ <- F_magnitude <- NULL
  AP_F_Z <- AP_F_Y <- AP_F_X <- NULL

  Y <- dplyr::enquo(Y)
  X <- dplyr::enquo(X)
  Z <- dplyr::enquo(Z)
  New_R <- paste0(New_Name, "_APR")
  New_U <- paste0(New_Name, "_APU")
  New_F <- paste0(New_Name, "_APF")

  #Create the frontal plane directions (right positive, Up positive)
  data <- .data %>%
    dplyr::mutate(
      AP_R_X = (RHX-LHX)/sqrt((RHX-LHX)^2+(RHZ-LHZ)^2),
      AP_R_Y = c(0),
      AP_R_Z = (RHZ-LHZ)/sqrt((RHX-LHX)^2+(RHZ-LHZ)^2),
      AP_U_X  = c(0),  #FP up vectors
      AP_U_Y  = c(1),
      AP_U_Z  = c(0),

      #Generate cross product of AP_R and AP_U vectors
      #This gives the direction Forward (forward and up directions combined give the sagital plane)
      FX =  AP_U_Y * AP_R_Z -  AP_U_Z * AP_R_Y,
      FY =  AP_U_Z * AP_R_X -  AP_U_X * AP_R_Z,
      FZ =  AP_U_X * AP_R_Y -  AP_U_Y * AP_R_X,
      F_magnitude = sqrt( FX^2 + FY^2 + FZ^2 ),

      #Sagtial Plane Forward unit Vectors
      AP_F_X  = FX/F_magnitude,
      AP_F_Y  = FY/F_magnitude,
      AP_F_Z  = FZ/F_magnitude)

  # Project the global 3D Joint positions onto the anatomical directions
  data <- data %>%
    dplyr::mutate(
      !!New_R := AP_R_X*!!X + AP_R_Y*!!Y + AP_R_Z*!!Z,
      !!New_U := AP_U_X*!!X + AP_U_Y*!!Y + AP_U_Z*!!Z,
      !!New_F := AP_F_X*!!X + AP_F_Y*!!Y + AP_F_Z*!!Z) %>%
    dplyr::select(New_R, New_U, New_F)

  return(data)
}


#project single joint to the Movement Planes(MP) ----
#project_single_joint_to_MP
#' project_single_joint_to_MP()
#'
#' project_single_joint_to_MP() projects the global joint center positions of a single joint onto the movement planes (MP). MP is calculated by first creating a direction going from
#' the position of the hip joint centers at the first frame to the position of the hip joint centers at the last frame.\cr Please see the GitHub README.me for a
#' more in-depth explanation.
#'
#' @param .data A tibble containing the global 3D positions of the joint given in the parameters X, Y, Z and the 3D  positions of both hip joints.
#' @param Y The name of the global Y coordinate column (up direction) of the joint you wish to project to the movement plane
#' @param X The name of the global X coordinate column of the joint you wish to project to the movement plane
#' @param Z The name of the global Z coordinate column of the joint you wish to project to the movement plane
#' @param New_Name The abreviated name of the new joint, the name of the returned variables will start with the value given in New_Name
#'
#' @return A tibble containig three columns with coordinates in the forward, up, and right direction. The variables are named '"New_Name"_MPF', '"New_Name"_MPU' and '"New_Name"_MPR'
#' @export
#'
#' @examples
#' df <- dplyr::filter(mocapr::mocapr_data, movement_nr == 1)
#' project_single_joint_to_MP(df, Y=LSY, X=LSX, Z=LSZ, New_Name = "LS")
#'
project_single_joint_to_MP <- function(.data, Y, X, Z, New_Name ="New"){

  # Avoid "No visible binding for global variable ..." when performing check()
  HAX <- HAY <- HAZ <- NULL
  key <- value <- NULL
  HAX_Last <- HAX_First <- HAZ_Last <- HAZ_First <- NULL
  MPU_Y <- MPU_Z <- MPU_X <- NULL
  MPF_Y <- MPF_Z <- MPF_X <- NULL
  MPR_Y <- MPR_Z <- MPR_X <- NULL
  SX <- SY <- SZ <- NULL
  S_magnitude <- NULL
  frame <- NULL

  # Capture (enquo) arguments
  Y <- dplyr::enquo(Y)
  X <- dplyr::enquo(X)
  Z <- dplyr::enquo(Z)
  New_F <- paste0(New_Name, "_MPF")
  New_U <- paste0(New_Name, "_MPU")
  New_R <- paste0(New_Name, "_MPR")

  #Create movement plane (MP)
  MP <- .data %>%
    dplyr::filter(frame == min(frame) | frame == max(frame) ) %>%
    dplyr::select(frame, HAX, HAY, HAZ) %>%
    tidyr::gather(key, value, -frame) %>%
    dplyr::mutate(
      frame = dplyr::case_when(
        frame == min(frame) ~ "First",
        frame == max(frame) ~ "Last")) %>%
    dplyr::mutate(key = paste0(key, "_", frame)) %>%
    dplyr::select(-frame) %>%
    tidyr::spread(key, value) %>%
    dplyr::summarise(
      Y = 1,
      X = ( HAX_Last - HAX_First ) / sqrt( (HAX_Last - HAX_First)^2 + (HAZ_Last - HAZ_First)^2 ),
      Z = ( HAZ_Last - HAZ_First ) / sqrt( (HAX_Last - HAX_First)^2 + (HAZ_Last - HAZ_First)^2 ))

  #Create MP directions (forward positive, Up positive)
  data <- .data %>%
    dplyr::mutate(
      MPF_X = MP$X,
      MPF_Y = 0,
      MPF_Z = MP$Z,
      MPU_X  = 0,  #MP up vectors
      MPU_Y  = 1,
      MPU_Z  = 0,

      #Generate cross product of _MDF and _MDU vectors
      #This gives the direction Left
      SX = MPU_Y * MPF_Z - MPU_Z * MPF_Y,
      SY = MPU_Z * MPF_X - MPU_X * MPF_Z,
      SZ = MPU_X * MPF_Y - MPU_Y * MPF_X,
      S_magnitude = sqrt( SX^2 + SY^2 + SZ^2 ),

      #Sagtial Plane Forward Vectors
      MPR_X  = SX/S_magnitude,
      MPR_Y  = SY/S_magnitude,
      MPR_Z  = SZ/S_magnitude)

  # Project the global 3D Joint positions onto the movement directions
  data <- data %>%
    dplyr::mutate(
      !!New_F := MPF_X*!!X + MPF_Y*!!Y + MPF_Z*!!Z,
      !!New_U := MPU_X*!!X + MPU_Y*!!Y + MPU_Z*!!Z,
      !!New_R := MPR_X*!!X + MPR_Y*!!Y + MPR_Z*!!Z) %>%
    dplyr::select(New_F, New_U, New_R)

  return(data)
}


#' project_full_body_to_MP()
#' project_full_body_to_MP() uses project_single_joint_to_MP() to project a pre-specified collection of joint centers onto the movement planes of the subject.
#' The pre-specified joint centers are the following from the left and right side: toe, ankle, knee, hip, wrist, elbow, and shoulder.
#' Please see the GitHub README.md fpr a more detailed description.
#'
#' @param .data A tibble containing 3D positions of the following left and right joints: toe, ankle, knee, hip, wrist, elbow, and shoulder
#'
#' @return A tibble with the positions of the pre-specied joint-centers in the movement planes.
#' @export
#'
#' @examples
#' df <- dplyr::filter(mocapr::mocapr_data, movement_nr == 1)
#' project_full_body_to_MP(df)
project_full_body_to_MP <- function(.data){

  # Avoid "No visible binding for global variable ..." when performing check()
  LSY <- LSX <- LSZ <- LEY <- LEX <- LEZ <- LWY <- LWX <- LWZ <- NULL
  RSY <- RSX <- RSZ <- REY <- REX <- REZ <- RWY <- RWX <- RWZ <- NULL
  LKY <- LKX <- LKZ <- LHY <- LHX <- LHZ <- LAY <- LAX <- LAZ <- LTY <- LTX <- LTZ <- NULL
  RKY <- RKX <- RKZ <- RHY <- RHX <- RHZ <- RAY <- RAX <- RAZ <- RTY <- RTX <- RTZ <- NULL


  dplyr::bind_cols(.data,
                   #Upper extremity
                   project_single_joint_to_MP(.data, Y=LSY, X=LSX, Z=LSZ, New_Name = "LS"),
                   project_single_joint_to_MP(.data, Y=LEY, X=LEX, Z=LEZ, New_Name = "LE"),
                   project_single_joint_to_MP(.data, Y=LWY, X=LWX, Z=LWZ, New_Name = "LW"),
                   project_single_joint_to_MP(.data, Y=RSY, X=RSX, Z=RSZ, New_Name = "RS"),
                   project_single_joint_to_MP(.data, Y=REY, X=REX, Z=REZ, New_Name = "RE"),
                   project_single_joint_to_MP(.data, Y=RWY, X=RWX, Z=RWZ, New_Name = "RW"),
                   #Lower extremity
                   project_single_joint_to_MP(.data,Y=LKY, X=LKX, Z=LKZ, New_Name = "LK"),
                   project_single_joint_to_MP(.data,Y=LHY, X=LHX, Z=LHZ, New_Name = "LH"),
                   project_single_joint_to_MP(.data,Y=LAY, X=LAX, Z=LAZ, New_Name = "LA"),
                   project_single_joint_to_MP(.data,Y=LTY, X=LTX, Z=LTZ, New_Name = "LT"),
                   project_single_joint_to_MP(.data,Y=RKY, X=RKX, Z=RKZ, New_Name = "RK"),
                   project_single_joint_to_MP(.data,Y=RHY, X=RHX, Z=RHZ, New_Name = "RH"),
                   project_single_joint_to_MP(.data,Y=RAY, X=RAX, Z=RAZ, New_Name = "RA"),
                   project_single_joint_to_MP(.data,Y=RTY, X=RTX, Z=RTZ, New_Name = "RT"))
}


#' project_full_body_to_AP()
#' project_full_body_to_AP() uses project_single_joint_to_AP() to project a pre-specified collection of joint centers onto the anatomical planes of the subject.
#' The pre-specified joint centers are the following from the left and right side: toe, ankle, knee, hip, wrist, elbow, and shoulder.
#' Please see the GitHub README.md fpr a more detailed description.
#'
#' @param .data A tibble containing 3D positions of the following left and right joints: toe, ankle, knee, hip, wrist, elbow, and shoulder
#'
#' @return A tibble with the positions of the pre-specied joint-centers in the anatomical planes.
#' @export
#'
#' @examples
#' df <- dplyr::filter(mocapr::mocapr_data, movement_nr == 1)
#' project_full_body_to_AP(df)
project_full_body_to_AP <- function(.data){

  # Avoid "No visible binding for global variable ..." when performing check()
  LSY <- LSX <- LSZ <- LEY <- LEX <- LEZ <- LWY <- LWX <- LWZ <- NULL
  RSY <- RSX <- RSZ <- REY <- REX <- REZ <- RWY <- RWX <- RWZ <- NULL
  LKY <- LKX <- LKZ <- LHY <- LHX <- LHZ <- LAY <- LAX <- LAZ <- LTY <- LTX <- LTZ <- NULL
  RKY <- RKX <- RKZ <- RHY <- RHX <- RHZ <- RAY <- RAX <- RAZ <- RTY <- RTX <- RTZ <- NULL

  dplyr::bind_cols(.data,
                   #Upper extremity
                   project_single_joint_to_AP(.data, Y=LSY, X=LSX, Z=LSZ, New_Name = "LS"),
                   project_single_joint_to_AP(.data, Y=LEY, X=LEX, Z=LEZ, New_Name = "LE"),
                   project_single_joint_to_AP(.data, Y=LWY, X=LWX, Z=LWZ, New_Name = "LW"),
                   project_single_joint_to_AP(.data, Y=RSY, X=RSX, Z=RSZ, New_Name = "RS"),
                   project_single_joint_to_AP(.data, Y=REY, X=REX, Z=REZ, New_Name = "RE"),
                   project_single_joint_to_AP(.data, Y=RWY, X=RWX, Z=RWZ, New_Name = "RW"),
                   #Lower extremity
                   project_single_joint_to_AP(.data,Y=LKY, X=LKX, Z=LKZ, New_Name = "LK"),
                   project_single_joint_to_AP(.data,Y=LHY, X=LHX, Z=LHZ, New_Name = "LH"),
                   project_single_joint_to_AP(.data,Y=LAY, X=LAX, Z=LAZ, New_Name = "LA"),
                   project_single_joint_to_AP(.data,Y=LTY, X=LTX, Z=LTZ, New_Name = "LT"),
                   project_single_joint_to_AP(.data,Y=RKY, X=RKX, Z=RKZ, New_Name = "RK"),
                   project_single_joint_to_AP(.data,Y=RHY, X=RHX, Z=RHZ, New_Name = "RH"),
                   project_single_joint_to_AP(.data,Y=RAY, X=RAX, Z=RAZ, New_Name = "RA"),
                   project_single_joint_to_AP(.data,Y=RTY, X=RTX, Z=RTZ, New_Name = "RT"))
}
