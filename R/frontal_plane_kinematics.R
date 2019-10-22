# add_frontal_plane_knee_angle----
#' Calculate frontal plane knee kinematics.
#' Positive values of LFPKA and RFPKA reflects lateral deviation of the knee (knee varus).
#'
#' @param .data A tibble containg knee and ankle spatial joint center positions in the anatomical frontal plane. These positions can be created from
#' global spatial joint center positions using \code{project_full_body_to_AP()}
#'
#' @return The tibble supplied in \code{.data} argument with the added columns \code{LFPKA} and \code{RFPKA}.
#' @export
#'
#' @examples
#' # Prepare data
#' df <- data.frame(
#'                  LA_APR = c(10,5,0,-5,-10),
#'                  LK_APR = c(0,0,0,0,0),
#'                  LA_APU = c(0,0,0,0,0),
#'                  LK_APU = c(10,10,10,10,10),
#'                  RA_APR = c(20,15,10,5,0),
#'                  RK_APR = c(10,10,10,10,10),
#'                  RA_APU = c(0,0,0,0,0),
#'                  RK_APU = c(10,10,10,10,10))
#'
#' add_frontal_plane_knee_angle(df)
add_frontal_plane_knee_angle <- function(.data){
  # Avoid  "no visible binding for global variable ..." when running check()
  LFPKA <- LA_APR <- LK_APR <- LK_APU <- LA_APU <- NULL
  RFPKA <- RA_APR <- RK_APR <- RK_APU <- RA_APU <- NULL

  .data %>%
    dplyr::mutate(
      #tan(angle) = opposite/adjacent
      LFPKA = (atan((LA_APR-LK_APR)/(LK_APU - LA_APU))/pi*180),
      RFPKA = (atan((RA_APR-RK_APR)/(RK_APU - RA_APU))/pi*180)*-1)
}

# add_frontal_plane_projection_angle----
#' add fontal plane projection angle kinematics to a mocap tibble
#'
#' \code{add_frontal_plane_projection_angle()} adds the frontal plane projection angle to a tibble.
#' \cr\cr*Please note that the frontal plane projection angle has been developed for 2D video analysis of motions that do not involve the height of hip joint being
#' at the level of the knee joint of lower (e.g. single-leg-squats). Due to planar cross talk, the projected kinemtics produce high, and potentially misleading, values when the height of the hip joint
#' approaches that of the knee joint. The frontal plane projeciton kinematics are likely only usefull for analyzing motions that do
#' not involve deep positions of the pelvis (e.g. single-leg squats, gait etc.). \cr Please see examples for further detail.*
#'
#' @param .data A tibble that must contain lower extremity spatial joint postions in the anatomical plane (please see \code{project_full_body_to_AP()})
#'
#' @return The tibble supplied in the .data argument with two extra columns (LFPPA and RFPPA).
#' @export
#'
#' @examples
#' # Prepare data
#' df <- data.frame(
#'                  LH_APR = c(10,10,10, 0,10,20),
#'                  LK_APR = c(10,10,10,10,10,10),
#'                  LA_APR = c( 0,10,20,10,10,10),
#'                  LH_APU = c(30,30,30,30,30,30),
#'                  LK_APU = c(10,10,10,10,10,10),
#'                  LA_APU = c( 0, 0, 0, 0, 0, 0),
#'                  RH_APR = c(10,10,10, 0,10,20),
#'                  RK_APR = c(10,10,10,10,10,10),
#'                  RA_APR = c( 0,10,20,10,10,10),
#'                  RH_APU = c(30,30,30,30,30,30),
#'                  RK_APU = c(10,10,10,10,10,10),
#'                  RA_APU = c( 0, 0, 0, 0, 0, 0))
#'
#'add_frontal_plane_projection_angle(df)
#'
#'# Example to view L and R FPPA
#'df <- mocapr::mocapr_synthetic_data
#'df <- dplyr::filter(df, sample == 2)
#'df <- add_frontal_plane_projection_angle(df)
#'animate_anatomical(df,
#'                   planes = "R",
#'                   return_plot = TRUE,
#'                   col_facets = col_fa,
#'                   row_facets = row_fa)+
#'    ggplot2::geom_text(ggplot2::aes(label = paste0("LFPPA: ", round(LFPPA, 1)),
#'                       x = mean(value)-15, y= max(U)+10), color = "black", size = 3)+
#'    ggplot2::geom_text(ggplot2::aes(label = paste0("RFPPA: ", round(RFPPA, 1)),
#'                       x = mean(value)+15, y= max(U)+10), color = "black", size = 3)
#'
#'# Explore the planar cross talk at deep positions of the hip-joint centers.

#'df2 <- dplyr::filter(mocapr::mocapr_synthetic_data, sample == 2)
#'df2 <- add_frontal_plane_knee_angle(df2)
#'df2 <- add_frontal_plane_knee_deviation(df2)
#'df2 <- add_frontal_plane_projection_angle(df2)
#'animate_anatomical(df2,
#'                   planes = "R",
#'                   return_plot = TRUE,
#'                   col_facets = col_fa,
#'                   row_facets = row_fa)+
#'ggplot2::geom_text(ggplot2::aes(label = paste0("LFPPA: ", round(LFPPA, 1)),
#'                   x = mean(value)-15, y= max(U)+30), color = "black", size = 3)+
#'ggplot2::geom_text(ggplot2::aes(label = paste0("RFPPA: ", round(RFPPA, 1)),
#'                   x = mean(value)+15, y= max(U)+30), color = "black", size = 3)+
#'ggplot2::geom_text(ggplot2::aes(label = paste0("LFPKA: ", round(LFPKA, 1)),
#'                   x = mean(value)-15, y= max(U)+20), color = "black", size = 3)+
#'ggplot2::geom_text(ggplot2::aes(label = paste0("RFPKA: ", round(RFPKA, 1)),
#'                   x = mean(value)+15, y= max(U)+20), color = "black", size = 3)+
#'ggplot2::geom_text(ggplot2::aes(label = paste0("LFPKD: ", round(LFPKD, 1)),
#'                   x = mean(value)-15, y= max(U)+10), color = "black", size = 3)+
#'ggplot2::geom_text(ggplot2::aes(label = paste0("RFPKD: ", round(RFPKD, 1)),
#'                   x = mean(value)+15, y= max(U)+10), color = "black", size = 3)
#'
add_frontal_plane_projection_angle <- function(.data){
  # Avoid "No visible binding for global variable ..." when running check()
  LH_APU <- LK_APU <- LA_APU <- LH_APR <- LK_APR <- LA_APR <- NULL
  RH_APU <- RK_APU <- RA_APU <- RH_APR <- RK_APR <- RA_APR <- NULL
  L_Hip_Knee_dR <- R_Hip_Knee_dR <- L_Knee_Ankle_dR <- R_Knee_Ankle_dR <- NULL
  L_Hip_Knee_dU <- R_Hip_Knee_dU <- L_Knee_Ankle_dU <- R_Knee_Ankle_dU <- NULL
  LK_lat <- RK_lat <- LFPPA <- RFPPA <- NULL

  # Function
  .data %>%
    dplyr::mutate(
      #Left side
      L_Hip_Knee_dR   = LK_APR - LH_APR,
      L_Hip_Knee_dU   = LK_APU - LH_APU,
      L_Knee_Ankle_dR = LA_APR - LK_APR,
      L_Knee_Ankle_dU = LA_APU - LK_APU,
      LFPPA = acos(
        ( (L_Hip_Knee_dR * L_Knee_Ankle_dR) + (L_Hip_Knee_dU * L_Knee_Ankle_dU) ) /
          ( sqrt(L_Hip_Knee_dR^2 + L_Hip_Knee_dU ^2) * (sqrt(L_Knee_Ankle_dR^2 + L_Knee_Ankle_dU ^2 ))) ) /
        pi*180*-1,
      #Right side
      R_Hip_Knee_dR   = RK_APR - RH_APR,
      R_Hip_Knee_dU   = RK_APU - RH_APU,
      R_Knee_Ankle_dR = RA_APR - RK_APR,
      R_Knee_Ankle_dU = RA_APU - RK_APU,
      RFPPA = acos(
        ( (R_Hip_Knee_dR * R_Knee_Ankle_dR) + (R_Hip_Knee_dU * R_Knee_Ankle_dU) ) /
          ( sqrt(R_Hip_Knee_dR^2 + R_Hip_Knee_dU ^2) * (sqrt(R_Knee_Ankle_dR^2 + R_Knee_Ankle_dU ^2 ))) ) /
        pi*180*-1
    ) %>%

    # Determine if knee is medial or lateral to the hip-ankle line
    dplyr::mutate(
      LK_lat  = 0 > (   (LA_APU-LH_APU)*LK_APR - (LA_APR-LH_APR)*LK_APU + LA_APR*LH_APU - LA_APU*LH_APR) / sqrt( (LA_APU-LH_APU)^2 + (LA_APR-LH_APR)^2 ),
      RK_lat  = 0 > -1*((RA_APU-RH_APU)*RK_APR - (RA_APR-RH_APR)*RK_APU + RA_APR*RH_APU - RA_APU*RH_APR) / sqrt( (RA_APU-RH_APU)^2 + (RA_APR-RH_APR)^2 )
    ) %>%

    # Change sign of LFPPA and RFPPA if knee is not medial
    dplyr::mutate(
      LFPPA = dplyr::if_else(!LK_lat & LFPPA < 0, LFPPA *-1, LFPPA),
      RFPPA = dplyr::if_else(!RK_lat & RFPPA < 0, RFPPA *-1, RFPPA)
    ) %>%

    # Remove abundent variables
    dplyr::select(
      -L_Hip_Knee_dR, -L_Hip_Knee_dU, -L_Knee_Ankle_dR, -L_Knee_Ankle_dU,
      -R_Hip_Knee_dR, -R_Hip_Knee_dU, -R_Knee_Ankle_dR, -R_Knee_Ankle_dU,
      -LK_lat, -RK_lat)
}

# add_frontal_plane_knee_deviation----
#' Add measures of medial and lateral knee deviation
#'
#'\code{add_frontal_plane_knee_deviation()} adds distance measures of medial and lateral knee deviation in the frontal plane to the supplied tibble in the \code{.data.} argument.
#'The measure is calculated by drawing a line going through the hip and ankle joints. From this line, the shortest distance to the knee joint is calculated.
#'Negative values indicate medial deviation of the knee.
#'\cr\cr*Please note that the measure is prone to planar cross talk from especially knee flexion. \cr See examples for details.*
#'
#' @param .data A tibble containg hip, knee, and ankle spatial joint center positions in the anatomical frontal plane. These positions can be created from
#' global spatial joint center positions using \code{project_full_body_to_AP()}
#' @return The tibble supplied in \code{.data} argument with the added columns \code{LFPKD} and \code{RFPKD}.
#' @export
#'
#' @examples
#' #' # Prepare data
#' df <- data.frame(
#'                  LH_APR = c(10,10,10, 0,10,20),
#'                  LK_APR = c(10,10,10,10,10,10),
#'                  LA_APR = c( 0,10,20,10,10,10),
#'                  LH_APU = c(30,30,30,30,30,30),
#'                  LK_APU = c(10,10,10,10,10,10),
#'                  LA_APU = c( 0, 0, 0, 0, 0, 0),
#'                  RH_APR = c(10,10,10, 0,10,20),
#'                  RK_APR = c(10,10,10,10,10,10),
#'                  RA_APR = c( 0,10,20,10,10,10),
#'                  RH_APU = c(30,30,30,30,30,30),
#'                  RK_APU = c(10,10,10,10,10,10),
#'                  RA_APU = c( 0, 0, 0, 0, 0, 0))
#'
#'add_frontal_plane_knee_deviation(df)
#'
#'# Explore the planar cross talk at deep positions of the hip-joint centers.
#'
#'df2 <- dplyr::filter(mocapr::mocapr_synthetic_data, sample == 2)
#'df2 <- add_frontal_plane_knee_angle(df2)
#'df2 <- add_frontal_plane_knee_deviation(df2)
#'df2 <- add_frontal_plane_projection_angle(df2)
#'animate_anatomical(df2,
#'                   planes = "R",
#'                   return_plot = TRUE,
#'                   col_facets = col_fa,
#'                   row_facets = row_fa)+
#'ggplot2::geom_text(ggplot2::aes(label = paste0("LFPPA: ", round(LFPPA, 1)),
#'                   x = mean(value)-15, y= max(U)+30), color = "black", size = 3)+
#'ggplot2::geom_text(ggplot2::aes(label = paste0("RFPPA: ", round(RFPPA, 1)),
#'                   x = mean(value)+15, y= max(U)+30), color = "black", size = 3)+
#'ggplot2::geom_text(ggplot2::aes(label = paste0("LFPKA: ", round(LFPKA, 1)),
#'                   x = mean(value)-15, y= max(U)+20), color = "black", size = 3)+
#'ggplot2::geom_text(ggplot2::aes(label = paste0("RFPKA: ", round(RFPKA, 1)),
#'                   x = mean(value)+15, y= max(U)+20), color = "black", size = 3)+
#'ggplot2::geom_text(ggplot2::aes(label = paste0("LFPKD: ", round(LFPKD, 1)),
#'                   x = mean(value)-15, y= max(U)+10), color = "black", size = 3)+
#'ggplot2::geom_text(ggplot2::aes(label = paste0("RFPKD: ", round(RFPKD, 1)),
#'                   x = mean(value)+15, y= max(U)+10), color = "black", size = 3)
#'
add_frontal_plane_knee_deviation <- function(.data){
  # Avoid "No visible binding for global variable ..." when running check()
  LH_APU <- LK_APU <- LA_APU <- LH_APR <- LK_APR <- LA_APR <- NULL
  RH_APU <- RK_APU <- RA_APU <- RH_APR <- RK_APR <- RA_APR <- NULL

  .data %>%
    dplyr::mutate(
      #Frontal Plane
      LFPKD = (    (LA_APU-LH_APU)*LK_APR - (LA_APR-LH_APR)*LK_APU + LA_APR*LH_APU - LA_APU*LH_APR) / sqrt( (LA_APU-LH_APU)^2 + (LA_APR-LH_APR)^2 ),
      RFPKD = -1*( (RA_APU-RH_APU)*RK_APR - (RA_APR-RH_APR)*RK_APU + RA_APR*RH_APU - RA_APU*RH_APR) / sqrt( (RA_APU-RH_APU)^2 + (RA_APR-RH_APR)^2 ))
}

# add_knee_ankle_hip_ratios----
#' Add knee-to-hip, ankle-to-hip, and knee-to-ankle separation distance ratios.
#'
#' The ratios are calculated in the anatomical frontal plane. The ratios are calculated using both lower extremities. Therefore, they
#' should only be used to analyze bilateral symetric movements (e.g., squats, or bilateral jump-landings).
#'
#' @param .data A tbbile containg ankle, knee, and hip spatial joint center positions in the anatomical frontal plane. These positions can
#' be generated from global spatial positions using \code{project_full_body_to_AP()}.
#'
#' @return The tibble supplied in \code{.data} argument with the added columns \code{KHR}, \code{AHR}, and \code{KAR}.
#' @export
#'
#' @examples
#' # Prepare data
#'df <- dplyr::filter(mocapr::mocapr_synthetic_data, sample == 1)
#'add_knee_ankle_hip_ratios(df)
#'
#'# The ratio measures are unaffected by the depth of the pelvis.
#'
#'df2 <- dplyr::filter(mocapr::mocapr_synthetic_data, sample == 2)
#'df2 <- add_knee_ankle_hip_ratios(df2)
#'animate_anatomical(df2,
#'                   planes = "R",
#'                   return_plot = TRUE,
#'                   col_facets = col_fa,
#'                   row_facets = row_fa)+
#'ggplot2::geom_text(ggplot2::aes(label = paste0("KHR: ", round(KHR, 2)),
#'                   x = mean(value), y= max(U)+30), color = "black", size = 3)+
#'ggplot2::geom_text(ggplot2::aes(label = paste0("AHR: ", round(AHR, 2)),
#'                   x = mean(value), y= max(U)+20), color = "black", size = 3)+
#'ggplot2::geom_text(ggplot2::aes(label = paste0("KAR: ", round(KAR, 2)),
#'                   x = mean(value), y= max(U)+10), color = "black", size = 3)
add_knee_ankle_hip_ratios <- function(.data){
  # Avoid "No visible binding for global variable ..." when running check()
  LH_APU <- LK_APU <- LA_APU <- LH_APR <- LK_APR <- LA_APR <- NULL
  RH_APU <- RK_APU <- RA_APU <- RH_APR <- RK_APR <- RA_APR <- NULL

  # Function
  .data %>%
    dplyr::mutate(
      KHR = sqrt( (LK_APR-RK_APR)^2 + (LK_APU-RK_APU)^2 ) / sqrt( (LH_APR-RH_APR)^2 + (LH_APU-RH_APU)^2 ),
      AHR = sqrt( (LA_APR-RA_APR)^2 + (LA_APU-RA_APU)^2 ) / sqrt( (LH_APR-RH_APR)^2 + (LH_APU-RH_APU)^2 ),
      KAR = sqrt( (LK_APR-RK_APR)^2 + (LK_APU-RK_APU)^2 ) / sqrt( (LA_APR-RA_APR)^2 + (LA_APU-RA_APU)^2 ))
}
