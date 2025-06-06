# add_frontal_plane_knee_angle----
#' Calculate frontal plane knee kinematics in anatomical or movement plane.
#'
#' Positive values of LFPKA and RFPKA reflect lateral deviation of the knee (knee varus).
#'
#' @section Equation:
#' \if{html}{\out{<div style="text-align: center">}\figure{eqFPKA.png}{options: style="width:750px;max-width:90\%;"}\out{</div>}}
#' @section Figure:
#' \if{html}{\out{<div style="text-align: center">}\figure{pcFPKA.png}{options: style="width:750px;max-width:50\%;"}\out{</div>}}
#'
#' @param .data A tibble containing knee and ankle spatial joint center positions.
#' These can be created from global joint center positions using either
#' \code{project_full_body_to_AP()} for anatomical projection or
#' \code{project_full_body_to_MP()} for movement plane projection.
#'
#' @param plane A character string specifying which plane to use for angle calculation.
#' Must be either \code{"anatomical"} (default) or \code{"movement"}.
#' When \code{"anatomical"}, the angle is calculated using the anatomical frontal plane
#' (Y-up, X-right). When \code{"movement"}, the angle is calculated in the task-specific
#' movement frontal plane (UP, RIGHT).
#'
#' @return The tibble supplied in \code{.data}, with added columns:
#' \itemize{
#'   \item \code{LFPKA} – Left Frontal Plane Knee Angle
#'   \item \code{RFPKA} – Right Frontal Plane Knee Angle
#' }
#'
#' @export
#'
#' @references
#' Stone EE, Butler M, McRuer A, Gray A, Marks J, Skubic M.
#' Evaluation of the Microsoft Kinect for screening ACL injury.
#' Conf Proc IEEE Eng Med Biol Soc. 2013;2013:4152-5.
#'
#' Ortiz A, Rosario-Canales M, Rodriguez A, Seda A, Figueroa C, Venegas-Rios HL.
#' Reliability and concurrent validity between two-dimensional and three-dimensional evaluations
#' of knee valgus during drop jumps. Open access journal of sports medicine. 2016;7:65-73.
#'
#' Harsted S, Holsgaard-Larsen A, Hestbaek L, Boyle E, Lauridsen HH.
#' Concurrent validity of lower extremity kinematics and jump characteristics captured in
#' pre-school children by a markerless 3D motion capture system.
#' Chiropr Man Therap. 2019;27:39.
#'
#' @examples
#' # Simulated example for anatomical frontal plane
#' df <- data.frame(
#'   LA_APR = c(10,5,0,-5,-10),
#'   LK_APR = c(0,0,0,0,0),
#'   LA_APU = c(0,0,0,0,0),
#'   LK_APU = c(10,10,10,10,10),
#'   RA_APR = c(20,15,10,5,0),
#'   RK_APR = c(10,10,10,10,10),
#'   RA_APU = c(0,0,0,0,0),
#'   RK_APU = c(10,10,10,10,10)
#' )
#'
#' add_frontal_plane_knee_angle(df, plane = "anatomical")
add_frontal_plane_knee_angle <- function(.data, plane = "anatomical") {
  # Bindings to avoid notes in R CMD check
  LFPKA <- RFPKA <- NULL

  if (!plane %in% c("anatomical", "movement")) {
    stop('`plane` must be one of "anatomical" or "movement"')
  }

  # Determine suffix based on selected plane
  suffix_R <- ifelse(plane == "anatomical", "APR", "MPR")
  suffix_U <- ifelse(plane == "anatomical", "APU", "MPU")

  # Column names based on selected plane
  LA_R <- paste0("LA_", suffix_R)
  LK_R <- paste0("LK_", suffix_R)
  LA_U <- paste0("LA_", suffix_U)
  LK_U <- paste0("LK_", suffix_U)

  RA_R <- paste0("RA_", suffix_R)
  RK_R <- paste0("RK_", suffix_R)
  RA_U <- paste0("RA_", suffix_U)
  RK_U <- paste0("RK_", suffix_U)

  .data %>%
    dplyr::mutate(
      LFPKA = atan((.data[[LA_R]] - .data[[LK_R]]) / (.data[[LK_U]] - .data[[LA_U]])) * 180 / pi,
      RFPKA = atan((.data[[RA_R]] - .data[[RK_R]]) / (.data[[RK_U]] - .data[[RA_U]])) * -180 / pi
    )
}


# add_frontal_plane_projection_angle----
#' add fontal plane projection angle kinematics to a mocap tibble.
#'
#' \code{add_frontal_plane_projection_angle()} adds the frontal plane projection angle to a tibble. Negative values implies dynamic knee valgus.\cr
#'   \code{mocapr} calculates the angle in the anatomical planes rather than using global coordinates. You must, therefore, likely run the function \code{project_full_body_to_AP()} in order
#'   to obtain the joint center positions in the anatomical planes that are nescceary in order to compute the the frontal plane projection angle. \cr
#' \cr
#'  The naming convention for *the frontal plane projection angle* differs in the litterature,
#'  the measure has also been named *the frontal plane **knee** projection angle* and *the frontal plane knee angle*. It was originally described by McLean et. al as
#'  *the frontal plane knee angle*, and calculated using global coordinate positions of lower extremity joint centers.2D vectors were computed from the knee to hip centre (kh)
#'   and from the knee to ankle centre (ka). The cross product and vector norms were computed from the global coordinates, and the frontal plane projection angle was computed as:
#'
#'   \deqn{\Theta = aSin( |kh x ka|  /  |kh||ka| )}
#' \cr
#' *Please note that the frontal plane projection angle has been developed for 2D video analysis of motions that do not involve the height of hip joint being
#' at the level of the knee joint of lower (e.g. single-leg-squats). Due to planar cross talk, the projected kinemtics produce high, and potentially misleading, values when the height of the hip joint
#' approaches that of the knee joint. The frontal plane projeciton kinematics are likely only usefull for analyzing motions that do
#' not involve deep positions of the pelvis (e.g. single-leg squats, gait etc.). \cr Please see examples for further detail.*
#'
#' @section Equation:
#' \if{html}{\out{<div style="text-align: center">}\figure{eqFPPA.png}{options: style="width:750px;max-width:90\%;"}\out{</div>}}
#' @section Figure:
#' \if{html}{\out{<div style="text-align: center">}\figure{pcFPPA.png}{options: style="width:750px;max-width:50\%;"}\out{</div>}}
#'
#' @param .data A tibble that must contain lower extremity spatial joint postions in the anatomical plane (please see \code{project_full_body_to_AP()})
#'
#' @return The tibble supplied in the .data argument with two extra columns (LFPPA and RFPPA).
#' @export
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
#'@references McLean SG, Walker K, Ford KR, Myer GD, Hewett TE, van den Bogert AJ. Evaluation of a two dimensional analysis method as a screening and evaluation tool for anterior cruciate ligament injury. Br J Sports Med. 2005;39(6):355-62.
#' \cr\cr Harsted S, Holsgaard-Larsen A, Hestbaek L, Boyle E, Lauridsen HH. Concurrent validity of lower extremity kinematics and jump characteristics captured in pre-school children by a markerless 3D motion capture system. Chiropr Man Therap. 2019;27:39.
#' \cr\cr Willson JD, Davis IS. Utility of the frontal plane projection angle in females with patellofemoral pain. The Journal of orthopaedic and sports physical therapy. 2008;38(10):606-15.
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
#' \code{add_frontal_plane_knee_deviation()} adds distance measures of medial and lateral knee deviation in the frontal plane to the supplied tibble in the \code{.data.} argument.
#' The measure is calculated in the anatomical frontal plane by drawing a line going through the hip and ankle joints. From this line, the shortest distance to the knee joint is calculated.
#' Negative values indicate medial deviation of the knee (dynamic knee valgus).\cr
#' "Frontal plane knee deviation" has also been described by Krosshaug et. al and Willis et. al as "Medial knee position" (see references).
#' They use the "Medial knee position" to calculate the "Medial knee *deviation*."\cr
#'  \cr
#' Please note that the measure is prone to planar cross talk from especially knee flexion (See examples for details).
#'
#' @section Equation:
#' \if{html}{\out{<div style="text-align: center">}\figure{eqFPKD.png}{options: style="width:750px;max-width:90\%;"}\out{</div>}}
#' @section Figure:
#' \if{html}{\out{<div style="text-align: center">}\figure{pcFPKD.png}{options: style="width:750px;max-width:50\%;"}\out{</div>}}
#'
#'
#' @param .data A tibble containg hip, knee, and ankle spatial joint center positions in the anatomical frontal plane. These positions can be created from
#' global spatial joint center positions using \code{project_full_body_to_AP()}
#' @return The tibble supplied in \code{.data} argument with the added columns \code{LFPKD} and \code{RFPKD}.
#' @export
#' @references Krosshaug T, Steffen K, Kristianslund E, Nilstad A, Mok KM, Myklebust G, et al. The Vertical Drop Jump Is a Poor Screening Test for ACL Injuries in Female Elite Soccer and Handball Players: A Prospective Cohort Study of 710 Athletes. Am J Sports Med. 2016;44(4):874-83.
#' \cr\cr Harsted S, Holsgaard-Larsen A, Hestbaek L, Boyle E, Lauridsen HH. Concurrent validity of lower extremity kinematics and jump characteristics captured in pre-school children by a markerless 3D motion capture system. Chiropr Man Therap. 2019;27:39.
#' \cr\cr Willis BW, Hocker K, Razu S, Gray AD, Skubic M, Sherman SL, et al. Relationship Between 2-Dimensional Frontal Plane Measures and the Knee Abduction Angle During the Drop Vertical Jump. J Sport Rehab. 2019;28(4):399.
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
#' Add knee-to-hip, ankle-to-hip, and knee-to-ankle separation ratios.
#'
#' The ratios are calculated in the anatomical frontal plane, using joint-center positions from both lower extremities. Therefore, they
#' should only be used to analyze bilateral symetric movements (e.g., squats, or bilateral jump-landings).\cr
#' \cr
#' Knee-to-hip separation ratio (KHR) is the distance between the knee joint centers divided by the distance between the hip joint centers.\cr
#' Ankle-to-hip separation ratio (AHR) is the distance between the ankle joint centers divided by the distance between the hip joint centers.\cr
#' Knee-to-ankle separation ratio (KASR) is the distance between the knee joint centers divided by the distance between the ankle joint centers.\cr
#' \cr
#' knee-to-hip and ankle-to-hip separation ratio was orignially described Barber-Westin et al. and Noyes et al. as "Normalised knee separation distance"
#' and "Normalised ankle separation distance". Mizner et. al described the "Knee-to-ankle separation ratio" in 2012. The measures were originally developed for 2D analysis of video recordings.
#'
#' @section Equations:
#' **Knee-to-hip separation ratio:**
#' \if{html}{\out{<div style="text-align: center">}\figure{eqKHR.png}{options: style="width:750px;max-width:90\%;"}\out{</div>}}
#' **Ankle-to-hip separation ratio:**
#' \if{html}{\out{<div style="text-align: center">}\figure{eqAHR.png}{options: style="width:750px;max-width:90\%;"}\out{</div>}}
#' **Knee-to-ankle separation ratio:**
#' \if{html}{\out{<div style="text-align: center">}\figure{eqKASR.png}{options: style="width:750px;max-width:90\%;"}\out{</div>}}
#' @section Figures:
#' **Knee-to-hip separation ratio:**
#' \if{html}{\out{<div style="text-align: center">}\figure{pcKHR.png}{options: style="width:750px;max-width:50\%;"}\out{</div>}}
#' **Ankle-to-hip separation ratio:**
#' \if{html}{\out{<div style="text-align: center">}\figure{pcAHR.png}{options: style="width:750px;max-width:50\%;"}\out{</div>}}
#' **Knee-to-ankle separation ratio:**
#' \if{html}{\out{<div style="text-align: center">}\figure{pcKASR.png}{options: style="width:750px;max-width:50\%;"}\out{</div>}}
#'
#'
#'
#' @param .data A tbbile containg ankle, knee, and hip spatial joint center positions in the anatomical frontal plane. These positions can
#' be generated from global spatial positions using \code{project_full_body_to_AP()}.
#'
#' @return The tibble supplied in \code{.data} argument with the added columns \code{KHR}, \code{AHR}, and \code{KASR}.
#' @export
#' @references Barber-Westin SD, Galloway M, Noyes FR, Corbett G, Walsh C. Assessment of lower limb neuromuscular control in prepubescent athletes. Am J Sports Med. 2005;33(12):1853-60.
#' \cr\cr Noyes FR, Barber-Westin SD, Fleckenstein C, Walsh C, West J. The drop-jump screening test: difference in lower limb control by gender and effect of neuromuscular training in female athletes. Am J Sports Med. 2005;33(2):197-207.
#' \cr\cr Mizner RL, Chmielewski TL, Toepke JJ, Tofte KB. Comparison of 2-dimensional measurement techniques for predicting knee angle and moment during a drop vertical jump. Clin J Sport Med. 2012;22(3):221-7.
#' \cr\cr Harsted S, Holsgaard-Larsen A, Hestbaek L, Boyle E, Lauridsen HH. Concurrent validity of lower extremity kinematics and jump characteristics captured in pre-school children by a markerless 3D motion capture system. Chiropr Man Therap. 2019;27:39.
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
#'ggplot2::geom_text(ggplot2::aes(label = paste0("KASR: ", round(KASR, 2)),
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
      KASR = sqrt( (LK_APR-RK_APR)^2 + (LK_APU-RK_APU)^2 ) / sqrt( (LA_APR-RA_APR)^2 + (LA_APU-RA_APU)^2 ))
}
