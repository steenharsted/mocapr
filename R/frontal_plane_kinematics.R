# add_frontal_plane_knee_angle----
add_frontal_plane_knee_angle <- function(.data){
  .data %>%
    mutate(
      #tan(angle) = opposite/adjacent
      LFPKA = (atan((LA_APR-LK_APR)/(LK_APU - LA_APU))/pi*180),
      RFPKA = (atan((RA_APR-RK_APR)/(RK_APU - RA_APU))/pi*180)*-1)
}

# add_frontal_plane_projection_angle----
add_frontal_plane_projection_angle <- function(.data){
  directions <- .data %>%
    mutate(
      #Left side
      L_Hip_Knee_dR   = LK_APR - LH_APR,
      L_Hip_Knee_dU   = LK_APU - LH_APU,
      L_Knee_Ankle_dR = LA_APR - LK_APR,
      L_Knee_Ankle_dU = LA_APU - LK_APU,
      LFPPA = acos(
        ( (L_Hip_Knee_dR * L_Knee_Ankle_dR) + (L_Hip_Knee_dU * L_Knee_Ankle_dU) ) /
          ( sqrt(L_Hip_Knee_dR^2 + L_Hip_Knee_dU ^2) * (sqrt(L_Knee_Ankle_dR^2 + L_Knee_Ankle_dU ^2 ))) ) /
        pi*180,
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
    select(-L_Hip_Knee_dR, -L_Hip_Knee_dU, -L_Knee_Ankle_dR, -L_Knee_Ankle_dU,
           -R_Hip_Knee_dR, -R_Hip_Knee_dU, -R_Knee_Ankle_dR, -R_Knee_Ankle_dU)

}

# add_frontal_plane_knee_deviation----
add_frontal_plane_knee_deviation <- function(.data){
  .data %>%
    mutate(
      #Frontal Plane
      LFPKD = (    (LA_APU-LH_APU)*LK_APR - (LA_APR-LH_APR)*LK_APU + LA_APR*LH_APU - LA_APU*LH_APR) / sqrt( (LA_APU-LH_APU)^2 + (LA_APR-LH_APR)^2 ),
      RFPKD = -1*( (RA_APU-RH_APU)*RK_APR - (RA_APR-RH_APR)*RK_APU + RA_APR*RH_APU - RA_APU*RH_APR) / sqrt( (RA_APU-RH_APU)^2 + (RA_APR-RH_APR)^2 ))
}
# add_knee_ankle_hip_ratios----
add_knee_anke_hip_ratios <- function(.data){
  .data %>%
    mutate(
      KHR = sqrt( (LK_APR-RK_APR)^2 + (LK_APU-RK_APU)^2 ) / sqrt( (LH_APR-RH_APR)^2 + (LH_APU-RH_APU)^2 ),
      AHR = sqrt( (LA_APR-RA_APR)^2 + (LA_APU-RA_APU)^2 ) / sqrt( (LH_APR-RH_APR)^2 + (LH_APU-RH_APU)^2 ),
      KAR = sqrt( (LK_APR-RK_APR)^2 + (LK_APU-RK_APU)^2 ) / sqrt( (LA_APR-RA_APR)^2 + (LA_APU-RA_APU)^2 ))
}


# add_hip_distance----
add_hip_and_hip_knee_distance <- function(.data){
  .data %>%
    mutate(
      hip_dist      = sqrt(  (LHX-RHX)^2 + (LHY-RHY)^2 + (LHZ-RHZ)^2 ),
      hip_knee_dist = sqrt(  (LKX-LHX)^2 + (LKY-LHY)^2 + (LKZ-LHZ)^2))
}


