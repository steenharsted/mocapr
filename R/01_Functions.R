
#captury import function----
#' import_captury
#'
#' Import_Captury() takes the filepath and filename of a .csv file containg motion capture data captured and exported using the CapturyLive motion capture system. The
#' .csv file is then imported and cleaned and returned as a tibble. All joint angles and global joint center positions are in abreviated names (e.g. left knee flexion =
#' LKF, global Y coordinate of the right hip joint is RHY).Please see the GitHub README.me for a more detailed description.
#'
#' @param filename Path and filename of a .csv file containg motion capture data from the Captury system
#' @param frames_pr_second Recorded frames pr. second used in the setup when capuring the data. Defaults to 50.
#'
#' @return A tibble containg joint angles and global joint center positions of the: toes, ankles, knees, hips, center of gravity, shoulders, elbows, and wrists.
#' @export
#'
#' @examples dontrun{}
import_captury <- function(filename, frames_pr_second = 50){

  #Name Variables----
  df <-
    suppressWarnings(
      readr::read_delim(
        paste0(filename), ";", escape_double = FALSE, col_names = FALSE, trim_ws = TRUE, skip = 6)) %>%
    #Captury exports containts camera positions on the last 10 rows of the first columns
    #Remove rows with information on cameara positions
    dplyr::filter(dplyr::row_number() < dplyr::n()-10) %>%
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
      time_Seconds = frame/frames_pr_second,
      #Create average postions of the Hips, we need these to create the jump direction later
      HAX = (LHX+RHX)/2,
      HAY = (LHY+RHY)/2,
      HAZ = (LHZ+RHZ)/2) %>%
    dplyr::select(mocap_system, frame, time_Seconds, dplyr::everything())
  dplyr::as_tibble(df)
}




#project single joint to the Anatomical Planes(AP)----
#' project_single_joint_to_AP()
#'
#'project_single_joint_to_AP() takes the global 3D coordinates of a single joint and project these coordinates onto the anatomical planes of the subject (the frontal and the sagital plane).
#'The frontal plane is defined as the plane between the two hip joint centers that is perpendicular to the floor. The sagital plane is perpendicular to the frontal
#'and floor plane.
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
#' @export
#'
#' @examples dontrun{}
project_single_joint_to_AP<- function(.data, Y, X, Z, New_Name ="New"){
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
#' project_single_joint_to_MP() projects the global joint center positions of a single joint onto the movement plane (MP). MP is calculated by first creating a direction going from
#' the position of the hip joint centers at the first frame to the position of the hip joint centers at the last frame. Please see the GitHub README.me for a
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
#' @examples dontrun{}
project_single_joint_to_MP <- function(.data, Y, X, Z, New_Name ="New"){
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
        #This gives the direction Right
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


#Animate Movement----
#' animate_movement().
#' Please see GitHub README.me for a more detailed description.
#'
#' @param .data A tibble containing joint center positions in the movement plane generated using the project_to_MP() function.
#' @param animate Defaults to TRUE. If false the function will provide a plot faceted on frames.
#' @param ... These parameters are passed to the gganimate::animate() function
#'
#' @return Defaults to an animated gif. Different outputs can be achieved by passing different arguments via ... to the gganimate::animate() function. If animate = FALSE a ggplot plot is returned.
#' @export
#'
#' @examples dontrun{}
  animate_movement <- function(.data, animate = TRUE, ...){

    #Make Data Frame
    df <- .data %>%
      #Select only Frame number and Joint center positions from the joints we wish to plot.
      dplyr::select(frame, dplyr::ends_with("_MPF"), dplyr::ends_with("_MPU"), dplyr::ends_with("_MPR")) %>%

      # Create data for the torso and head (center of hips NH_, center of shoulder NS_, center of cranium NC_)
      dplyr::mutate(
        #Center of Hip
        NH_MPU = (LH_MPU+RH_MPU)/2,
        NH_MPF = (LH_MPF+RH_MPF)/2,
        NH_MPR = (LH_MPR+RH_MPR)/2,
        #Center of shoulder
        NS_MPU = (LS_MPU+RS_MPU)/2,
        NS_MPF = (LS_MPF+RS_MPF)/2,
        NS_MPR = (LS_MPR+RS_MPR)/2,
        #Center of cranium
        NC_MPU = NS_MPU + (NS_MPU-NH_MPU)*0.3,
        NC_MPF = NS_MPF + (NS_MPF-NH_MPF)*0.5,
        NC_MPR = NS_MPR + (NS_MPR-NH_MPR)*0.3,)

    df_plot <- df %>%
      ## Transform data into long format
      tidyr::gather(key, value, - frame) %>%
      tidyr::extract(key, into = c("Joint", "Plane", "Dir"), regex = "(.+)_(MP)(.+)") %>%
      tidyr::spread(Dir, value) %>%
      tidyr::gather(Dir, value, F, R) %>%

      #Create groups for all the joints and extremities. This is needed for ggplot to  connect the correct joints together with geom_path()
      dplyr::mutate(
        Joint = factor(Joint),
        Joint = forcats::fct_relevel(Joint, "LT", "LA", "LK", "LH", "LS", "LE", "LW", "NH", "NS", "NC", "RT", "RA", "RK", "RH", "RS", "RE", "RW"),
        Side =  dplyr::case_when(
          Joint %in% c("LT", "LA", "LK", "LH") ~ "Left Leg",
          Joint %in% c("RT", "RA", "RK", "RH") ~ "Right Leg",
          Joint %in% c("LS", "LE", "LW") ~ "Left Arm",
          Joint %in% c("RS", "RE", "RW") ~ "Right Arm",
          TRUE ~ "No_side"),

        #Create a larger size for the Torso
        Size_Path = dplyr::case_when(
          Joint == "NH" ~ 2,
          TRUE ~ 1),

        #Create a larger size for the Cranium
        Size_Point = dplyr::case_when(
          Joint == "NC" ~ 10,
          TRUE ~ 3)) %>%

      #Arrange the data according to joint. This will make ggplot connect the joints as we wish
      dplyr::arrange(frame, Joint) %>%

      #Lets plot it!
      ggplot2::ggplot(ggplot2::aes(x = value, y = U, group = Side, color = Side))+
        ggplot2::geom_point(ggplot2::aes(size = Size_Point))+
        ggplot2::geom_path(ggplot2::aes(size = Size_Path))+
        ggplot2::ylab("Height (mm)")+
        ggplot2::xlab("(mm)")+
        ggplot2::coord_equal()+
        ggplot2::guides(size = FALSE)+
        ggplot2::theme_bw()+
        ggplot2::theme(
          axis.text.x = ggplot2::element_blank(),
          axis.ticks.x = ggplot2::element_blank(),
          axis.title.x = ggplot2::element_blank(),
          legend.position = "bottom",
          legend.title = ggplot2::element_blank(),
          strip.text.x = ggplot2::element_blank(),
          strip.text.y = ggplot2::element_blank())

    #Animation stuff
    if(animate){
    df_plot <- df_plot +
      ggplot2::facet_grid(cols = dplyr::vars(Dir))+
      gganimate::transition_time(frame) +
      gganimate::ease_aes('linear')

    return(gganimate::animate(df_plot, ...))
    }
    df_plot+
      ggplot2::facet_grid(rows = dplyr::vars(Dir), cols = vars(frame))
  }

##Animate antomical (Function)----
#' animate_anatomical()
#'
#' @param .data A tibble containing joint center positions in the anatomical planes generated using the project_to_AP() function.
#' @param animate Defaults to TRUE. If false the function will provide a plot faceted on frames.
#' @param ... These arguments are passed to the gganimate::animate() function.
#'
#' @return Defaults to an animated gif. Different outputs can be achieved by passing different arguments via ... to the gganimate::animate() function. If animate = FALSE a ggplot plot is returned.
#' @export
#'
#' @examples dontrun{}
  animate_anatomical <- function(.data, animate = TRUE, ...){

    #Make Data Frame
    df <- .data %>%

      #To prevent jitter this section is inserted. It stabilizies the joint center positions around the center
      #of the hip joints. This should be made more generic e.g. vars(ends_with("APR"))
      dplyr::mutate(
        Normaliser_R = RH_APR+(LH_APR-RH_APR)/2,
        Normaliser_F = RH_APF+(LH_APF-RH_APF)/2,
        #Upper exremities
        RS_APR = RS_APR-Normaliser_R,
        LS_APR = LS_APR-Normaliser_R,
        RE_APR = RE_APR-Normaliser_R,
        LE_APR = LE_APR-Normaliser_R,
        RW_APR = RW_APR-Normaliser_R,
        LW_APR = LW_APR-Normaliser_R,
        RS_APF = RS_APF-Normaliser_F,
        LS_APF = LS_APF-Normaliser_F,
        RE_APF = RE_APF-Normaliser_F,
        LE_APF = LE_APF-Normaliser_F,
        RW_APF = RW_APF-Normaliser_F,
        LW_APF = LW_APF-Normaliser_F,
        #Lower extremities
        RH_APR = RH_APR-Normaliser_R,
        LH_APR = LH_APR-Normaliser_R,
        RK_APR = RK_APR-Normaliser_R,
        LK_APR = LK_APR-Normaliser_R,
        RA_APR = RA_APR-Normaliser_R,
        LA_APR = LA_APR-Normaliser_R,
        RT_APR = RT_APR-Normaliser_R,
        LT_APR = LT_APR-Normaliser_R,
        RH_APF = RH_APF-Normaliser_F,
        LH_APF = LH_APF-Normaliser_F,
        RK_APF = RK_APF-Normaliser_F,
        LK_APF = LK_APF-Normaliser_F,
        RA_APF = RA_APF-Normaliser_F,
        LA_APF = LA_APF-Normaliser_F,
        RT_APF = RT_APF-Normaliser_F,
        LT_APF = LT_APF-Normaliser_F) %>%

      #Select only Frame number and Joint center positions from the joints we wish to plot.
      dplyr::select(frame, dplyr::ends_with("_APR"), dplyr::ends_with("_APU"), dplyr::ends_with("_APF")) %>%

      ## Create Sagital plane data (center of hips NH_, center of shoulder NS_, center of cranium NC_)
      dplyr::mutate(
        NH_APU = (LH_APU+RH_APU)/2,
        NH_APR = (LH_APR+RH_APR)/2,
        NH_APF = (LH_APF+RH_APF)/2,
        NS_APU = (LS_APU+RS_APU)/2,
        NS_APR = (LS_APR+RS_APR)/2,
        NS_APF = (LS_APF+RS_APF)/2,
        NC_APU = NS_APU + (NS_APU-NH_APU)*0.3,
        NC_APR = NS_APR + (NS_APR-NH_APR)*0.3,
        NC_APF = NS_APF + (NS_APF-NH_APF)*0.5
      )

    df_plot <- df %>%
      ## Transform data into long format
      tidyr::gather(key, value, - frame) %>%
      tidyr::extract(key, into = c("Joint", "Plane", "Dir"), regex = "(.+)_(AP)(.+)") %>%
      tidyr::spread(Dir, value) %>%
      tidyr::gather(Dir, value, R, F) %>%

      #Create groups for all the joints and extremities. This is needed for ggplot to  connect the correct joints together with geom_path()
      dplyr::mutate(
        Joint = factor(Joint),
        Joint = forcats::fct_relevel(Joint, "LT", "LA", "LK", "LH", "LS", "LE", "LW", "NH", "NS", "NC", "RT", "RA", "RK", "RH", "RS", "RE", "RW"),
        Side =  dplyr::case_when(
          Joint %in% c("LT", "LA", "LK", "LH") ~ "Left Leg",
          Joint %in% c("RT", "RA", "RK", "RH") ~ "Right Leg",
          Joint %in% c("LS", "LE", "LW") ~ "Left Arm",
          Joint %in% c("RS", "RE", "RW") ~ "Right Arm",
          TRUE ~ "No_side"),

        #Create a larger size for the Torso
        Size_Path = dplyr::case_when(
          Joint == "NH" ~ 2,
          TRUE ~ 1),

        #Create a larger size for the Cranium
        Size_Point = dplyr::case_when(
          Joint == "NC" ~ 10,
          TRUE ~ 3)) %>%

      #Arrange the data according to joint. This will make ggplot connect the joints as we wish
      dplyr::arrange(frame, Joint) %>%

      #Lets plot it!
      ggplot2::ggplot(ggplot2::aes(x = value, y = U, group = Side, color = Side))+
        ggplot2::geom_point(ggplot2::aes(size = Size_Point))+
        ggplot2::geom_path(ggplot2::aes(size = Size_Path))+
        ggplot2::ylab("Height (mm)")+
        ggplot2::xlab("(mm)")+
        ggplot2::coord_equal()+
        ggplot2::guides(size = FALSE)+
        ggplot2::theme_bw()+
        ggplot2::theme(
          axis.text.x = ggplot2::element_blank(),
          axis.ticks.x = ggplot2::element_blank(),
          axis.title.x = ggplot2::element_blank(),
          legend.position = "bottom",
          legend.title = ggplot2::element_blank(),
          strip.text.x = ggplot2::element_blank(),
          strip.text.y = ggplot2::element_blank())

    #Animation stuff
    if(animate){
    df_plot <- df_plot +
      ggplot2::facet_grid(cols = dplyr::vars(Dir))+
      gganimate::transition_time(frame) +
      gganimate::ease_aes('linear')
    return(gganimate::animate(df_plot, ...))
    }

    df_plot+
      ggplot2::facet_grid(rows = dplyr::vars(Dir), cols = vars(frame))
  }

#Animate Global----
#' animate_global().
#' Please see GitHub README.me for a more detailed description.
#'
#' @param .data A tibble containing global joint center positions (X, Y, Z).
#' @param animate Defaults to TRUE. If false the function will provide a plot faceted on frames.
#' @param ... These parameters are passed to the gganimate::animate() function
#'
#' @return Defaults to an animated gif. Different outputs can be achieved by passing different arguments via ... to the gganimate::animate() function. If animate = FALSE a ggplot plot is returned.
#' @export
#'
#' @examples dontrun{}
  animate_global <- function(.data, animate = TRUE, ...){
    #   #Make Data Frame
    df <- .data %>%
      #Select only Frame number and Joint center positions from the joints we wish to plot.
      dplyr::select(frame,
                    LTX, LTY, LTZ,
                    LAX, LAY, LAZ,
                    LKX, LKY, LKZ,
                    LHX, LHY, LHZ,
                    LWX, LWY, LWZ,
                    LEX, LEY, LEZ,
                    LSX, LSY, LSZ,
                    RTX, RTY, RTZ,
                    RAX, RAY, RAZ,
                    RKX, RKY, RKZ,
                    RHX, RHY, RHZ,
                    RWX, RWY, RWZ,
                    REX, REY, REZ,
                    RSX, RSY, RSZ,
      ) %>%

      # Create data for the torso and head (center of hips NH_, center of shoulder NS_, center of cranium NC_)
      dplyr::mutate(
        #Center of Hip
        NHY = (LHY + RHY)/2,
        NHX = (LHX + RHX)/2,
        NHZ = (LHZ + RHZ)/2,
        #Center of shoulder
        NSY = (LSY + RSY)/2,
        NSX = (LSX + RSX)/2,
        NSZ = (LSZ + RSZ)/2,
        #Center of cranium
        NCY = NSY + (NSY - NHY)*0.3,
        NCX = NSX + (NSX - NHX)*0.5,
        NCZ = NSZ + (NSZ - NHZ)*0.3)

    df_plot <- df %>%
      ## Transform data into long format
      tidyr::gather(key, value, - frame) %>%
      tidyr::extract(key, into = c("Joint", "Dir"), regex = "(..)(.)") %>%
      tidyr::spread(Dir, value) %>%
      tidyr::gather(Dir, value, X, Z) %>%

      #Create groups for all the joints and extremities. This is needed for ggplot to  connect the correct joints together with geom_path()
      dplyr::mutate(
        Joint = factor(Joint),
        Joint = forcats::fct_relevel(Joint, "LT", "LA", "LK", "LH", "LS", "LE", "LW", "NH", "NS", "NC", "RT", "RA", "RK", "RH", "RS", "RE", "RW"),
        Side =  dplyr::case_when(
          Joint %in% c("LT", "LA", "LK", "LH") ~ "Left Leg",
          Joint %in% c("RT", "RA", "RK", "RH") ~ "Right Leg",
          Joint %in% c("LS", "LE", "LW") ~ "Left Arm",
          Joint %in% c("RS", "RE", "RW") ~ "Right Arm",
          TRUE ~ "No_side"),

        #Create a larger size for the Torso
        Size_Path = dplyr::case_when(
          Joint == "NH" ~ 2,
          TRUE ~ 1),

        #Create a larger size for the Cranium
        Size_Point = dplyr::case_when(
          Joint == "NC" ~ 10,
          TRUE ~ 3)) %>%

      #Arrange the data according to joint. This will make ggplot connect the joints as we wish
      dplyr::arrange(frame, Joint) %>%

      #Lets plot it!
      ggplot2::ggplot(ggplot2::aes(x = value, y = Y, group = Side, color = Side))+
      ggplot2::geom_point(ggplot2::aes(size = Size_Point))+
      ggplot2::geom_path(ggplot2::aes(size = Size_Path))+
      ggplot2::ylab("Height (mm)")+
      ggplot2::xlab("(mm)")+
      ggplot2::coord_equal()+
      ggplot2::guides(size = FALSE)+
      ggplot2::theme_bw()+
      ggplot2::theme(
        axis.text.x = ggplot2::element_blank(),
        axis.ticks.x = ggplot2::element_blank(),
        axis.title.x = ggplot2::element_blank(),
        legend.position = "bottom",
        legend.title = ggplot2::element_blank(),
        strip.text.x = ggplot2::element_blank(),
        strip.text.y = ggplot2::element_blank())

    #Animation stuff
    if(animate){
      df_plot <- df_plot +
        ggplot2::facet_grid(cols = dplyr::vars(Dir))+
        gganimate::transition_time(frame) +
        gganimate::ease_aes('linear')

      return(gganimate::animate(df_plot, ...))
    }
    df_plot+
      ggplot2::facet_grid(rows = dplyr::vars(Dir), cols = vars(frame))
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
#' @examples dontrun{}
  project_full_body_to_MP <- function(.data){
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
  #' @examples dontrun{}
  project_full_body_to_AP <- function(.data){
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
