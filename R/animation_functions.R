#Animate Movement----
#' Animate motioncapture data in the planes of the movement.
#'
#' \code{animate_movement()} animates motioncapture data that is projected onto the planes of the movment using \code{mocapr::project_full_body_to_MP()}.
#' Each frame contains a view from the side and front of the direction of movement.
#' The definitions of front and side are dictated by a direction going from the position of the subject at first frame to the position of the subject
#' at the last frame of the movement.
#'
#' @param .data A tibble containing joint center positions in the anatomical planes generated using the project_to_MP() function.
#' @param planes Planes to animate. Must be one or both of c("R", "F"). The planes are created by combining the suplied direction with the up-direction. Defaults to c("R", "F").
#' @param line_colored_size The size of the colored lines connecting the joint centers.
#' @param line_black_size The size of the black lines connecting the joint centers.
#' @param point_size The size of the joint centers when \code{use_geom_point = TRUE}.
#' @param circle_size The size of the joint centers when \code{use_geom_point = FALSE}.
#' @param head_scale The size of the head relative to the joint-centers.
#' @param torso_scale The size of the torso line relative to the remaing lines.
#' @param return_data Return the wrangled data before it is sent to ggplot. This is useful for understanding the data-structure and for developmental purposes.
#' @param return_plot Return a plot instead of an animaiton. This is useful for customizing the plot before passing it to gganimate::
#' @param reduce_data Defaults to FALSE. If TRUE the function will reduce the input data to only include the variables that are needed for the plot or animation.This may improve performance slightly.
#'
#' @inheritParams mocap_plot_basic
#' @inheritParams mocap_plot_avatar
#' @inheritParams mocap_finish_animation
#'
#' @return Defaults to an animated gif. Different outputs can be achieved by passing different arguments via ... to the gganimate::animate() function. If return_plot = TRUE a ggplot plot is returned. If return_data = TRUE a tibble is returned.
#' @export
#'
#' @examples
#' # Prepare data
#' df <- dplyr::filter(mocapr::mocapr_data, movement_nr == 1)
#' df <- project_full_body_to_MP(df)
#' df <- dplyr::filter(df, frame %in% c(1, 50, 100, 150, 200))
#'
#' # Return wrangled data
#' mocapr::animate_movement(df, return_data = TRUE)
#'
#' # Return plot factted on frame
#' mocapr::animate_movement(df,
#'   return_plot = TRUE,
#'   planes_in_rows_or_cols = "rows",
#'   col_facets = frame
#'   )
#'
#' # Same as above, but use ggforce::geom_circle()
#' mocapr::animate_movement(df,
#'   return_plot = TRUE,
#'   use_geom_point = FALSE,
#'   planes_in_rows_or_cols = "rows",
#'   col_facets = frame
#'   )
#'
  animate_movement <- function(.data,
                               planes = c("R", "F"),
                               planes_in_rows_or_cols = c("cols"),
                               row_facets = NULL,
                               col_facets = NULL,
                               subject = NULL,
                               remove_facet_labels = TRUE,
                               use_geom_point = TRUE,
                               line_colored = TRUE,
                               line_colored_size = 1,
                               line_colored_alpha = 1,
                               line_black = TRUE,
                               line_black_size = 1,
                               line_black_alpha = 0.5,
                               point = TRUE,
                               point_size = 1.5,
                               circle_size = 40,
                               point_alpha = 1,
                               head_scale = 2,
                               torso_scale = 1.5,
                               return_data = FALSE,
                               return_plot = FALSE,
                               reduce_data = FALSE,
                               ...){

    # Avoid "No visible binding for global variable ...." in check
    RH_MPR <- LH_MPR <- RH_MPF <- LH_MPF <- RH_MPU <- LH_MPU <- NULL
    RK_MPR <- LK_MPR <- RK_MPF <- LK_MPF <- RK_MPU <- LK_MPU <- NULL
    RA_MPR <- LA_MPR <- RA_MPF <- LA_MPF <- RA_MPU <- LA_MPU <- NULL
    RT_MPR <- LT_MPR <- RT_MPF <- LT_MPF <- RT_MPU <- LT_MPU <- NULL
    RS_MPR <- LS_MPR <- RS_MPF <- LS_MPF <- RS_MPU <- LS_MPU <- NULL
    NS_MPR <- NS_MPF <- NS_MPU <- NULL
    NH_MPR <- NH_MPF <- NH_MPU <- NULL
    frame <- key <- value <- Dir <- R <- Joint <- Side <- Side_frame <- U <- NULL
    size_path_color <- size_point <- size_path_black <- size_circle <- NULL

    #Make Data Frame
    df <- .data


    if(reduce_data) {
      #Select only Frame number and Joint center positions from the joints we wish to plot.
      df <- df %>%
        dplyr::select(frame, dplyr::ends_with("_MPR"), dplyr::ends_with("_MPU"), dplyr::ends_with("_MPF"), {{row_facets}}, {{col_facets}}, {{subject}})
    }


    # Create data for the torso and head (center of hips NH_, center of shoulder NS_, center of cranium NC_)
    df <- df %>%
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
        NC_MPU = NS_MPU + (NS_MPU-NH_MPU)*0.4,
        NC_MPF = NS_MPF + (NS_MPF-NH_MPF)*0.5,
        NC_MPR = NS_MPR + (NS_MPR-NH_MPR)*0.3,)

    df_data <- df %>%
      ## Transform data into long format
      tidyr::gather(key, value, dplyr::ends_with("_MPF"), dplyr::ends_with("_MPU"), dplyr::ends_with("_MPR")) %>%
      tidyr::extract(key, into = c("Joint", "Plane", "Dir"), regex = "(.+)_(MP)(.+)") %>%
      tidyr::spread(Dir, value) %>%
      tidyr::gather(Dir, value, F, R) %>%
      dplyr::filter(Dir %in% planes) %>%


      #Create groups for all the joints and extremities. This is needed for ggplot to  connect the correct joints together with geom_path()
      dplyr::mutate(
        Joint = factor(Joint),
        Joint = forcats::fct_relevel(Joint, "LT", "LA", "LK", "LH", "LS", "LE", "LW", "NH", "NS", "NC", "RT", "RA", "RK", "RH", "RS", "RE", "RW"),
        Side =  dplyr::case_when(
          Joint %in% c("LT", "LA", "LK", "LH") ~ "Left Leg",
          Joint %in% c("RT", "RA", "RK", "RH") ~ "Right Leg",
          Joint %in% c("LS", "LE", "LW") ~ "Left Arm",
          Joint %in% c("RS", "RE", "RW") ~ "Right Arm",
          TRUE ~ "Center"),
        Side = factor(Side, levels = c("Left Arm", "Left Leg", "Center", "Right Arm", "Right Leg")),
        Side_frame = paste0(as.character(Side), as.character(frame), {{subject}}),

        #Create a larger size for the Torso
        size_path_color = dplyr::case_when(
          Joint == "NH" ~ line_colored_size*torso_scale,
          TRUE ~ line_colored_size),

        size_path_black = dplyr::case_when(
          Joint == "NH" ~ line_black_size*torso_scale,
          TRUE ~ line_black_size),

        #Create a larger size for the Cranium and smaller feet
        size_point = dplyr::case_when(
          Joint == "NC" ~ point_size*head_scale,
          Joint %in% c("LE", "RE") ~ point_size * 0.9,
          Joint %in% c("LW", "RW") ~ point_size * 0.8,
          Joint %in% c("LK", "RK") ~ point_size * 0.95,
          Joint %in% c("LA", "RA") ~ point_size * 0.9,
          Joint %in% c("LT", "RT") ~ point_size * 0.8,
          TRUE ~ point_size),

        size_circle = dplyr::case_when(
          Joint == "NC" ~ circle_size*head_scale,
          Joint %in% c("LE", "RE") ~ circle_size * 0.9,
          Joint %in% c("LW", "RW") ~ circle_size * 0.8,
          Joint %in% c("LA", "RA") ~ circle_size * 0.95,
          Joint %in% c("LA", "RA") ~ circle_size * 0.9,
          Joint %in% c("LT", "RT") ~ circle_size * 0.8,
          TRUE ~ circle_size),

      ) %>%

      #Arrange the data according to joint. This will make ggplot connect the joints as we wish
      dplyr::arrange(frame, Joint)

    if(return_data) {
      return(df_data)
    }


    # Basic plot setup
    df_plot <- mocapr::mocap_plot_basic(df_data,
                                        planes = {{planes}},
                                        planes_in_rows_or_cols = {{planes_in_rows_or_cols}},
                                        row_facets = {{row_facets}},
                                        col_facets = {{col_facets}},
                                        subject = {{subject}},
                                        remove_facet_labels = {{remove_facet_labels}},
                                        remove_grid = FALSE)

    # Add avatar
    df_plot <- mocapr::mocap_plot_avatar(df_plot,
                                         use_geom_point = {{use_geom_point}},
                                         line_colored = {{line_colored}},
                                         line_colored_alpha = {{line_colored_alpha}},
                                         line_black = {{line_black}},
                                         line_black_alpha = {{line_black_alpha}},
                                         point = {{point}},
                                         point_alpha = {{point_alpha}})

    if(return_plot){return(df_plot)}

    # Animate it
    mocap_finish_animation(df_plot, ...)

    }

##Animate antomical (Function)----
#' Animate motioncapture data in the anatomical planes
#'
#' \code{animate_anatomical()} animates motioncapture data that is projected onto the anatomical planes of the body using \code{mocapr::project_full_body_to_AP()}.
#' Each frame contains a front and side view of the animated person. The definitions of front and side are dictated by the orientation of the pelvis.
#'
#' @param .data A tibble containing joint center positions in the anatomical planes generated using the project_to_AP() function.
#' @param planes Planes to animate. Must be one or both of c("R", "F"). The planes are created by combining the suplied direction with the up-direction. Defaults to c("R", "F").
#' @param line_colored_size The size of the colored lines connecting the joint centers.
#' @param line_black_size The size of the black lines connecting the joint centers.
#' @param point_size The size of the joint centers when \code{use_geom_point = TRUE}.
#' @param circle_size The size of the joint centers when \code{use_geom_point = FALSE}.
#' @param head_scale The size of the head relative to the joint-centers.
#' @param torso_scale The size of the torso line relative to the remaing lines.
#' @param return_data Return the wrangled data before it is sent to ggplot. This is useful for understanding the data-structure and for developmental purposes.
#' @param return_plot Return a plot instead of an animaiton. This is useful for customizing the plot before passing it to gganimate::
#' @param reduce_data Defaults to FALSE. If TRUE the function will reduce the input data to only include the variables that are needed for the plot or animation.This may improve performance slightly.
#'
#' @inheritParams mocap_plot_basic
#' @inheritParams mocap_plot_avatar
#' @inheritParams mocap_finish_animation
#'
#' @return Defaults to an animated gif. Different outputs can be achieved by passing different arguments via ... to the gganimate::animate() function. If return_plot = TRUE a ggplot plot is returned. If return_data = TRUE a tibble is returned.
#' @export
#'
#' @examples
#' # Prepare data
#' df <- dplyr::filter(mocapr::mocapr_data, movement_nr == 1)
#' df <- mocapr::project_full_body_to_AP(df)
#' df <- dplyr::filter(df, frame %in% c(1, 50, 100, 150, 200))
#'
#' # Create a facetted plot
#' animate_anatomical(df,
#'               planes_in_rows_or_cols = "rows",
#'               use_geom_point = FALSE,
#'               return_plot = TRUE,
#'               col_facets = frame)
  animate_anatomical <- function(.data,
                                 planes = c("R", "F"),
                                 planes_in_rows_or_cols = c("cols"),
                                 row_facets = NULL,
                                 col_facets = NULL,
                                 subject = NULL,
                                 remove_facet_labels = TRUE,
                                 use_geom_point = TRUE,
                                 line_colored = TRUE,
                                 line_colored_size = 2,
                                 line_colored_alpha = 1,
                                 line_black = TRUE,
                                 line_black_size = 2,
                                 line_black_alpha = 0.5,
                                 point = TRUE,
                                 point_size = 4,
                                 circle_size = 40,
                                 point_alpha = 1,
                                 head_scale = 1.5,
                                 torso_scale = 1.5,
                                 return_data = FALSE,
                                 return_plot = FALSE,
                                 reduce_data = FALSE,
                                 ...){

    # Avoid "No visible binding for global variable ...." in check
    RH_APR <- LH_APR <- RH_APF <- LH_APF <- RH_APU <- LH_APU <- NULL
    RK_APR <- LK_APR <- RK_APF <- LK_APF <- RK_APU <- LK_APU <- NULL
    RA_APR <- LA_APR <- RA_APF <- LA_APF <- RA_APU <- LA_APU <- NULL
    RT_APR <- LT_APR <- RT_APF <- LT_APF <- RT_APU <- LT_APU <- NULL
    RS_APR <- LS_APR <- RS_APF <- LS_APF <- RS_APU <- LS_APU <- NULL
    NS_APR <- NS_APF <- NS_APU <- NULL
    NH_APR <- NH_APF <- NH_APU <- NULL
    Normaliser_R <- Normaliser_F <- NULL
    frame <- key <- value <- Dir <- R <- Joint <- Side <- Side_frame <- U <- NULL
    size_path_color <- size_point <- size_path_black <- size_circle <- NULL


    # Prevent jitter. Stabilizie the joint center positions around the center of the hip joints
    df <- .data %>%
      dplyr::mutate(
        Normaliser_R = RH_APR+(LH_APR-RH_APR)/2,
        Normaliser_F = RH_APF+(LH_APF-RH_APF)/2) %>%
      dplyr::mutate_at(dplyr::vars(tidyselect::ends_with("_APR")), ~.-Normaliser_R) %>%
      dplyr::mutate_at(dplyr::vars(tidyselect::ends_with("_APF")), ~.-Normaliser_F) %>%
      dplyr::select(-Normaliser_R, -Normaliser_F)


    if(reduce_data) {
      #Select only Frame number and Joint center positions from the joints we wish to plot.
      df <- df %>%
        dplyr::select(frame, dplyr::ends_with("_APR"), dplyr::ends_with("_APU"), dplyr::ends_with("_APF"), {{row_facets}}, {{col_facets}}, {{subject}})
    }


    df <- df %>%
      ## Create Sagital plane data (center of hips NH_, center of shoulder NS_, center of cranium NC_)
      dplyr::mutate(
        NH_APU = (LH_APU+RH_APU)/2,
        NH_APR = (LH_APR+RH_APR)/2,
        NH_APF = (LH_APF+RH_APF)/2,
        NS_APU = (LS_APU+RS_APU)/2,
        NS_APR = (LS_APR+RS_APR)/2,
        NS_APF = (LS_APF+RS_APF)/2,
        NC_APU = NS_APU + (NS_APU-NH_APU)*0.4,
        NC_APR = NS_APR + (NS_APR-NH_APR)*0.3,
        NC_APF = NS_APF + (NS_APF-NH_APF)*0.5
      )


    ## Transform data into long format
    df_data <- df %>%
      tidyr::gather(key, value, dplyr::ends_with("_APR"), dplyr::ends_with("_APU"), dplyr::ends_with("_APF"))


    df_data <- df_data %>%
      tidyr::extract(key, into = c("Joint", "Plane", "Dir"), regex = "(.+)_(AP)(.+)") %>%
      tidyr::spread(Dir, value) %>%
      tidyr::gather(Dir, value, R, F) %>%
      dplyr::filter(Dir %in% planes) %>%

      #Create groups for all the joints and extremities. This is needed for ggplot to  connect the correct joints together with geom_path()
      dplyr::mutate(
        Joint = factor(Joint),
        Joint = forcats::fct_relevel(Joint, "LT", "LA", "LK", "LH", "LS", "LE", "LW", "NH", "NS", "NC", "RT", "RA", "RK", "RH", "RS", "RE", "RW"),
        Side =  dplyr::case_when(
          Joint %in% c("LT", "LA", "LK", "LH") ~ "Left Leg",
          Joint %in% c("RT", "RA", "RK", "RH") ~ "Right Leg",
          Joint %in% c("LS", "LE", "LW") ~ "Left Arm",
          Joint %in% c("RS", "RE", "RW") ~ "Right Arm",
          TRUE ~ "Center"),
        Side = factor(Side, levels = c("Left Arm", "Left Leg", "Center", "Right Arm", "Right Leg")),
        Side_frame = paste0(as.character(Side), as.character(frame), {{subject}}),

        #Create a larger size for the Torso
        size_path_color = dplyr::case_when(
          Joint == "NH" ~ line_colored_size*torso_scale,
          TRUE ~ line_colored_size),

        size_path_black = dplyr::case_when(
          Joint == "NH" ~ line_black_size*torso_scale,
          TRUE ~ line_black_size),

        #Create a larger size for the Cranium and smaller feet
        size_point = dplyr::case_when(
          Joint == "NC" ~ point_size*head_scale,
          Joint %in% c("LE", "RE") ~ point_size * 0.9,
          Joint %in% c("LW", "RW") ~ point_size * 0.8,
          Joint %in% c("LA", "RA") ~ point_size * 0.9,
          Joint %in% c("LT", "RT") ~ point_size * 0.8,
          TRUE ~ point_size),

        size_circle = dplyr::case_when(
          Joint == "NC" ~ circle_size*head_scale,
          Joint %in% c("LE", "RE") ~ circle_size * 0.9,
          Joint %in% c("LW", "RW") ~ circle_size * 0.8,
          Joint %in% c("LA", "RA") ~ circle_size * 0.9,
          Joint %in% c("LT", "RT") ~ circle_size * 0.8,
          TRUE ~ circle_size),

        ) %>%

      #Arrange the data according to joint. This will make ggplot connect the joints as we wish
      dplyr::arrange(frame, Joint)

    if(return_data) {
      return(df_data)
    }

    # Basic plot setup
    df_plot <- mocapr::mocap_plot_basic(df_data,
                                        planes = {{planes}},
                                        planes_in_rows_or_cols = {{planes_in_rows_or_cols}},
                                        row_facets = {{row_facets}},
                                        col_facets = {{col_facets}},
                                        subject = {{subject}},
                                        remove_facet_labels = {{remove_facet_labels}})

    # Add avatar
    df_plot <- mocapr::mocap_plot_avatar(df_plot,
                                         use_geom_point = {{use_geom_point}},
                                         line_colored = {{line_colored}},
                                         line_colored_alpha = {{line_colored_alpha}},
                                         line_black = {{line_black}},
                                         line_black_alpha = {{line_black_alpha}},
                                         point = {{point}},
                                         point_alpha = {{point_alpha}})

    if(return_plot){return(df_plot)}

    # Animate it
    mocap_finish_animation(df_plot, ...)
  }

#Animate Global----
#' Animate motioncapture data using global spatial coordinates
#'
#' \code{animate_global()} animates motioncapture data using global spatial coordinates.
#' Each frame contains a X-Y and Z-Y plot of the captured person. The definitions of X-Y and Z-Y are dictated by the calibration of the system.
#'
#' @param .data A tibble containing joint center positions with global spatial positions of toes, ankles, knees, hip, shoulder, elbows, wrists.
#' @param planes Planes to animate. Must be one or both of c("X", "Z"). The planes are created by combining the suplied direction with the up-direction. Defaults to c("R", "F").
#' @param line_colored_size The size of the colored lines connecting the joint centers.
#' @param line_black_size The size of the black lines connecting the joint centers.
#' @param point_size The size of the joint centers when \code{use_geom_point = TRUE}.
#' @param circle_size The size of the joint centers when \code{use_geom_point = FALSE}.
#' @param head_scale The size of the head relative to the joint-centers.
#' @param torso_scale The size of the torso line relative to the remaing lines.
#' @param return_data Return the wrangled data before it is sent to ggplot. This is useful for understanding the data-structure and for developmental purposes.
#' @param return_plot Return a plot instead of an animaiton. This is useful for customizing the plot before passing it to gganimate::
#' @param reduce_data Defaults to FALSE. If TRUE the function will reduce the input data to only include the variables that are needed for the plot or animation.This may improve performance slightly.
#'
#' @inheritParams mocap_plot_basic
#' @inheritParams mocap_plot_avatar
#' @inheritParams mocap_finish_animation
#'
#' @return Defaults to an animated gif. Different outputs can be achieved by passing different arguments via ... to the gganimate::animate() function. If return_plot = TRUE a ggplot plot is returned. If return_data = TRUE a tibble is returned.
#' @export
#'
#' @examples
#' df <- dplyr::filter(mocapr::mocapr_data, movement_nr == 1)
#' df <- dplyr::filter(df, frame %in% c(1, 50, 75, 100))
#' mocapr::animate_global(df,
#'                        planes_in_rows_or_cols = "rows",
#'                        col_facets = frame,
#'                        return_plot = TRUE)
animate_global <- function(.data,
                             planes = c("X", "Z"),
                             planes_in_rows_or_cols = c("cols"),
                             row_facets = NULL,
                             col_facets = NULL,
                             subject = NULL,
                             remove_facet_labels = TRUE,
                             use_geom_point = TRUE,
                             line_colored = TRUE,
                             line_colored_size = 1,
                             line_colored_alpha = 1,
                             line_black = TRUE,
                             line_black_size = 1,
                             line_black_alpha = 0.5,
                             point = TRUE,
                             point_size = 2,
                             circle_size = 40,
                             point_alpha = 1,
                             head_scale = 1.5,
                             torso_scale = 1.5,
                             return_data = FALSE,
                             return_plot = FALSE,
                             reduce_data = FALSE,
                             ...){

    #Avoid "No visible binding for global variable ..." in check
    LTX <-  LTY <-  LTZ <- LAX <-  LAY <-  LAZ <- LKX <-  LKY <-  LKZ <- LHX <-  LHY <-  LHZ <- NULL
    LWX <-  LWY <-  LWZ <- LEX <-  LEY <-  LEZ <- LSX <-  LSY <-  LSZ <- NULL
    RTX <-  RTY <-  RTZ <- RAX <-  RAY <-  RAZ <- RKX <-  RKY <-  RKZ <- RHX <-  RHY <-  RHZ <- NULL
    RWX <-  RWY <-  RWZ <- REX <-  REY <-  REZ <- RSX <-  RSY <-  RSZ <- NULL
    NHX <-  NHY <-  NHZ <- NSX <-  NSY <-  NSZ <- NCX <-  NCY <-  NCZ <- NULL
    frame <- key <- value <- Dir <- X <- Z <- Joint <- Side <- Side_frame <- Y <- NULL
    size_path_color <- size_point <- size_path_black <- size_circle <-  NULL

    # Make dataframe
    df <- .data

    if(reduce_data) {
      # Select only Frame number and Joint center positions from the joints we wish to plot.
      df <-  df %>%
        dplyr::select(frame, {{row_facets}}, {{col_facets}}, {{subject}},
                    LTX, LTY, LTZ, LAX, LAY, LAZ, LKX, LKY, LKZ, LHX, LHY, LHZ,
                    RTX, RTY, RTZ, RAX, RAY, RAZ,RKX, RKY, RKZ, RHX, RHY, RHZ,
                    LWX, LWY, LWZ, LEX, LEY, LEZ, LSX, LSY, LSZ,
                    RWX, RWY, RWZ, REX, REY, REZ, RSX, RSY, RSZ
                    )
    }


    # Create data for the torso and head (center of hips NH_, center of shoulder NS_, center of cranium NC_)
    df <- df %>%
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
        NCY = NSY + (NSY - NHY)*0.4,
        NCX = NSX + (NSX - NHX)*0.5,
        NCZ = NSZ + (NSZ - NHZ)*0.3)


        ## Transform data into long format
    df_data <- df %>%
      tidyr::gather(key, value,
                    LTX, LTY, LTZ, LAX, LAY, LAZ, LKX, LKY, LKZ, LHX, LHY, LHZ,
                    RTX, RTY, RTZ, RAX, RAY, RAZ,RKX, RKY, RKZ, RHX, RHY, RHZ,
                    LWX, LWY, LWZ, LEX, LEY, LEZ, LSX, LSY, LSZ,
                    RWX, RWY, RWZ, REX, REY, REZ, RSX, RSY, RSZ,
                    NHX, NHY, NHZ, NSX, NSY, NSZ, NCY, NCX, NCZ) %>%
      tidyr::extract(key, into = c("Joint", "Dir"), regex = "(..)(.)") %>%
      tidyr::spread(Dir, value) %>%
      tidyr::gather(Dir, value, X, Z) %>%
      dplyr::filter(Dir %in% planes) %>%

      #Create groups for all the joints and extremities. This is needed for ggplot to  connect the correct joints together with geom_path()
      dplyr::mutate(
        Joint = factor(Joint),
        Joint = forcats::fct_relevel(Joint, "LT", "LA", "LK", "LH", "LS", "LE", "LW", "NH", "NS", "NC", "RT", "RA", "RK", "RH", "RS", "RE", "RW"),
        Side =  dplyr::case_when(
          Joint %in% c("LT", "LA", "LK", "LH") ~ "Left Leg",
          Joint %in% c("RT", "RA", "RK", "RH") ~ "Right Leg",
          Joint %in% c("LS", "LE", "LW") ~ "Left Arm",
          Joint %in% c("RS", "RE", "RW") ~ "Right Arm",
          TRUE ~ "Center"),
        Side = factor(Side, levels = c("Left Arm", "Left Leg", "Center", "Right Arm", "Right Leg")),
        Side_frame = paste0(as.character(Side), as.character(frame), {{subject}}),

        #Create a larger size for the Torso
        size_path_color = dplyr::case_when(
          Joint == "NH" ~ line_colored_size*torso_scale,
          TRUE ~ line_colored_size),

        size_path_black = dplyr::case_when(
          Joint == "NH" ~ line_black_size*torso_scale,
          TRUE ~ line_black_size),

        #Create a larger size for the Cranium and smaller feet
        size_point = dplyr::case_when(
          Joint == "NC" ~ point_size*head_scale,
          Joint %in% c("LE", "RE") ~ point_size * 0.9,
          Joint %in% c("LW", "RW") ~ point_size * 0.8,
          Joint %in% c("LA", "RA") ~ point_size * 0.9,
          Joint %in% c("LT", "RT") ~ point_size * 0.8,
          TRUE ~ point_size),

        size_circle = dplyr::case_when(
          Joint == "NC" ~ circle_size*head_scale,
          Joint %in% c("LE", "RE") ~ circle_size * 0.9,
          Joint %in% c("LW", "RW") ~ circle_size * 0.8,
          Joint %in% c("LA", "RA") ~ circle_size * 0.9,
          Joint %in% c("LT", "RT") ~ circle_size * 0.8,
          TRUE ~ circle_size),

      ) %>%

      #Arrange the data according to joint. This will make ggplot connect the joints as we wish
      dplyr::arrange(frame, Joint)

    if(return_data) {
      return(df_data)
    }

    # Basic plot setup
    df_plot <- mocapr::mocap_plot_basic(df_data,
                                        planes = {{planes}},
                                        planes_in_rows_or_cols = {{planes_in_rows_or_cols}},
                                        row_facets = {{row_facets}},
                                        col_facets = {{col_facets}},
                                        subject = {{subject}},
                                        remove_facet_labels = {{remove_facet_labels}},
                                        remove_grid = FALSE
    )


    # Add avatar
    df_plot <- mocapr::mocap_plot_avatar(df_plot,
                                         use_geom_point = {{use_geom_point}},
                                         line_colored = {{line_colored}},
                                         line_colored_alpha = {{line_colored_alpha}},
                                         line_black = {{line_black}},
                                         line_black_alpha = {{line_black_alpha}},
                                         point = {{point}},
                                         point_alpha = {{point_alpha}},
                                         up_column = Y)

    if(return_plot){return(df_plot)}

    # Animate it
    mocap_finish_animation(df_plot, ...)
}


# Plot
#' Set up basic ggplot2 plotting structure for mocapr plots and animations
#'
#' \code{mocap_plot_basic} is caled by the functions \code{\link{animate_global}}, \code{\link{animate_anatomical}}, and \code{\link{animate_movement}} functions
#' to set up the basic structure of the plots and animations
#'
#' @param .data A tibble containg mocap data in the mocapr format.
#' @param planes What planes should be included in the final plot/animation? defaults to c("R", "F"), use c("X", "Z") if you are using global coordinates.
#' @param planes_in_rows_or_cols Facet the chosen planes in either rows or columns. Must be one of c("rows", "cols"). Defaults to "cols".
#' @param row_facets Make additional row-facets in the animation using a given variable. Defaults to NULL.
#' @param col_facets Make additional column-facets in the animation using a given variable. Defaults to NULL.
#' @param subject Column that contains subject ID. Is only needed if more than one subject is present in the same frame.
#' @param remove_facet_labels Remove the facet labels. Defaults to \code{TRUE}.
#' @param remove_grid Remove the grid lines. Defaults to \code{TRUE}
#'
#' @return a ggplot2 object
#' @export
#'
#' @examples
#' df <- dplyr::filter(mocapr::mocapr_data, movement_nr == 1)
#' df <- dplyr::filter(df, frame %in% c(1, 50, 75, 100))
#' df <- mocapr::animate_global(df,
#'                        return_data = TRUE)
#' mocap_plot_basic(df,
#'                  planes = c("X", "Y"), # Because the data comes from animate_global()
#'                  planes_in_rows_or_cols = "rows",
#'                  col_facets = frame)
mocap_plot_basic <- function(.data,
                             planes = c("R", "F"),
                             planes_in_rows_or_cols = c("cols"),
                             row_facets = NULL,
                             col_facets = NULL,
                             subject = NULL,
                             remove_facet_labels = TRUE,
                             remove_grid = TRUE
                             ){
  # Avoid "No visible binding for global...."
  Side_frame <- Dir <- theme <- NULL

  df_plot <- .data %>%
    ggplot2::ggplot(ggplot2::aes(group = Side_frame))+
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
      panel.grid.major = ggplot2::element_blank())+
    ggplot2::scale_size_identity()

    # How should the planes be faceted (make NULL if no facet is needed)
    if(planes_in_rows_or_cols == "rows"){
      rowplanes <- rlang::quo(Dir)
      colplanes <- NULL}
    if(planes_in_rows_or_cols == "cols"){
      rowplanes <- NULL
      colplanes <- rlang::quo(Dir)}

    if( length(planes) <= 1){
      rowplanes <- NULL
      colplanes <- NULL
    }

  # Remove facet labels
  if(remove_facet_labels){
    df_plot <- df_plot +
      ggplot2::theme(
        strip.text.x = ggplot2::element_blank(),
        strip.text.y = ggplot2::element_blank())
  }

  # Remove grid
  if(remove_grid){
    df_plot <- df_plot+
      ggplot2::theme(
        panel.grid.major = ggplot2::element_blank(),
        panel.grid.minor = ggplot2::element_blank())
  }


    df_plot <- df_plot+
      ggplot2::facet_grid(cols = dplyr::vars(!!colplanes, {{col_facets}}), rows = dplyr::vars(!!rowplanes, {{row_facets}}))

    df_plot
}

#' Add an avatar to a mocap plot or animation
#'
#' \code{mocap_plot_avartar} is called by the functions \code{\link{animate_global}}, \code{\link{animate_anatomical}}, and \code{\link{animate_movement}} functions
#' after they have called \code{\link{mocap_plot_basic}}
#'
#' @param .plot A ggplot2 object containg mocap data in the mocapr format as it it created by \code{\link{mocap_plot_basic}}
#' @param use_geom_point Defaults to TRUE. If TRUE, points in the animation will be created using ggplot2::geom_point(). If FALSE, points are created using ggforce::geom_circle(). This will ensure correct size-proportions of the points, and that no points are croped out of the animation. However, it comes at the prize of much longer rendering-times.
#' @param line_colored Shall the joints be connected by colored lines? (\code{TRUE} or \code{FALSE}). Defaults to \code{TRUE}.
#' @param line_colored_alpha The alpha value of the colored lines connecting the joint centers.
#' @param line_black Shall the joints be connected by black lines? (\code{TRUE} or \code{FALSE}). Defaults to \code{TRUE}.
#' @param line_black_alpha The alhpa value of the black lines connecting the joint centers.
#' @param point Shall the joint centers be marked by points? (\code{TRUE} or \code{FALSE}). Defaults to \code{TRUE}.
#' @param point_alpha The alpha of the points or circles that mark the joint centers.
#' @param up_column The unquoted name of the column that contains the Up direction. Defaults to \code{U}, must be \code{Y} if the plots is prepared using \code{\link{animate_global}}.
#'
#' @return A ggplot2 object
#' @export
#'
#' @examples
#' df <- dplyr::filter(mocapr::mocapr_data, movement_nr == 1)
#' df <- dplyr::filter(df, frame %in% c(1, 50, 75, 100))
#' df <- mocapr::animate_global(df,
#'                              return_data = TRUE)
#' df <- mocap_plot_basic(df,
#'                  planes = c("X", "Y"), # Because the data comes from animate_global()
#'                  planes_in_rows_or_cols = "rows",
#'                  col_facets = frame)
#' mocap_plot_avatar(df,
#'                   up_column = Y) # Because the data comes from animate_global()
mocap_plot_avatar <- function(.plot,
                              use_geom_point = TRUE,
                              line_colored = TRUE,
                              line_colored_alpha = 1,
                              line_black = TRUE,
                              line_black_alpha = 0.5,
                              point = TRUE,
                              point_alpha = 1,
                              up_column = U){
  # Avoid "no visible global ..."
  U <- value <- size_path_color <- Side <- size_point <- size_circle <- NULL
  size_path_black <- NULL

  if(!ggplot2::is.ggplot(.plot)){
    stop(".plot supplied in the first argument must be a ggplot object")
  }

  df_plot <- .plot

  if(line_colored){
    df_plot <- df_plot+
      ggplot2::geom_path(ggplot2::aes(x = value, y = {{up_column}}, size = size_path_color, color = Side), alpha = line_colored_alpha)
  }

  if(point & use_geom_point){
    df_plot <- df_plot+
      ggplot2::geom_point(ggplot2::aes(x = value, y = {{up_column}}, fill = Side, size = size_point), color = "black", shape = 21, alpha = point_alpha)
  }

  if(point & !use_geom_point){
    df_plot <- df_plot+
      ggforce::geom_circle(ggplot2::aes(x0 = value, y0 = {{up_column}}, r = size_circle, fill = Side), color = "black", alpha = point_alpha)
  }

  if(line_black){
    df_plot <- df_plot+
      ggplot2::geom_path(ggplot2::aes(x = value, y = {{up_column}}, size = size_path_black), color = "black", alpha = line_black_alpha)
  }

  df_plot
}

#' Make an animation from a \code{mocapr} plot
#'
#' @param .data A ggplot2 object containing mocap data in the mocapr format.
#' @param ... Additional arguments are passed to \code{gganimate::}\code{\link[gganimate]{animate}}.
#'
#' @return A gif
#' @export
#'
#' @examples
#' df <- dplyr::filter(mocapr::mocapr_data, movement_nr == 1)
#' df <- mocapr::project_full_body_to_AP(df)
#' df <- mocapr::animate_anatomical(df,
#'                                  return_data = TRUE)
#' df <- mocapr::mocap_plot_basic(df)
#' df <- mocapr::mocap_plot_avatar(df)
#' # mocap_finish_animation(df)
mocap_finish_animation <- function(.data, ...){
  # Avoid "no visible binding for ..."
  frame <- NULL


  df_plot <- .data+
    gganimate::transition_time(frame)+
    gganimate::ease_aes('linear')

  gganimate::animate(df_plot, ...)
}


