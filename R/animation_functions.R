#Animate Movement----
#' animate_movement().
#' Please see GitHub README.me for a more detailed description.
#'
#' @param .data A tibble containing joint center positions in the movement plane generated using the project_to_MP() function.
#' @param use_geom_point Defaults to TRUE. If TRUE, points in the animation will be created using ggplot2::geom_point(). If FALSE, points are created using ggforce::geom_circle(). This will ensure correct size-proportions of the points, and that no points are croped out of the animation. However, it comes at the prize of much longer rendering-times.
#' @param planes Planes to animate. Must be one or both of c("R", "F"). The planes are created by combining the suplied direction with the up-direction. Defaults to c("R", "F").
#' @param planes_in_rows_or_cols Facet the chosen planes in either rows or columns. Must be one of c("rows", "cols"). Defaults to "cols".
#' @param row_facets Make additional row-facets in the animation using a given variable. Defaults to NULL.
#' @param remove_facet_labels Remove the facet labels. Defaults to TRUE.
#' @param col_facets Make additional column-facets in the animation using a given variable. Defaults to NULL.
#' @param return_plot Return a plot instead of an animaiton. This is useful for customizing the plot before passing it to gganimate::
#' @param ... These arguments are passed to the gganimate::animate() function.
#'
#' @return Defaults to an animated gif. Different outputs can be achieved by passing different arguments via ... to the gganimate::animate() function. If animate = FALSE a ggplot plot is returned.
#' @export
#'
#' @examples\dontrun{}
  animate_movement <- function(.data,
                               planes = c("R", "F"),
                               planes_in_rows_or_cols = c("cols"),
                               use_geom_point = TRUE,
                               row_facets = NULL,
                               col_facets = NULL,
                               remove_facet_labels = TRUE,
                               return_plot = FALSE,
                               ...){

    #Make Data Frame
    df <- .data %>%
      #Select only Frame number and Joint center positions from the joints we wish to plot.
      dplyr::select(frame, dplyr::ends_with("_MPF"), dplyr::ends_with("_MPU"), dplyr::ends_with("_MPR"), {{row_facets}}, {{col_facets}}) %>%

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
        NC_MPU = NS_MPU + (NS_MPU-NH_MPU)*0.4,
        NC_MPF = NS_MPF + (NS_MPF-NH_MPF)*0.5,
        NC_MPR = NS_MPR + (NS_MPR-NH_MPR)*0.3,)

    df_plot <- df %>%
      ## Transform data into long format
      tidyr::gather(key, value, dplyr::ends_with("_MPF"), dplyr::ends_with("_MPU"), dplyr::ends_with("_MPR"))

    df_plot <- df_plot %>%
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
          TRUE ~ "No_side"),

        #Create a larger size for the Torso
        Size_Path = dplyr::case_when(
          Joint == "NH" ~ 3,
          TRUE ~ 2),

        #Create a larger size for the Cranium
        Size_Point = dplyr::case_when(
          Joint == "NC" ~ 8,
          TRUE ~ 4)) %>%

      #Arrange the data according to joint. This will make ggplot connect the joints as we wish
      dplyr::arrange(frame, Joint) %>%

      #Lets plot it!
      ggplot2::ggplot(ggplot2::aes(group = Side, color = Side))+
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
          legend.title = ggplot2::element_blank())

    if(remove_facet_labels){
      df_plot <- df_plot +
        ggplot2::theme(
          strip.text.x = ggplot2::element_blank(),
          strip.text.y = ggplot2::element_blank())
    }

    if(planes_in_rows_or_cols == "rows"){
      rowplanes <- rlang::quo(Dir)
      colplanes <- NULL}
    if(planes_in_rows_or_cols == "cols"){
      rowplanes <- NULL
      colplanes <- rlang::quo(Dir)}

    #Animation stuff
    if(use_geom_point){
        df_plot <- df_plot +
          ggplot2::geom_path(ggplot2::aes(x = value, y = U, color = Side, size = Size_Path))+
          ggplot2::geom_point(ggplot2::aes(x = value, y = U, size = Size_Point))+
          ggplot2::geom_path(ggplot2::aes(x = value, y = U), size = 0.75, color = "black")+
          ggplot2::geom_point(ggplot2::aes(x = value, y = U), size = 1, color = "black")+
          ggplot2::facet_grid(cols = dplyr::vars(!!colplanes, {{col_facets}}), rows = dplyr::vars(!!rowplanes, {{row_facets}}))

        if(return_plot){return(df_plot)}

        df_plot <- df_plot+
          gganimate::transition_time(frame)+
          gganimate::ease_aes('linear')
        return(gganimate::animate(df_plot, ...))

        }else{
          df_plot <- df_plot +
            ggforce::geom_circle(ggplot2::aes(x0 = value, y0 = U, r = Size_Point*12, fill = Side))+
            ggplot2::geom_path(ggplot2::aes(x = value, y = U), size = 0.75, color = "black")+
            ggforce::geom_circle(ggplot2::aes(x0 = value, y0 = U, r = 20), color = "black", fill = "black")+
            ggplot2::facet_grid(cols = dplyr::vars(!!colplanes, {{col_facets}}), rows = dplyr::vars(!!rowplanes, {{row_facets}}))

          if(return_plot){return(df_plot)}

          df_plot <- df_plot+
            gganimate::transition_time(frame)+
            gganimate::ease_aes('linear')
          return(gganimate::animate(df_plot, ...))
      }
}


##Animate antomical (Function)----
#' Animate motioncapture data in the anatomical planes
#'
#' \code{animate_anatomical()} animates motioncapture data that is projected onto the anatomical planes of the body using \code{mocapr::project_full_body_to_AP()}.
#' Each frame contains a front and side view of the animated person. The definitions of front and side are dictated by the orientation of the pelvis.
#'
#' @param .data A tibble containing joint center positions in the anatomical planes generated using the project_to_AP() function.
#' @param planes Planes to animate. Must be one or both of c("R", "F"). The planes are created by combining the suplied direction with the up-direction. Defaults to c("R", "F").
#' @param planes_in_rows_or_cols Facet the chosen planes in either rows or columns. Must be one of c("rows", "cols"). Defaults to "cols".
#' @param row_facets Make additional row-facets in the animation using a given variable. Defaults to NULL.
#' @param col_facets Make additional column-facets in the animation using a given variable. Defaults to NULL.
#' @param remove_facet_labels Remove the facet labels. Defaults to TRUE.
#' @param use_geom_point Defaults to TRUE. If TRUE, points in the animation will be created using ggplot2::geom_point(). If FALSE, points are created using ggforce::geom_circle(). This will ensure correct size-proportions of the points, and that no points are croped out of the animation. However, it comes at the prize of much longer rendering-times.
#' @param line_colored_size The size of the colored lines connecting the joint centers.
#' @param line_black_size The size of the black lines connecting the joint centers.
#' @param line_black_alpha The alhpa value of the black lines connecting the joint centers.
#' @param point_size The size of the joint centers when use_geom_point = TRUE.
#' @param circle_size The size of the joint centers when use_geom_point = FALSE.
#' @param head_scale The size of the head relative to the joint-centers.
#' @param torso_scale The size of the torso line relative to the remaing lines.
#' @param return_data Return the wrangled data before it is sent to ggplot. This is useful for understanding the data-structure and for developmental purposes.
#' @param return_plot Return a plot instead of an animaiton. This is useful for customizing the plot before passing it to gganimate::
#' @param reduce_data Defaults to FALSE. If TRUE the function will reduce the input data to only include the variables that are needed for the plot or animation.This may improve performance slightly.
#' @param ... These arguments are passed to the gganimate::animate() function.

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
                                 remove_facet_labels = TRUE,
                                 use_geom_point = TRUE,
                                 line_colored_size = 2,
                                 line_black_size = 2,
                                 line_black_alpha = 0.5,
                                 point_size = 4,
                                 circle_size = 40,
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
        dplyr::select(frame, dplyr::ends_with("_APR"), dplyr::ends_with("_APU"), dplyr::ends_with("_APF"), {{row_facets}}, {{col_facets}})
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
        Side_frame = paste0(as.character(Side), as.character(frame)),

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

    #Lets plot it!
    df_plot <- df_data %>%
      ggplot2::ggplot(ggplot2::aes(group = Side_frame, color = Side))+
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
          panel.grid.major = ggplot2::element_blank(),
          panel.grid.minor = ggplot2::element_blank())+
        ggplot2::scale_size_identity()

    # Remove facet labels
    if(remove_facet_labels){
      df_plot <- df_plot +
        ggplot2::theme(
          strip.text.x = ggplot2::element_blank(),
          strip.text.y = ggplot2::element_blank())
    }

    # How should the planes be faceted (make NULL if no facet is needed)
    if(planes_in_rows_or_cols == "rows"){
      rowplanes <- rlang::quo(Dir)
      colplanes <- NULL}
    if(planes_in_rows_or_cols == "cols"){
      rowplanes <- NULL
      colplanes <- rlang::quo(Dir)}

    if( all(planes == "R") |  all(planes == "F")){
      rowplanes <- NULL
      colplanes <- NULL
    }


    #Animation and plotting stuff
    # With geom_point
    if(use_geom_point){
      df_plot <- df_plot +
            ggplot2::geom_path(ggplot2::aes(x = value, y = U, size = size_path_color))+
            ggplot2::geom_point(ggplot2::aes(x = value, y = U, fill = Side, size = size_point), color = "black", shape = 21)+
            ggplot2::geom_path(ggplot2::aes(x = value, y = U, size = size_path_black), color = "black", alpha = line_black_alpha)+
            ggplot2::facet_grid(cols = dplyr::vars(!!colplanes, {{col_facets}}), rows = dplyr::vars(!!rowplanes, {{row_facets}}))

      if(return_plot){return(df_plot)}
      df_plot <- df_plot+
            gganimate::transition_time(frame)+
            gganimate::ease_aes('linear')
      return(gganimate::animate(df_plot, ...))
      }else{
        # With geom_circle
        df_plot <- df_plot +
          ggplot2::geom_path(ggplot2::aes(x = value, y = U, size = size_path_color))+
          ggforce::geom_circle(ggplot2::aes(x0 = value, y0 = U, r = size_circle, fill = Side), color = "black")+
          ggplot2::geom_path(ggplot2::aes(x = value, y = U, size = size_path_black), color = "black", alpha = line_black_alpha)+
          ggplot2::facet_grid(cols = dplyr::vars(!!colplanes, {{col_facets}}), rows = dplyr::vars(!!rowplanes, {{row_facets}}))

        if(return_plot){return(df_plot)}
        df_plot <- df_plot+
          gganimate::transition_time(frame)+
          gganimate::ease_aes('linear')
        return(gganimate::animate(df_plot, ...))
      }
  }

#Animate Global----
#' animate_global().
#' Please see GitHub README.me for a more detailed description.
#'
#' @param .data A tibble containing joint center positions in the anatomical planes generated using the project_to_AP() function.
#' @param use_geom_point Defaults to TRUE. If TRUE, points in the animation will be created using ggplot2::geom_point(). If FALSE, points are created using ggforce::geom_circle(). This will ensure correct size-proportions of the points, and that no points are croped out of the animation. However, it comes at the prize of much longer rendering-times.
#' @param planes Planes to animate. Must be one or both of c("R", "F"). The planes are created by combining the suplied direction with the up-direction. Defaults to c("R", "F").
#' @param planes_in_rows_or_cols Facet the chosen planes in either rows or columns. Must be one of c("rows", "cols"). Defaults to "cols".
#' @param row_facets Make additional row-facets in the animation using a given variable. Defaults to NULL.
#' @param col_facets Make additional column-facets in the animation using a given variable. Defaults to NULL.
#' @param remove_facet_labels Remove the facet labels. Defaults to TRUE.
#' @param return_plot Return a plot instead of an animaiton. This is useful for customizing the plot before passing it to gganimate::
#' @param ... These arguments are passed to the gganimate::animate() function.
#'
#' @return Defaults to an animated gif. Different outputs can be achieved by passing different arguments via ... to the gganimate::animate() function. If animate = FALSE a ggplot plot is returned.
#' @export
#'
#' @examples
#' mocapr::animate_global(dplyr::filter(mocapr::mocapr_data, movement_nr == 1, frame %in% c(1, 10, 20, 25)), return_plot = TRUE)
#' \dontrun{
#' jump_1 <-  dplyr::filter(mocapr::mocapr_data, movement_nr == 1)
#' mocapr::animate_global(jump_1, nframes = nrow(jump_1), fps = 50)
#' }
  animate_global <- function(.data,
                             planes = c("X", "Z"),
                             planes_in_rows_or_cols = c("cols"),
                             use_geom_point = TRUE,
                             row_facets = NULL,
                             col_facets = NULL,
                             remove_facet_labels = TRUE,
                             return_plot = FALSE,
                             ...){
    #   #Make Data Frame
    df <- .data %>%
      #Select only Frame number and Joint center positions from the joints we wish to plot.
      dplyr::select(frame, {{row_facets}}, {{col_facets}},
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
        NCY = NSY + (NSY - NHY)*0.4,
        NCX = NSX + (NSX - NHX)*0.5,
        NCZ = NSZ + (NSZ - NHZ)*0.3)

    df_plot <- df %>%
      ## Transform data into long format
      tidyr::gather(key, value, dplyr::ends_with("X"), dplyr::ends_with("Y"), dplyr::ends_with("Z")) %>%
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
          TRUE ~ "No_side"),

        #Create a larger size for the Torso
        Size_Path = dplyr::case_when(
          Joint == "NH" ~ 2,
          TRUE ~ 1),

        #Create a larger size for the Cranium
        Size_Point = dplyr::case_when(
          Joint == "NC" ~ 8,
          TRUE ~ 4)) %>%

      #Arrange the data according to joint. This will make ggplot connect the joints as we wish
      dplyr::arrange(frame, Joint) %>%

      #Lets plot it!
      ggplot2::ggplot(ggplot2::aes(group = Side, color = Side))+
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
        legend.title = ggplot2::element_blank())

    if(remove_facet_labels){
      df_plot <- df_plot +
        ggplot2::theme(
          strip.text.x = ggplot2::element_blank(),
          strip.text.y = ggplot2::element_blank())
    }

    if(planes_in_rows_or_cols == "rows"){
      rowplanes <- rlang::quo(Dir)
      colplanes <- NULL}
    if(planes_in_rows_or_cols == "cols"){
      rowplanes <- NULL
      colplanes <- rlang::quo(Dir)}

    #Animation stuff
    if(use_geom_point){
        df_plot <- df_plot +
          ggplot2::geom_path(ggplot2::aes(x = value, y = Y, color = Side, size = Size_Path))+
          ggplot2::geom_point(ggplot2::aes(x = value, y = Y, size = Size_Point))+
          ggplot2::geom_path(ggplot2::aes(x = value, y = Y), size = 0.75, color = "black")+
          ggplot2::geom_point(ggplot2::aes(x = value, y = Y), size = 1.5, color = "black")+
          ggplot2::facet_grid(cols = dplyr::vars(!!colplanes, {{col_facets}}), rows = dplyr::vars(!!rowplanes, {{row_facets}}))

        if(return_plot){return(df_plot)}

        df_plot <- df_plot+
          gganimate::transition_time(frame)+
          gganimate::ease_aes('linear')
        return(gganimate::animate(df_plot, ...))

      }else{
        df_plot <- df_plot +
          ggforce::geom_circle(ggplot2::aes(x0 = value, y0 = Y, r = Size_Point*12, fill = Side))+
          ggplot2::geom_path(ggplot2::aes(x = value, y = Y), color = "black", size = 1, alpha = 0.8)+
          ggforce::geom_circle(ggplot2::aes(x0 = value, y0 = Y, r = 20), fill = "black", color = "black")+
          ggplot2::facet_grid(cols = dplyr::vars(!!colplanes, {{col_facets}}), rows = dplyr::vars(!!rowplanes, {{row_facets}}))

        if(return_plot){return(df_plot)}

        df_plot <- df_plot+
          gganimate::transition_time(frame)+
          gganimate::ease_aes('linear')
        return(gganimate::animate(df_plot, ...))
      }
    }




