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
#' @examples \dontrun{}
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
#' animate_anatomical() animates motioncapture data that is projected onto the anatomical planes of the body using mocapr::project_full_body_to_AP().
#'
#' @param .data A tibble containing joint center positions in the anatomical planes generated using the project_to_AP() function.
#' @param use_geom_point Defaults to TRUE. If TRUE, points in the animation will be created using ggplot2::geom_point(). If FALSE, points are created using ggforce::geom_circle(). This will ensure correct size-proportions of the points, and that no points are croped out of the animation. However, it comes at the prize of much longer rendering-times.
#' @param planes Planes to animate. Must be one or both of c("R", "F"). The planes are created by combining the suplied direction with the up-direction. Defaults to c("R", "F").
#' @param planes_in_rows_or_cols Facet the chosen planes in either rows or columns. Must be one of c("rows", "cols"). Defaults to "cols".
#' @param row_facets Make additional row-facets in the animation using a given variable. Defaults to NULL.
#' @param col_facets Make additional column-facets in the animation using a given variable. Defaults to NULL.
#' @param remove_facet_labels Remove the facet labels. Defaults to TRUE.
#' @param line_size The size of the lines connecting the joint centers.
#' @param point_size The size of the joint centers when use_geom_point = TRUE.
#' @param circle_size The size of the joint centers when use_geom_point = FALSE.
#' @param head_scale The size of the head relative to the joint-centers.
#' @param torso_scale The size of the torso line relative to the remaing lines.
#' @param return_plot Return a plot instead of an animaiton. This is useful for customizing the plot before passing it to gganimate::
#' @param ... These arguments are passed to the gganimate::animate() function.

#' @return Defaults to an animated gif. Different outputs can be achieved by passing different arguments via ... to the gganimate::animate() function. If return_plot = TRUE a ggplot plot is returned.
#' @export
#'
#' @examples \dontrun{}
  animate_anatomical <- function(.data,
                                 planes = c("R", "F"),
                                 planes_in_rows_or_cols = c("cols"),
                                 use_geom_point = TRUE,
                                 row_facets = NULL,
                                 col_facets = NULL,
                                 remove_facet_labels = TRUE,
                                 line_size = 1,
                                 point_size = 2,
                                 circle_size = 60,
                                 head_scale = 1.5,
                                 torso_scale = 2,
                                 return_plot = FALSE,
                                 ...){

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
        LT_APF = LT_APF-Normaliser_F)


    #Select only Frame number and Joint center positions from the joints we wish to plot.
    df <- df %>%

      dplyr::select(frame, dplyr::ends_with("_APR"), dplyr::ends_with("_APU"), dplyr::ends_with("_APF"), {{row_facets}}, {{col_facets}})


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
    df_plot <- df %>%
      tidyr::gather(key, value, dplyr::ends_with("_APR"), dplyr::ends_with("_APU"), dplyr::ends_with("_APF"))


    df_plot <- df_plot %>%
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
          TRUE ~ "No_side"),

        #Create a larger size for the Torso
        size_path = dplyr::case_when(
          Joint == "NH" ~ line_size*torso_scale,
          TRUE ~ line_size),

        #Create a larger size for the Cranium
        size_point = dplyr::case_when(
          Joint == "NC" ~ point_size*head_scale,
          TRUE ~ point_size),

        size_circle = dplyr::case_when(
          Joint == "NC" ~ circle_size*head_scale,
          TRUE ~ circle_size),

        ) %>%

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
          legend.title = ggplot2::element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())+
        ggplot2::scale_size(
          breaks = c(sort(unique(c(point_size, point_size * head_scale, line_size, line_size * torso_scale)))),
          range = c(min(point_size, line_size), max(point_size * head_scale, line_size * torso_scale)))

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
            ggplot2::geom_path(ggplot2::aes(x = value, y = U, size = size_path))+
            ggplot2::geom_point(ggplot2::aes(x = value, y = U, fill = Side, size = size_point), color = "black", shape = 21)+
            ggplot2::geom_path(ggplot2::aes(x = value, y = U), size = line_size, color = "black", alpha = 0.5)+
            ggplot2::facet_grid(cols = dplyr::vars(!!colplanes, {{col_facets}}), rows = dplyr::vars(!!rowplanes, {{row_facets}}))

      if(return_plot){return(df_plot)}
      df_plot <- df_plot+
            gganimate::transition_time(frame)+
            gganimate::ease_aes('linear')
      return(gganimate::animate(df_plot, ...))
      }else{
        df_plot <- df_plot +
          ggplot2::geom_path(ggplot2::aes(x = value, y = U, size = size_path))+
          ggforce::geom_circle(ggplot2::aes(x0 = value, y0 = U, r = size_circle, fill = Side), color = "black")+
          ggplot2::geom_path(ggplot2::aes(x = value, y = U), size = line_size, color = "black", alpha = 0.5)+
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




