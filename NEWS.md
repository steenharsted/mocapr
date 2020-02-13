# mocapr 0.0.1.9006
### Major changes
* added import_captury_csv() - this function correctly imports the new export format from the CapturyLive system
* deprecated the function add_jump_events()

# mocapr 0.0.1.9005
### Major changes
* Add arguments to include line_colored, line_black, and points (TRUE/FALSE)
* Add arguments to control the alpha of line_colored, line_black, and points
* Break the animation functions into smaller functions for better functionality and customization options.
* Addded the function add_phases_jump(), this will replace the function add_jump_events()
* deprecated the function add_jump_events()

# mocapr 0.0.1.9004
### Major changes
* project_single_joint_to_MP() and project_full_body_to_MP() now has the argument .method to support different ways of projecting data.
* added unit tests and package coverage
* mocapr_data now contains 11 movements

# mocapr 0.0.1.9003
### Major changes
* Added the functions  
  + align_movements()  
  + add_frontal_plane_knee_angle()  
  + add_frontal_plane_projection_angle()  
  + add_frontal_plane_knee_deviation()  
  + add_knee_ankle_hip_ratios()  
  + add_jump_length_and_height()  
  + add_jump_events()  
  + add_squat_events()  

* Added the sample data set
  + mocapr_synthetic_data

* The animate functions now include arguments that allow the user to control the size of point, circles and lines included in the animation or plot
  + line_size
  + point_size
  + circle_size
  + head_scale
  + torso_scale

# mocapr 0.0.1

### Major Changes
* The animate functions now include the following extra arguments:
  + planes # Which planes do you wish to include in the animation/plot
  + planes_in_rows_or_cols = c("cols") # Should the planes be facetted over rows or columns
  + use_geom_point = TRUE # Do you want to use `ggplot2::geom_point()` or `ggforce::geom_circle` for visualizing the joint-centers
  + row_facets = NULL # Facet the plot/animation over rows using another variable than the planes variable.
  + col_facets = NULL # Facet the plot/animation over columns using another variable than the planes variable.
  + remove_facet_labels = TRUE # Remove the facet labels
  + return_plot = FALSE # Return a plot instead of an animation. This has two uses. 1) Return a plot. 2) modify the layout of the plot before you animate it.

### Breaking changes
* To return a plot you now need the option `return_plot = TRUE` , the old option `animate = FALSE` has been removed.
