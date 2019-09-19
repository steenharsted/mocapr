# mocapr 0.0.2
### Major changes
* Added the function align_movements()
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