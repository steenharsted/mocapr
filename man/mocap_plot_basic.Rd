% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/animation_functions.R
\name{mocap_plot_basic}
\alias{mocap_plot_basic}
\title{Set up basic ggplot2 plotting structure for mocapr plots and animations}
\usage{
mocap_plot_basic(
  .data,
  planes = c("R", "F"),
  planes_in_rows_or_cols = c("cols"),
  row_facets = NULL,
  col_facets = NULL,
  subject = NULL,
  remove_facet_labels = TRUE,
  remove_grid = TRUE
)
}
\arguments{
\item{.data}{A tibble containg mocap data in the mocapr format.}

\item{planes}{What planes should be included in the final plot/animation? defaults to c("R", "F"), use c("X", "Z") if you are using global coordinates.}

\item{planes_in_rows_or_cols}{Facet the chosen planes in either rows or columns. Must be one of c("rows", "cols"). Defaults to "cols".}

\item{row_facets}{Make additional row-facets in the animation using a given variable. Defaults to NULL.}

\item{col_facets}{Make additional column-facets in the animation using a given variable. Defaults to NULL.}

\item{subject}{Column that contains subject ID. Is only needed if more than one subject is present in the same frame.}

\item{remove_facet_labels}{Remove the facet labels. Defaults to \code{TRUE}.}

\item{remove_grid}{Remove the grid lines. Defaults to \code{TRUE}}
}
\value{
a ggplot2 object
}
\description{
\code{mocap_plot_basic} is caled by the functions \code{\link{animate_global}}, \code{\link{animate_anatomical}}, and \code{\link{animate_movement}} functions
to set up the basic structure of the plots and animations
}
\examples{
df <- dplyr::filter(mocapr::mocapr_data, movement_nr == 1)
df <- dplyr::filter(df, frame \%in\% c(1, 50, 75, 100))
df <- mocapr::animate_global(df,
                       return_data = TRUE)
mocap_plot_basic(df,
                 planes = c("X", "Y"), # Because the data comes from animate_global()
                 planes_in_rows_or_cols = "rows",
                 col_facets = frame)
}
