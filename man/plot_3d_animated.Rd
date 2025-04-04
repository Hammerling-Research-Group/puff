% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plots.R
\name{plot_3d_animated}
\alias{plot_3d_animated}
\title{Plot a 3D Animated Plot for Concentration Over Time}
\usage{
plot_3d_animated(
  data,
  grid_coords,
  start,
  end,
  output_dt,
  frames = 100,
  transition = 99,
  plot_type = "contour",
  save = FALSE
)
}
\arguments{
\item{data}{A matrix or array of grid concentration results from `simulate_grid_mode()`.}

\item{grid_coords}{A list containing the same grid coordinates passed to `simulate_grid_mode()`.}

\item{start}{A character string specifying the start time of the simulation (e.g., "YYYY-MM-DD HH:MM:SS").}

\item{end}{A character string specifying the end time of the simulation (e.g., "YYYY-MM-DD HH:MM:SS").}

\item{output_dt}{A character string or numeric value specifying the time interval between outputs.}

\item{frames}{Numeric. Duration between frames in the animation (milliseconds). Default is 100.}

\item{transition}{Numeric. Duration for transitioning between frames (milliseconds). Default is 99.}

\item{plot_type}{Character. "contour" (default) or "scatter" to specify the type of plot.}

\item{save}{Logical. If `TRUE`, saves the plot as an HTML file named `2D_heatmap.html` and specifies saved location. Default set to `FALSE`.}
}
\value{
A `plotly` object representing the animated plot.
}
\description{
This function generates a 3D animated plot (scatter or contour) using `plotly` to visualize the
movement of a plume over time. The animation is based on grid concentration data
from `simulate_grid_mode()` output.
}
\examples{
\dontrun{
grid_concentrations <- array(...)  # 3D concentration array
grid_coords <- list(x = ..., y = ..., z = ...)
plot_3d_animated(grid_concentrations, grid_coords, "2025-01-01 00:00:00",
                 "2025-01-01 01:00:00", "1 min", 100, 99, plot_type = "contour", save = TRUE)
}
}
