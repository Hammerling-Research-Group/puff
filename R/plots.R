#' Plot a 2D Animated Heatmap for Concentration Over Time
#'
#' This function generates a 2D animated heatmap using `plotly` to visualize the
#' movement of a plume over time. The animation is based on grid concentration data
#' from `simulate_grid_mode()` output.
#'
#' @param data A matrix or array of grid concentration results from `simulate_grid_mode()`.
#' @param grid_coords A list containing the same grid coordinates passed to `simulate_grid_mode()`.
#' @param start A character string specifying the start time of the simulation (e.g., "YYYY-MM-DD HH:MM:SS").
#' @param end A character string specifying the end time of the simulation (e.g., "YYYY-MM-DD HH:MM:SS").
#' @param output_dt A character string or numeric value specifying the time interval between outputs.
#' @param frames Numeric. Duration between frames in the animation (milliseconds).
#' @param transition Numeric. Duration for transitioning between frames (milliseconds).
#' @param save Logical. If `TRUE`, saves the plot as an HTML file named `2D_heatmap.html` and specifies saved location. Default set to `FALSE`.
#'
#' @return A `plotly` object representing the animated heatmap.
#' @export
#'
#' @examples
#' \dontrun{
#' data <- simulate_grid_mode(...)
#' grid_coords <- list(x = ..., y = ..., z = ...)
#' plot_2d_animated(data, grid_coords, "2025-01-01 00:00:00",
#'                  "2025-01-01 01:00:00", "1 min", 100, 99, save = FALSE)
#' }
plot_2d_animated <- function(data, grid_coords, start, end, output_dt,
                             frames = 100, transition = 99,
                             save = FALSE) {
  # chk: pkgs installed
  if (!requireNamespace("plotly", quietly = TRUE)) {
    stop("The 'plotly' package is required but not installed. Please install it.")
  }
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("The 'dplyr' package is required but not installed. Please install it.")
  }
  if (!requireNamespace("htmlwidgets", quietly = TRUE) && save) {
    stop("The 'htmlwidgets' package is required to save the plot as an HTML file. Please install it.")
  }

  # chk: input classes / types
  if (!is.matrix(data) && !is.array(data)) {
    stop("'data' must be a matrix or an array.")
  }
  if (!is.list(grid_coords) ||
      !all(c("x", "y", "z") %in% names(grid_coords))) {
    stop("'grid_coords' must be a list containing 'x', 'y', and 'z' coordinates.")
  }
  if (!is.character(start) || !is.character(end)) {
    stop("'start' and 'end' must be character strings in the format 'YYYY-MM-DD HH:MM:SS'.")
  }
  if (as.POSIXct(start) >= as.POSIXct(end)) {
    stop("'start' time must be earlier than 'end' time.")
  }
  if (!is.character(output_dt) && !is.numeric(output_dt)) {
    stop("'output_dt' must be a valid time interval (e.g., '1 min', '1 hour').")
  }
  if (!is.numeric(frames) || frames <= 0) {
    stop("'frames' must be a positive numeric value.")
  }
  if (!is.numeric(transition) || transition < 0) {
    stop("'transition' must be a non-negative numeric value.")
  }
  if (!is.logical(save)) {
    stop("'save' must be a logical value (TRUE or FALSE).")
  }

  # setup
  x_coords <- grid_coords$x
  y_coords <- grid_coords$y
  z_coords <- grid_coords$z

  # chk: empty or mismatched coordinate lengths?
  if (length(x_coords) == 0 || length(y_coords) == 0 || length(z_coords) == 0) {
    stop("Grid coordinates ('x', 'y', 'z') must not be empty.")
  }
  if (nrow(data) != length(seq(
    from = as.POSIXct(start),
    to = as.POSIXct(end),
    by = output_dt
  ))) {
    stop("Number of time steps in 'data' does not match the sequence of timestamps derived from 'start', 'end', and 'output_dt'.")
  }

  time_stamps <- seq(
    from = as.POSIXct(start),
    to = as.POSIXct(end),
    by = output_dt
  )

  # build concentration df
  concentration_data <- data.frame()

  for (t_idx in seq_along(time_stamps)) {
    grid_concentration <- matrix(
      data[t_idx, ],
      nrow = length(x_coords),
      ncol = length(y_coords)
    )
    if (ncol(grid_concentration) != length(y_coords)) {
      warning("Mismatch between the dimensions of 'data' and the grid coordinates. Check your inputs.")
    }
    long_data <- expand.grid(
      x = x_coords,
      y = y_coords,
      z = z_coords
    )
    long_data$concentration <- as.vector(grid_concentration)
    long_data$time <- as.numeric(t_idx)
    concentration_data <- rbind(
      concentration_data,
      long_data
    )
  }

  concentration_data <- concentration_data |>
    dplyr::mutate(time = as.factor(time))

  # plot
  fig_heatmap <- plotly::plot_ly(
    data = concentration_data,
    x = ~x,
    y = ~y,
    z = ~concentration,
    frame = ~time,
    type = "heatmap",
    colorscale = list(c(0, 'blue'), c(0.5, 'yellow'), c(1, 'red')),
    colorbar = list(title = "Concentration (ppm)"),
    zmin = 0,
    zmax = max(concentration_data$concentration, na.rm = TRUE)
  ) |>
    plotly::layout(
      title = "Plume Movement Over Time (2D)",
      xaxis = list(title = "X Coordinate"),
      yaxis = list(title = "Y Coordinate")
    ) |>
    plotly::animation_opts(
      frame = frames,
      transition = transition,
      redraw = TRUE
    )

  # save?
  if (save == TRUE) {
    tryCatch({
      htmlwidgets::saveWidget(fig_heatmap, "2D_heatmap.html", selfcontained = TRUE)
      message("Plot successfully saved as '2D_heatmap.html', at: .", getwd())
    }, error = function(e) {
      warning("Failed to save the plot as an HTML file. Error: ", e$message)
    })
  }

  return(fig_heatmap)
}

