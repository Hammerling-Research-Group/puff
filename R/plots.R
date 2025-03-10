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
#' @param interpolate_grid Logical. If `TRUE`, applies interpolation to refine grid resolution and make the heatmap smoother. Default `FALSE`.
#' @param granularity Numeric. Sets the number of points in the finer grid resolution when `interpolate_grid = TRUE`. Default `100`.
#'
#' @return A `plotly` object representing the animated heatmap.
#' @export
plot_2d_animated <- function(data, grid_coords, start, end, output_dt,
                             frames = 100, transition = 99,
                             save = FALSE, interpolate_grid = FALSE,
                             granularity = 100) {
  # chk: required packages
  if (!requireNamespace("plotly", quietly = TRUE)) stop("The 'plotly' package is required but not installed.")
  if (!requireNamespace("dplyr", quietly = TRUE)) stop("The 'dplyr' package is required but not installed.")
  if (!requireNamespace("htmlwidgets", quietly = TRUE) && save) stop("The 'htmlwidgets' package is required to save the plot.")
  if (interpolate_grid && !requireNamespace("akima", quietly = TRUE)) stop("The 'akima' package is required for interpolation but not installed.")

  # chk: input classes / types
  if (!is.matrix(data) && !is.array(data)) stop("'data' must be a matrix or an array.")
  if (!is.list(grid_coords) || !all(c("x", "y", "z") %in% names(grid_coords))) stop("'grid_coords' must be a list containing 'x', 'y', and 'z' coordinates.")
  if (!is.character(start) || !is.character(end)) stop("'start' and 'end' must be character strings in the format 'YYYY-MM-DD HH:MM:SS'.")
  if (as.POSIXct(start) >= as.POSIXct(end)) stop("'start' time must be earlier than 'end' time.")
  if (!is.character(output_dt) && !is.numeric(output_dt)) stop("'output_dt' must be a valid time interval (e.g., '1 min', '1 hour').")
  if (!is.numeric(frames) || frames <= 0) stop("'frames' must be a positive numeric value.")
  if (!is.numeric(transition) || transition < 0) stop("'transition' must be a non-negative numeric value.")
  if (!is.logical(save)) stop("'save' must be a logical value (TRUE or FALSE).")

  # setup
  x_coords <- grid_coords$x
  y_coords <- grid_coords$y
  z_coords <- grid_coords$z

  # chk: empty or mismatched coordinate lengths?
  if (length(x_coords) == 0 || length(y_coords) == 0 || length(z_coords) == 0) stop("Grid coordinates ('x', 'y', 'z') must not be empty.")
  if (nrow(data) != length(seq(from = as.POSIXct(start), to = as.POSIXct(end), by = output_dt))) {
    stop("Number of time steps in 'data' does not match the sequence of timestamps derived from 'start', 'end', and 'output_dt'.")
  }

  time_stamps <- seq(from = as.POSIXct(start), to = as.POSIXct(end), by = output_dt)

  # build concentration df
  concentration_data <- data.frame()

  for (t_idx in seq_along(time_stamps)) {
    grid_concentration <- matrix(data[t_idx, ], nrow = length(x_coords), ncol = length(y_coords))
    if (ncol(grid_concentration) != length(y_coords)) {
      warning("Mismatch between the dimensions of 'data' and the grid coordinates. Check your inputs.")
    }

    long_data <- expand.grid(x = x_coords, y = y_coords, z = z_coords)
    long_data$concentration <- as.vector(grid_concentration)
    long_data$time <- as.numeric(t_idx)

    concentration_data <- rbind(concentration_data, long_data)
  }

  # interpolation (if TRUE)
  if (interpolate_grid) {
    suppressWarnings({

      fine_x <- seq(min(x_coords), max(x_coords), length.out = granularity)
      fine_y <- seq(min(y_coords), max(y_coords), length.out = granularity)

      interp_data <- data.frame()

      for (t_idx in seq_along(time_stamps)) {
        z_values <- matrix(data[t_idx, ], nrow = length(x_coords), ncol = length(y_coords))

        interp_result <- akima::interp(
          x = rep(x_coords, times = length(y_coords)),
          y = rep(y_coords, each = length(x_coords)),
          z = as.vector(z_values),
          xo = fine_x,
          yo = fine_y,
          duplicate = "strip"
        )

        temp_df <- expand.grid(x = interp_result$x, y = interp_result$y)
        temp_df$concentration <- as.vector(interp_result$z)
        temp_df$concentration[is.na(temp_df$concentration)] <- 0
        temp_df$time <- t_idx

        interp_data <- rbind(interp_data, temp_df)
      }

      concentration_data <- interp_data
    })
  }

  concentration_data <- concentration_data |>
    dplyr::mutate(time = as.factor(time))

  # plot
  suppressWarnings({

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
    zmax = max(concentration_data$concentration, na.rm = TRUE),
    zsmooth = if (interpolate_grid) "best" else NULL
  ) |>
    plotly::layout(
      title = "Plume Movement Over Time (2D)",
      xaxis = list(title = "X Coordinate"),
      yaxis = list(title = "Y Coordinate")
    ) |>
    plotly::animation_opts(frame = frames, transition = transition, redraw = TRUE)
  })

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


#' Plot a 3D Animated Plot for Concentration Over Time
#'
#' This function generates a 3D animated plot (scatter or contour) using `plotly` to visualize the
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
#' @param plot_type Character. "contour" (default) or "scatter" to specify the type of plot.
#' @param save Logical. If `TRUE`, saves the plot as an HTML file named `2D_heatmap.html` and specifies saved location. Default set to `FALSE`.
#'
#' @return A `plotly` object representing the animated plot.
#' @export
#'
#' @examples
#' \dontrun{
#' grid_concentrations <- array(...)  # 3D concentration array
#' grid_coords <- list(x = ..., y = ..., z = ...)
#' plot_3d_animated(grid_concentrations, grid_coords, "2025-01-01 00:00:00",
#'                  "2025-01-01 01:00:00", "1 min", 100, 99, plot_type = "contour", save = TRUE)
#' }
plot_3d_animated <- function(data, grid_coords, start, end, output_dt,
                             frames = 100, transition = 99, plot_type = "contour", save = FALSE) {
  # chk: pkgs
  if (!requireNamespace("plotly", quietly = TRUE)) stop("The 'plotly' package is required but not installed.")
  if (!requireNamespace("dplyr", quietly = TRUE)) stop("The 'dplyr' package is required but not installed.")
  if (!requireNamespace("htmlwidgets", quietly = TRUE) && save) stop("The 'htmlwidgets' package is required to save the plot.")

  # chk: data types / classes + other qc
  if (!is.array(data)) stop("'data' must be a 3D array.")
  if (!is.list(grid_coords) || !all(c("x", "y", "z") %in% names(grid_coords))) stop("'grid_coords' must contain 'x', 'y', and 'z'.")
  if (!is.character(start) || !is.character(end)) stop("'start' and 'end' must be character strings.")
  if (as.POSIXct(start) >= as.POSIXct(end)) stop("'start' time must be earlier than 'end' time.")
  if (!is.character(output_dt) && !is.numeric(output_dt)) stop("'output_dt' must be a valid time interval.")
  if (!is.numeric(frames) || frames <= 0) stop("'frames' must be a positive number.")
  if (!is.numeric(transition) || transition < 0) stop("'transition' must be non-negative.")
  if (!plot_type %in% c("scatter", "contour")) stop("'plot_type' must be either 'scatter' or 'contour'.")
  if (!is.logical(save)) stop("'save' must be TRUE or FALSE.")

  # setup
  x_coords <- grid_coords$x
  y_coords <- grid_coords$y
  z_coords <- grid_coords$z

  time_stamps <- seq(
    from = as.POSIXct(start),
    to = as.POSIXct(end),
    by = output_dt)

  if (dim(data)[1] != length(time_stamps)) {
    stop("Mismatch between concentration data time steps and time stamps.")
  }

  # build concentration df
  concentration_data <- data.frame()

  for (t_idx in seq_along(time_stamps)) {
    grid_concentration <- array(
      data[t_idx, ],
      dim = c(
        length(x_coords),
        length(y_coords),
        length(z_coords)
      )
    )
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

  if (plot_type == "scatter") {
    # plot: scatter
    plot <- plotly::plot_ly(
      data = concentration_data,
      x = ~x,
      y = ~y,
      z = ~z,
      frame = ~time,
      type = "scatter3d",
      mode = "markers",
      marker = list(
        size = 10,
        color = ~concentration,
        colorscale = list(
          c(0, 'blue'),
          #c(0.25, 'cyan'),
          c(0.5, 'yellow'),
          #c(0.75, 'orange'),
          c(1, 'red')
        ),
        colorbar = list(title = "Concentration (ppm)"),
        opacity = 1
      )
    ) |>
      plotly::layout(
        scene = list(
          xaxis = list(title = "X Coordinate"),
          yaxis = list(title = "Y Coordinate"),
          zaxis = list(title = "Z Coordinate")
        ),
        title = "Plume Movement Over Time (3D): Scatter Plot"
      )
  } else if (plot_type == "contour") {
    # plot: contour
    frame_scales <- concentration_data |>
      dplyr::group_by(time) |>
      dplyr::summarise(
        isomin = min(concentration, na.rm = TRUE),
        isomax = max(concentration, na.rm = TRUE)
      )

    concentration_data <- concentration_data |>
      dplyr::left_join(frame_scales, by = "time") |>
      dplyr::mutate(
        scaled_concentration = (concentration - isomin) / (isomax - isomin)
      )

    plot <- plotly::plot_ly(
      data = concentration_data,
      x = ~x,
      y = ~y,
      z = ~z,
      value = ~scaled_concentration,
      frame = ~time,
      type = "volume",
      isomin = 0,
      isomax = 1,
      surface = list(show = TRUE, count = 30),
      colorscale = list(
        c(0, 'blue'),
        #c(0.25, 'cyan'),
        c(0.5, 'yellow'),
        #c(0.75, 'orange'),
        c(1, 'red')
      ),
      colorbar = list(title = "Concentration (ppm)"),
      opacity = 0.2
    ) |>
      plotly::layout(
        scene = list(
          xaxis = list(title = "X Coordinate"),
          yaxis = list(title = "Y Coordinate"),
          zaxis = list(title = "Z Coordinate")
        ),
        title = "Plume Movement Over Time (3D): Contour Plot"
      )
  }

  # animation opts
  plot <- plot |>
    plotly::animation_opts(
      frame = frames,
      transition = transition,
      redraw = TRUE
    )

  # save?
  if (save) {
    tryCatch({
      file_name <- ifelse(plot_type == "scatter", "3D_scatter.html", "3D_contour.html")
      htmlwidgets::saveWidget(plot, file_name, selfcontained = TRUE)
      message("Plot successfully saved as '", file_name, "', at: ", getwd())
    }, error = function(e) {
      warning("Failed to save the plot as an HTML file. Error: ", e$message)
    })
  }

  return(plot)
}

#' Plot Single Emission Rate Sensor Concentrations
#'
#' This function generates a plot of sensor concentrations over time using
#' precomputed sensor data (e.g., from `simulate_sensor_mode()`).
#'
#' @param sensor_concentrations Data frame. Output from a sensor simulation function,
#'   which must include a column named "Group.1" which contains the timestamps (e.g., "YYYY-MM-DD HH:MM:SS") and a column "Sensor_1"
#'   for the sensor concentration values.
#' @param sensor_coords Numeric vector. Coordinates (x, y, z) of the sensor.
#'
#' @return A ggplot object showing sensor concentrations over time.
#'
#' @export
#'
#'@examples
#' \dontrun{
#' # Assuming 'sensor_concentrations' is a data frame obtained from simulate_sensor_mode
#'
#' sensor_coords <- matrix(c(10, 0, 0), ncol = 3)
#' single_emission_rate_plot(sensor_concentrations, sensor_coords)
#' }
single_emission_rate_plot <- function(sensor_concentrations, sensor_coords) {

  # sensor_concentrations must fit the data from simulate_sensor_mode
  if (!is.data.frame(sensor_concentrations)) {
    stop("sensor_concentrations must be a data frame.")
  }
  if (nrow(sensor_concentrations) == 0) {
    stop("sensor_concentrations is empty. Please provide data from simulate_sensor_mode.")
  }
  if (!"Group.1" %in% names(sensor_concentrations)) {
    stop("sensor_concentrations must contain a column named 'Group.1' with POSIX time values.")
  }
  sensor_cols <- grep("^Sensor_\\d+$", names(sensor_concentrations), value = TRUE)
  if (length(sensor_cols) == 0) {
    stop("sensor_concentrations must contain at least one sensor concentration column (e.g., 'Sensor_1').")
  }
  # sensor_coords must be either a numeric vector of length 3 or a matrix/data.frame with 3 columns.
  if (is.matrix(sensor_coords) || is.data.frame(sensor_coords)) {
    sensor_coords <- as.matrix(sensor_coords)
    if (ncol(sensor_coords) != 3) {
      stop("sensor_coords must have exactly 3 columns (x, y, z).")
    }
    # If multiple sensors are provided, check that the number of rows equals the number of sensor columns.
    if (nrow(sensor_coords) > 1) {
      if (length(sensor_cols) != nrow(sensor_coords)) {
        stop("The number of sensor concentration columns in sensor_concentrations (",
             length(sensor_cols),
             ") does not match the number of sensor coordinate rows provided (",
             nrow(sensor_coords), ").")
      }
    }
  } else if (is.vector(sensor_coords)) {
    if (length(sensor_coords) != 3) {
      stop("sensor_coords must be a numeric vector of length 3 if only one sensor is provided.")
    }
  } else {
    stop("sensor_coords must be either a numeric vector of length 3 or a matrix/data.frame with 3 columns.")
  }
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("The 'ggplot2' package is required but not installed.")
  }

  sensor_concentrations$timestamp <- as.POSIXct(
    sensor_concentrations$Group.1, format = "%Y-%m-%d %H:%M:%S"
  )

  sensor_data <- data.frame(
    x = rep(sensor_coords[1], nrow(sensor_concentrations)),
    y = rep(sensor_coords[2], nrow(sensor_concentrations)),
    timestamp = sensor_concentrations$timestamp,
    concentration = sensor_concentrations$Sensor_1
  )

  sensor_data$time_label <- format(sensor_data$timestamp, "%H:%M")

  plot <- ggplot2::ggplot(sensor_data, ggplot2::aes(x = timestamp, y = concentration)) +
    ggplot2::geom_point(ggplot2::aes(color = concentration, size = concentration), alpha = 0.7) +
    ggplot2::scale_color_gradientn(
      colors = c("blue", "yellow", "red"),
      values = scales::rescale(c(0, 0.5, 1))  # Correct mapping
    ) +
    ggplot2::scale_size_continuous(range = c(1, 10)) +
    ggplot2::labs(
      title = "Sensor Concentrations Over Time",
      x = "Time",
      y = "Concentration (ppm)",
      color = "Concentration",
      size = "Concentration"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position = "right")

  return(plot)
}


#' Plot Time Series of Sensor Concentrations
#'
#' This function plots the time series of sensor concentrations.
#'
#' @param sensor_concentrations A data frame (or matrix) containing the output from
#'   the sensor simulation function (e.g., `simulate_sensor_mode()`). It must include:
#'   \describe{
#'     \item{Group.1}{A character vector of timestamps in the format `"YYYY-MM-DD HH:MM:SS"`.}
#'     \item{Sensor_1}{A numeric vector of sensor concentration values corresponding to each timestamp.}
#'   }
#'
#' @return A ggplot object showing the time series of sensor concentrations.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Assuming 'sensor_concentrations' is a data frame obtained from simulate_sensor_mode
#' time_series_plot(sensor_concentrations)
#' }
time_series_plot <- function(sensor_concentrations) {
  #sensor_concentrations must fit data from simulate_sensor_mode
  if (!is.data.frame(sensor_concentrations) && !is.matrix(sensor_concentrations)) {
    stop("sensor_concentrations must be a data.frame or matrix.")
  }
  sensor_concentrations <- as.data.frame(sensor_concentrations)
  if (nrow(sensor_concentrations) == 0) {
    stop("sensor_concentrations is empty. Please provide data from simulate_sensor_mode.")
  }
  if (!"Group.1" %in% names(sensor_concentrations)) {
    stop("sensor_concentrations must contain a column named 'Group.1' with POSIX time values.")
  }
  sensor_cols <- grep("^Sensor_\\d+$", names(sensor_concentrations), value = TRUE)
  if (length(sensor_cols) == 0) {
    stop("sensor_concentrations must contain at least one sensor concentration column (e.g., 'Sensor_1').")
  }
  for (col in sensor_cols) {
    if (!is.numeric(sensor_concentrations[[col]])) {
      stop("Sensor concentration column '", col, "' must be numeric.")
    }
  }
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("The 'dplyr' package is required but not installed.")
  }

  if (!requireNamespace("tidyr", quietly = TRUE)) {
    stop("The 'tidyr' package is required but not installed.")
  }

  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("The 'ggplot2' package is required but not installed.")
  }

  sensor_long <- sensor_concentrations |>
    dplyr::mutate(time2 = seq(0, nrow(sensor_concentrations) - 1)) |>
    tidyr::pivot_longer(
      cols = dplyr::starts_with("Sensor"),
      names_to = "Sensor",
      values_to = "Concentration"
    ) |>
    dplyr::rename(time = Group.1)

  sensor_long |>
    dplyr::mutate(Sensor = stringr::str_replace(Sensor, "Sensor_1", "Test Sensor")) |>
    ggplot2::ggplot(ggplot2::aes(x = time2, y = Concentration, group = Sensor)) +
    ggplot2::geom_line() +
    ggplot2::facet_wrap(~ Sensor) +
    ggplot2::labs(
      title = "Sensor Concentrations Over Time",
      subtitle = "With Time Steps",
      x = "Time from emission start (min)",
      y = "Concentration"
    ) +
    ggplot2::theme_bw()
}

#' Faceted Time Series Plot of Methane Concentrations and Wind Data
#'
#' This function creates a faceted plot showing methane concentrations along with wind data over time.
#'
#' @param sensor_concentrations Data frame. Output from a sensor simulation function,
#'   which must include a column named "Group.1" which contains the timestamps (e.g., "YYYY-MM-DD HH:MM:SS") and a column "Sensor_1"
#'   for the sensor concentration values.
#' @param wind_data A list containing wind data with components u and v
#' @param output_dt Integer. Desired time resolution (in seconds) for the final output of concentrations.
#' @param start_time POSIXct. Start time of the simulation.
#' @param end_time POSIXct. End time of the simulation.
#'
#' @return A ggplot object with faceted time series plots of methane concentrations and wind data.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Assuming 'sensor_concentrations' is a data frame obtained from simulate_sensor_mode
#'output_dt <- 60
#'start_time <- as.POSIXct("2024-01-01 12:00:00")
#'end_time <- as.POSIXct("2024-01-01 13:00:00")
#'wind_data <- list(
#'   wind_u = runif(3601, min = -3, max = 0.7),
#'   wind_v = runif(3601, min = -3, max = 1.5)
#'   )
#'
#' faceted_time_series_plot(sensor_concentrations, wind_data, start_time, end_time, output_dt)
#' }
faceted_time_series_plot <- function(sensor_concentrations, sensor_coords, wind_data, start_time, end_time, output_dt) {

  # Check required packages
  for(pkg in c("dplyr", "tidyr", "ggplot2", "scales", "patchwork")) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      stop(sprintf("The '%s' package is required but not installed.", pkg))
    }
  }

  ############################
  ##  Panel 1: Sensor Plot  ##
  ############################
  # This code is exactly as in single_emission_rate_plot

  if (!is.data.frame(sensor_concentrations)) {
    stop("sensor_concentrations must be a data frame.")
  }
  if (nrow(sensor_concentrations) == 0) {
    stop("sensor_concentrations is empty. Please provide data from simulate_sensor_mode.")
  }
  if (!"Group.1" %in% names(sensor_concentrations)) {
    stop("sensor_concentrations must contain a column named 'Group.1' with POSIX time values.")
  }
  sensor_cols <- grep("^Sensor_\\d+$", names(sensor_concentrations), value = TRUE)
  if (length(sensor_cols) == 0) {
    stop("sensor_concentrations must contain at least one sensor concentration column (e.g., 'Sensor_1').")
  }
  # sensor_coords must be either a numeric vector of length 3 or a matrix/data.frame with 3 columns.
  if (is.matrix(sensor_coords) || is.data.frame(sensor_coords)) {
    sensor_coords <- as.matrix(sensor_coords)
    if (ncol(sensor_coords) != 3) {
      stop("sensor_coords must have exactly 3 columns (x, y, z).")
    }
    if (nrow(sensor_coords) > 1) {
      if (length(sensor_cols) != nrow(sensor_coords)) {
        stop("The number of sensor concentration columns in sensor_concentrations (",
             length(sensor_cols),
             ") does not match the number of sensor coordinate rows provided (",
             nrow(sensor_coords), ").")
      }
    }
  } else if (is.vector(sensor_coords)) {
    if (length(sensor_coords) != 3) {
      stop("sensor_coords must be a numeric vector of length 3 if only one sensor is provided.")
    }
  } else {
    stop("sensor_coords must be either a numeric vector of length 3 or a matrix/data.frame with 3 columns.")
  }

  # Convert the timestamp column to POSIXct using the expected format.
  sensor_concentrations$timestamp <- as.POSIXct(sensor_concentrations$Group.1, format = "%Y-%m-%d %H:%M:%S", tz="UTC")

  sensor_data <- data.frame(
    x = rep(sensor_coords[1], nrow(sensor_concentrations)),
    y = rep(sensor_coords[2], nrow(sensor_concentrations)),
    timestamp = sensor_concentrations$timestamp,
    concentration = sensor_concentrations$Sensor_1
  )

  sensor_data$time_label <- format(sensor_data$timestamp, "%H:%M")

  p1 <- ggplot2::ggplot(sensor_data, ggplot2::aes(x = timestamp, y = concentration)) +
    ggplot2::geom_point(ggplot2::aes(color = concentration, size = concentration), alpha = 0.7) +
    ggplot2::scale_color_gradientn(
      colors = c("blue", "yellow", "red"),
      values = scales::rescale(c(0, 0.5, 1))
    ) +
    ggplot2::scale_size_continuous(range = c(1, 10)) +
    ggplot2::labs(
      title = "Sensor Concentrations Over Time",
      x = "Time",
      y = "Concentration (ppm)",
      color = "Concentration",
      size = "Concentration"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position = "right")

  #############################
  ##  Panel 2: Wind Plot     ##
  #############################
  if (!is.list(wind_data)) {
    stop("wind_data must be a list containing wind components.")
  }
  if (length(wind_data) < 2) {
    stop("wind_data must contain at least two elements (for u and v components).")
  }

  wind_u <- wind_data$wind_u
  wind_v <- wind_data$wind_v

  time_sequence <- seq(from = start_time, to = end_time, by = output_dt)
  wind_u_subset <- wind_u[seq_len(length(time_sequence))]
  wind_v_subset <- wind_v[seq_len(length(time_sequence))]

  wind_df <- data.frame(
    time = as.POSIXct(sensor_concentrations$Group.1[seq_len(length(time_sequence))], format = "%Y-%m-%d %H:%M:%S", tz = "UTC"),
    wind_u = wind_u_subset,
    wind_v = wind_v_subset
  ) %>%
    dplyr::mutate(
      wind_speed = sqrt(wind_u^2 + wind_v^2),
      angle = atan2(wind_v, wind_u) * (180 / pi),
      direction = dplyr::case_when(
        angle >= -22.5 & angle < 22.5 ~ "E",
        angle >= 22.5 & angle < 67.5 ~ "NE",
        angle >= 67.5 & angle < 112.5 ~ "N",
        angle >= 112.5 & angle < 157.5 ~ "NW",
        angle >= -67.5 & angle < -22.5 ~ "SE",
        angle >= -112.5 & angle < -67.5 ~ "S",
        angle >= -157.5 & angle < -112.5 ~ "SW",
        TRUE ~ "W"
      )
    )

  wind_scale_factor <- 0.1

  p2 <- ggplot2::ggplot() +
    ggplot2::geom_spoke(
      data = wind_df,
      ggplot2::aes(x = time, y = 0, angle = atan2(wind_v, wind_u), radius = wind_speed * wind_scale_factor),
      arrow = ggplot2::arrow(length = grid::unit(0.1, "cm"))
    ) +
    ggplot2::geom_text(
      data = wind_df,
      ggplot2::aes(x = time, y = 0.5, label = direction),
      size = 3, vjust = -0.5
    ) +
    ggplot2::labs(
      title = "Wind Speed and Direction Over Time",
      x = "Time",
      y = "Wind"
    ) +
    ggplot2::scale_x_datetime(date_labels = "%H:%M:%S") +
    ggplot2::theme_minimal()

  combined_plot <- p1 / p2 +
    patchwork::plot_annotation(title = "Sensor Concentrations and Wind Speed Over Time")

  return(combined_plot)
}

#' Create a Site Map of Sensors and Sources
#'
#' This function generates a site map showing the locations of sensors and sources.
#' It accepts inputs as either data frames or matrices. In the event that extra
#' columns (e.g., z coordinates) are present, only the first two columns (or the
#' columns named "x" and "y" if available) will be used.
#'
#' @param sensors A data frame or matrix containing sensor locations. It must include:
#'   - Either columns named \code{x} and \code{y}, or at least two columns where
#'     the first two are taken as \code{x} and \code{y} coordinates.
#' @param sources A data frame or matrix containing source locations. It must include:
#'   - Either columns named \code{x} and \code{y}, or at least two columns where
#'     the first two are taken as \code{x} and \code{y} coordinates.
#'
#' @return A ggplot object showing a site map of sensors and sources.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' sensors <- data.frame(x = c(1, 2, 3), y = c(4, 5, 6))
#' sources <- data.frame(x = c(7, 8), y = c(9, 10))
#'
#' create_site_map(sensors, sources)
#' }
create_site_map <- function(sensors, sources) {

  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("The 'ggplot2' package is required but not installed.")
  }

  # Helper function to process the coordinate input
  process_coords <- function(coords, label) {
    # Accept matrices by converting them to data frames.
    if (is.matrix(coords)) {
      coords <- as.data.frame(coords)
    } else if (!is.data.frame(coords)) {
      stop(sprintf("'%s' must be a data frame or matrix.", label))
    }

    # If columns "x" and "y" exist, extract them.
    if (all(c("x", "y") %in% names(coords))) {
      coords <- coords[, c("x", "y"), drop = FALSE]
    } else {
      # If not, ensure there are at least two columns and use the first two.
      if (ncol(coords) < 2) {
        stop(sprintf("'%s' must have at least two columns representing x and y coordinates.", label))
      }
      coords <- coords[, 1:2, drop = FALSE]
      names(coords)[1:2] <- c("x", "y")
    }
    return(coords)
  }

  # Process both sensors and sources
  sensors <- process_coords(sensors, "sensors")
  sources <- process_coords(sources, "sources")

  sensors$type <- "Sensor"
  sources$type <- "Source"

  combined <- rbind(sensors, sources)

  ggplot2::ggplot(combined, ggplot2::aes(x = x, y = y, shape = type, color = type)) +
    ggplot2::geom_point(size = 4, stroke = 1.5) +
    ggplot2::scale_shape_manual(values = c("Sensor" = 21, "Source" = 4)) +
    ggplot2::scale_color_manual(values = c("Sensor" = "blue", "Source" = "red")) +
    ggplot2::labs(
      title = "Site Map",
      x = "Longitude",
      y = "Latitude"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position = "bottom")
}
