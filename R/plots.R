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
#' @param frames Numeric. Duration between frames in the animation (milliseconds). Default is 100.
#' @param transition Numeric. Duration for transitioning between frames (milliseconds). Default is 99.
#' @param save Logical. If `TRUE`, saves the plot as an HTML file named `2D_heatmap.html` and specifies saved location. Default set to `FALSE`.
#' @param interpolate_grid Logical. If `TRUE`, applies interpolation to refine grid resolution and make the heatmap smoother. Default `FALSE`.
#' @param granularity Numeric. Sets the number of points in the finer grid resolution when `interpolate_grid = TRUE`. Default `100`.
#'
#' @return A `plotly` object representing the animated heatmap.
#'
#' @examples
#' \donttest{
#' set.seed(123)
#'
#' sim_dt <- 10
#' puff_dt <- 10
#' output_dt <- 60
#' start_time <- "2024-01-01 12:00:00"
#' end_time <- "2024-01-01 13:00:00"
#' source_coords <- c(0, 0, 2.5)
#' emission_rate <- 3.5
#' wind_data <- data.frame(
#'   wind_u = runif(3601, min = -3, max = 0.7),
#'   wind_v = runif(3601, min = -3, max = 1.5)
#' )
#'
#' grid_coords <- list(
#'   x = seq(-2, 2, by = 1),
#'   y = seq(-2, 2, by = 1),
#'   z = seq(0, 5, by = 1)
#' )
#'
#' out <- simulate_grid_mode(
#'   start_time = start_time,
#'   end_time = end_time,
#'   source_coords = source_coords,
#'   emission_rate = emission_rate,
#'   wind_data = wind_data,
#'   grid_coords = grid_coords,
#'   sim_dt = sim_dt,
#'   puff_dt = puff_dt,
#'   output_dt = output_dt,
#'   puff_duration = 1200
#' )
#'
#' plot_2d_animated(data = out,
#'   grid_coords = grid_coords,
#'   start = start_time,
#'   end = end_time,
#'   output_dt = output_dt)
#' }
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

  z_index <- 1
  n_x <- length(x_coords)
  n_y <- length(y_coords)
  n_z <- length(z_coords)

  for (t_idx in seq_along(time_stamps)) {
    values_at_t <- data[t_idx, ]  # should be length n_x * n_y * n_z

    slice_start <- (z_index - 1) * n_x * n_y + 1
    slice_end <- z_index * n_x * n_y
    slice_values <- values_at_t[slice_start:slice_end]

    grid_concentration <- matrix(slice_values, nrow = n_x, ncol = n_y)

    if (ncol(grid_concentration) != length(y_coords)) {
      warning("Mismatch between the dimensions of 'data' and the grid coordinates. Check your inputs.")
    }

    long_data <- expand.grid(x = x_coords, y = y_coords)
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
#' @param frames Numeric. Duration between frames in the animation (milliseconds). Default is 100.
#' @param transition Numeric. Duration for transitioning between frames (milliseconds). Default is 99.
#' @param plot_type Character. "contour" (default) or "scatter" to specify the type of plot.
#' @param save Logical. If `TRUE`, saves the plot as an HTML file named `2D_heatmap.html` and specifies saved location. Default set to `FALSE`.
#'
#' @return A `plotly` object representing the animated plot.
#' @export
#'
#' @examples
#' \donttest{
#' set.seed(123)
#'
#' sim_dt <- 10
#' puff_dt <- 10
#' output_dt <- 60
#' start_time <- "2024-01-01 12:00:00"
#' end_time <- "2024-01-01 13:00:00"
#' source_coords <- c(0, 0, 2.5)
#' emission_rate <- 3.5
#' wind_data <- data.frame(
#'   wind_u = runif(3601, min = -3, max = 0.7),
#'   wind_v = runif(3601, min = -3, max = 1.5)
#' )
#'
#' grid_coords <- list(
#'   x = seq(-2, 2, by = 1),
#'   y = seq(-2, 2, by = 1),
#'   z = seq(0, 5, by = 1)
#' )
#'
#' out <- simulate_grid_mode(
#'   start_time = start_time,
#'   end_time = end_time,
#'   source_coords = source_coords,
#'   emission_rate = emission_rate,
#'   wind_data = wind_data,
#'   grid_coords = grid_coords,
#'   sim_dt = sim_dt,
#'   puff_dt = puff_dt,
#'   output_dt = output_dt,
#'   puff_duration = 1200
#' )
#'
#' plot_3d_animated(out,
#'    grid_coords,
#'    start_time, end_time,
#'    output_dt)
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


#' Plot Multiple Emission Rate Sensor Concentrations
#'
#' This function generates a faceted (if multiple sensors) bubble plot of sensor concentrations over time
#' using precomputed sensor data (e.g., from `simulate_sensor_mode()`).
#'
#' @param sensor_concentrations Data frame. Output from a sensor simulation function,
#'   which must include a column named "Group.1" for timestamps and one or more columns
#'   named "Sensor_1", "Sensor_2", etc., for the sensor concentration values.
#' @param sensor_coords Numeric vector or matrix. Coordinates (x, y, z) of the sensor(s).
#' @param text_size Default at 12.
#'
#' @return A ggplot object showing sensor concentrations over time, faceted by sensor.
#'
#' @examples
#' set.seed(123)
#' sim_dt <- 10
#' puff_dt <- 10
#' output_dt <- 60
#' start_time <- "2024-01-01 12:00:00"
#' end_time <- "2024-01-01 13:00:00"
#' emission_rate <- 3.5
#' wind_data <- data.frame(
#'   wind_u = runif(3601, min = -3, max = 0.7),
#'   wind_v = runif(3601, min = -3, max = 1.5)
#' )
#' source_coords <- c(0, 0, 2.5)
#' sensor_coords <- matrix(c(-6.525403221327715e-15, -35.52264, 2.01775), ncol = 3, byrow = TRUE)
#'
#' out <- simulate_sensor_mode(
#'   start_time, end_time, source_coords,
#'   emission_rate, wind_data, sensor_coords, sim_dt, puff_dt, output_dt, puff_duration = 1200
#' )
#'
#' single_emission_rate_plot(out, sensor_coords)
#' @export
single_emission_rate_plot <- function(sensor_concentrations, sensor_coords, text_size = 12) {

  if (!is.data.frame(sensor_concentrations)) stop("sensor_concentrations must be a data frame.")
  if (nrow(sensor_concentrations) == 0) stop("sensor_concentrations is empty.")
  if (!"Group.1" %in% names(sensor_concentrations)) stop("Missing 'Group.1' column for timestamps.")

  sensor_cols <- grep("^Sensor_\\d+$", names(sensor_concentrations), value = TRUE)
  if (length(sensor_cols) == 0) stop("No sensor concentration columns found (e.g., 'Sensor_1').")

  if (is.matrix(sensor_coords) || is.data.frame(sensor_coords)) {
    sensor_coords <- as.matrix(sensor_coords)
    if (ncol(sensor_coords) != 3) stop("sensor_coords must have 3 columns (x, y, z).")
    if (nrow(sensor_coords) != length(sensor_cols)) {
      stop("Number of sensor coordinate rows does not match number of sensor columns.")
    }
  } else if (is.numeric(sensor_coords) && length(sensor_coords) == 3) {
    sensor_coords <- matrix(sensor_coords, nrow = 1)
  } else {
    stop("sensor_coords must be a numeric vector of length 3 or a matrix/data.frame with 3 columns.")
  }

  if (!requireNamespace("ggplot2", quietly = TRUE) || !requireNamespace("tidyr", quietly = TRUE)) {
    stop("Packages 'ggplot2' and 'tidyr' are required.")
  }

  sensor_concentrations$timestamp <- as.POSIXct(sensor_concentrations$Group.1, format = "%Y-%m-%d %H:%M:%S")

  long_data <- tidyr::pivot_longer(
    sensor_concentrations,
    cols = tidyselect::all_of(sensor_cols),
    names_to = "sensor",
    values_to = "concentration"
  )

  plot <- ggplot2::ggplot(long_data, ggplot2::aes(x = timestamp, y = concentration)) +
    ggplot2::geom_point(ggplot2::aes(color = concentration, size = concentration), alpha = 0.7) +
    ggplot2::scale_color_gradientn(
      colors = c("blue", "yellow", "red"),
      values = scales::rescale(c(min(long_data$concentration, na.rm = TRUE),
                                 mean(long_data$concentration, na.rm = TRUE),
                                 max(long_data$concentration, na.rm = TRUE))),
      breaks = pretty(long_data$concentration, n = 8),
      guide = "legend"
    ) +
    ggplot2::scale_size_continuous(
      range = c(1, 10),
      breaks = pretty(long_data$concentration, n = 8),
      guide = "legend"
    ) +
    ggplot2::labs(
      title = "Sensor Concentrations (ppm) Over Time",
      x = "Time",
      y = "Concentration (ppm)",
      color = "Concentration (ppm)",
      size = "Concentration (ppm)"
    ) +
    ggplot2::facet_wrap(~ sensor) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      legend.position = "right",
      text = ggplot2::element_text(size = text_size)
    )

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
#' @param text_size Default at 12.
#'
#' @return A ggplot object showing the time series of sensor concentrations.
#'
#' @examples
#' set.seed(123)
#' sim_dt <- 10
#' puff_dt <- 10
#' output_dt <- 60
#' start_time <- "2024-01-01 12:00:00"
#' end_time <- "2024-01-01 13:00:00"
#' emission_rate <- 3.5
#' wind_data <- data.frame(
#'   wind_u = runif(3601, min = -3, max = 0.7),
#'   wind_v = runif(3601, min = -3, max = 1.5)
#' )
#' source_coords <- c(0, 0, 2.5)
#' sensor_coords <- matrix(c(-6.525403221327715e-15, -35.52264, 2.01775), ncol = 3, byrow = TRUE)
#'
#' out <- simulate_sensor_mode(
#'   start_time, end_time, source_coords,
#'   emission_rate, wind_data, sensor_coords, sim_dt, puff_dt, output_dt, puff_duration = 1200
#' )
#'
#' time_series_plot(out)
#' @export
time_series_plot <- function(sensor_concentrations, text_size = 12) {

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

  for(pkg in c("dplyr", "tidyr", "ggplot2")) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      stop(sprintf("The '%s' package is required but not installed.", pkg))
    }
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
    ggplot2::ggplot(ggplot2::aes(x = time2, y = Concentration, group = Sensor)) +
    ggplot2::geom_line() +
    ggplot2::facet_wrap(~ Sensor) +
    ggplot2::labs(
      title = "Sensor Concentrations (ppm) Over Time",
      x = "Time from start",
      y = "Concentration (ppm)"
    ) +
    ggplot2::theme_bw() +
    ggplot2::theme(text = ggplot2::element_text(size = text_size))
}


#' Faceted Time Series Plot of Methane Concentrations and Wind Data
#'
#' This function creates a faceted bubble plot of methane concentrations and a shared wind rose plot.
#'
#' @param sensor_concentrations Data frame. Output from simulate_sensor_mode().
#' @param sensor_coords A data frame or matrix containing sensor locations.
#' @param wind_data A list with wind_u and wind_v components.
#' @param output_dt Time step (in seconds) for aligning wind data with concentration data.
#' @param start_time POSIXct start of simulation.
#' @param end_time POSIXct end of simulation.
#' @param text_size Default at 12.
#'
#' @return A ggplot object: faceted concentration plot + single wind rose plot.
#'
#' @examples
#' set.seed(123)
#' sim_dt <- 10
#' puff_dt <- 10
#' output_dt <- 60
#' start_time <- "2024-01-01 12:00:00"
#' end_time <- "2024-01-01 13:00:00"
#' emission_rate <- 3.5
#' wind_data <- data.frame(
#'   wind_u = runif(3601, min = -3, max = 0.7),
#'   wind_v = runif(3601, min = -3, max = 1.5)
#' )
#' source_coords <- c(0, 0, 2.5)
#' sensor_coords <- matrix(c(-6.525403221327715e-15, -35.52264, 2.01775), ncol = 3, byrow = TRUE)
#'
#' out <- simulate_sensor_mode(
#'   start_time, end_time, source_coords,
#'   emission_rate, wind_data, sensor_coords, sim_dt, puff_dt, output_dt, puff_duration = 1200
#' )
#'
#' faceted_time_series_plot(out, sensor_coords,
#'   wind_data, as.POSIXct(start_time), as.POSIXct(end_time),
#'   output_dt
#'   )
#' @export
faceted_time_series_plot <- function(sensor_concentrations,
                                     sensor_coords,
                                     wind_data,
                                     start_time,
                                     end_time,
                                     output_dt,
                                     text_size = 12) {

  for (pkg in c("dplyr", "tidyr", "ggplot2", "scales", "patchwork")) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      stop(sprintf("The '%s' package is required but not installed.", pkg))
    }
  }

  if (start_time > end_time) {
    stop("start_time must be before end_time")
  }

  sensor_cols <- grep("^Sensor_\\d+$", names(sensor_concentrations), value = TRUE)
  if (length(sensor_cols) == 0) stop("No sensor concentration columns found.")

  sensor_concentrations$timestamp <- as.POSIXct(sensor_concentrations$Group.1,
                                                format = "%Y-%m-%d %H:%M:%S", tz = "UTC")

  long_data <- tidyr::pivot_longer(
    sensor_concentrations,
    cols = tidyselect::all_of(sensor_cols),
    names_to = "sensor",
    values_to = "concentration"
  )

  # Panel 1: Faceted Bubble Plot
  p1 <- ggplot2::ggplot(long_data, ggplot2::aes(x = timestamp, y = concentration,
                                                color = concentration, size = concentration)) +
    ggplot2::geom_point(alpha = 0.7) +
    ggplot2::scale_color_gradientn(
      colors = c("blue", "yellow", "red"),
      values = scales::rescale(c(min(long_data$concentration, na.rm = TRUE),
                                 mean(long_data$concentration, na.rm = TRUE),
                                 max(long_data$concentration, na.rm = TRUE))),
      guide = "legend"
    ) +
    ggplot2::scale_size_continuous(
      range = c(1, 10),
      guide = "legend"
    ) +
    ggplot2::facet_wrap(~ sensor) +
    ggplot2::labs(
      title = "Sensor Concentrations (ppm) Over Time",
      x = "Time from start",
      y = "Concentration (ppm)",
      color = "Concentration (ppm)",
      size = "Concentration (ppm)"
    ) +
    ggplot2::theme_bw() +
    ggplot2::theme(text = ggplot2::element_text(size = text_size))

  # Panel 2: Wind Rose
  if (!inherits(start_time, "POSIXct")) {
    stop("start_time must be a POSIXct object")
  }
  if (!inherits(end_time, "POSIXct")) {
    stop("end_time must be a POSIXct object")
  }

  time_sequence <- seq(from = start_time, to = end_time, by = output_dt)
  wind_u <- wind_data$wind_u[seq_along(time_sequence)]
  wind_v <- wind_data$wind_v[seq_along(time_sequence)]

  wind_df <- data.frame(
    time = time_sequence,
    wind_u = wind_u,
    wind_v = wind_v
  ) |>
    dplyr::mutate(
      wind_speed = sqrt(wind_u^2 + wind_v^2),
      angle = (atan2(wind_v, wind_u) * (180 / pi) + 180) %% 360,
      direction = dplyr::case_when(
        angle >= -22.5 & angle < 22.5   ~ "E",
        angle >= 22.5 & angle < 67.5    ~ "NE",
        angle >= 67.5 & angle < 112.5   ~ "N",
        angle >= 112.5 & angle < 157.5  ~ "NW",
        angle >= -67.5 & angle < -22.5  ~ "SE",
        angle >= -112.5 & angle < -67.5 ~ "S",
        angle >= -157.5 & angle < -112.5 ~ "SW",
        TRUE ~ "W"
      )
    )

  wind_rose_data <- wind_df |>
    dplyr::group_by(direction) |>
    dplyr::summarise(
      count = dplyr::n(),
      mean_speed = mean(wind_speed, na.rm = TRUE),
      .groups = "drop"
    ) |>
    tidyr::complete(
      direction = c("N", "NE", "E", "SE", "S", "SW", "W", "NW"),
      fill = list(count = 0, mean_speed = 0)
    )

  # wind rose: radial bars = frequency, fill = wind speed
  p2 <- ggplot2::ggplot(wind_rose_data, ggplot2::aes(x = direction, y = count, fill = mean_speed)) +
    ggplot2::geom_col(color = "white", width = 1) +
    ggplot2::coord_polar(start = -pi / 8) +  # orient N to top
    ggplot2::scale_x_discrete(limits = c("N", "NE", "E", "SE", "S", "SW", "W", "NW")) +
    ggplot2::scale_fill_gradient(
      name = "Avg Wind Speed (m/s)"
    ) +
    ggplot2::labs(
      title = "Wind Conditions",
      x = NULL,
      y = NULL,
      caption = "Radial bar height = frequency; color = average wind speed"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.text.y = ggplot2::element_blank(),
      axis.ticks.y = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_line(color = "lightgray"),
      panel.grid.minor = ggplot2::element_blank(),
      text = ggplot2::element_text(size = text_size),
      legend.position = "right"
    )

  combined_plot <- p1 / p2 +
    patchwork::plot_annotation(title = "Sensor Concentrations and Wind Conditions")

  return(combined_plot)
}


#' Alternate version with wind rose at each time step + scatter plot of methane concentration time series
#'
#' @param sensor_concentrations Data frame. Output from a sensor simulation function,
#'   which must include a column named "Group.1" which contains the timestamps (e.g., "YYYY-MM-DD HH:MM:SS") and a column "Sensor_1"
#'   for the sensor concentration values.
#' @param sensor_coords A data frame or matrix containing sensor locations.
#' @param wind_data A list containing wind data with components u and v
#' @param output_dt Integer. Desired time resolution (in seconds) for the final output of concentrations.
#' @param start_time POSIXct. Start time of the simulation.
#' @param end_time POSIXct. End time of the simulation.
#' @param text_size Default at 12.
#'
#' @return A ggplot object with faceted time series plots of methane concentrations and wind rose data.
#'
#' @examples
#' set.seed(123)
#' sim_dt <- 10
#' puff_dt <- 10
#' output_dt <- 60
#' start_time <- "2024-01-01 12:00:00"
#' end_time <- "2024-01-01 13:00:00"
#' emission_rate <- 3.5
#' wind_data <- data.frame(
#'   wind_u = runif(3601, min = -3, max = 0.7),
#'   wind_v = runif(3601, min = -3, max = 1.5)
#' )
#' source_coords <- c(0, 0, 2.5)
#' sensor_coords <- matrix(c(-6.525403221327715e-15, -35.52264, 2.01775), ncol = 3, byrow = TRUE)
#'
#' out <- simulate_sensor_mode(
#'   start_time, end_time, source_coords,
#'   emission_rate, wind_data, sensor_coords, sim_dt, puff_dt, output_dt, puff_duration = 1200
#' )
#'
#' faceted_time_series_plot2(out, sensor_coords,
#'   wind_data, as.POSIXct(start_time), as.POSIXct(end_time),
#'   output_dt
#'   )
#' @export
faceted_time_series_plot2 <- function(sensor_concentrations,
                                      sensor_coords,
                                      wind_data,
                                      start_time,
                                      end_time,
                                      output_dt,
                                      text_size = 12) {

  # Check required packages
  for(pkg in c("dplyr", "tidyr", "ggplot2", "scales", "patchwork")) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      stop(sprintf("The '%s' package is required but not installed.", pkg))
    }
  }

  ############################
  ##  Panel 1: Sensor Plot  ##
  ############################

  if (!is.data.frame(sensor_concentrations)) {
    stop("sensor_concentrations must be a data frame.")
  }

  if (nrow(sensor_concentrations) == 0) {
    stop("sensor_concentrations is empty. Please provide data from simulate_sensor_mode.")
  }

  if (!"Group.1" %in% names(sensor_concentrations)) {
    stop("sensor_concentrations must contain a column named 'Group.1' with POSIX time values.")
  }

  if (!inherits(start_time, "POSIXct")) {
    stop("start_time must be a POSIXct object")
  }

  if (!inherits(end_time, "POSIXct")) {
    stop("end_time must be a POSIXct object")
  }

  if (start_time >= end_time) {
    stop("start_time must be before end_time")
  }

  if (!is.numeric(output_dt) || length(output_dt) != 1 || output_dt <= 0) {
    stop("output_dt must be a single positive numeric value")
  }

  sensor_concentrations$timestamp <- as.POSIXct(sensor_concentrations$Group.1,
                                                format = "%Y-%m-%d %H:%M:%S",
                                                tz = "UTC")

  sensor_data <- data.frame(
    x = rep(sensor_coords[1], nrow(sensor_concentrations)),
    y = rep(sensor_coords[2], nrow(sensor_concentrations)),
    timestamp = sensor_concentrations$timestamp,
    concentration = sensor_concentrations$Sensor_1
  )

  p1 <- ggplot2::ggplot(sensor_data, ggplot2::aes(x = timestamp, y = concentration)) +
    ggplot2::geom_point(ggplot2::aes(color = concentration,
                            size = concentration),
                        alpha = 0.7) +
    ggplot2::scale_color_gradientn(
      colors = c("blue", "yellow", "red"),
      values = scales::rescale(c(min(sensor_data$concentration),
                                 mean(sensor_data$concentration),
                                 max(sensor_data$concentration))),
      breaks = pretty(sensor_data$concentration, n = 7),
      guide = "legend"
    ) +
    ggplot2::scale_size_continuous(
      range = c(1, 10),
      breaks = pretty(sensor_data$concentration, n = 7),
      guide = "legend"
    ) +
    ggplot2::labs(
      title = "Sensor Concentrations (ppm)",
      x = "Time from start",
      y = "Concentration (ppm)",
      color = "Concentration (ppm)",
      size = "Concentration (ppm)",
      caption = "Point size and color correspond to concentration levels."
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position = "none",
                   text = ggplot2::element_text(size = text_size))

  #############################
  ##  Panel 2: Wind Rose Plot ##
  #############################

  if (!is.list(wind_data)) {
    stop("wind_data must be a list containing wind components.")
  }

  wind_u <- wind_data$wind_u
  wind_v <- wind_data$wind_v

  time_sequence <- seq(from = start_time,
                       to = end_time,
                       by = output_dt)
  wind_u_subset <- wind_u[seq_len(length(time_sequence))]
  wind_v_subset <- wind_v[seq_len(length(time_sequence))]

  wind_df <- data.frame(
    time = as.POSIXct(sensor_concentrations$Group.1[seq_len(length(time_sequence))],
                      format = "%Y-%m-%d %H:%M:%S", tz = "UTC"),
    wind_u = wind_u_subset,
    wind_v = wind_v_subset
  ) |>
    dplyr::mutate(
      wind_speed = sqrt(wind_u^2 + wind_v^2),
      angle = atan2(wind_v, wind_u) * (180 / pi),
      direction = dplyr::case_when(
        angle >= -22.5 & angle < 22.5  ~ "E",
        angle >= 22.5 & angle < 67.5   ~ "NE",
        angle >= 67.5 & angle < 112.5  ~ "N",
        angle >= 112.5 & angle < 157.5 ~ "NW",
        angle >= -67.5 & angle < -22.5 ~ "SE",
        angle >= -112.5 & angle < -67.5 ~ "S",
        angle >= -157.5 & angle < -112.5 ~ "SW",
        TRUE ~ "W"
      )
    )

  wind_rose_data <- wind_df |>
    dplyr::group_by(time) |>
    tidyr::expand(direction = c("N", "NE", "E", "SE", "S", "SW", "W", "NW")) |>
    dplyr::left_join(wind_df, by = c("time", "direction")) |>
    tidyr::replace_na(list(wind_speed = 0)) |>
    dplyr::mutate(time = format(time, "%H:%M")) |>
    tidyr::drop_na(time)

  p2 <- ggplot2::ggplot(wind_rose_data, ggplot2::aes(x = direction, y = wind_speed)) +
    ggplot2::geom_col(fill = "black",
                      width = 0.5 * max(wind_rose_data$wind_speed, na.rm = TRUE),
                      position = "identity") +
    ggplot2::coord_polar(start = -0.5) +
    ggplot2::facet_wrap(~ time, nrow = 1) +
    ggplot2::scale_y_continuous(limits = c(0, max(wind_rose_data$wind_speed)),
                                expand = c(0, 0)) +
    ggplot2::scale_x_discrete(limits = c("N", "NE", "E", "SE", "S", "SW", "W", "NW")) +
    ggplot2::labs(
      title = " ",
      x = NULL,
      y = "",
      caption = "Wind speed (m/s) corresponds to wedge width in wind rose plots."
    ) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      axis.text.y = ggplot2::element_blank(),
      axis.ticks.y = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      text = ggplot2::element_text(size = text_size)
    )


  ########################
  ##  Combine Plots    ##
  ########################
  combined_plot <- (p1 / p2) +
    patchwork::plot_layout(heights = c(2, 1)) +
    patchwork::plot_annotation(title = "Sensor Concentrations and Wind Roses Over Time")

  return(combined_plot)

}


#' Create a Site Map of Sensors and Sources
#'
#' This function generates a site map with an adjacent compass rose.
#'
#' @param sensors Coordinates for sensor locations.
#' @param sources Coordinates for source locations.
#' @param text_size Numeric. Font size for labels. Default is 12.
#'
#' @return A patchwork-combined ggplot object: site map + compass rose.
#'
#' @examples
#' source_coords <- c(0, 0, 2.5)
#'
#' n_sensors <- 8
#' radius <- 20
#' z_height <- 2.0
#'
#' angles <- seq(0, 2 * pi, length.out = n_sensors + 1)[- (n_sensors + 1)]
#'
#' sensor_coords <- matrix(
#'   cbind(radius * cos(angles), radius * sin(angles), rep(z_height, n_sensors)),
#'   ncol = 3
#' )
#'
#' create_site_map(sensor_coords, source_coords)
#' @export
create_site_map <- function(sensors, sources, text_size = 12) {
  for (pkg in c("ggplot2", "patchwork")) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      stop(sprintf("The '%s' package is required but not installed.", pkg))
    }
  }

  to_xy_df <- function(obj, label) {
    if (is.numeric(obj) && is.null(dim(obj))) {
      if (length(obj) < 2) stop(sprintf("'%s' must have at least two values to represent x and y.", label))
      obj <- matrix(obj[1:2], ncol = 2)
    }

    df <- as.data.frame(obj)
    if (ncol(df) < 2) stop(sprintf("'%s' must have at least two columns to represent x and y coordinates.", label))
    df <- df[, 1:2]
    names(df) <- c("x", "y")
    df$type <- label
    df
  }

  sensor_df <- to_xy_df(sensors, "Sensor")
  source_df <- to_xy_df(sources, "Source")
  site_df <- rbind(sensor_df, source_df)

  site_map <- ggplot2::ggplot(site_df, ggplot2::aes(x = x, y = y)) +
    ggplot2::geom_point(
     ggplot2::aes(shape = factor(type), color = factor(type)),
     size = 4, stroke = 1.5
    ) +
   ggplot2::scale_shape_manual(values = c("Sensor" = 21, "Source" = 4)) +
   ggplot2::scale_color_manual(values = c("Sensor" = "blue", "Source" = "red")) +
   ggplot2::labs(title = "Site Map",
                  x = "Longitude",
                  y = "Latitude",
                  col = " ",
                  shape = " ") +
    ggplot2::theme_bw() +
    ggplot2::theme(
      legend.position = "bottom",
      text = ggplot2::element_text(size = text_size)
    )

  compass_df <- data.frame(
    x = 0, y = 0,
    xend = c(0, 0, 1, -1),
    yend = c(1, -1, 0, 0),
    label = c("N", "S", "E", "W")
  )

  compass_plot <- ggplot2::ggplot() +
    ggplot2::geom_segment(data = compass_df,
                          ggplot2::aes(x = x, y = y, xend = xend, yend = yend),
                          arrow = ggplot2::arrow(length = ggplot2::unit(0.2, "cm")),
                          linewidth = 1) +
    ggplot2::geom_text(data = compass_df,
                       ggplot2::aes(x = 1.2 * xend, y = 1.2 * yend, label = label),
                       size = text_size / 3.5) +
    ggplot2::xlim(-1.5, 1.5) +
    ggplot2::ylim(-1.5, 1.5) +
    ggplot2::coord_fixed() +
    ggplot2::theme_void() +
    ggplot2::labs(title = " ")

  site_map + compass_plot + patchwork::plot_layout(widths = c(4, 1))
}
