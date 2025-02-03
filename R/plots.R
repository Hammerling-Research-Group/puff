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
#' @param plot_type Character. "scatter" (default) or "contour" to specify the type of plot.
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
#'                  "2025-01-01 01:00:00", "1 min", 100, 99, plot_type = "scatter", save = TRUE)
#' }
plot_3d_animated <- function(data, grid_coords, start, end, output_dt,
                             frames = 100, transition = 99, plot_type = "scatter", save = FALSE) {
  # chk: pkgs
  if (!requireNamespace("plotly", quietly = TRUE)) {
    stop("The 'plotly' package is required but not installed.")
  }

  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("The 'dplyr' package is required but not installed.")
  }

  if (!requireNamespace("htmlwidgets", quietly = TRUE) && save) {
    stop("The 'htmlwidgets' package is required to save the plot.")
  }

  # chk: data types / classes + other qc
  if (!is.array(data)) {
    stop("'data' must be a 3D array.")
  }

  if (!is.list(grid_coords) || !all(c("x", "y", "z") %in% names(grid_coords))) {
    stop("'grid_coords' must contain 'x', 'y', and 'z'.")
  }

  if (!is.character(start) || !is.character(end)) {
    stop("'start' and 'end' must be character strings.")
  }

  if (as.POSIXct(start) >= as.POSIXct(end)) {
    stop("'start' time must be earlier than 'end' time.")
  }

  if (!is.character(output_dt) && !is.numeric(output_dt)) {
    stop("'output_dt' must be a valid time interval.")
  }

  if (!is.numeric(frames) || frames <= 0) {
    stop("'frames' must be a positive number.")
  }

  if (!is.numeric(transition) || transition < 0) {
    stop("'transition' must be non-negative.")
  }

  if (!plot_type %in% c("scatter", "contour")) {
    stop("'plot_type' must be either 'scatter' or 'contour'.")
  }

  if (!is.logical(save)) {
    stop("'save' must be TRUE or FALSE.")
  }

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
          c(0.25, 'cyan'),
          c(0.5, 'yellow'),
          c(0.75, 'orange'),
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
        c(0.25, 'cyan'),
        c(0.5, 'yellow'),
        c(0.75, 'orange'),
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

  plot <- ggplot(sensor_data, aes(x = timestamp, y = y, fill = concentration)) +
    geom_tile() +
    scale_fill_gradientn(colors = c("blue", "red", "yellow"), name = "Concentration") +
    labs(
      title = "Sensor Concentrations Over Time",
      x = "Time (Hour:Minute)",
      y = NULL
    ) +
    theme_minimal() +
    theme(
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      panel.grid = element_blank()
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
  sensor_long <- sensor_concentrations |>
    mutate(time2 = seq(0, nrow(sensor_concentrations) - 1)) |>
    pivot_longer(
      cols = starts_with("Sensor"),
      names_to = "Sensor",
      values_to = "Concentration"
    ) |>
    rename(time = Group.1)

  sensor_long |>
    mutate(Sensor = str_replace(Sensor, "Sensor_1", "Test Sensor")) |>
    ggplot(aes(x = time2, y = Concentration, group = Sensor)) +
    geom_line() +
    facet_wrap(~ Sensor) +
    labs(
      title = "Sensor Concentrations Over Time",
      subtitle = "With Time Steps",
      x = "Time from emission start (min)",
      y = "Concentration"
    ) +
    theme_bw()
}

#' Faceted Time Series Plot of Methane Concentrations and Wind Data
#'
#' This function creates a faceted plot showing methane concentrations along with wind data over time.
#'
#' @param sensor_concentrations Data frame. Output from a sensor simulation function,
#'   which must include a column named "Group.1" which contains the timestamps (e.g., "YYYY-MM-DD HH:MM:SS") and a column "Sensor_1"
#'   for the sensor concentration values.
#'   }
#' @param wind_data A list containing wind data with components u and v
#'   }
#' @param time_sequence A POSIXct vector of time steps corresponding to the wind data.
#'
#' @return A ggplot object with faceted time series plots of methane concentrations and wind data.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Assuming 'sensor_concentrations' is a data frame obtained from simulate_sensor_mode
#'
#' wind_data <- list(
#'   wind_u = runif(length(time_sequence), min = -5, max = 5),
#'   wind_v = runif(length(time_sequence), min = -5, max = 5)
#' )
#'
#' faceted_time_series_plot(sensor_concentrations, wind_data, time_sequence)
#' }
faceted_time_series_plot <- function(sensor_concentrations, wind_data, time_sequence) {
  sensor_concentrations <- sensor_concentrations %>%
    rename(
      time = Group.1,
      concentration = Sensor_1
    )

  # Subset wind data to match the length of time_sequence
  wind_u_subset <- wind_data$wind_u[seq_len(length(time_sequence))]
  wind_v_subset <- wind_data$wind_v[seq_len(length(time_sequence))]

  # Combine wind data with sensor concentrations
  sensor_concentrations <- sensor_concentrations %>%
    mutate(
      wind_u = wind_u_subset[seq_len(nrow(sensor_concentrations))],
      wind_v = wind_v_subset[seq_len(nrow(sensor_concentrations))]
    )

  # Pivot data into long format for plotting
  sensor_concentrations_long <- sensor_concentrations %>%
    pivot_longer(
      cols = c(concentration, wind_u, wind_v),
      names_to = "variable",
      values_to = "value"
    )

  facet_labels <- c(
    concentration = "Methane Concentration (kg/m³)",
    wind_u = "Wind Component U (m/s)",
    wind_v = "Wind Component V (m/s)"
  )

  ggplot(sensor_concentrations_long, aes(x = time, y = value, color = variable, group = variable)) +
    geom_line() +
    geom_point() +
    facet_wrap(~ variable, scales = "free_y", ncol = 1, labeller = labeller(variable = facet_labels)) +
    labs(
      title = "Time Series of Methane Concentration and Wind Data",
      x = "Time",
      y = "Value"
    ) +
    theme_minimal() +
    theme(legend.position = "none")
}

#' Create a Site Map of Sensors and Sources
#'
#' This function generates a site map showing the locations of sensors and sources.
#'
#' @param sensors A data frame containing sensor locations. It must include:
#'   \describe{
#'     \item{x}{A numeric vector of sensor x-coordinates.}
#'     \item{y}{A numeric vector of sensor y-coordinates.}
#'   }
#' @param sources A data frame containing source locations. It must include:
#'   \describe{
#'     \item{x}{A numeric vector of source x-coordinates.}
#'     \item{y}{A numeric vector of source y-coordinates.}
#'   }
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
  # Add a type column for sensors and sources
  sensors$type <- "Sensor"
  sources$type <- "Source"

  # Combine sensors and sources into one data frame
  combined <- rbind(sensors, sources)

  ggplot(combined, aes(x = x, y = y, shape = type, color = type)) +
    geom_point(size = 4, stroke = 1.5) +
    scale_shape_manual(values = c("Sensor" = 21, "Source" = 4)) +
    scale_color_manual(values = c("Sensor" = "blue", "Source" = "red")) +
    labs(
      title = "Site Map of Locations",
      x = "Longitude",
      y = "Latitude",
      shape = "Location Type",
      color = "Location Type"
    ) +
    theme_minimal() +
    theme(legend.position = "bottom")
}
