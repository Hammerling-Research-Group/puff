#' Simulate Atmospheric Concentration at Sensor Locations
#'
#' This function simulates atmospheric methane concentrations at one or more sensor locations
#' using a Gaussian puff forward model. It supports one or multiple emission sources and assumes
#' each puff maintains a constant wind speed and direction throughout its lifetime. The function
#' accounts for puff dispersion based on wind conditions and atmospheric stability class.
#'
#' @param start_time POSIXct. Start time of the simulation.
#' @param end_time POSIXct. End time of the simulation.
#' @param source_coords Numeric vector or matrix. Source coordinates in meters (x, y, z). If a single source,
#'   pass as a vector. For multiple sources, use a matrix where each row is a source.
#' @param emission_rate Numeric. Emission rate from each source in kg/hr. Applied uniformly to all sources.
#' @param wind_data Data frame. Must contain `wind_u` and `wind_v` columns representing wind speed components
#'   in the x (east-west) and y (north-south) directions at each simulation timestep.
#' @param sensor_coords Numeric matrix. Sensor coordinates in meters (x, y, z); one row per sensor.
#' @param sim_dt Integer. Simulation time step in seconds (default: 1). Controls how often the simulation
#'   updates concentrations.
#' @param puff_dt Integer. Puff emission interval in seconds (default: 1). Controls how often a new puff is emitted.
#' @param output_dt Integer. Desired resolution in seconds for output concentrations.
#' @param puff_duration Numeric. Lifetime of each puff in seconds (default: 1200). Puffs are removed after this time.
#'
#' @return A data frame with aggregated sensor concentrations across time.
#'   Rows represent time intervals (`output_dt`), columns represent sensors (`Sensor_1`, `Sensor_2`, etc.).
#'
#' @details
#' - Each source emits puffs at regular intervals (`puff_dt`) with a fixed mass based on `emission_rate`.
#' - Wind speed and direction at the time of puff emission are used to advect the puff and determine dispersion.
#' - Puff position is analytically computed at each timestep based on wind, without tracking in-between steps.
#' - Puff dispersion is computed using stability-class-based sigma values from a fast lookup.
#' - Total sensor concentration is the sum of all active puff contributions at each timestep.
#' - Concentrations are aggregated into intervals matching `output_dt` before being returned.
#'
#' @references Jia, M., Fish, R., Daniels, W., Sprinkle, B. and Hammerling, D., 2024.
#'   Filling a critical need: a lightweight and fast Gaussian puff model implementation.
#'   doi: <10.26434/chemrxiv-2023-hc95q-v3>
#'
#' @examples
#' \dontrun{
#'   set.seed(123)
#'   sim_dt <- 10
#'   puff_dt <- 10
#'   output_dt <- 60
#'   start_time <- as.POSIXct("2024-01-01 12:00:00")
#'   end_time <- as.POSIXct("2024-01-01 13:00:00")
#'
#'   source_coords <- matrix(c(0, 0, 2.5), ncol = 3, byrow = TRUE)
#'
#'   sensor_coords <- matrix(c(
#'    -20, 0, 2.0,
#'      0, -20, 2.0,
#'     20, 0, 2.0,
#'      0, 20, 2.0,
#'     10, 10, 2.0
#'   ), ncol = 3, byrow = TRUE)
#'
#'   wind_data <- data.frame(
#'     wind_u = runif(3601, min = -3, max = 0.7),
#'     wind_v = runif(3601, min = -3, max = 1.5)
#'   )
#'
#'   out <- simulate_sensor_mode(
#'     start_time = start_time,
#'     end_time = end_time,
#'     source_coords = source_coords,
#'     emission_rate = 3.5,
#'     wind_data = wind_data,
#'     sensor_coords = sensor_coords,
#'     sim_dt = sim_dt,
#'     puff_dt = puff_dt,
#'     output_dt = output_dt,
#'     puff_duration = 1200
#'   )
#' }
#' @export
simulate_sensor_mode <- function(start_time, end_time,
                                  source_coords, emission_rate,
                                  wind_data, sensor_coords,
                                  sim_dt = 1, puff_dt = 1, output_dt,
                                  puff_duration = 1200) {

  if (!is.data.frame(wind_data)) {
    stop("`wind_data` must be a data frame or tibble.")
  }

  if (is.numeric(source_coords) && is.null(dim(source_coords))) {
    source_coords <- matrix(source_coords, nrow = 1)
  }

  if (!is.matrix(source_coords) || ncol(source_coords) != 3) {
    stop("`source_coords` must be a numeric vector of length 3 or a matrix with 3 columns (x, y, z).")
  }

  sim_timestamps <- seq(from = as.POSIXct(start_time),
                        to = as.POSIXct(end_time),
                        by = sim_dt)
  n_steps <- length(sim_timestamps)

  output_timestamps <- seq(from = as.POSIXct(start_time),
                           to = as.POSIXct(end_time),
                           by = output_dt)

  sensor_x <- sensor_coords[, 1]
  sensor_y <- sensor_coords[, 2]
  sensor_z <- sensor_coords[, 3]
  n_sensors <- nrow(sensor_coords)

  total_concentrations <- matrix(0, nrow = n_steps, ncol = n_sensors,
                                 dimnames = list(NULL, paste0("Sensor_", 1:n_sensors)))

  # loop over each source
  for (src_idx in seq_len(nrow(source_coords))) {
    src <- source_coords[src_idx, ]

    active_puffs <- data.frame(
      time_emitted = numeric(0),
      wind_u = numeric(0),
      wind_v = numeric(0),
      wind_speed = numeric(0),
      stab_class = character(0),
      mass = numeric(0),
      stringsAsFactors = FALSE
    )

    concentrations <- matrix(0, nrow = n_steps, ncol = n_sensors)

    for (t_idx in seq_len(n_steps)) {
      current_time <- sim_timestamps[t_idx]
      current_elapsed <- as.numeric(difftime(current_time, start_time, units = "secs"))

      # emit new puff
      if (t_idx == 1 || current_elapsed %% puff_dt == 0) {
        wind_u <- wind_data$wind_u[t_idx]
        wind_v <- wind_data$wind_v[t_idx]
        wind_speed <- sqrt(wind_u^2 + wind_v^2)
        stab_class <- get_stab_class(wind_speed, current_time)
        q_per_puff <- (emission_rate / 3600) * puff_dt

        new_puff <- data.frame(
          time_emitted = current_elapsed,
          wind_u = wind_u,
          wind_v = wind_v,
          wind_speed = wind_speed,
          stab_class = as.character(stab_class),
          mass = q_per_puff,
          stringsAsFactors = FALSE
        )

        active_puffs <- dplyr::bind_rows(active_puffs, new_puff)
      }

      active_puffs$time_elapsed <- current_elapsed - active_puffs$time_emitted
      active_puffs <- active_puffs[active_puffs$time_elapsed <= puff_duration, ]
      if (nrow(active_puffs) == 0) next

      puff_x <- src[1] + active_puffs$wind_u * active_puffs$time_elapsed
      puff_y <- src[2] + active_puffs$wind_v * active_puffs$time_elapsed

      total_dist <- sqrt((puff_x - src[1])^2 + (puff_y - src[2])^2)

      compute_concentration <- function(puff_idx) {
        gpuff(
          Q = active_puffs$mass[puff_idx],
          stab_class = active_puffs$stab_class[puff_idx],
          x_p = puff_x[puff_idx],
          y_p = puff_y[puff_idx],
          x_r_vec = sensor_x,
          y_r_vec = sensor_y,
          z_r_vec = sensor_z,
          total_dist = total_dist[puff_idx],
          H = src[3],
          U = active_puffs$wind_speed[puff_idx]
        )
      }

      sensor_contributions <- sapply(seq_len(nrow(active_puffs)), compute_concentration)

      if (is.vector(sensor_contributions)) {
        sensor_contributions <- matrix(sensor_contributions, nrow = n_sensors)
      }

      concentrations[t_idx, ] <- rowSums(sensor_contributions)
    }

    # agg concentration across all sources
    total_concentrations <- total_concentrations + concentrations
  }

  # aggregate to match output_dt
  c <- aggregate(total_concentrations,
                 by = list(cut(sim_timestamps, breaks = output_timestamps)),
                 FUN = mean)
  rownames(c) <- NULL

  return(c)
}
