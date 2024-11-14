#' Simulate Methane Concentration at Sensor Locations
#'
#' Calculates methane concentration over time at specified sensor locations
#' due to emissions from multiple sources. The simulation incorporates
#' advection, dispersion, and stability class calculations based on wind data.
#'
#' @param time_stamps_sim POSIXct vector. The sequence of timestamps for each simulation time step.
#' @param sensor_coords Matrix. A matrix with three columns representing the x, y, and z coordinates
#'   of each sensor where concentrations are calculated.
#' @param n_sim Integer. The number of simulation time steps.
#' @param num_sources Integer. The number of sources emitting methane.
#' @param source_coords Matrix. A matrix with three columns representing the x, y, and z coordinates
#'   of each source.
#' @param emission_rates Numeric vector. The emission rates for each source, controlling the rate of methane release.
#' @param wind_data List. A list containing wind speed components (`wind_u` for the x-direction and `wind_v`
#'   for the y-direction) interpolated for each simulation time step.
#' @param puff_duration Numeric. The time in seconds that a puff remains active before dissipating. Default is 1200 seconds.
#' @param puff_dt Numeric. The time in seconds between consecutive puff emissions. Default is 300 seconds.
#'
#' @return Matrix. A matrix with dimensions `n_sim` by the number of sensors, containing methane concentrations
#'   at each sensor for each time step.
#'
#' @examples
#' time_stamps <- seq(as.POSIXct("2024-01-01 00:00:00"), by = "min", length.out = 60)
#' sensor_coords <- matrix(c(200, 200, 0, 300, 300, 0, 400, 400, 0), ncol = 3, byrow = TRUE)
#' source_coords <- matrix(c(25, 25, 2, 75, 25, 2, 50, 75, 3), ncol = 3, byrow = TRUE)
#' emission_rates <- c(1, 5, 10)
#' wind_data <- list(wind_u = runif(60, 2, 5), wind_v = runif(60, 1, 3))
#' simulate_sensor_mode(time_stamps, sensor_coords, 60, 3, source_coords, emission_rates, wind_data)
#' @export
simulate_sensor_mode <- function(sim_dt, puff_dt, output_dt,
                                 start_time, end_time,
                                 source_coords, emission_rate,
                                 wind_data, sensor_coords,
                                 puff_duration, stab_class) {

  # set things up for sim + output
  sim_timestamps <- seq(from = as.POSIXct(start_time),
                        to = as.POSIXct(end_time),
                        by = sim_dt)
  n_steps <- length(sim_timestamps)

  output_timestamps <- seq(from = as.POSIXct(start_time),
                           to = as.POSIXct(end_time),
                           by = output_dt)
  
  n_sensors <- nrow(sensor_coords)

  concentrations <- matrix(0, nrow = n_steps, ncol = n_sensors,
                           dimnames = list(NULL, paste0("Sensor_", 1:n_sensors)))

  # initialize active puffs as a df for now for simplicity
  active_puffs <- data.frame(
    x = numeric(0), 
    y = numeric(0), 
    z = numeric(0),
    time_emitted = numeric(0), 
    time_elapsed = numeric(0)
  )

  # loop over each time/sim step
  for (t_idx in seq_len(n_steps)) {
    current_time <- sim_timestamps[t_idx]

    # store wind data for current step
    wind_u <- wind_data$wind_u[t_idx]
    wind_v <- wind_data$wind_v[t_idx]
    wind_speed <- sqrt(wind_u^2 + wind_v^2)

    # emit new puffs if arrived at a puff emission interval
    if (t_idx == 1 || as.numeric(difftime(current_time, start_time, units = "secs")) %% puff_dt == 0) {
      active_puffs <- rbind(active_puffs, data.frame(
        x = source_coords[1],
        y = source_coords[2],
        z = source_coords[3],
        time_emitted = as.numeric(difftime(current_time, start_time, units = "secs")),
        time_elapsed = 0
      ))
    }

    # update positions and lifetimes of active puffs per sim_dt
    active_puffs$time_elapsed <- as.numeric(difftime(current_time, start_time, units = "secs")) - active_puffs$time_emitted
    active_puffs$x <- active_puffs$x + wind_u * sim_dt
    active_puffs$y <- active_puffs$y + wind_v * sim_dt

    # drop dead/expired puffs
    active_puffs <- active_puffs[active_puffs$time_elapsed <= puff_duration, ]

    # loops to calc concentration contributions for each sensor
    for (s_idx in seq_len(n_sensors)) {
      sensor_x <- sensor_coords[s_idx, 1]
      sensor_y <- sensor_coords[s_idx, 2]
      sensor_z <- sensor_coords[s_idx, 3]

      for (puff in seq_len(nrow(active_puffs))) {
        puff_x <- active_puffs$x[puff]
        puff_y <- active_puffs$y[puff]
        total_dist <- sqrt((sensor_x - puff_x)^2 + (sensor_y - puff_y)^2)

        # calculate concentration
        concentration <- gpuff(
          Q = emission_rate,
          stab.class = stab_class,
          x.p = puff_x,
          y.p = puff_y,
          x.r.vec = sensor_x,
          y.r.vec = sensor_y,
          z.r.vec = sensor_z,
          total.dist = total_dist,
          H = source_coords[3],
          U = wind_speed
        )

        concentrations[t_idx, s_idx] <- concentrations[t_idx, s_idx] + concentration
      }
    }
  }

  # aggregate concentrations to fit with output_dt resolution
  c <- aggregate(concentrations, by = list(cut(sim_timestamps, 
                                               breaks = output_timestamps)), 
                 mean) # TODO: can update how we think about this (e.g., mean, median, etc.; could be tuneable arg)
  rownames(c) <- NULL

  return(c)
}
