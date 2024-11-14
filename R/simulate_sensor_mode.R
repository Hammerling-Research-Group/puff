#' Simulate Atmospheric Concentration at Sensor Locations
#'
#' Simulates atmospheric concentrations at sensor locations over time using a Gaussian puff forward model. 
#'   Concentrations are calculated based on emission rates, wind conditions, and puff dispersion.
#'
#' @usage simulate_sensor_mode(sim_dt, puff_dt, output_dt, start_time, end_time, source_coords, 
#'   emission_rate, wind_data, sensor_coords, puff_duration, stab_class)
#'
#' @param sim_dt Integer. Simulation time step (in seconds), determining how frequently the simulation 
#'   updates the positions of puffs and calculates concentrations. This is the base resolution of the simulation.
#' @param puff_dt Integer. Time interval (in seconds), determining how frequently new puffs are emitted into 
#'   the simulation.
#' @param output_dt Integer. Desired time resolution (in seconds) for the final output of concentrations.
#' @param start_time POSIXct. Start time of the simulation.
#' @param end_time POSIXct. End time of the simulation.
#' @param source_coords Numeric vector. Coordinates of the emission source in meters (x, y, z). 
#'   E.g, \code{c(x = 10, y = 0, z = 1.5)}.
#' @param emission_rate Numeric. Emission rate of the source in kg/s.
#' @param wind_data Data frame. Wind data containing columns \code{wind_u} and \code{wind_v}, 
#'   representing wind components in the x and y directions, respectively, for each simulation step.
#' @param sensor_coords Numeric matrix. Coordinates of sensors in meters (x, y, z format); row = sensor.
#' @param puff_duration Numeric. Lifetime of each puff (in seconds) before it dissipates. Default at 1200.
#' @param stab_class Character. Atmospheric stability class influencing dispersion (e.g., "A", "B", "C").
#'
#' @return A data frame containing aggregated methane concentrations at sensor locations, with rows 
#'   corresponding to output timestamps and columns to sensors.
#'
#' @details The simulation emits puffs from the specified source at intervals of \code{puff_dt} 
#'   and updates their positions based on wind data at intervals of \code{sim_dt}. Concentrations 
#'   at sensor locations are calculated using a Gaussian puff forward model, which accounts for dispersion 
#'   based on the stability class and other inputs. Finally, concentrations are aggregated 
#'   to match the \code{output_dt} resolution.
#'
#' @examples
#' \dontrun{
#'   set.seed(123)
#'   sim_dt <- 10
#'   puff_dt <- 10
#'   output_dt <- 60
#'   start_time <- "2024-01-01 00:00:00"
#'   end_time <- "2024-01-01 01:00:00"
#'   source_coords <- c(0, 0, 2.5)
#'   emission_rate <- 3.5
#'   set.seed(123)
#'   wind_data <- list(
#'     wind_u = runif(3601, min = -3, max = 0.7), 
#'     wind_v = runif(3601, min = -3, max = 1.5)
#'   )
#'   sensor_coords <- matrix(c(-6.525403221327715e-15, -35.52264, 2.01775), ncol = 3, byrow = TRUE)
#'   stab_class <- "C"
#'
#'   simulate_sensor_mode(
#'     sim_dt, puff_dt, output_dt, start_time, end_time, source_coords, 
#'     emission_rate, wind_data, sensor_coords, puff_duration, stab_class
#'   )
#' }
#'
#' @export
simulate_sensor_mode <- function(sim_dt, puff_dt, output_dt,
                                 start_time, end_time,
                                 source_coords, emission_rate,
                                 wind_data, sensor_coords,
                                 puff_duration = 1200, stab_class) {

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
