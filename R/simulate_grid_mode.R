#' Simulate Atmospheric Concentration on a Grid
#'
#' Simulates atmospheric concentrations at every grid point over time using the Gaussian puff forward model.
#' Concentrations are calculated based on emission rates, wind conditions, and puff dispersion.
#'
#' @usage simulate_grid_mode(sim_dt, puff_dt, output_dt, start_time, end_time, source_coords,
#'   emission_rate, wind_data, grid_coords, puff_duration)
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
#' @param grid_coords List. A list with three numeric vectors specifying the grid points for \code{x}, \code{y}, and \code{z}.
#'   E.g., \code{list(x = seq(0, 50, by = 1), y = seq(0, 50, by = 1), z = c(2.5))}.
#' @param puff_duration Numeric. Lifetime of each puff (in seconds) before it dissipates. Default at 1200.
#' @references Jia, M., Fish, R., Daniels, W., Sprinkle, B. and Hammerling, D., 2024. Filling a critical need: a lightweight and fast Gaussian puff model implementation. doi: <10.26434/chemrxiv-2023-hc95q-v3>
#' @return An array of methane concentrations at every grid point over time
#'
#' @examples
#' \dontrun{
#' set.seed(123)
#' sim_dt <- 4
#' puff_dt <- 4
#' output_dt <- 60
#' start_time <- "2024-01-01 12:00:00"
#' end_time <- "2024-01-01 13:00:00"
#' source_coords <- c(0, 0, 2.5)
#' emission_rate <- 3.5
#'
#' fake_times <- seq(0, 10, length.out = 61)
#' wind_speeds <- rep(3, 61)
#' wind_data <- data.frame(
#'  wind_u = runif(3601, min = -3, max = 0.7),
#'  wind_v = runif(3601, min = -3, max = 1.5)
#' )
#'
#' grid_coords <- list(
#'   x = seq(-5, 5, by = 1),
#'   y = seq(-5, 5, by = 1),
#'   z = c(2.5)
#' )
#'
#' grid_concentrations_original <- simulate_grid_mode(
#'     sim_dt = sim_dt,
#'     puff_dt = puff_dt,
#'     output_dt = output_dt,
#'     start_time = start_time,
#'     end_time = end_time,
#'     source_coords = source_coords,
#'     emission_rate = emission_rate,
#'     wind_data = wind_data,
#'     grid_coords = grid_coords,
#'     puff_duration = 1200
#' )
#' }
#' @export
simulate_grid_mode <- function(sim_dt, puff_dt, output_dt,
                               start_time, end_time,
                               source_coords, emission_rate,
                               wind_data, grid_coords,
                               puff_duration = 1200) {

  # setup
  sim_timestamps <- seq(from = as.POSIXct(start_time),
                        to = as.POSIXct(end_time),
                        by = sim_dt)

  n_steps <- length(sim_timestamps)

  output_timestamps <- seq(from = as.POSIXct(start_time),
                           to = as.POSIXct(end_time),
                           by = output_dt)

  n_output_steps <- length(output_timestamps)

  x_seq <- grid_coords$x
  y_seq <- grid_coords$y
  z_seq <- grid_coords$z

  grid_dim <- c(length(x_seq), length(y_seq), length(z_seq))

  n_gridpoints <- prod(grid_dim)

  # init a 2D matrix for output
  concentrations <- matrix(0, nrow = n_output_steps, ncol = n_gridpoints)

  # init active puffs as a data frame
  active_puffs <- data.frame(
    x = numeric(0),
    y = numeric(0),
    z = numeric(0),
    time_emitted = numeric(0),
    time_elapsed = numeric(0)
  )

  # loop over sim steps
  for (t_idx in seq_len(n_steps)) {
    current_time <- sim_timestamps[t_idx]

    wind_u <- wind_data$wind_u[t_idx]
    wind_v <- wind_data$wind_v[t_idx]
    wind_speed <- sqrt(wind_u^2 + wind_v^2)

    stab_class <- get.stab.class(wind_speed, current_time)

    # emit new puffs if it's a puff emission interval
    # first: convert emission rate (Q) from kg/hr to kg/s and calculate mass per puff (q)
    q_per_puff <- (emission_rate / 3600) * puff_dt # [kg]

    if (t_idx == 1 || as.numeric(difftime(current_time, start_time, units = "secs")) %% puff_dt == 0) {
      new_puffs <- data.frame(
        x = source_coords[1],
        y = source_coords[2],
        z = source_coords[3],
        time_emitted = as.numeric(difftime(current_time, start_time, units = "secs")),
        time_elapsed = 0,
        wind_u = wind_u,
        wind_v = wind_v,
        mass = q_per_puff  # mass per puff
      )
      active_puffs <- rbind(active_puffs, new_puffs)
    }

    # update positions and lifetimes of active puffs per sim_dt
    active_puffs$time_elapsed <- as.numeric(difftime(current_time, start_time, units = "secs")) - active_puffs$time_emitted
    active_puffs$x <- active_puffs$x + active_puffs$wind_u * sim_dt
    active_puffs$y <- active_puffs$y + active_puffs$wind_v * sim_dt

    active_puffs <- active_puffs[active_puffs$time_elapsed <= puff_duration, ]

    # calculate concentration contributions for each grid point
    grid_concentrations <- array(0, dim = grid_dim)

    for (puff_idx in seq_len(nrow(active_puffs))) {
      puff_x <- active_puffs$x[puff_idx]
      puff_y <- active_puffs$y[puff_idx]
      puff_z <- active_puffs$z[puff_idx]

      for (i in seq_along(x_seq)) {
        for (j in seq_along(y_seq)) {
          for (k in seq_along(z_seq)) {
            grid_x <- x_seq[i]
            grid_y <- y_seq[j]
            grid_z <- z_seq[k]

            # dist from puff to grid point
            total_dist <- sqrt((source_coords[1] - puff_x)^2 + (source_coords[2] - puff_y)^2)

            concentration <- gpuff(
              Q = active_puffs$mass[puff_idx], # mass per puff instead of raw emission rate
              stab.class = stab_class,
              x.p = puff_x,
              y.p = puff_y,
              x.r.vec = grid_x,
              y.r.vec = grid_y,
              z.r.vec = grid_z,
              total.dist = total_dist,
              H = source_coords[3],
              U = wind_speed
            )

            grid_concentrations[i, j, k] <- grid_concentrations[i, j, k] + concentration
          }
        }
      }
    }

    # adj concentrations to match output time resolution
    if (t_idx %% (output_dt / sim_dt) == 0) {
      output_idx <- t_idx / (output_dt / sim_dt)
      concentrations[output_idx, ] <- as.vector(grid_concentrations)
    }
  }

  return(concentrations)
}
