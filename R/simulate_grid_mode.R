#' Simulate Atmospheric Concentration on a Grid
#'
#' Simulates methane concentrations at each grid point over time using the Gaussian puff forward model.
#' Supports one or more emission sources. Each puff retains constant wind speed and direction
#' throughout its lifetime, and corresponding dispersion parameters are determined at the time of emission.
#'
#' @usage simulate_grid_mode(start_time, end_time, source_coords, emission_rate, wind_data,
#'   grid_coords, sim_dt, puff_dt, output_dt, puff_duration, ws, wd)
#'
#' @param start_time POSIXct. Start time of the simulation.
#' @param end_time POSIXct. End time of the simulation.
#' @param source_coords Numeric vector or matrix. Coordinates of the emission source(s) in meters (x, y, z).
#'   If simulating multiple sources, provide a matrix with one row per source.
#'   E.g., \code{matrix(c(0, 0, 2.5, 10, 10, 2.5), ncol = 3, byrow = TRUE)}.
#' @param emission_rate Numeric. Emission rate in kg/hr per source. If multiple sources are provided, this
#'   value will be assumed the same for each. (Note: source-specific rates are not yet supported.)
#' @param wind_data Data frame. Must contain either columns `wind_u` and `wind_v` (wind vector components in x/y directions)
#'   or columns representing wind speed and direction, declared as `ws` and `wd`.
#' @param ws Optional. String. Name of the column in `wind_data` containing wind speeds (m/s).
#'   Required if `wind_data` contains polar wind components instead of Cartesian (`wind_u`, `wind_v`).
#' @param wd Optional. String. Name of the column in `wind_data` containing wind directions (degrees from).
#' @param grid_coords List. A list with three numeric vectors specifying the grid points for
#'   \code{x}, \code{y}, and \code{z} coordinates, e.g.,
#'   \code{list(x = seq(-50, 50, by = 5), y = seq(-50, 50, by = 5), z = c(2.5))}.
#' @param sim_dt Integer. Simulation time step in seconds (default = 1). Determines how frequently puff positions are updated.
#' @param puff_dt Integer. Puff emission interval in seconds (default = 1). New puffs are emitted from each source at this frequency.
#' @param output_dt Integer. Desired time resolution (in seconds) for final output concentrations.
#' @param puff_duration Numeric. Maximum puff lifetime in seconds (default = 1200). Puffs beyond this age are discarded.
#'
#' @note All time parameters should be positive, with `puff_dt > sim_dt` and `out_dt > sim_dt`. Also, `puff_dt` should be a positive integer multiple of `sim_dt`, i.e. `puff_dt = n*sim_dt` for some positive integer `n`. This prevents the code having to interpolate the concentration values in time, although it is likely that this constraint could be avoided.
#'
#' @return A matrix of concentrations (ppm) with rows representing output time steps and columns
#'   representing grid points. Columns correspond to the flattened grid defined by \code{expand.grid(grid_coords)}.
#'
#' @details
#' - Each source (from one to many) emits puffs at intervals of \code{puff_dt}.
#' - Each puff maintains a fixed wind vector and dispersion parameters.
#' - Puffs are advected over time based on their individual wind vectors.
#' - Concentration contributions from all active puffs are computed at each grid point and summed.
#' - Concentrations are aggregated and returned at a coarser time resolution defined by \code{output_dt}.
#'
#' @references Jia, M., Fish, R., Daniels, W., Sprinkle, B. and Hammerling, D. (2024) <doi:10.26434/chemrxiv-2023-hc95q-v3>
#'
#' @examples
#' \donttest{
#' set.seed(123)
#'
#' sim_dt <- 7
#' puff_dt <- 7
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
#'   z = c(2.5)
#' )
#'   out <- simulate_grid_mode(
#'     start_time = start_time,
#'     end_time = end_time,
#'     source_coords = source_coords,
#'     emission_rate = emission_rate,
#'     wind_data = wind_data,
#'     grid_coords = grid_coords,
#'     sim_dt = sim_dt,
#'     puff_dt = puff_dt,
#'     output_dt = output_dt,
#'     puff_duration = 1200
#'   )
#' }
#' @export
simulate_grid_mode <- function(start_time, end_time,
                               source_coords, emission_rate,
                               wind_data, grid_coords,
                               sim_dt = 1, puff_dt = 1, output_dt,
                               puff_duration = 1200,
                               ws = NULL,
                               wd = NULL) {

  if (!is.data.frame(wind_data)) stop("`wind_data` must be a data frame.")
  if (is.numeric(source_coords) && is.null(dim(source_coords))) {
    source_coords <- matrix(source_coords, nrow = 1)
  }

  if (!is.null(ws) && !is.null(wd)) {
    if (!all(c(ws, wd) %in% names(wind_data))) {
      stop("Specified `ws` and/or `wd` not found in `wind_data`.")
    }
    wind_components <- wind_vector_convert(
      wind_speeds = wind_data[[ws]],
      wind_directions = wind_data[[wd]]
    )
    wind_data$wind_u <- wind_components$u
    wind_data$wind_v <- wind_components$v
  }

  if (!all(c("wind_u", "wind_v") %in% names(wind_data))) {
    stop("`wind_data` must contain either `wind_u` and `wind_v`, or valid `ws` and `wd`.")
  }

  sim_timestamps <- seq(from = as.POSIXct(start_time),
                        to = as.POSIXct(end_time),
                        by = sim_dt)
  n_steps <- length(sim_timestamps)

  output_timestamps <- seq(from = as.POSIXct(start_time),
                           to = as.POSIXct(end_time),
                           by = output_dt)
  n_output_steps <- length(output_timestamps)

  grid_df <- expand.grid(
    x = grid_coords$x,
    y = grid_coords$y,
    z = grid_coords$z
  )
  n_gridpoints <- nrow(grid_df)

  total_concentrations <- matrix(0, nrow = n_output_steps, ncol = n_gridpoints)

  # loop over sources
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

    concentrations <- matrix(0, nrow = n_output_steps, ncol = n_gridpoints)

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
      puff_z <- src[3]

      grid_conc_step <- numeric(n_gridpoints)

      # compute contribution of each puff to every grid point
      for (puff_idx in seq_len(nrow(active_puffs))) {
        dx <- grid_df$x - puff_x[puff_idx]
        dy <- grid_df$y - puff_y[puff_idx]
        dz <- grid_df$z - puff_z

        total_dist <- sqrt((src[1] - puff_x[puff_idx])^2 + (src[2] - puff_y[puff_idx])^2)

        conc <- gpuff(
          Q = active_puffs$mass[puff_idx],
          stab_class = active_puffs$stab_class[puff_idx],
          x_p = puff_x[puff_idx],
          y_p = puff_y[puff_idx],
          x_r_vec = grid_df$x,
          y_r_vec = grid_df$y,
          z_r_vec = grid_df$z,
          total_dist = total_dist,
          H = src[3],
          U = active_puffs$wind_speed[puff_idx]
        )

        grid_conc_step <- grid_conc_step + conc
      }

      if (t_idx %% (output_dt / sim_dt) == 0) {
        output_idx <- t_idx / (output_dt / sim_dt)
        concentrations[output_idx, ] <- grid_conc_step
      }
    }

    # agg from source
    total_concentrations <- total_concentrations + concentrations
  }

  return(total_concentrations)
}
