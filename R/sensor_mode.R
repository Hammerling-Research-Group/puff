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
simulate_sensor_mode <- function(time_stamps_sim, sensor_coords, 
                                 n_sim, num_sources, source_coords, 
                                 emission_rates, wind_data, 
                                 puff_duration = 1200) {
  ch4_sim <- matrix(0, nrow = n_sim, ncol = nrow(sensor_coords))
  
  for (i in 1:n_sim) {
    time_elapsed <- i * sim_dt
    current_time <- time_stamps_sim[i]
    
    # loop over sources
    for (src in 1:num_sources) {
      source_x <- source_coords[src, 1]
      source_y <- source_coords[src, 2]
      source_z <- source_coords[src, 3]
      
      # grab wind data at the ith time step
      u <- wind_data$wind_u[i]
      v <- wind_data$wind_v[i]
      
      # loop over sensor locations
      for (j in 1:nrow(sensor_coords)) {
        sensor_x <- sensor_coords[j, 1]
        sensor_y <- sensor_coords[j, 2]
        sensor_z <- sensor_coords[j, 3]
        
        # concentration for each sensor point
        concentration <- gaussian_puff(
          x = sensor_x,
          y = sensor_y,
          z = sensor_z,
          t = time_elapsed,
          q = emission_rates[src],
          u = u,
          v = v,
          z0 = source_z,
          wind_speed = sqrt(u^2 + v^2),  # adding: calc total wind speed from components
          sim_time = current_time
        )
        
        ch4_sim[i, j] <- ch4_sim[i, j] + concentration
      }
    }
  }
  
  return(ch4_sim) 
}
