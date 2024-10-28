#' Simulate Methane Concentrations at Sensor Locations
#'
#' @description This function simulates methane concentrations at specified sensor locations
#'    based on emissions from multiple sources and given wind data over a specified
#'    time period.
#'
#' @usage simulate_sensor_mode(time_stamps_sim, sensor_coords, n_sim, num_sources, source_coords, emission_rates, wind_data, puff_duration = 1200)
#' @param time_stamps_sim A vector of simulation time steps (in seconds).
#' @param sensor_coords A matrix or data frame with sensor coordinates (x, y, z), where each row represents a sensor.
#' @param n_sim An integer specifying the number of simulation steps to run.
#' @param num_sources An integer specifying the number of emission sources.
#' @param source_coords A matrix or data frame with source coordinates (x, y, z), where each row represents a source.
#' @param emission_rates A vector of emission rates for each source.
#' @param wind_data A data frame containing wind speed components `wind_u` and `wind_v` for each time step.
#' @param puff_duration Numeric. The duration of the puff in seconds, with a default value of 1200.
#'
#' @return A matrix with `n_sim` rows and `nrow(sensor_coords)` columns, where each entry represents the simulated
#' methane concentration at a specific sensor location for a given time step.
#'
#' @examples
#' time_stamps_sim <- seq(0, 3600, by = 60)
#' sensor_coords <- matrix(c(10, 10, 2, 20, 20, 2), ncol = 3, byrow = TRUE)
#' source_coords <- matrix(c(0, 0, 1), ncol = 3)
#' emission_rates <- c(1.0)
#' wind_data <- data.frame(wind_u = rep(2, length(time_stamps_sim)),
#'                         wind_v = rep(1, length(time_stamps_sim)))
#' simulate_sensor_mode(time_stamps_sim, sensor_coords, n_sim = 60, num_sources = 1,
#'                      source_coords = source_coords, emission_rates = emission_rates,
#'                      wind_data = wind_data)
#'
#' @export
simulate_sensor_mode <- function(time_stamps_sim, sensor_coords, 
                                 n_sim, num_sources, source_coords, 
                                 emission_rates, wind_data, 
                                 puff_duration = 1200) {
  ch4_sim <- matrix(0, nrow = n_sim, ncol = nrow(sensor_coords))
  
  for (i in 1:n_sim) {
    time_elapsed <- i * sim_dt
    
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
        
        # compute concentration for each sensor point
        concentration <- gaussian_puff(
          sensor_x, sensor_y, sensor_z, time_elapsed, 
          sigma_y = 0.5 + 0.1 * time_elapsed, 
          sigma_z = 0.5 + 0.1 * time_elapsed, 
          q = emission_rates[src], 
          u = u, 
          v = v,  # including 'v' component for advection in the y direction per Ryker
          z0 = source_z
        )
        
        ch4_sim[i, j] <- ch4_sim[i, j] + concentration
      }
    }
  }
  
  return(ch4_sim) 
}

  return(ch4_sim)
}
