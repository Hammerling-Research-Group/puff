#' @title Convert between (ws, wd) and (u,v)
#' @description This function
#' @usage wind_vector_convert(wind_speeds,wind_directions)
#' @param wind_speeds A list of float values of wind speeds in m/s at each time stamp
#' @param wind_directions A list of float values of wind directions in degrees at each time stamp following
#' the conventional definition: 0 -> wind blowing from North, 90 -> E, 180 -> S, 270 -> W
#' @return Quantities corresponding to the conversion direction
#' @export
#' @examples
#' wind_vector_convert(speed_vec,direction_vec)
wind_vector_convert <- function(wind_speeds, wind_directions) {
  theta <- (270 - wind_directions) * pi / 180  # convert degrees to radians and shift by 270
  u <- wind_speeds * cos(theta)  # u (x-direction)
  v <- wind_speeds * sin(theta)  # v (y-direction)
  
  return(list(u = u, v = v))
}

#' @title Convert between (ws, wd) and (u,v)
#' @description This function
#' @usage interpolate_wind_data(wind_speeds, wind_directions, sim_start, sim_end, puff_dt)
#' @param wind_speeds A list of float values of wind speeds in m/s at each time stamp
#' @param wind_directions A list of float values of wind directions in degrees at each time stamp following
#' the conventional definition: 0 -> wind blowing from North, 90 -> E, 180 -> S, 270 -> W
#' @param sim_start Date & time stamps of simulation start time
#' @param sim_end Date & time stamps of simulation end time
#' @param puff_dt A scalar time interval between two puffs
#' @return Quantities corresponding to the conversion direction
#' @export
#' @examples
#' interpolate_wind_data(speed_vec,direction_vec,start,end,60)
interpolate_wind_data <- function(wind_speeds, wind_directions, sim_start, sim_end, puff_dt) {
  # convert wind speed and direction to u and v components
  wind_uv <- wind_vector_convert(wind_speeds, wind_directions)
  
  # time series of wind data at the observation interval
  obs_times <- seq(as.POSIXct(sim_start), as.POSIXct(sim_end), length.out = length(wind_speeds))
  
  # simulate times for every minute (e.g., puff_dt = 60 seconds) over the simulation duration
  sim_times <- seq(as.POSIXct(sim_start), as.POSIXct(sim_end), by = puff_dt)
  
  # interpolate wind (u and v) to match simulation resolution
  wind_u_interpolated <- approx(x = obs_times, y = wind_uv$u, xout = sim_times)$y
  wind_v_interpolated <- approx(x = obs_times, y = wind_uv$v, xout = sim_times)$y
  
  return(
    list(wind_u = wind_u_interpolated, 
         wind_v = wind_v_interpolated)
    )
}

#' @title Convert between (ws, wd) and (u,v)
#' @description This function
#' @usage gaussian_puff(x,y,z,t,sigma_y,sigma_z,q,u,z0)
#' @param x The horizontal position to calculate the concentration (east-west)
#' @param y The lateral position relative to the center of the puff (north-south)
#' @param z The vertical position to calculate the concentration (height)
#' @param t Time passed since the given puff
#' @param sigma_y Dispersion of the puff in the y direction; shows how far the puff has spread horizontally
#' @param sigma_z Dispersion of the puff in the z direction; shows how far the puff has spread vertically
#' @param q Emission rate
#' @param u Wind speed, which moves the puff along the x-axis over time
#'@param z0 Release height into the air
#' @return Quantities corresponding to the conversion direction
#' @export
#' @examples
#' Filler
gaussian_puff <- function(x,y,z,t,sigma_y,sigma_z,q,u,z0){

}

