#' @title Convert between (ws, wd) and (u,v)
#' @description This function converts between coordinate systems by changing from degrees to radians
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

#' @title Resample wind_speeds and wind_directions to the simulation resolution by interpolation
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

#' @title Calculate the concentration of methane emissions at a sensor point
#' @description This function
#' @usage gaussian_puff(x,y,z,t,q,u,z0,wind_speed,sim_time)
#' @param x Numeric value(s) of x-coordinate (east-west) where concentration is calculated.
#' @param y Numeric value(s) of y-coordinate (north-south) where concentration is calculated.
#' @param z Numeric value(s) of z-coordinate (height) where concentration is calculated.
#' @param t Numeric value(s) of time elapsed since the release of the puff.
#' @param q Numeric value(s) of emission rate of the pollutant.
#' @param u Numeric value(s) of wind speed component from the x (east-west) direction.
#' @param v Numeric value(s) of wind speed component from the y (north-south) direction.
#' @param z0 Numeric value(s) of release height of the puff into the atmosphere.
#' @param wind_speed Numeric value of total wind speed, calculated as the magnitude of the wind vector.
#' @param sim_time POSIXct of current simulation time, used to determine day or night for stability classification.
#' @return Quantities corresponding to concentration at sensor point(s)
#' @export
#' @examples
#' gaussian_puff(sensor_x,sensor_y,sensor_z,time_elapsed,emission_rates,u,v,source_z,sqrt(u^2+v^2),current_time)
gaussian_puff <- function(x,y,z,t,q,u,z0,wind_speed=sqrt(u^2+v^2),sim_time){

  # calculate downwind distance
  downwind_distance <- wind_speed*t

  # classify stability - add function to this file or just to a file in the package and how to cite?
  stab_class <- get.stab.class(u,t) # is u just supposed to be the x component in this case?

  # calculate sigma values - same question as other function
  sigma_vec <- compute.sigma.vals(stab_class,downwind_distance)
  sigma_y <- sigma_vec[1]
  sigma_z <- sigma_vec[2]

  # adjust position of x and y
  u <- u*t # x component
  v <- v*t # y component

  # calculate the factor in which concentration of puffs decreases
  #exp_factor_x <-
  #exp_factor_z1 <- I'm unsure what the calculation for these are based on Will's code
  #exp_factor_z2 <- I'm  guessing there's a relationship between wind speed and the height in these factors

  # calculate concentration
  conversion_factor = (1e6) * (1.524) # convert kg/m^3 to ppm
  concentration <- ((q / ((2 * pi)^(3/2) * sigma_y^2 * sigma_z)) *
                      exp_factor_x * (exp_factor_z1 + exp_factor_z2)) * conversion_factor
}

