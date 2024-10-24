#' @title Convert between (ws, wd) and (u,v)
#' @description This function
#' @usage wind_vector_convert(wind_speeds,wind_directions)
#' @param wind_speeds A list of float values of wind speeds in m/s at each time stamp
#' @param wind_directions A list of float values of wind directions in degrees at each time stamp following
#' the conventional definition: 0 -> wind blowing from North, 90 -> E, 180 -> S, 270 -> W
#' @return Quantities corresponding to the conversion direction
#' @export
#' @examples
#' Filler
wind_vector_convert <- function(wind_speeds,wind_directions){

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
#' Filler
interpolate_wind_data <- function(wind_speeds, wind_directions, sim_start, sim_end, puff_dt){
 ns = difftime(sim_start,sim_end,units='secs')
 n_obs = floor(ns/puff_dt)+1
 #time_stamps = unsure how to complete this in R, I found 2 functions that I'm not sure would work
 if (length(wind_speeds) == n_obs-1){
   wind_speeds <- append(wind_speeds,wind_speeds[-1])
 }

 if (length(wind_directions) == n_obs-1){
   wind_speeds <- append(wind_directions,wind_directions[-1])
 }
 wind_uv <- wind_vector_convert(wind_speeds, wind_directions)
 wind_u <- wind_uv$u
 wind_v <- wind_uv$v

 wind_df <- data.frame(wind_u = wind_u, wind_v = wind_v, time = time_stamps)

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

