#' @title Determine Whether a Time is During the Day
#' @description This function checks the time and classifies it as day or not
#' @usage is.day(time)
#' @param time A time value that is used to determine whether it's day or night.
#' @return A character T or F representing whether or not it is daytime.
#' @examples
#' is.day(8)
#' @export
is.day <- function(time){
  # function to check if a time is during the day
  # will be used to assign stability class in forward model
  # NOTE: times currently set to work in summer, future work will make this
  # more flexible.
  time.hour <- as.numeric(format(time, format = "%H"))

  if (time.hour >= 7 & time.hour <= 18){
    return(T)
  } else {
    return(F)
  }
}

#' @title Determine Stability Class Based on Wind Speed and Time of Day
#' @description This function calculates the stability class based on wind speed (U) and the time of day.
#'              It categorizes the atmosphere's stability as one of several classes (A-F) depending on the inputs.
#' @usage get.stab.class(U, time)
#' @param U A numeric value representing the wind speed in meters per second.
#' @param time A time value that is used to determine whether it's day or night.
#' @return A character vector representing the stability class(es) ("A" to "F").
#' @examples
#' get.stab.class(3, 12
#' @export
get.stab.class <- function(U, time){

  # Determine stability class based on wind speed and time of day
  if (U < 2){
    stab.class <- ifelse(is.day(time), list(c("A", "B")), list(c("E", "F")))[[1]]
  } else if (U >= 2 & U < 3){
    stab.class <- ifelse(is.day(time), list(c("B")),      list(c("E", "F")))[[1]]
  } else if (U >= 3 & U < 5){
    stab.class <- ifelse(is.day(time), list(c("B", "C")), list(c("D", "E")))[[1]]
  } else if (U >= 5 & U < 6){
    stab.class <- ifelse(is.day(time), list(c("C", "D")), list(c("D")))[[1]]
  } else {
    stab.class <- ifelse(is.day(time), list(c("D")),      list(c("D")))[[1]]
  }

  return(stab.class)

}

#' @title Find Average Sigma Values Based On Stability Class and Total Distance Traveled
#' @description This function calculates the average sigma values based on stability class and the total distance traveled.
#'              It reports the averages of the sigma values dependent on the inputs.
#' @usage compute.sigma.vals(stab.class, total.dist)
#' @param stab.class A character vector representing the stability class(es) ("A" to "F").
#' @param total.dist A numeric value representing the distance traveled in --units--.
#' @return A numeric vector representing the average sigma values over the stability classes passed to the function.
#' @examples
#' compute.sigma.vals(A, 0.7)
#' @export
compute.sigma.vals <- function(stab.class, total.dist){

  n.stab.class <- length(stab.class)

  sigma.y.vals <- sigma.z.vals <- vector(length = n.stab.class)

  # Loop through stability classes and get a, b, c, and d parameter values
  # based on the stability class and total distance traveled.
  for (stab.class.it in 1:n.stab.class){

    if (stab.class[stab.class.it] == "A"){

      if (total.dist <= 0.1){
        a <- 122.8
        b <- 0.9447
      } else if (total.dist <= 0.15){
        a <- 158.08
        b <- 1.0542
      } else if (total.dist <= 0.20){
        a <- 170.22
        b <- 1.0932
      } else if (total.dist <= 0.25){
        a <- 179.52
        b <- 1.1262
      } else if (total.dist <= 0.3){
        a <- 217.41
        b <- 1.2644
      } else if (total.dist <= 0.4){
        a <- 258.89
        b <- 1.4094
      } else if (total.dist <= 0.5){
        a <- 346.75
        b <- 1.7283
      } else {
        a <- 453.85
        b <- 2.1166
      }

      c <- 24.1670
      d <- 2.5334

    } else if (stab.class[stab.class.it] == "B"){

      if (total.dist <= 0.2){
        a <- 90.673
        b <- 0.93198
      } else if (total.dist <= 0.4){
        a <- 98.483
        b <- 0.98332
      } else {
        a <- 109.3
        b <- 1.09710
      }

      c <- 18.333
      d <- 1.8096

    } else if (stab.class[stab.class.it] == "C"){

      a <- 61.141
      b <- 0.91465
      c <- 12.5
      d <- 1.0857

    } else if (stab.class[stab.class.it] == "D"){

      if (total.dist <= 0.3){
        a <- 34.459
        b <- 0.86974
      } else if (total.dist <= 1){
        a <- 32.093
        b <- 0.81066
      } else if (total.dist <= 3){
        a <- 32.093
        b <- 0.64403
      } else if (total.dist <= 10){
        a <- 33.504
        b <- 0.60486
      } else if (total.dist <= 30){
        a <- 36.65
        b <- 0.56589
      } else {
        a <- 44.053
        b <- 0.51179
      }

      c <- 8.333
      d <- 0.72382

    } else if (stab.class[stab.class.it] == "E"){

      if (total.dist <= 0.1){
        a <- 24.260
        b <- 0.83660
      } else if (total.dist <= 0.3){
        a <- 23.331
        b <- 0.81956
      } else if (total.dist <= 1){
        a <- 21.628
        b <- 0.75660
      } else if (total.dist <= 2){
        a <- 21.628
        b <- 0.63077
      } else if (total.dist <= 4){
        a <- 22.534
        b <- 0.57154
      } else if (total.dist <= 10){
        a <- 24.703
        b <- 0.50527
      } else if (total.dist <= 20){
        a <- 26.970
        b <- 0.46713
      } else if (total.dist <= 40){
        a <- 35.420
        b <- 0.37615
      } else {
        a <- 47.618
        b <- 0.29592
      }

      c <- 6.25
      d <- 0.54287

    } else if (stab.class[stab.class.it] == "F"){

      if (total.dist <= 0.2){
        a <- 15.209
        b <- 0.81558
      } else if (total.dist <= 0.7) {
        a <- 14.457
        b <- 0.78407
      } else if (total.dist <= 1){
        a <- 13.953
        b <- 0.68465
      } else if (total.dist <= 2){
        a <- 13.953
        b <- 0.63227
      } else if (total.dist <= 3){
        a <- 14.823
        b <- 0.54503
      } else if (total.dist <= 7){
        a <- 16.187
        b <- 0.46490
      } else if (total.dist <= 15){
        a <- 17.836
        b <- 0.41507
      } else if (total.dist <= 30){
        a <- 22.651
        b <- 0.32681
      } else if (total.dist <= 60){
        a <- 27.074
        b <- 0.27436
      } else {
        a <- 34.219
        b <- 0.21716
      }

      c <- 4.1667
      d <- 0.36191
    }

    # If the puff has moved, get sigma values.
    # If total distance = 0, then the puff has just been initialized and should
    # not contribute to the overall concentration.
    if (total.dist > 0){

      big.theta <- 0.017453293 * (c - d * log(total.dist))
      sigma.y.vals[stab.class.it] <- 465.11628 * total.dist * tan(big.theta)

      sigma.z <- a * (total.dist)^b
      sigma.z.vals[stab.class.it] <- ifelse(sigma.z > 5000, 5000, sigma.z)

    } else {

      sigma.y.vals[stab.class.it] <- sigma.z.vals[stab.class.it] <- NA
    }

  }

  # Average sigma values over stability classes that were passed to this function.
  sigma.y <- mean(sigma.y.vals)
  sigma.z <- mean(sigma.z.vals)

  return(c(sigma.y, sigma.z))
}

#' @title Convert between (ws, wd) and (u,v)
#' @description This function converts between coordinate systems by changing from degrees to radians
#' @usage wind_vector_convert(wind_speeds,wind_directions)
#' @param wind_speeds A list of float values of wind speeds in m/s at each time stamp
#' @param wind_directions A list of float values of wind directions in degrees at each time stamp following
#' the conventional definition: 0 -> wind blowing from North, 90 -> E, 180 -> S, 270 -> W
#' @return Quantities corresponding to the conversion direction
#' @examples
#' wind_vector_convert(speed_vec,direction_vec)
#' @export
wind_vector_convert <- function(wind_speeds, wind_directions) {
  theta <- (270 - wind_directions) * pi / 180  # convert degrees to radians and shift by 270 per Ryker
  u <- wind_speeds * cos(theta)  # u (x-direction)
  v <- wind_speeds * sin(theta)  # v (y-direction)

  return(list(u = u, v = v))
}

#' @title Resample wind_speeds and wind_directions to the simulation resolution by interpolation
#' @usage interpolate_wind_data(wind_speeds, wind_directions, sim_start, sim_end, puff_dt)
#' @param wind_speeds A list of float values of wind speeds in m/s at each time stamp
#' @param wind_directions A list of float values of wind directions in degrees at each time stamp following
#' the conventional definition: 0 -> wind blowing from North, 90 -> E, 180 -> S, 270 -> W
#' @param sim_start Date & time stamps of simulation start time
#' @param sim_end Date & time stamps of simulation end time
#' @param puff_dt A scalar time interval between two puffs
#' @return Quantities corresponding to the conversion direction
#' @examples
#' interpolate_wind_data(speed_vec, direction_vec, start, end, 60)
#' @export
interpolate_wind_data <- function(wind_speeds, wind_directions, sim_start, sim_end, puff_dt) {
  # convert wind speed and direction to u and v components
  wind_uv <- wind_vector_convert(wind_speeds, wind_directions)

  # time series of wind data at the observation interval
  obs_times <- seq(as.POSIXct(sim_start), as.POSIXct(sim_end), length.out = length(wind_speeds))

  # TODO the below comment looks out of date (the puff_dt = 60 part) -Ryker
  # simulate times for every minute (puff_dt = 60 seconds) over the simulation duration
  sim_times <- seq(as.POSIXct(sim_start), as.POSIXct(sim_end), by = puff_dt)

  # interpolate wind (u and v) to match simulation resolution
  wind_u_interpolated <- approx(x = obs_times, y = wind_uv$u, xout = sim_times)$y
  wind_v_interpolated <- approx(x = obs_times, y = wind_uv$v, xout = sim_times)$y

  return(
    list(wind_u = wind_u_interpolated,
         wind_v = wind_v_interpolated)
    )
}

#' Gaussian Puff Concentration Calculation
#'
#' @description Calculates the concentration of an emission event at a specified location and time due to a Gaussian puff.
#'
#' This function uses wind speed and direction components, advection adjustments, and stability class calculations to
#'   accurately measure the dispersion of a puff in the atmosphere.
#'
#' @usage gpuff(Q, stab.class, x.p, y.p, x.r.vec, y.r.vec, z.r.vec, total.dist, H, U)
#'
#' @param Q Numeric. The emission rate of the pollutant.
#' @param stab.class Character vector. Represents the stability class(es) ("A" to "F").
#' @param x.p Numeric.  Unsure
#' @param y.p Numeric. Unusure
#' @param x.r.vec Numeric. The x-coordinate (east-west) where the concentration is calculated.
#' @param y.r.vec Numeric. The y-coordinate (north-south) where the concentration is calculated.
#' @param z.r.vec Numeric. The z-coordinate (height) where the concentration is calculated.
#' @param total.dist Numeric. The total distance the puff has traveled in m.
#' @param H Numeric. Unsure
#' @param U Numeric.  Value representing the wind speed in meters per second.
#'
#' @return Numeric. The pollutant concentration at the specified (x, y, z) location and time `t`.
#' @return Quantities corresponding to concentration at sensor point(s)
#' @examples
#' gpuff(Q, stab.class, x.p, y.p, x.r.vec, y.r.vec, z.r.vec, total.dist, H, U)
#' @export
gpuff <- function(Q, stab.class,
                  x.p, y.p,
                  x.r.vec, y.r.vec, z.r.vec,
                  total.dist,
                  H, U){

  # converts kg/m^3 to ppm of METHANE
  conversion.factor <- (1e6) * (1.524)

  # Convert total distance from m to km for stability class stuff
  total.dist <- total.dist / 1000

  # Get sigma values for the stability classes passed to this function.
  sigma.vec <- compute.sigma.vals(stab.class, total.dist) # original

  sigma.y <- sigma.vec[1]
  sigma.z <- sigma.vec[2]

  # Calculate the contaminant concentration (kg/m^3) using Gaussian puff model
  C  = (Q / ( (2*pi)^(3/2) * sigma.y^2 * sigma.z )) *
    exp( -0.5 * ( (x.r.vec - x.p)^2 + (y.r.vec - y.p)^2 ) / sigma.y^2) *
    ( exp( -0.5*(z.r.vec - H)^2/sigma.z^2 ) + exp( -0.5*(z.r.vec + H)^2 / sigma.z^2 ) )

  # Convert from kg/m^3 to ppm
  C <- C*conversion.factor

  # Convert NAs to zeros. NAs come from NA sigma vals when total dist = 0
  C <- ifelse(is.na(C), 0, C)

  return(C)
}
