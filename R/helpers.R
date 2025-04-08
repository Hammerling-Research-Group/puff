#' @title Determine Whether a Time is During the Day
#' @description This function checks the time and classifies it as day or not
#' @usage is_day(time)
#' @param time A time value that is used to determine whether it's day or night.
#' @return A character T or F representing whether or not it is daytime.
#' @examples
#' out <- is_day(8)
#' @export
is_day <- function(time){

  time_hour <- as.numeric(format(time, format = "%H"))

  if (time_hour >= 7 & time_hour <= 18){
    return(T)
  } else {
    return(F)
  }
}


#' @title Determine Stability Class Based on Wind Speed and Time of Day
#' @description This function calculates the stability class based on wind speed (U) and the time of day.
#'              It categorizes the atmosphere's stability as one of several classes (A-F) depending on the inputs.
#' @usage get_stab_class(U, time)
#' @param U A numeric value representing the wind speed in meters per second.
#' @param time A time value that is used to determine whether it's day or night.
#' @return A character vector representing the stability class(es) ("A" to "F").
#' @examples
#' out <- get_stab_class(3, 12)
#' @export
get_stab_class <- function(U, time){

  # chk if U is missing or NA
  if (is.na(U) || is.null(U)) {
    warning("Wind speed (U) is missing. Assigning neutral stability class 'D'. Correct if desired.")
    return("D")  # default: neutral stability
  }

  # ensure U is numeric
  if (!is.numeric(U)) {
    stop("Wind speed (U) must be numeric.")
  }

  # Determine stability class based on wind speed and time of day
  if (U < 2){
    stab_class <- ifelse(is_day(time), list(c("A", "B")), list(c("E", "F")))[[1]]
  } else if (U >= 2 & U < 3){
    stab_class <- ifelse(is_day(time), list(c("B")),      list(c("E", "F")))[[1]]
  } else if (U >= 3 & U < 5){
    stab_class <- ifelse(is_day(time), list(c("B", "C")), list(c("D", "E")))[[1]]
  } else if (U >= 5 & U < 6){
    stab_class <- ifelse(is_day(time), list(c("C", "D")), list(c("D")))[[1]]
  } else {
    stab_class <- ifelse(is_day(time), list(c("D")),      list(c("D")))[[1]]
  }

  return(stab_class)

}


#' Compute Sigma Values Based on Stability Class and Distance
#'
#' @param stab_class Character vector of stability classes ("A" to "F")
#' @param total_dist Numeric vector of distances in km (must match length of stab_class or be scalar)
#' @return 2-row matrix with sigma_y (row 1) and sigma_z (row 2) values
#' @examples
#' out <- compute_sigma_vals("A", 0.7)
#' @export
compute_sigma_vals <- function(stab_class, total_dist) {
  # chks: validate inputs
  if (is.null(total_dist)) return(matrix(NA_real_, nrow = 2, ncol = 1))
  if (anyNA(total_dist)) return(matrix(NA_real_, nrow = 2, ncol = 1))
  if (!is.numeric(total_dist)) stop("total_dist must be numeric.")

  # vectorize if one of the inputs is scalar
  if (length(total_dist) == 1 && length(stab_class) > 1) {
    total_dist <- rep(total_dist, length(stab_class))
  }
  if (length(stab_class) == 1 && length(total_dist) > 1) {
    stab_class <- rep(stab_class, length(total_dist))
  }

  if (length(total_dist) != length(stab_class)) {
    stop("stab_class and total_dist must be of equal length or scalar.")
  }

  # convert distance to km if needed (assume already in km from gpuff())
  dist_km <- total_dist

  # preallocate output
  n <- length(dist_km)
  sigma_y <- numeric(n)
  sigma_z <- numeric(n)

  # create parameter lookups
  class_params <- list(
    A = list(c_vals = c(122.8,158.08,170.22,179.52,217.41,258.89,346.75,453.85),
             b_vals = c(0.9447,1.0542,1.0932,1.1262,1.2644,1.4094,1.7283,2.1166),
             cutoffs = c(0.1,0.15,0.2,0.25,0.3,0.4,0.5,Inf),
             c = 24.167, d = 2.5334),

    B = list(c_vals = c(90.673,98.483,109.3),
             b_vals = c(0.93198,0.98332,1.0971),
             cutoffs = c(0.2,0.4,Inf),
             c = 18.333, d = 1.8096),

    C = list(a = 61.141, b = 0.91465, c = 12.5, d = 1.0857),

    D = list(c_vals = c(34.459,32.093,32.093,33.504,36.65,44.053),
             b_vals = c(0.86974,0.81066,0.64403,0.60486,0.56589,0.51179),
             cutoffs = c(0.3,1,3,10,30,Inf),
             c = 8.333, d = 0.72382),

    E = list(c_vals = c(24.26,23.331,21.628,21.628,22.534,24.703,26.97,35.42,47.618),
             b_vals = c(0.8366,0.81956,0.7566,0.63077,0.57154,0.50527,0.46713,0.37615,0.29592),
             cutoffs = c(0.1,0.3,1,2,4,10,20,40,Inf),
             c = 6.25, d = 0.54287),

    F = list(c_vals = c(15.209,14.457,13.953,13.953,14.823,16.187,17.836,22.651,27.074,34.219),
             b_vals = c(0.81558,0.78407,0.68465,0.63227,0.54503,0.4649,0.41507,0.32681,0.27436,0.21716),
             cutoffs = c(0.2,0.7,1,2,3,7,15,30,60,Inf),
             c = 4.1667, d = 0.36191)
  )

  # compute sigma_y and sigma_z
  for (i in seq_len(n)) {
    class_i <- toupper(stab_class[i])
    d_i <- dist_km[i]

    if (d_i <= 0 || !(class_i %in% names(class_params))) {
      sigma_y[i] <- NA
      sigma_z[i] <- NA
      next
    }

    params <- class_params[[class_i]]

    # a/b selection logic
    if (!is.null(params$c_vals)) {
      bin <- which(d_i <= params$cutoffs)[1]
      a <- params$c_vals[bin]
      b <- params$b_vals[bin]
    } else {
      a <- params$a
      b <- params$b
    }

    # sigma Z
    sigma_z_val <- a * d_i^b
    sigma_z[i] <- min(sigma_z_val, 5000)

    # sigma Y
    big_theta <- 0.017453293 * (params$c - params$d * log(d_i))
    sigma_y[i] <- 465.11628 * d_i * tan(big_theta)
  }

  return(rbind(sigma_y, sigma_z))  # [2 x N]
}


#' @title Convert between (ws, wd) and (u,v)
#' @description This function converts between coordinate systems by changing from degrees to radians
#' @usage wind_vector_convert(wind_speeds,wind_directions)
#' @param wind_speeds A list of float values of wind speeds in m/s at each time stamp
#' @param wind_directions A list of float values of wind directions in degrees at each time stamp following
#' the conventional definition: 0 -> wind blowing from North, 90 -> E, 180 -> S, 270 -> W
#' @return Quantities corresponding to the conversion direction
#' @examples
#' out <- wind_vector_convert(c(5, 10), c(0, 90))
#' @export
wind_vector_convert <- function(wind_speeds, wind_directions) {

  theta <- (270 - wind_directions) * pi / 180

  u <- wind_speeds * cos(theta)  # (x-direction)

  v <- wind_speeds * sin(theta)  # (y-direction)

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
#' out <- interpolate_wind_data(c(2, 3), c(90, 180),
#'   "2024-01-01 00:00:00", "2024-01-01 01:00:00", 300
#' )
#' @export
interpolate_wind_data <- function(wind_speeds, wind_directions, sim_start, sim_end, puff_dt) {

  wind_uv <- wind_vector_convert(wind_speeds, wind_directions)

  obs_times <- seq(as.POSIXct(sim_start), as.POSIXct(sim_end), length.out = length(wind_speeds))

  sim_times <- seq(as.POSIXct(sim_start), as.POSIXct(sim_end), by = puff_dt)

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
#' @usage gpuff(Q, stab_class, x_p, y_p, x_r_vec, y_r_vec, z_r_vec, total_dist, H, U)
#'
#' @param Q Numeric. Mass per puff. E.g., for 100 puffs/hour of a 100 kg/hr emission, put 1 kg of mass into each puff.
#' @param stab_class Character vector. Stability class ("A" to "F").
#' @param x_p Numeric. Puff position in the X direction.
#' @param y_p Numeric. Puff position in the Y direction.
#' @param x_r_vec Numeric vector. The x-coordinate (east-west) where the concentration is calculated.
#' @param y_r_vec Numeric vector. The y-coordinate (north-south) where the concentration is calculated.
#' @param z_r_vec Numeric vector. The z-coordinate (height) where the concentration is calculated.
#' @param total_dist Numeric. The total distance the puff has traveled from the source in m.
#' @param H Numeric. Source height.
#' @param U Numeric. Wind speed in m/s.
#'
#' @return Numeric. Pollutant concentration at the specified (x, y, z) locations and time `t`.
#' @examples
#' out <- gpuff(Q = 1, stab_class = "D", x_p = 0, y_p = 0,
#'   x_r_vec = 100, y_r_vec = 0, z_r_vec = 2,
#'   total_dist = 100, H = 2, U = 5
#' )
#' @export
gpuff <- function(Q, stab_class,
                  x_p, y_p,
                  x_r_vec, y_r_vec, z_r_vec,
                  total_dist,
                  H, U){

  # converts kg/m^3 to ppm of methane
  conversion.factor <- (1e6) * (1.524)

  # convert total distance from m to km for stability class stuff
  total_dist <- total_dist / 1000

  # get sigma values for the stability classes
  sigma.vec <- compute_sigma_vals(stab_class, total_dist)

  sigma.y <- sigma.vec[1]
  sigma.z <- sigma.vec[2]

  # calculate the contaminant concentration (kg/m^3) via Gaussian puff
  C  = (Q / ( (2*pi)^(3/2) * sigma.y^2 * sigma.z )) *
    exp( -0.5 * ( (x_r_vec - x_p)^2 + (y_r_vec - y_p)^2 ) / sigma.y^2) *
    ( exp( -0.5*(z_r_vec - H)^2/sigma.z^2 ) + exp( -0.5*(z_r_vec + H)^2 / sigma.z^2 ) )

  # convert from kg/m^3 to ppm
  C <- C*conversion.factor

  # convert NAs to zeros; note: NAs come from NA sigma vals when total dist = 0
  C <- ifelse(is.na(C), 0, C)

  return(C)
}
