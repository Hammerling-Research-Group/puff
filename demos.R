devtools::load_all()

## SINGLE EMISSION RATE CONCENTRATION PLOT

sim_dt <- 10
puff_dt <- 10
output_dt <- 60
start_time <- as.POSIXct("2024-01-01 12:00:00")
end_time   <- as.POSIXct("2024-01-01 12:10:00")
source_coords <- c(0, 0, 2.5)
emission_rate <- 3.5

# Generate simulation time stamps and compute the number of steps.
sim_times <- seq(from = as.POSIXct(start_time), to = as.POSIXct(end_time), by = sim_dt)
n_steps   <- length(sim_times)
# Create a time vector in seconds relative to the start time.
times_sec <- as.numeric(difftime(sim_times, sim_times[1], units = "secs"))

# Create variable wind data: wind speed oscillates sinusoidally.
wind_data <- data.frame(
  wind_u = 2 + 0.5 * sin(2 * pi * times_sec / max(times_sec)),  # oscillates around 2 m/s
  wind_v = 1 + 0.5 * cos(2 * pi * times_sec / max(times_sec))   # oscillates around 1 m/s
)

# Define sensor coordinateS
# Here the sensor is placed at (100, 0, 0) (i.e., 100 m east of the source at ground level).
sensor_coords <- matrix(c(100, 0, 0), ncol = 3, byrow = TRUE)

sensor_concentrations <- simulate_sensor_mode(
  sim_dt        = sim_dt,
  puff_dt       = puff_dt,
  output_dt     = output_dt,
  start_time    = start_time,
  end_time      = end_time,
  source_coords = source_coords,  # source location
  emission_rate = emission_rate,
  wind_data     = wind_data,
  sensor_coords = sensor_coords,  # sensor location (only one sensor in this case)
  puff_duration = 1200          # duration (in sec) that each puff remains active
)

single_emission_rate_plot(sensor_concentrations, sensor_coords)




## SITE MAP
sensors <- data.frame(x = c(1, 2, 3), y = c(4, 5, 6))
sources <- data.frame(x = c(7, 8), y = c(9, 10))

create_site_map(sensors, sources)

## Example 1: Data frames with extra columns (e.g., z coordinates)
sensors_df <- data.frame(x = c(1, 2, 3), y = c(4, 5, 6), z = c(7, 8, 9))
sources_df <- data.frame(x = c(7, 8), y = c(9, 10), z = c(11, 12))
create_site_map(sensors_df, sources_df)

## Example 2: Data frames with only x and y columns
sensors_df <- data.frame(x = 1, y = 4)
sources_df <- data.frame(x = c(7, 8), y = c(9, 10))
create_site_map(sensors_df, sources_df)

## Example 3: Matrices as input
sensors_mat <- matrix(c(1, 4, 2, 5, 3, 6), ncol = 2, byrow = TRUE)
sources_mat <- matrix(c(7, 9, 8, 10), ncol = 2, byrow = TRUE)
create_site_map(sensors_mat, sources_mat)

## Example 4: Data frames without x/y column names (the first two columns are used)
sensors_df <- data.frame(a = c(1, 2, 3), b = c(4, 5, 6))
sources_df <- data.frame(a = c(7, 8), b = c(9, 10))
create_site_map(sensors_df, sources_df)

## FACETED TIME SERIES WTH METHANE CONCENTRATION AND WIND DATA

# EXAMPLE 1: Valid Input with Named Wind Components and a Single Sensor
sim_dt <- 10
puff_dt <- 10
output_dt <- 60
start_time <- as.POSIXct("2024-01-01 12:00:00")
end_time   <- as.POSIXct("2024-01-01 12:10:00")
source_coords <- c(0, 0, 2.5)
emission_rate <- 3.5
wind_data <- list(
  wind_u = runif(61, min = -3, max = 0.7),
  wind_v = runif(61, min = -3, max = 1.5)
)
# sensor_coords with one sensor
sensor_coords <- matrix(c(1, 2, 3), ncol = 3, byrow = TRUE)

sensor_concentrations <- simulate_sensor_mode(sim_dt, puff_dt, output_dt,
                                              start_time, end_time,
                                              source_coords, emission_rate,
                                              wind_data, sensor_coords)

faceted_time_series_plot(sensor_concentrations, wind_data, start_time, end_time, output_dt)

# EXAMPLE 2: Invalid Wind Data (Only One Component Provided)
sim_dt <- 10
puff_dt <- 10
output_dt <- 60
start_time <- as.POSIXct("2024-01-01 12:00:00")
end_time   <- as.POSIXct("2024-01-01 12:05:00")
source_coords <- c(0, 0, 2.5)
emission_rate <- 3.5
wind_data <- list(
  runif(31, min = -3, max = 0.7)  # Only one component provided
)
sensor_coords <- matrix(c(1, 2, 3), ncol = 3, byrow = TRUE)

sensor_concentrations <- simulate_sensor_mode(sim_dt, puff_dt, output_dt,
                                              start_time, end_time,
                                              source_coords, emission_rate,
                                              wind_data, sensor_coords)

faceted_time_series_plot(sensor_concentrations, wind_data, start_time, end_time, output_dt)

# EXAMPLE 3:  Higher Resolution for Dynamic X-Axis Labeling with One Sensor

set.seed(123)
sim_dt <- 10
puff_dt <- 10
output_dt <- 30
start_time <- as.POSIXct("2024-01-01 12:00:00")
end_time <- as.POSIXct("2024-01-01 13:00:00")
source_coords <- c(0, 0, 2.5)
emission_rate <- 3.5
wind_data <- list(
  wind_u = runif(3601, min = -3, max = 0.7),
  wind_v = runif(3601, min = -3, max = 1.5)
)
# sensor_coords with one sensors

sensor_coords <- matrix(c(1, 2, 3), ncol = 3, byrow = TRUE)

sensor_concentrations <- simulate_sensor_mode(sim_dt, puff_dt, output_dt,
                                              start_time, end_time,
                                              source_coords, emission_rate,
                                              wind_data, sensor_coords)

faceted_time_series_plot(sensor_concentrations, wind_data, start_time, end_time, output_dt)


# EXAMPLE 4: Valid Input with Unnamed Wind Components and Multiple Sensors
sim_dt <- 10
puff_dt <- 10
output_dt <- 60
start_time <- as.POSIXct("2024-01-01 12:00:00")
end_time   <- as.POSIXct("2024-01-01 12:10:00")
source_coords <- c(0, 0, 2.5)
emission_rate <- 3.5
wind_data <- list(
  runif(61, min = -3, max = 0.7),
  runif(61, min = -3, max = 1.5)
)
# sensor_coords with two sensors
sensor_coords <- matrix(c(1, 2, 3,
                          4, 5, 6), ncol = 3, byrow = TRUE)

sensor_concentrations <- simulate_sensor_mode(sim_dt, puff_dt, output_dt,
                                              start_time, end_time,
                                              source_coords, emission_rate,
                                              wind_data, sensor_coords)

faceted_time_series_plot(sensor_concentrations, wind_data, start_time, end_time, output_dt)
