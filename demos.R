devtools::load_all()

## SINGLE EMISSION RATE CONCENTRATION PLOT

sim_dt <- 10
puff_dt <- 10
output_dt <- 60
start_time <- as.POSIXct("2024-01-01 12:00:00")
end_time   <- as.POSIXct("2024-01-01 12:10:00")
emission_rate <- 3.6

sim_times <- seq(from = as.POSIXct(start_time), to = as.POSIXct(end_time), by = sim_dt)
n_steps   <- length(sim_times)
times_sec <- as.numeric(difftime(sim_times, sim_times[1], units = "secs"))

wind_data <- data.frame(
  wind_u = 2 + 0.5 * sin(2 * pi * times_sec / max(times_sec)),
  wind_v = 1 + 0.5 * cos(2 * pi * times_sec / max(times_sec))
)

sensors <- matrix(c(-105.139155,40.595749,2.4,
                    -105.139609,40.595802,2.4,
                    -105.139256,40.596088, 2.4,
                    -105.139756, 40.596092,2.4,
                    -105.139849, 40.595483,2.4,
                    -105.13926, 40.595453,2.4,
                    -105.140355, 40.595482,2.4,
                    -105.140577, 40.595809,2.4,
                    -105.140135, 40.595808,2.4,
                    -105.140608, 40.596101,2.4),
                  ncol=3, byrow = TRUE)
sources <- matrix(c(-105.13986, 40.5957,4.5,
                    -105.13942, 40.59592, 2,
                    -105.13941,40.59565,2,
                    -105.140313,40.59594,2,
                    -105.140294, 40.59563,2),
                  ncol=3, byrow = TRUE)
sensor_concentrations <- simulate_sensor_mode(
  sim_dt        = sim_dt,
  puff_dt       = puff_dt,
  output_dt     = output_dt,
  start_time    = start_time,
  end_time      = end_time,
  source_coords = sources,
  emission_rate = emission_rate,
  wind_data     = wind_data,
  sensor_coords = sensors,  # sensor location (only one sensor in this case)
  puff_duration = 1200          # duration (in sec) that each puff remains active
)

single_emission_rate_plot(sensor_concentrations, sensors)




## SITE MAP
sensors <- matrix(c(-105.139155,40.595749,2.4,
                    -105.139609,40.595802,2.4,
                    -105.139256,40.596088, 2.4,
                    -105.139756, 40.596092,2.4,
                    -105.139849, 40.595483,2.4,
                    -105.13926, 40.595453,2.4,
                    -105.140355, 40.595482,2.4,
                    -105.140577, 40.595809,2.4,
                    -105.140135, 40.595808,2.4,
                    -105.140608, 40.596101,2.4),
                  ncol=3, byrow = TRUE)

sources <- matrix(c(-105.13986, 40.5957,4.5,
                    -105.13942, 40.59592, 2,
                    -105.13941,40.59565,2,
                    -105.140313,40.59594,2,
                   -105.140294, 40.59563,2),
                  ncol=3, byrow = TRUE)

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
emission_rate <- 3.6
wind_data <- data.frame(
  wind_u = runif(61, min = -3, max = 0.7),
  wind_v = runif(61, min = -3, max = 1.5)
)
# sensor_coords with one sensor

sensors <- matrix(c(-105.139155,40.595749,2.4,
                    -105.139609,40.595802,2.4,
                    -105.139256,40.596088, 2.4,
                    -105.139756, 40.596092,2.4,
                    -105.139849, 40.595483,2.4,
                    -105.13926, 40.595453,2.4,
                    -105.140355, 40.595482,2.4,
                    -105.140577, 40.595809,2.4,
                    -105.140135, 40.595808,2.4,
                    -105.140608, 40.596101,2.4),
                  ncol=3, byrow = TRUE)
sources <- matrix(c(-105.13986, 40.5957,4.5,
                    -105.13942, 40.59592, 2,
                    -105.13941,40.59565,2,
                    -105.140313,40.59594,2,
                    -105.140294, 40.59563,2),
                  ncol=3, byrow = TRUE)

sensor_concentrations <- simulate_sensor_mode(sim_dt, puff_dt, output_dt,
                                              start_time, end_time,
                                              sources, emission_rate,
                                              wind_data, sensors)
faceted_time_series_plot(sensor_concentrations, sensors, wind_data, start_time, end_time, output_dt)

# new approach with wind rose plots instead
faceted_time_series_plot2(sensor_concentrations, sensors, wind_data, start_time, end_time, output_dt)

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
wind_data <- data.frame(
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
wind_data <- data.frame(
  wind_u = runif(61, min = -3, max = 0.7),
  wind_v = runif(61, min = -3, max = 1.5)
)
# sensor_coords with two sensors
sensor_coords <- matrix(c(1, 2, 3,
                          4, 5, 6), ncol = 3, byrow = TRUE)

sensor_concentrations <- simulate_sensor_mode(sim_dt, puff_dt, output_dt,
                                              start_time, end_time,
                                              source_coords, emission_rate,
                                              wind_data, sensor_coords)


faceted_time_series_plot(sensor_concentrations, wind_data, start_time, end_time, output_dt)

# 2d animated
start_time <- "2024-01-01 12:00:00"
end_time <- "2024-01-01 13:00:00"
grid_coords <- list(
  x = seq(-5, 5, by = 1),
  y = seq(-5, 5, by = 1),
  z = c(2.5)
)

grid_results <- readr::read_rds(file.choose())

plot_2d_animated(data = grid_results$grid_concentrations_1,
                 grid_coords = grid_coords,
                 start = start_time,
                 end = end_time,
                 output_dt = output_dt,
                 interpolate_grid = TRUE)


# 3d animated
start_time <- "2024-01-01 12:00:00"
end_time <- "2024-01-01 13:00:00"
grid_coords2 <- list(
  x = seq(1, 25, by = 1),
  y = seq(1, 25, by = 1),
  z = seq(1, 10, by = 1)
)

grid_concentrations <- readr::read_rds(file.choose())

plot_3d_animated(data = grid_concentrations,
                 grid_coords = grid_coords2,
                 start = start_time,
                 end = end_time,
                 output_dt = output_dt,
                 save = FALSE)

