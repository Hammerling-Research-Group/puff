library(testthat)

# setup
sim_dt <- 10
puff_dt <- 10
output_dt <- 60
start_time <- as.POSIXct("2024-01-01 12:00:00")
end_time   <- as.POSIXct("2024-01-01 12:10:00")
source_coords <- c(0, 0, 2.5)
emission_rate <- 3.5

sim_times <- seq(from = start_time, to = end_time, by = sim_dt)
n_steps   <- length(sim_times)
times_sec <- as.numeric(difftime(sim_times, sim_times[1], units = "secs"))

wind_data <- data.frame(
  wind_u = 2 + 0.5 * sin(2 * pi * times_sec / max(times_sec)),
  wind_v = 1 + 0.5 * cos(2 * pi * times_sec / max(times_sec))
)

sensor_coords <- matrix(c(100, 0, 0), ncol = 3, byrow = TRUE)

sensor_concentrations <- simulate_sensor_mode(
  sim_dt        = sim_dt,
  puff_dt       = puff_dt,
  output_dt     = output_dt,
  start_time    = start_time,
  end_time      = end_time,
  source_coords = source_coords,
  emission_rate = emission_rate,
  wind_data     = wind_data,
  sensor_coords = sensor_coords,
  puff_duration = 1200
)

## Tests for single_emission_rate_plot ##

test_that("single_emission_rate_plot returns a ggplot object with valid toy data", {
  p <- single_emission_rate_plot(sensor_concentrations, sensor_coords)
  expect_s3_class(p, "ggplot")
})

test_that("single_emission_rate_plot errors when sensor_concentrations is not a data frame", {
  bad_data <- list(a = 1, b = 2)
  expect_error(
    single_emission_rate_plot(bad_data, sensor_coords),
    "sensor_concentrations must be a data frame"
  )
})

test_that("single_emission_rate_plot errors when sensor_concentrations is empty", {
  empty_data <- data.frame(Group.1 = character(), Sensor_1 = numeric())
  expect_error(
    single_emission_rate_plot(empty_data, sensor_coords),
    "sensor_concentrations is empty"
  )
})

test_that("single_emission_rate_plot errors when 'Group.1' column is missing", {
  bad_data <- sensor_concentrations
  bad_data$Group.1 <- NULL
  expect_error(
    single_emission_rate_plot(bad_data, sensor_coords),
    "must contain a column named 'Group.1'"
  )
})

test_that("single_emission_rate_plot errors when no sensor concentration columns exist", {
  bad_data <- sensor_concentrations
  bad_data$Sensor_1 <- NULL
  expect_error(
    single_emission_rate_plot(bad_data, sensor_coords),
    "must contain at least one sensor concentration column"
  )
})

test_that("single_emission_rate_plot errors with sensor_coords vector of incorrect length", {
  bad_coords <- c(10, 0)  # Only 2 elements instead of 3
  expect_error(
    single_emission_rate_plot(sensor_concentrations, bad_coords),
    "sensor_coords must be a numeric vector of length 3"
  )
})

test_that("single_emission_rate_plot errors with sensor_coords matrix of wrong dimensions", {
  bad_coords <- matrix(c(10, 0, 0, 5), ncol = 2)
  expect_error(
    single_emission_rate_plot(sensor_concentrations, bad_coords),
    "sensor_coords must have exactly 3 columns"
  )
})

test_that("single_emission_rate_plot errors when multiple sensor coordinates do not match sensor columns", {
  # For a simulation with one sensor, providing two rows should trigger an error.
  bad_coords <- matrix(c(10, 0, 0, 5, 5, 5), nrow = 2, ncol = 3, byrow = TRUE)
  expect_error(
    single_emission_rate_plot(sensor_concentrations, bad_coords),
    "The number of sensor concentration columns"
  )
})


## Tests for time_series_plot ##

test_that("time_series_plot returns a ggplot object with valid toy data", {
  p <- time_series_plot(sensor_concentrations)
  expect_s3_class(p, "ggplot")
})

test_that("time_series_plot errors when input is not a data.frame or matrix", {
  bad_data <- list(a = 1)
  expect_error(
    time_series_plot(bad_data),
    "sensor_concentrations must be a data.frame or matrix"
  )
})

test_that("time_series_plot errors when sensor_concentrations is empty", {
  empty_data <- data.frame(Group.1 = character(), Sensor_1 = numeric())
  expect_error(
    time_series_plot(empty_data),
    "sensor_concentrations is empty"
  )
})

test_that("time_series_plot errors when 'Group.1' column is missing", {
  bad_data <- sensor_concentrations
  bad_data$Group.1 <- NULL
  expect_error(
    time_series_plot(bad_data),
    "must contain a column named 'Group.1'"
  )
})

test_that("time_series_plot errors when sensor concentration column is not numeric", {
  bad_data <- sensor_concentrations
  bad_data$Sensor_1 <- as.character(bad_data$Sensor_1)
  expect_error(
    time_series_plot(bad_data),
    "Sensor concentration column 'Sensor_1' must be numeric"
  )
})

## tests for faceted_time_series_plot

test_that("faceted_time_series_plot returns a ggplot object with valid toy data", {
  good_data <- sensor_concentrations
  good_data$Group.1 <- as.POSIXct(good_data$Group.1)

  wind_plot <- list(
    wind_u = rep(1, nrow(good_data)),
    wind_v = rep(1, nrow(good_data))
  )
  start_posix <- min(good_data$Group.1)
  end_posix <- max(good_data$Group.1)
  p <- faceted_time_series_plot(good_data, wind_plot,
                                sensor_coords = as.numeric(sensor_coords),
                                start_posix, end_posix, output_dt)
  expect_s3_class(p, "ggplot")
})

test_that("faceted_time_series_plot errors when sensor_concentrations is not a data frame", {
  bad_data <- list(a = 1)
  wind_plot <- list(wind_u = 1:10, wind_v = 1:10)
  start_posix <- as.POSIXct("2024-01-01 12:00:00")
  end_posix <- as.POSIXct("2024-01-01 12:10:00")
  expect_error(
    faceted_time_series_plot(bad_data, wind_plot, start_posix, end_posix, output_dt),
    "sensor_concentrations must be a data frame"
  )
})

test_that("faceted_time_series_plot errors when 'Group.1' column is missing", {
  bad_data <- sensor_concentrations
  bad_data$Group.1 <- NULL
  wind_plot <- list(wind_u = 1:10, wind_v = 1:10)
  start_posix <- as.POSIXct("2024-01-01 12:00:00")
  end_posix <- as.POSIXct("2024-01-01 12:09:00")

  expect_error(
    faceted_time_series_plot(bad_data, wind_plot,
                             sensor_coords = as.numeric(sensor_coords),
                             start_posix, end_posix, output_dt),
    "sensor_concentrations must contain a column named 'Group.1' with POSIX time values."
  )
})

test_that("faceted_time_series_plot errors when start_time is not POSIXct", {
  good_data <- sensor_concentrations
  wind_plot <- list(wind_u = 1:10, wind_v = 1:10)
  start_bad <- "2024-01-01 12:00:00"
  end_posix <- as.POSIXct("2024-01-01 12:09:00")

  expect_error(
    faceted_time_series_plot(good_data, wind_plot,
                             sensor_coords = as.numeric(sensor_coords),
                             start_time = start_bad,
                             end_time = end_posix, output_dt),
    "start_time must be a POSIXct object"
  )
})

test_that("faceted_time_series_plot errors when end_time is not POSIXct", {
  good_data <- sensor_concentrations
  wind_plot <- list(wind_u = 1:10, wind_v = 1:10)
  start_posix <- as.POSIXct("2024-01-01 12:09:00")
  end_bad <- "2024-01-01 12:00:00"

  expect_error(
    faceted_time_series_plot(
      good_data, wind_plot,
      sensor_coords = as.numeric(sensor_coords),
      start_time = start_posix,
      end_time = end_bad, output_dt
    ),
    "end_time must be a POSIXct object"
  )
})

test_that("faceted_time_series_plot errors when start_time is after end_time", {
  good_data <- sensor_concentrations
  wind_plot <- list(wind_u = 1:10, wind_v = 1:10)
  start_posix <- as.POSIXct("2024-01-01 12:10:00")
  end_posix <- as.POSIXct("2024-01-01 12:00:00")

  expect_error(
    faceted_time_series_plot(good_data, wind_plot,
                             start_time = start_posix,
                             end_time = end_posix, output_dt),
    "start_time must be before end_time"
  )
})

test_that("faceted_time_series_plot errors when output_dt is invalid", {
  good_data <- sensor_concentrations
  good_data$Group.1 <- as.POSIXct(good_data$Group.1)
  wind_plot <- list(wind_u = 1:10, wind_v = 1:10)
  start_posix <- as.POSIXct("2024-01-01 12:00:00")
  end_posix <- as.POSIXct("2024-01-01 12:09:00")

  expect_error(
    faceted_time_series_plot(
      good_data, wind_plot,
      start_time = start_posix,
      end_time = end_posix,
      output_dt = -10
    ),
    "output_dt must be a single positive numeric value"
  )
})

## tests for faceted_time_series_plot2

test_that("faceted_time_series_plot2 returns a ggplot object with valid toy data", {
  good_data <- sensor_concentrations
  good_data$Group.1 <- as.POSIXct(good_data$Group.1)

  wind_plot <- list(
    wind_u = rep(1, nrow(good_data)),
    wind_v = rep(1, nrow(good_data))
  )
  start_posix <- min(good_data$Group.1)
  end_posix <- max(good_data$Group.1)
  p <- faceted_time_series_plot2(good_data, wind_plot,
                                 sensor_coords = as.numeric(sensor_coords),
                                 start_posix, end_posix, output_dt)
  expect_s3_class(p, "ggplot")
})

test_that("faceted_time_series_plot2 errors when sensor_concentrations is not a data frame", {
  bad_data <- list(a = 1)
  wind_plot <- list(wind_u = 1:10, wind_v = 1:10)
  start_posix <- as.POSIXct("2024-01-01 12:00:00")
  end_posix <- as.POSIXct("2024-01-01 12:10:00")
  expect_error(
    faceted_time_series_plot2(bad_data, wind_plot, start_posix, end_posix, output_dt),
    "sensor_concentrations must be a data frame"
  )
})

test_that("faceted_time_series_plot2 errors when 'Group.1' column is missing", {
  bad_data <- sensor_concentrations
  bad_data$Group.1 <- NULL
  wind_plot <- list(wind_u = 1:10, wind_v = 1:10)
  start_posix <- as.POSIXct("2024-01-01 12:00:00")
  end_posix <- as.POSIXct("2024-01-01 12:09:00")

  expect_error(
    faceted_time_series_plot2(bad_data, wind_plot,
                              sensor_coords = as.numeric(sensor_coords),
                              start_posix, end_posix, output_dt),
    "sensor_concentrations must contain a column named 'Group.1' with POSIX time values."
  )
})

test_that("faceted_time_series_plot2 errors when start_time is not POSIXct", {
  good_data <- sensor_concentrations
  wind_plot <- list(wind_u = 1:10, wind_v = 1:10)
  start_bad <- "2024-01-01 12:00:00"
  end_posix <- as.POSIXct("2024-01-01 12:09:00")

  expect_error(
    faceted_time_series_plot2(good_data, wind_plot,
                              sensor_coords = as.numeric(sensor_coords),
                              start_time = start_bad,
                              end_time = end_posix, output_dt),
    "start_time must be a POSIXct object"
  )
})

test_that("faceted_time_series_plot2 errors when end_time is not POSIXct", {
  good_data <- sensor_concentrations
  wind_plot <- list(wind_u = 1:10, wind_v = 1:10)
  start_posix <- as.POSIXct("2024-01-01 12:09:00")
  end_bad <- "2024-01-01 12:00:00"

  expect_error(
    faceted_time_series_plot2(
      good_data, wind_plot,
      sensor_coords = as.numeric(sensor_coords),
      start_time = start_posix,
      end_time = end_bad, output_dt
    ),
    "end_time must be a POSIXct object"
  )
})

test_that("faceted_time_series_plot2 errors when start_time is after end_time", {
  good_data <- sensor_concentrations
  wind_plot <- list(wind_u = 1:10, wind_v = 1:10)
  start_posix <- as.POSIXct("2024-01-01 12:10:00")
  end_posix <- as.POSIXct("2024-01-01 12:00:00")

  expect_error(
    faceted_time_series_plot2(good_data, wind_plot,
                              start_time = start_posix,
                              end_time = end_posix, output_dt),
    "start_time must be before end_time"
  )
})

test_that("faceted_time_series_plot2 errors when output_dt is invalid", {
  good_data <- sensor_concentrations
  good_data$Group.1 <- as.POSIXct(good_data$Group.1)
  wind_plot <- list(wind_u = 1:10, wind_v = 1:10)
  start_posix <- as.POSIXct("2024-01-01 12:00:00")
  end_posix <- as.POSIXct("2024-01-01 12:09:00")

  expect_error(
    faceted_time_series_plot2(
      good_data, wind_plot,
      start_time = start_posix,
      end_time = end_posix,
      output_dt = -10
    ),
    "output_dt must be a single positive numeric value"
  )
})

## tests for create_site_map

test_that("create_site_map returns a ggplot object with valid data frames", {
  sensors <- data.frame(x = c(1, 2, 3), y = c(4, 5, 6))
  sources <- data.frame(x = c(7, 8), y = c(9, 10))
  p <- create_site_map(sensors, sources)
  expect_s3_class(p, "ggplot")
})

test_that("create_site_map returns a ggplot object with valid matrices", {
  sensors <- matrix(c(1, 4, 2, 5, 3, 6), ncol = 2, byrow = TRUE)
  sources <- matrix(c(7, 9, 8, 10), ncol = 2, byrow = TRUE)
  p <- create_site_map(sensors, sources)
  expect_s3_class(p, "ggplot")
})

test_that("create_site_map errors when sensors is not a data frame or matrix", {
  bad_sensors <- "not a data frame"
  sources <- data.frame(x = c(7, 8), y = c(9, 10))
  expect_error(
    create_site_map(bad_sensors, sources),
    "'sensors' must be a data frame or matrix"
  )
})

test_that("create_site_map errors when sensors has fewer than two columns", {
  bad_sensors <- data.frame(x = 1:3)
  sources <- data.frame(x = c(7, 8), y = c(9, 10))
  expect_error(
    create_site_map(bad_sensors, sources),
    "must have at least two columns"
  )
})

test_that("create_site_map errors when sources has fewer than two columns", {
  sensors <- data.frame(x = c(1, 2, 3), y = c(4, 5, 6))
  bad_sources <- data.frame(x = c(7, 8))
  expect_error(
    create_site_map(sensors, bad_sources),
    "must have at least two columns"
  )
})
