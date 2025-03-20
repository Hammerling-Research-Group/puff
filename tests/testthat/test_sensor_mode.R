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

result <- simulate_sensor_mode(
  sim_dt = sim_dt,
  puff_dt = puff_dt,
  output_dt = output_dt,
  start_time = start_time,
  end_time = end_time,
  source_coords = source_coords,
  emission_rate = emission_rate,
  wind_data = wind_data,
  sensor_coords = sensor_coords,
  puff_duration = 1200
)

# tests
test_that("simulate_sensor_mode returns a data frame", {
  expect_s3_class(result, "data.frame")
})

test_that("simulate_sensor_mode errors when wind_data is not a data frame", {
  bad_wind_data <- list(
    runif(61, min = -3, max = 0.7),
    runif(61, min = -3, max = 1.5)
  )

  expect_error(
    simulate_sensor_mode(
      sim_dt = sim_dt,
      puff_dt = puff_dt,
      output_dt = output_dt,
      start_time = start_time,
      end_time = end_time,
      source_coords = source_coords,
      emission_rate = emission_rate,
      wind_data = bad_wind_data,
      sensor_coords = sensor_coords,
      puff_duration = 1200
    ),
    "`wind_data` must be a data frame or tibble. Supplied input is of type list"
  )
})

test_that("Concentration values are reasonable given emission rate and distance", {
  max_concentration <- max(as.matrix(result[,-1]))
  expect_true(max_concentration <= 10)
})

test_that("No NA values in the concentration output", {
  expect_true(all(!is.na(as.matrix(result[,-1]))))
})

test_that("Number of output rows corresponds to output_dt intervals", {
  expected_rows <- length(seq(from = start_time + output_dt, to = end_time, by = output_dt))
  expect_equal(nrow(result), expected_rows)
})

test_that("Concentration values are non-negative", {
  concentration_values <- as.matrix(result[,-1])
  expect_true(all(concentration_values >= 0))
})

test_that("Output time intervals match output_dt", {
  output_times <- as.POSIXct(result[, 1], format = "%Y-%m-%d %H:%M:%S", tz = "UTC")

  diffs <- as.numeric(diff(output_times, units = "secs"))

  expect_false(anyNA(output_times), info = "There are NA values in the time column")
  expect_false(any(duplicated(output_times)), info = "There are duplicated timestamps")

  expect_true(all(abs(diffs - output_dt) < 1e-6), info = "Time intervals do not match output_dt")
})
