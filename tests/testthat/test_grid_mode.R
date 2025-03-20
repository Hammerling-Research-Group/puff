library(testthat)

# setup
sim_dt <- 10
puff_dt <- 10
output_dt <- 60
start_time <- "2024-01-01 12:00:00"
end_time <- "2024-01-01 13:00:00"
source_coords <- c(0, 0, 2.5)
emission_rate <- 3.5

fake_times <- seq(0, 10, length.out = 61)
wind_speeds <- rep(3, 61)
wind_directions <- 120 * abs(cos(fake_times))
wind_directions[31:61] <- wind_directions[31:61] - 40 * abs(sin(6 * fake_times[31:61]))
wind_data <- interpolate_wind_data(wind_speeds, wind_directions, start_time, end_time, puff_dt)

grid_coords <- list(
  x = seq(-1, 1, by = 1),
  y = seq(-1, 1, by = 1),
  z = c(2.5)
)

grid_concentrations_original <- simulate_grid_mode(
    sim_dt = sim_dt,
    puff_dt = puff_dt,
    output_dt = output_dt,
    start_time = start_time,
    end_time = end_time,
    source_coords = source_coords,
    emission_rate = emission_rate,
    wind_data = wind_data,
    grid_coords = grid_coords,
    puff_duration = 1200
)

# tests
test_that("Returns the correct output dimensions", {
  expected_steps <- length(seq(from = as.POSIXct(start_time), to = as.POSIXct(end_time), by = output_dt))
  expected_grid_points <- length(grid_coords$x) * length(grid_coords$y) * length(grid_coords$z)
  expect_equal(dim(grid_concentrations_original), c(expected_steps, expected_grid_points))
})

test_that("Concentration values are non-negative", {
  expect_true(all(grid_concentrations_original >= 0))
})

test_that("Time intervals match", {
  output_times <- seq(from = as.POSIXct(start_time, tz = "UTC"), to = as.POSIXct(end_time, tz = "UTC"), by = output_dt)
  diffs <- as.integer(diff(output_times, units = "secs"))
  expect_true(length(diffs) + 1 == length(output_times))
})

test_that("Concentration values are reasonable given the emission rate and grid size", {
  max_concentration <- max(grid_concentrations_original)
  expect_true(max_concentration <= 10)
})

test_that("No NA values in the output concentration array", {
  expect_true(all(!is.na(grid_concentrations_original)))
})
