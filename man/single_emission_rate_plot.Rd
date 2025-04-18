% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plots.R
\name{single_emission_rate_plot}
\alias{single_emission_rate_plot}
\title{Plot Multiple Emission Rate Sensor Concentrations}
\usage{
single_emission_rate_plot(sensor_concentrations, sensor_coords, text_size = 12)
}
\arguments{
\item{sensor_concentrations}{Data frame. Output from a sensor simulation function,
which must include a column named "Group.1" for timestamps and one or more columns
named "Sensor_1", "Sensor_2", etc., for the sensor concentration values.}

\item{sensor_coords}{Numeric vector or matrix. Coordinates (x, y, z) of the sensor(s).}

\item{text_size}{Default at 12.}
}
\value{
A ggplot object showing sensor concentrations over time, faceted by sensor.
}
\description{
This function generates a faceted (if multiple sensors) bubble plot of sensor concentrations over time
using precomputed sensor data (e.g., from `simulate_sensor_mode()`).
}
\examples{
set.seed(123)
sim_dt <- 10
puff_dt <- 10
output_dt <- 60
start_time <- "2024-01-01 12:00:00"
end_time <- "2024-01-01 13:00:00"
emission_rate <- 3.5
wind_data <- data.frame(
  wind_u = runif(3601, min = -3, max = 0.7),
  wind_v = runif(3601, min = -3, max = 1.5)
)
source_coords <- c(0, 0, 2.5)
sensor_coords <- matrix(c(-6.525403221327715e-15, -35.52264, 2.01775), ncol = 3, byrow = TRUE)

out <- simulate_sensor_mode(
  start_time, end_time, source_coords,
  emission_rate, wind_data, sensor_coords, sim_dt, puff_dt, output_dt, puff_duration = 1200
)

single_emission_rate_plot(out, sensor_coords)
}
