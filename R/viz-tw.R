
library(ggplot2)
library(dplyr)
library(tidyverse)

## SINGLE EMISSION RATE
set.seed(123)

sim_dt <- 10
puff_dt <- 10
output_dt <- 60
start_time <- as.POSIXct("2024-01-01 12:00:00")
end_time <- as.POSIXct("2024-01-01 13:00:00")
source_coords <- c(0, 0, 2.5)
emission_rate <- 3.5
wind_data <- list(
  wind_u = runif(3601, min = -3, max = 0.7),
  wind_v = runif(3601, min = -3, max = 1.5)
)
sensor_coords <- matrix(c(-6.525403221327715e-15, -35.52264, 2.01775), ncol = 3, byrow = TRUE)

sensor_concentrations <- simulate_sensor_mode(
  sim_dt = sim_dt,
  puff_dt = puff_dt,
  output_dt = output_dt,
  start_time = start_time,
  end_time = end_time,
  source_coords = source_coords,
  emission_rate = emission_rate,
  wind_data = wind_data,
  sensor_coords = sensor_coords
)

sensor_concentrations$timestamp <- as.POSIXct(sensor_concentrations$Group.1, format = "%Y-%m-%d %H:%M:%S")

sensor_data <- data.frame(
  x = rep(sensor_coords[1], nrow(sensor_concentrations)),
  y = rep(sensor_coords[2], nrow(sensor_concentrations)),
  timestamp = sensor_concentrations$timestamp,
  concentration = sensor_concentrations$Sensor_1
)

sensor_data$time_label <- format(sensor_data$timestamp, "%H:%M")

ggplot(sensor_data, aes(x = timestamp, y = y, fill = concentration)) +
  geom_tile() +
  scale_fill_gradientn(colors = c("blue", "red", "yellow"), name = "Concentration") +
  labs(
    title = "Sensor Concentrations Over Time",
    x = "Time (Hour:Minute)",
    y = NULL
  ) +
  theme_minimal() +
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    panel.grid = element_blank()
  )

## VARIABLE EMISSION RATE
emission_rates <- seq(0, 3.5, by = 0.5)

all_res <- list()

for (emission_rate in emission_rates) {
  sensor_concentrations <- simulate_sensor_mode(
    sim_dt = sim_dt,
    puff_dt = puff_dt,
    output_dt = output_dt,
    start_time = start_time,
    end_time = end_time,
    source_coords = source_coords,
    emission_rate = emission_rate,
    wind_data = wind_data,
    sensor_coords = sensor_coords
  )

  sensor_long <- sensor_concentrations |>
    mutate(time2 = seq(0, nrow(sensor_concentrations)-1)) |>
    pivot_longer(
      cols = starts_with("Sensor"),
      names_to = "Sensor",
      values_to = "Concentration"
    ) |>
    rename(time = Group.1) |>
    mutate(Emission_Rate = emission_rate)

  all_res[[as.character(emission_rate)]] <- sensor_long
}

full <- bind_rows(all_res)

# viz
full |>
  mutate(Sensor = str_replace(Sensor, "Sensor_1", "Testing Sensor")) |>
  ggplot(aes(x = time2, y = Concentration, group = Sensor)) +
  geom_line() +
  facet_grid(Emission_Rate ~ Sensor) +
  labs(
    title = "Sensor Concentrations Over Time Varying Emission Rate",
    subtitle = "With Time Steps",
    x = "Time from emission start (min)",
    y = "Concentration"
  ) +
  theme_bw()

## TIME SERIES
sensor_long <- sensor_concentrations |>
  mutate(time2 = seq(0, nrow(sensor_concentrations)-1)) |>
  pivot_longer(
    cols = starts_with("Sensor"),
    names_to = "Sensor",
    values_to = "Concentration"
  ) |>
  rename(time = Group.1)

sensor_long |>
  mutate(Sensor = str_replace(Sensor, "Sensor_1", "Test Sensor")) |>
  ggplot(aes(x = time2, y = Concentration, group = Sensor)) +
  geom_line() +
  facet_wrap(~ Sensor) +
  labs(
    title = "Sensor Concentrations Over Time",
    subtitle = "With Time Steps",
    x = "Time from emission start (min)",
    y = "Concentration"
  ) +
  theme_bw()

## TIME SHOTS
# Choose specific times for snapshots
sensor_concentrations$Group.1 <- as.POSIXct(as.character(sensor_concentrations$Group.1), format = "%Y-%m-%d %H:%M:%S")
snapshot_times <- as.POSIXct(c("2024-01-01 12:10:00", "2024-01-01 12:30:00", "2024-01-01 12:50:00"))
snapshots <- sensor_concentrations[sensor_concentrations$Group.1 %in% snapshot_times, ]

# Generate snapshot plots
plots <- lapply(1:nrow(snapshots), function(i) {
  time_point <- snapshots$Group.1[i]
  concentration <- snapshots$concentration[i]
  wind_u <- wind_data$wind_u[seq_along(wind_data$wind_u) %in% seq(1, 3601, length.out = 60)][i]
  wind_v <- wind_data$wind_v[seq_along(wind_data$wind_v) %in% seq(1, 3601, length.out = 60)][i]

  ggplot() +
    geom_point(aes(x = sensor_coords[1, 1],
                   y = sensor_coords[1, 2],
                   size = concentration,
                   color = concentration),
               shape = 21, fill = "blue") +
    geom_segment(aes(
      x = sensor_coords[1, 1],
      y = sensor_coords[1, 2],
      xend = sensor_coords[1, 1] + wind_u,
      yend = sensor_coords[1, 2] + wind_v
    ), arrow = arrow(length = unit(0.2, "cm")), color = "red") +
    labs(
      title = paste("Snapshot at", format(time_point, "%Y-%m-%d %H:%M:%S")),
      x = "X Coordinate",
      y = "Y Coordinate",
      size = "Concentration",
      color = "Concentration"
    ) +
    theme_minimal()
})

# Display plots
plots[[1]]  # Snapshot 1
plots[[2]]  # Snapshot 2
plots[[3]]  # Snapshot 3


## FACETED TIME SERIES AND WIND DATA PLOTS
set.seed(123)

sim_dt <- 10
puff_dt <- 10
output_dt <- 60
start_time <- as.POSIXct("2024-01-01 12:00:00")
end_time <- as.POSIXct("2024-01-01 13:00:00")
source_coords <- c(0, 0, 2.5)
emission_rate <- 3.5
wind_data <- list(
  wind_u = runif(3601, min = -3, max = 0.7),  # Simulating wind_u
  wind_v = runif(3601, min = -3, max = 1.5)   # Simulating wind_v
)
time_sequence <- seq(start_time, end_time, by = output_dt)
sensor_coords <- matrix(c(-6.525403221327715e-15, -35.52264, 2.01775), ncol = 3, byrow = TRUE)

sensor_concentrations <- simulate_sensor_mode(
  sim_dt = sim_dt,
  puff_dt = puff_dt,
  output_dt = output_dt,
  start_time = start_time,
  end_time = end_time,
  source_coords = source_coords,
  emission_rate = emission_rate,
  wind_data = wind_data,
  sensor_coords = sensor_coords
)

# Rename columns for clarity
sensor_concentrations <- sensor_concentrations %>%
  rename(
    time = Group.1,
    concentration = Sensor_1
  )

# Subset wind data to match time steps
wind_u_subset <- wind_data$wind_u[seq(1, length(time_sequence))]
wind_v_subset <- wind_data$wind_v[seq(1, length(time_sequence))]

# Combine wind data with concentrations
sensor_concentrations <- sensor_concentrations %>%
  mutate(wind_u = wind_u_subset[1:nrow(sensor_concentrations)],
         wind_v = wind_v_subset[1:nrow(sensor_concentrations)])

# Transform data for plotting
sensor_concentrations_long <- sensor_concentrations %>%
  pivot_longer(
    cols = c(concentration, wind_u, wind_v),  # Include concentration and wind data
    names_to = "variable",
    values_to = "value"
  )
facet_labels <- c(
  concentration = "Methane Concentration (kg/m³)",
  wind_u = "Wind Component U (m/s)",
  wind_v = "Wind Component V (m/s)"
)

# Create a faceted plot with labels
ggplot(sensor_concentrations_long, aes(x = time, y = value, color = variable, group = variable)) +
  geom_line() +
  geom_point() +
  facet_wrap(~ variable, scales = "free_y", ncol = 1, labeller = labeller(variable = facet_labels)) +
  labs(
    title = "Time Series of Methane Concentration and Wind Data",
    x = "Time",
    y = "Value"
  ) +
  theme_minimal() +
  theme(legend.position = "none")


## SITE MAP
# How many sources and sensors are allowed to be input?
# What kind of data are we inputting (coordinates, distances,etc.)?
# Are we just considering 2 or 3 dimensions?

# OPTION 1: X,Y Coordinate Input
create_site_map <- function(sensors, sources) {
  # Combine sensors and sources into one data frame
  sensors$type <- "Sensor"
  sources$type <- "Source"
  combined <- rbind(
    sensors,
    sources
  )

  ggplot(combined, aes(x = x, y = y, shape = type, color = type)) +
    # Plot points with different shapes and colors based on the 'type'
    geom_point(size = 4, stroke = 1.5) +
    scale_shape_manual(values = c("Sensor" = 21, "Source" = 4)) +
    scale_color_manual(values = c("Sensor" = "blue", "Source" = "red")) +
    # Add labels and theme
    labs(
      title = "Site Map of Locations",
      x = "Longitude",
      y = "Latitude",
      shape = "Location Type",  # Legend title for shapes
      color = "Location Type"   # Legend title for colors
    ) +
    theme_minimal() +
    theme(legend.position = "bottom")
}

# Example usage
# Create data frames for sensors and sources
example_sensors <- data.frame(
  x = c(1, 3, 5, 7),
  y = c(2, 4, 6, 8)
)

example_sources <- data.frame(
  x = c(2, 6),
  y = c(3, 7)
)

# Generate the map
create_site_map(example_sensors, example_sources)

# OPTION 2: Distance Input from Origin
create_relative_site_map <- function(sensor_offsets, source_offsets, origin = c(0, 0)) {
  # Calculate sensor coordinates
  sensors <- data.frame(
    x = origin[1] + sensor_offsets$x,
    y = origin[2] + sensor_offsets$y,
    type = "Sensor"
  )

  # Calculate source coordinates
  sources <- data.frame(
    x = origin[1] + source_offsets$x,
    y = origin[2] + source_offsets$y,
    type = "Source"
  )

  # Combine data
  combined <- rbind(sensors, sources)

  # Create the plot
  ggplot(combined, aes(x = x, y = y, shape = type, color = type)) +
    geom_point(size = 4, stroke = 1.5) +
    scale_shape_manual(values = c("Sensor" = 21, "Source" = 4)) +
    scale_color_manual(values = c("Sensor" = "blue", "Source" = "red")) +
    labs(
      title = "Site Map with Relative Distances",
      x = "Longitude (relative)",
      y = "Latitude (relative)",
      shape = "Location Type",
      color = "Location Type"
    ) +
    theme_minimal() +
    theme(legend.position = "bottom")
}

# Example usage
# Define relative offsets for sensors and sources
sensor_offsets <- data.frame(
  x = c(0, 2, 4, 6),  # Distances along x-axis from the origin
  y = c(0, 2, 4, 6)   # Distances along y-axis from the origin
)

source_offsets <- data.frame(
  x = c(1, 5),         # Distances along x-axis
  y = c(1, 5)          # Distances along y-axis
)

# Generate the map
create_relative_site_map(sensor_offsets, source_offsets)
