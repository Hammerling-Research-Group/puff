devtools::load_all()

set.seed(123)

sim_dt <- 10
puff_dt <- 10
output_dt <- 60
start_time <- "2024-01-01 12:00:00"
end_time <- "2024-01-01 13:00:00"
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

library(ggplot2)
library(dplyr)

# Ensure timestamp is in POSIXct format
sensor_concentrations$timestamp <- as.POSIXct(sensor_concentrations$Group.1, format = "%Y-%m-%d %H:%M:%S")

# Example: Combine timestamp and concentration with sensor locations
sensor_data <- data.frame(
  x = rep(sensor_coords[1], nrow(sensor_concentrations)),  # x-coordinates (single sensor)
  y = rep(sensor_coords[2], nrow(sensor_concentrations)),  # y-coordinates (single sensor)
  timestamp = sensor_concentrations$timestamp,
  concentration = sensor_concentrations$Sensor_1
)

# Create time-based variable for better visualization (e.g., hour and minute)
sensor_data$time_label <- format(sensor_data$timestamp, "%H:%M")

# Create the heatmap
ggplot(sensor_data, aes(x = time_label, y = concentration, fill = concentration)) +
  geom_tile() +
  scale_fill_gradientn(colors = c("blue", "yellow", "red"), name = "Concentration") +
  labs(
    title = "Heatmap of Concentration Over Time",
    x = "Time (Hour:Minute)",
    y = "Concentration"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for readability

