# load current pkg version
devtools::load_all()


## SINGLE SENSOR IMPLEMENTATION
# define some simple/toy vals and data for quick testing
puff_dt <- 60  # puff creation interval in seconds
sim_dt <- 60  # sim time step in seconds (e.g., 60 = 1-minute intervals)
sim_start <- "2024-03-01 00:00:00"
sim_end <- "2024-03-01 01:00:00"

# create time stamps
time_stamps_sim <- seq(as.POSIXct(sim_start), as.POSIXct(sim_end), by = sim_dt)
n_sim <- length(time_stamps_sim)

# toy wind data (*will update with real wind data once code is stable)
wind_speeds <- c(1, 2, 4, 4, 2)  # speed in m/s
wind_directions <- c(85, 100, 115, 100, 90)  # directions in degrees

# Interpolate wind data to match the simulation resolution
wind_data <- interpolate_wind_data(wind_speeds, wind_directions,
                                   sim_start, sim_end,
                                   puff_dt)

## define emissions scenarios based on Meng's placement code (step 1)
# source coordinates and emission properties
source_coords <- matrix(c(
  80, 50, 3  # source A at (80, 50, 3)
), ncol = 3, byrow = TRUE)

# emission frequency for the source (p1)
emission_freq <- c(0.3)

# def 1 sensor coordinate as a start
sensor_coords <- matrix(c(150, 150, 0),
                        ncol = 3, byrow = TRUE)

# try it out
simple_sensor_simulation <- simulate_sensor_mode(
  time_stamps_sim = time_stamps_sim,
  sensor_coords = sensor_coords,
  n_sim = n_sim,
  num_sources = nrow(source_coords),
  source_coords = source_coords,
  emission_rates = emission_freq,
  wind_data = wind_data,
  puff_duration = 1200)

# Load necessary library for plotting
library(ggplot2)

# Prepare the data for plotting
# Convert time stamps to a data frame along with simulated concentration
sensor_data <- data.frame(
  time = time_stamps_sim,
  concentration = simple_sensor_simulation[, 1]  # Assuming one sensor for simplicity
)

# Plot the methane concentration at the sensor over time
ggplot(sensor_data, aes(x = time, y = concentration)) +
  geom_line(color = "blue") +
  labs(
    title = "Methane Emission Dispersion",
    x = "Time (min)",
    y = "Methane Concentration (ppm)"
  ) +
  theme_minimal()


##
## SCENARIO 1
# define some simple/toy vals and data for quick testing
puff_dt <- 60  # puff creation interval in seconds
sim_dt <- 60  # sim time step in seconds (e.g., 60 = 1-minute intervals)
sim_start <- "2024-01-01 00:00:00"
sim_end <- "2024-01-01 01:00:00"

# create time stamps
time_stamps_sim <- seq(as.POSIXct(sim_start), as.POSIXct(sim_end), by = sim_dt)
n_sim <- length(time_stamps_sim)

# toy wind data (*will update with real wind data once code is stable)
wind_speeds <- c(2, 3, 4, 3, 2)  # speed in m/s
wind_directions <- c(90, 100, 110, 100, 90)  # directions in degrees

# Interpolate wind data to match the simulation resolution
wind_data <- interpolate_wind_data(wind_speeds, wind_directions,
                                   sim_start, sim_end,
                                   puff_dt)

## define emissions scenarios based on Meng's placement code (step 1)
# source coordinates and emission properties
source_coords <- matrix(c(
  25, 25, 2,  # source A at (25, 25, 2)
  75, 25, 2,  # source B at (75, 25, 2)
  50, 75, 3   # source C at (50, 75, 3)
), ncol = 3, byrow = TRUE)

# emission frequency for the sources (p1, p2, p3)
emission_freq <- c(0.25, 0.25, 0.5)

# def 3 sensor coordinates as a start
sensor_coords <- matrix(c(200, 200, 0,
                          300, 300, 0,
                          400, 400, 0),
                        ncol = 3, byrow = TRUE)

# try it out
sensor_simulation1 <- simulate_sensor_mode(
  time_stamps_sim = time_stamps_sim,
  sensor_coords = sensor_coords,
  n_sim = n_sim,
  num_sources = nrow(source_coords),
  source_coords = source_coords,
  emission_rates = emission_freq,
  wind_data = wind_data,
  puff_duration = 1200)


##
## SCENARIO 2
puff_dt <- 60
sim_dt <- 60
sim_start <- "2024-02-01 00:00:00"
sim_end <- "2024-02-01 01:00:00"

# create time stamps
time_stamps_sim <- seq(as.POSIXct(sim_start), as.POSIXct(sim_end), by = sim_dt)
n_sim <- length(time_stamps_sim)

# toy wind data
wind_speeds <- c(2.5, 3.5, 4.5, 3.5, 2.5)  # speed in m/s, with more variation
wind_directions <- c(80, 95, 120, 95, 80)  # directions in degrees, more spread out

wind_data <- interpolate_wind_data(wind_speeds, wind_directions,
                                   sim_start, sim_end,
                                   puff_dt)

# source coordinates and emission properties
source_coords <- matrix(c(
  20, 30, 1.5,
  70, 20, 2.5,
  45, 80, 4
), ncol = 3, byrow = TRUE)

# emission frequency for the sources (more variable)
emission_freq <- c(0.2, 0.4, 0.6)

# new sensor coordinates, spread out further and adjusted for coverage
sensor_coords <- matrix(c(
  150, 250, 0,
  350, 350, 0,
  450, 450, 0,
  500, 100, 0
), ncol = 3, byrow = TRUE)

# try it out
sensor_simulation2 <- simulate_sensor_mode(
  time_stamps_sim = time_stamps_sim,
  sensor_coords = sensor_coords,
  n_sim = n_sim,
  num_sources = nrow(source_coords),
  source_coords = source_coords,
  emission_rates = emission_freq,
  wind_data = wind_data,
  puff_duration = 1200)


##
## viz idea
library(ggplot2)

time_steps <- nrow(sensor_simulation2)
sensor_ids <- c("Sensor 1", "Sensor 2", "Sensor 3", "Sensor 4")

sensor_data <- data.frame(
  time = rep(1:time_steps, each = 4),
  sensor_id = rep(sensor_ids, time_steps),
  concentration = as.vector(sensor_simulation2)
)

sensor_data |>
  dplyr::filter(time < 20) |>
  ggplot(aes(x = time, y = concentration,
             color = sensor_id)) +
  geom_line(aes(group = sensor_id), linewidth = 1) +
  facet_wrap(~sensor_id) +
  geom_point(aes(shape = sensor_id), size = 2) +
  labs(title = "Methane Emission Dispersion Across Sensors Over 20 Seconds",
       x = "Time (s)",
       y = "Methane Concentration") +
  theme_minimal()
