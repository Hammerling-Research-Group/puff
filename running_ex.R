# load current pkg version
devtools::load_all()

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
sensor_simulation <- simulate_sensor_mode(
  time_stamps_sim = time_stamps_sim,
  sensor_coords = sensor_coords,
  n_sim = n_sim,
  num_sources = nrow(source_coords),
  source_coords = source_coords,
  emission_rates = emission_freq,
  wind_data = wind_data,
  puff_duration = 1200)

print(sensor_simulation)
