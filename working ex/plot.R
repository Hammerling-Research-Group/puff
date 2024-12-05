## working plotting code

library(tidyverse)

# load existing puff code (including helpers)
devtools::load_all()

## ex
grid_coords <- list(
  x = seq(0, 20, by = 1),
  y = seq(0, 20, by = 1),
  z = c(2.5)
)

set.seed(123)

sim_dt <- 10
puff_dt <- 10
output_dt <- 60
start_time <- "2024-01-01 12:00:00"
end_time <- "2024-01-01 12:10:00"
source_coords <- c(25, 25, 5)
emission_rate <- 3
wind_data <- list(
  wind_u = runif(3601, min = -3, max = 0.7),
  wind_v = runif(3601, min = -3, max = 1.5)
)

# run grid mode
output <- simulate_grid_mode(
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


## viz static grid mode time steps x concentration
# def time steps (from sim)
output_timestamps <- seq(from = as.POSIXct(start_time),
                         to = as.POSIXct(end_time),
                         by = output_dt)

# create heatmaps for all time steps
c_heatmaps <- function(output, grid_coords, output_timestamps) {
  grid_idx <- expand.grid(
    x = grid_coords$x,
    y = grid_coords$y,
    z = grid_coords$z
  )

  full <- lapply(seq_len(nrow(output)), function(time_step) {
    data.frame(
      x = grid_idx$x,
      y = grid_idx$y,
      concentration = output[time_step, ],
      time = output_timestamps[time_step]
    )
  }) |>
    bind_rows()

  ggplot(full, aes(x = x, y = y, fill = concentration)) +
    geom_tile() +
    scale_fill_viridis_c(option = "plasma") +
    labs(
      title = "Methane Concentrations Over Time Across a Grid",
      x = "x",
      y = "y",
      fill = "Concentration"
    ) +
    theme_minimal() +
    facet_wrap(~time) +
    theme(
      strip.text = element_text(size = 10, face = "bold"),
      axis.text = element_text(size = 8)
    )
}

c_heatmaps(output, grid_coords, output_timestamps)


