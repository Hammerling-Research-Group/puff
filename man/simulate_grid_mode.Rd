% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/simulate_grid_mode.R
\name{simulate_grid_mode}
\alias{simulate_grid_mode}
\title{Simulate Atmospheric Concentration on a Grid}
\usage{
simulate_grid_mode(start_time, end_time, source_coords, emission_rate, wind_data,
  grid_coords, sim_dt, puff_dt, output_dt, puff_duration, ws, wd)
}
\arguments{
\item{start_time}{POSIXct. Start time of the simulation.}

\item{end_time}{POSIXct. End time of the simulation.}

\item{source_coords}{Numeric vector or matrix. Coordinates of the emission source(s) in meters (x, y, z).
If simulating multiple sources, provide a matrix with one row per source.
E.g., \code{matrix(c(0, 0, 2.5, 10, 10, 2.5), ncol = 3, byrow = TRUE)}.}

\item{emission_rate}{Numeric. Emission rate in kg/hr per source. If multiple sources are provided, this
value will be assumed the same for each. (Note: source-specific rates are not yet supported.)}

\item{wind_data}{Data frame. Must contain either columns `wind_u` and `wind_v` (wind vector components in x/y directions)
or columns representing wind speed and direction, declared as `ws` and `wd`.}

\item{grid_coords}{List. A list with three numeric vectors specifying the grid points for
\code{x}, \code{y}, and \code{z} coordinates, e.g.,
\code{list(x = seq(-50, 50, by = 5), y = seq(-50, 50, by = 5), z = c(2.5))}.}

\item{sim_dt}{Integer. Simulation time step in seconds (default = 1). Determines how frequently puff positions are updated.}

\item{puff_dt}{Integer. Puff emission interval in seconds (default = 1). New puffs are emitted from each source at this frequency.}

\item{output_dt}{Integer. Desired time resolution (in seconds) for final output concentrations.}

\item{puff_duration}{Numeric. Maximum puff lifetime in seconds (default = 1200). Puffs beyond this age are discarded.}

\item{ws}{Optional. String. Name of the column in `wind_data` containing wind speeds (m/s).
Required if `wind_data` contains polar wind components instead of Cartesian (`wind_u`, `wind_v`).}

\item{wd}{Optional. String. Name of the column in `wind_data` containing wind directions (degrees from).}
}
\value{
A matrix of concentrations (ppm) with rows representing output time steps and columns
  representing grid points. Columns correspond to the flattened grid defined by \code{expand.grid(grid_coords)}.
}
\description{
Simulates methane concentrations at each grid point over time using the Gaussian puff forward model.
Supports one or more emission sources. Each puff retains constant wind speed and direction
throughout its lifetime, and corresponding dispersion parameters are determined at the time of emission.
}
\details{
- Each source (from one to many) emits puffs at intervals of \code{puff_dt}.
- Each puff maintains a fixed wind vector and dispersion parameters.
- Puffs are advected over time based on their individual wind vectors.
- Concentration contributions from all active puffs are computed at each grid point and summed.
- Concentrations are aggregated and returned at a coarser time resolution defined by \code{output_dt}.
}
\note{
All time parameters should be positive, with `puff_dt > sim_dt` and `out_dt > sim_dt`. Also, `puff_dt` should be a positive integer multiple of `sim_dt`, i.e. `puff_dt = n*sim_dt` for some positive integer `n`. This prevents the code having to interpolate the concentration values in time, although it is likely that this constraint could be avoided.
}
\examples{
\donttest{
set.seed(123)

sim_dt <- 7
puff_dt <- 7
output_dt <- 60
start_time <- "2024-01-01 12:00:00"
end_time <- "2024-01-01 13:00:00"
source_coords <- c(0, 0, 2.5)
emission_rate <- 3.5
wind_data <- data.frame(
  wind_u = runif(3601, min = -3, max = 0.7),
  wind_v = runif(3601, min = -3, max = 1.5)
)

grid_coords <- list(
  x = seq(-2, 2, by = 1),
  y = seq(-2, 2, by = 1),
  z = c(2.5)
)
  out <- simulate_grid_mode(
    start_time = start_time,
    end_time = end_time,
    source_coords = source_coords,
    emission_rate = emission_rate,
    wind_data = wind_data,
    grid_coords = grid_coords,
    sim_dt = sim_dt,
    puff_dt = puff_dt,
    output_dt = output_dt,
    puff_duration = 1200
  )
}
}
\references{
Jia, M., Fish, R., Daniels, W., Sprinkle, B. and Hammerling, D. (2024) <doi:10.26434/chemrxiv-2023-hc95q-v3>
}
