% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plots.R
\name{create_site_map}
\alias{create_site_map}
\title{Create a Site Map of Sensors and Sources}
\usage{
create_site_map(sensors, sources)
}
\arguments{
\item{sensors}{A data frame containing sensor locations. It must include:
\describe{
  \item{x}{A numeric vector of sensor x-coordinates.}
  \item{y}{A numeric vector of sensor y-coordinates.}
}}

\item{sources}{A data frame containing source locations. It must include:
\describe{
  \item{x}{A numeric vector of source x-coordinates.}
  \item{y}{A numeric vector of source y-coordinates.}
}}
}
\value{
A ggplot object showing a site map of sensors and sources.
}
\description{
This function generates a site map showing the locations of sensors and sources.
}
\examples{
\dontrun{
sensors <- data.frame(x = c(1, 2, 3), y = c(4, 5, 6))
sources <- data.frame(x = c(7, 8), y = c(9, 10))

create_site_map(sensors, sources)
}
}
