# Gaussian Puff Model

[![Project Status: Active – The project has reached a stable, usable state and is being actively developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![CRAN_Status_Badge](https://www.r-pkg.org/badges/version/puff)](http://cran.r-project.org/package=puff) 
[![Downloads](https://cranlogs.r-pkg.org/badges/grand-total/puff)](https://cran.r-project.org/package=puff)
[![CI lint](https://github.com/Hammerling-Research-Group/puff/actions/workflows/lint.yml/badge.svg)](https://github.com/Hammerling-Research-Group/puff/actions/workflows/lint.yml)
[![CI tests](https://github.com/Hammerling-Research-Group/puff/actions/workflows/test.yml/badge.svg)](https://github.com/Hammerling-Research-Group/puff/actions/workflows/test.yml)
[![GitHub contributors](https://img.shields.io/github/contributors/Hammerling-Research-Group/puff.svg)](https://github.com/Hammerling-Research-Group/puff/graphs/contributors/)

The `puff` package is primarily a visualization-focused package aimed at offering many ways to visualize emission dispersion plumes given some site-level information (e.g., wind conditions, emission rate, etc.). 

Though focused on visualization, `puff` also provides functions for simulating and running the Gaussian puff model in either `sensor` or `grid` mode. Sensor mode corresponds to running the model based on specific sensor locations, where as grid mode corresponds to running the model across a full site. See the [paper](https://chemrxiv.org/engage/chemrxiv/article-details/672a296b7be152b1d00fcc60) or the [Python version](https://github.com/Hammerling-Research-Group/FastGaussianPuff) for more *model-specific* details. 

## Installation and Usage

Stable (on CRAN):

```{r}
install.packages("puff")

library(puff)
```

Dev:

```{r}
devtools::install_github("Hammerling-Research-Group/puff")
```

Once the user obtains output from the forward model (either via the `puff` package ([`simulate_sensor_mode()`](https://github.com/Hammerling-Research-Group/puff/blob/66d4ca87d25bceb9edd5f24f6aa9fe0fc2a17604/R/simulate_sensor_mode.R) or [`simulate_grid_mode()`](https://github.com/Hammerling-Research-Group/puff/blob/66d4ca87d25bceb9edd5f24f6aa9fe0fc2a17604/R/simulate_grid_mode.R)) or via some other approach), they can use their site details and concentration output to build numerous visualizations, including several static plots, 2D and 3D animated plots, and a site map generator based on sensor and source coordinates.

Please start with the package [vignette](https://github.com/Hammerling-Research-Group/puff/blob/main/vignettes/getting-started.Rmd) for more details on getting started. 

## Contribute

This software is being actively developed, with more features and updates forthcoming. Wide engagement with it and collaboration is welcomed!

Please note that before contributing, first read and abide by our group's [Code of Conduct](https://github.com/Hammerling-Research-Group/.github/blob/c2b84cdf1b723a4b23627b2aa59212aefd26b5cc/Code%20of%20Conduct.md). Then, review and abide by our group's workflow and [Standards of Development](https://github.com/Hammerling-Research-Group/.github/blob/01338522301ab2604fc11e95e9ef75cdde24752e/Standards.md). 

When you're ready, here are the best ways to contribute:

  - Submit an [issue](https://github.com/Hammerling-Research-Group/puff/issues) reporting a bug, requesting a feature enhancement, etc. 

  - Suggest changes directly via a [pull request](https://github.com/Hammerling-Research-Group/puff/pulls)

  - Reach out directly [here](https://ams.mines.edu/hammerling-research-group/) or [here](https://github.com/Hammerling-Research-Group) with ideas if you're uneasy with public interaction
