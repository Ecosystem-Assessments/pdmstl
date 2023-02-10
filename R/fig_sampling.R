# Script to make figures of sampling locations

# Setup
aoi <- sf::st_read("data/aoi/aoi.gpkg")

source("R/zzz.R")
source("R/get_basemap.R")
source("R/plot_basemap.R")
global_param()

# 4T Division

# Read data 
dat_4t <- read.csv("data/data-raw/groundfish_survey_4t_dfo-5b61d05c/groundfish_survey_4t_dfo-5b61d05c.csv")
dat_4t <- sf::st_as_sf(dat_4t, coords = c("longitude", "latitude"), crs = sf::st_crs("EPSG:4326"))

# Save figure in .png
png("figures/sampling_4t.png", 
    res = param$figures$resolution, 
    width = param$figures$width, 
    height = param$figures$height, 
    units = "mm", 
    pointsize = param$figures$pointsize)

# Plot data
par(family = 'serif', mar = c(.5, .5, .5, .5))

# Plot the basemap (aoi and cities)
plot_basemap(aoi)
box()

# Plot data
sampling_points <- sf::st_filter(dat_4t, aoi) |>
                     sf::st_geometry()
terra::points(terra::vect(sampling_points),
              pch = 16,
              cex = 0.2)

dev.off()


# Teleost

# Read data 
dat_teleost <- read.csv("data/data-raw/groundfish_survey_teleost_dfo-d6b6f3fa/groundfish_survey_teleost_dfo-d6b6f3fa.csv")
dat_teleost[,"lat"] <- rowMeans(dat_teleost[,c("start_latitude", "end_latitude")])
dat_teleost[,"lon"] <- rowMeans(dat_teleost[,c("start_longitude", "end_longitude")])
dat_teleost <- sf::st_as_sf(dat_teleost, coords = c("lon", "lat"), crs = sf::st_crs("EPSG:4326"))

# Save figure in .png
png("figures/sampling_teleost.png", 
    res = param$figures$resolution, 
    width = param$figures$width, 
    height = param$figures$height, 
    units = "mm", 
    pointsize = param$figures$pointsize)

# Plot data
par(family = 'serif', mar = c(.5, .5, .5, .5))

# Plot the basemap (aoi and cities)
plot_basemap(aoi)
box()

# Plot data
sampling_points <- sf::st_filter(dat_teleost, aoi) |>
                     sf::st_geometry()
terra::points(terra::vect(sampling_points), 
              pch = 16,
              cex = 0.2)

dev.off()
