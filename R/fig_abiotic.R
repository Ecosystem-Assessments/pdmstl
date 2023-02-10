#' Make figures for biotic data 

  source("R/zzz.R")
  source("R/get_basemap.R")
  source("R/plot_basemap.R")

  # Get area of study
  aoi <- sf::st_read("data/aoi/aoi.gpkg")
  aoi <- suppressWarnings(sf::st_simplify(aoi, 
                                          dTolerance = 100, 
                                          preserveTopology = F))
  # Get global parameters
  global_param()

  env <- c("arag",
           "bathy",
           "slope",
           "sat",
           "bottom_current_velocity_max",
           "bottom_dissolved_oxygen_mean",
           "bottom_dissolved_oxygen_range",
           "bottom_iron_mean",
           "bottom_nitrate_mean",
           "bottom_phosphate_mean",
           "bottom_salinity_mean",
           "bottom_salinity_range",
           "bottom_silicate_mean",
           "bottom_temperature_mean",
           "bottom_temperature_range",
           "mean_salinity",
           "mean_temperature",
           "sat")


  #---------- Plot every abiotic data ----------#

  lapply(env, function(x) {
    
    # Species name with first capital letter and space instead of underscore
    if(x %in% c("mean_salinity", "mean_temperature")) {
      dat <- terra::rast(sapply(2011:2020, function(i) {
        terra::rast(paste0("data/data-abiotic/",x,"_", i, ".tif"))
      })) |>
        terra::app(x = _, fun = mean, na.rm = TRUE)
    } else {
      dat <- terra::rast(paste0("data/data-abiotic/",x,".tif"))
    }

    # Define filename of the figures
    filename <- paste0("figures/env_", x, ".png")

    # Save figure in .png
    png(filename, 
        res = param$figures$resolution, 
        width = param$figures$width, 
        height = param$figures$height, 
        units = "mm")

    # Figure font family and margin
    par(family = 'serif', mar = c(.5, .5, .5, .5))

    plot(sf::st_geometry(aoi),
         border = "transparent")
    basemap("cities")
    global_param()
    box()

    # Plot data
    terra::plot(dat,
                add = TRUE)

    plot(sf::st_geometry(aoi),
         add = TRUE)
    # Cities
    plot(sf::st_geometry(cities), 
         add = TRUE, 
         pch = 21, 
         col = "#3e3e3e", 
         bg = "#9f9f9f", 
         cex = .4)
    for(i in 1:nrow(cities)) {
      text(x = cities$X[i]+cities$offX[i],
           y = cities$Y[i]+cities$offY[i],
           labels = cities$city[i],
           cex = .35,
           col = "#000000",
           adj = c(cities$adjX[i], .5))
    }

    dev.off()

  })