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

  env <- list.files("data/data-abiotic/", pattern = ".tif$") |>
           gsub(pattern = ".tif",
                replacement = "",
                x = _)

  #---------- Plot every abiotic data ----------#

  lapply(env, function(x) {
    
    # Load data
    dat <- terra::rast(sprintf("data/data-abiotic/%s.tif", x))

    # Define filename of the figures
    filename <- sprintf("figures/env_%s.png", x)

    # Save figure in .png
    png(filename, 
        res = param$figures$resolution, 
        width = param$figures$width, 
        height = param$figures$height, 
        units = "mm")

    # Figure font family and margin
    par(family = 'serif', mar = c(2, 2, 1, 4))

    plot(sf::st_geometry(aoi),
         border = "transparent")
    basemap("cities")
    box()

    # Plot data
    terra::plot(dat,
                add = TRUE,
                col = "#6699CC")
    legend("bottomright",
           "Zones de concentration significative\nde plumes de mer",
           col = "#6699CC",
           bty = "n",
           pch = 16,
           cex = 0.8,
           pt.cex = 1.3)
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
           cex = .65,
           col = "#000000",
           adj = c(cities$adjX[i], .5))
    }

    dev.off()

  })