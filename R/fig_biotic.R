#' Make figures for biotic data 

  source("R/zzz.R")
  source("R/get_basemap.R")
  source("R/plot_basemap.R")

  # Get area of study
  aoi <- sf::st_read("data/aoi/aoi.gpkg")
  aoi <- suppressWarnings(sf::st_simplify(aoi, 
                                          dTolerance = 100, 
                                          preserveTopology = F))

  # Read data
  sp_data <- list(anthoptilum_grandiflorum = sf::st_read("data/data-biotic/anthoptilum_grandiflorum.gpkg"),
                  balticina_finmarchica = sf::st_read("data/data-biotic/balticina_finmarchica.gpkg"),
                  pennatula_aculeata = sf::st_read("data/data-biotic/pennatula_aculeata.gpkg"),
                  ptilella_grandis = sf::st_read("data/data-biotic/ptilella_grandis.gpkg"))

  # Get global parameters
  global_param()
  
  #---------- Plot presence of every taxa ----------#

  sp <- c("anthoptilum_grandiflorum",
          "balticina_finmarchica",
          "pennatula_aculeata",
          "ptilella_grandis")

  lapply(sp, function(x) {
    
    # Species name with first capital letter and space instead of underscore
    species <- stringr::str_to_sentence(gsub("_", " ", x))

    filename <- paste0("figures/presence_absence_", x, ".png")

    # Save figure in .png
    png(filename, 
        res = param$figures$resolution, 
        width = param$figures$width, 
        height = param$figures$height, 
        units = "mm", 
        pointsize = param$figures$pointsize)

    # Figure font family and margin
    par(family = 'serif', mar = c(.5, .5, .5, .5))

    # Plot the basemap (aoi and cities)
    plot_basemap(aoi)
    box()

    # Plot data
    presence <- sf::st_filter(sp_data[[x]], aoi)

    plot(sf::st_geometry(presence[presence$presence == 1,]),
         add = TRUE, 
         pch = 16, 
         col = "red",
         cex = 0.4)

    plot(sf::st_geometry(presence[presence$presence == 0,]),
         add = TRUE, 
         pch = 16, 
         col = "black",
         cex = 0.2)
     
    legend(x = "bottomright",
           legend=c("Présences","Absences"),
           col=c("red", "black"),
           pch = 16,
           cex = 0.5,
           horiz = TRUE,
           bty = "n")

    dev.off()

  })

  lapply(sp, function(x) {
    
    # Species name with first capital letter and space instead of underscore
    species <- stringr::str_to_sentence(gsub("_", " ", x))

    # Define filename of the figures
    filename <- paste0("figures/presence_", x, ".png")

    # Save figure in .png
    png(filename, 
        res = param$figures$resolution, 
        width = param$figures$width, 
        height = param$figures$height, 
        units = "mm", 
        pointsize = param$figures$pointsize)

    # Figure font family and margin
    par(family = 'serif', mar = c(.5, .5, .5, .5))

    # Plot the basemap (aoi and cities)
    plot_basemap(aoi)
    box()

    # Plot data
    presence <- sf::st_filter(sp_data[[x]], aoi)

    plot(sf::st_geometry(presence[presence$presence == 1,]),
         add = TRUE, 
         pch = 16, 
         col = "red",
         cex = 0.3)

    dev.off()

  })
