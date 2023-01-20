#' Make figures for biotic data 
#'
#' @export
fig_biotic <- function(lang = "fr") {

  # Get area of study
  aoi <- sf::st_read("data/aoi/aoi.gpkg")
  aoi <- suppressWarnings(sf::st_simplify(aoi, 
                                          dTolerance = 100, 
                                          preserveTopology = F))
  bbox <- sf::st_bbox(aoi)

  # Read data
  sea_pens <- sf::st_read("data/data-biotic/sea_pens.gpkg")

  # Get global parameters
  global_param()
  
  #---------- Plot presence of every taxa ----------#

  lapply(unique(sea_pens$scientific_name), function(x) {
    
    # Species name with first capital letter and space instead of underscore
    species <- stringr::str_to_sentence(gsub("_", " ", x))

    # Define filename of the figures
    fig_folder <- ifelse(lang == "fr", "figures", "figures_en")
    filename <- paste0(fig_folder, "/data_", x, ".png")

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
    presence <- sf::st_filter(sea_pens[sea_pens$scientific_name == x,], aoi) |>
                  sf::st_geometry()
    plot(presence,
         add = TRUE, 
         pch = 16, 
         col = param$col$palette[4],
         cex = 0.3)
    
    text(x = bbox$xmin + (bbox$xmax-bbox$xmin)/200,
         y = bbox$ymax - (bbox$ymax-bbox$ymin)/100,
         labels = paste0("Presence of ", species),
         font = 1.75,
         adj = c(0,1),
         cex = .8)

    dev.off()

  })

}
