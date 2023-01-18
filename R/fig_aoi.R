#' Figure of study area
#'
#' @export

fig_aoi <- function(lang = "fr") {
  # ------------------
#  grid_terra <- terra::rast("data/grid/grid.tif")
#  e <-  terra::as.polygons(terra::ext(grid_terra)) |>
#    sf::st_as_sf()
#
#  grid_spoly <- sf::st_make_grid(e, cellsize = terra::res(grid_terra)) |>
#    sf::st_set_crs(4326)
  
  # ------------------
  aoi <- sf::st_read("data/aoi/aoi.gpkg")
  aoi <- suppressWarnings(sf::st_simplify(aoi, 
                                          dTolerance = 100, 
                                          preserveTopology = F))

  # ------------------
  basemap("cities")
  basemap("quebec")

  # Labels
  if (lang == "fr") {
    labs <- c("Zone d'étude","Secteur fluvial","Secteur maritime","Grille d'étude")
  } else if (lang == "en") {
    labs <- c("Study area","Fluvial sector","Marine sector","Study grid")
  }
  
  # ------------------
  global_param()

  # ------------------
  plotDat <- function(trans = "77") {
    # ------------------
#    plot(
#      grid_spoly,
#      lwd = .1,
#      border = paste0("#000000", trans),
#      add = TRUE
#    )

    # ------------------
    plot(
      sf::st_geometry(aoi),
      lwd = .5,
      border = "#000000",
      add = TRUE
    )
  }

  # ------------------------------------------------------------------------
  # Graph principal
  fold <- if (lang == "fr") {
    glue::glue('./figures/aoi.png')
  } else if (lang == "en") {
    glue::glue('./figures_en/aoi.png')
  }
  png(fold, 
      res = param$figures$resolution, 
      width = param$figures$width, 
      height = param$figures$height, 
      units = "mm", 
      pointsize = param$figures$pointsize)

  # ------------------
  par(family = 'serif', mar = c(.5, .5, .5, .5))

  # ------------------
  bbox <- sf::st_bbox(aoi)
  qc <- sf::st_bbox(quebec)

  # ------------------
  pal <- colorRampPalette(viridis::viridis(100))
  # pal <- colorRampPalette(param$col$integrated$palette)

  # ------------------
  # Basemap
  graphicsutils::plot0(x = c(bbox$xmin, bbox$xmax), y = c(bbox$ymin, bbox$ymax))
  box()

  # ------------------
  # Text
  text(x = bbox$xmin + (bbox$xmax-bbox$xmin)/20,
       y = bbox$ymax - (bbox$ymax-bbox$ymin)/20,
       labels = labs[1],
       font = 2,
       adj = c(0,.5),
       cex = .8
     )

  # ------------------
  # Data
  plotDat()

  # ------------------
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


  # ---------------------------
  # Québec
  par(new = TRUE)
  par(fig = c(.75,.905,.0525,.3), mar = c(0,0,0,0))
  graphicsutils::plot0(x = c(qc$xmin, qc$xmax), y = c(qc$ymin, qc$ymax))
  # ------------------
  plot(
    sf::st_geometry(quebec),
    lwd = .5,
    border = "#6b6b6b",
    add = TRUE
  )

  # ------------------
  plot(
    sf::st_geometry(aoi),
    lwd = .5,
    border = "#2196a8",
    col = "#2196a8",
    add = TRUE
  )

  # ----
  box(col = "#000000")

  # ---------------------------
  dev.off()
}