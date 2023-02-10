#' Plot basemap for biotic and abiotic figures 
#'
#' @export
plot_basemap <- function(aoi) {

  basemap("cities")

  global_param()
  
  # Data
  plot(sf::st_geometry(aoi),
       lwd = .5,
       border = "#00000077")

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

}
