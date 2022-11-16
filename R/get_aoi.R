#' Script to get the area of interest for the assessment
#'
#' @export
get_aoi <- function() {
  # Load federal bioregions
  dat <- pipedat:::basemap$can |>
         sf::st_make_valid()

  # Export data
  if (!file.exists("data/data-basemap/")) dir.create("data/data-basemap/")
  sf::st_write(
    obj = dat,
    dsn = "./data/data-basemap/aoi.geojson",
    delete_dsn = TRUE,
    quiet = TRUE
  )

  # Export for lazy load
  aoi <- dat
  save(aoi, file = './data/aoi.RData')
}