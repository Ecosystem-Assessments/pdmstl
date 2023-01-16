#' Prepare biotic data 
#'
#' @export
make_abiotic <- function() {
  library(stars)
  out <- here::here("data","data-biotic")
  chk_create(out)
  grd <- stars::read_stars("data/grid/grid.tif")
  
  # Biotic data
  south <- vroom::vroom("data/data-raw/groundfish_survey_4t_dfo-5b61d05c/groundfish_survey_4t_dfo-5b61d05c.csv")
  north <- vroom::vroom("data/data-raw/groundfish_survey_teleost_dfo-d6b6f3fa/groundfish_survey_teleost_dfo-d6b6f3fa.csv")
  
  # Select only target species 
  # WARNING: À compléter avec la liste à Lisa et faire le ménage des espèces
  nm <- c(
    "Pennatula aculeata",
    "Pennatulacea"
  )
  dat <- dat[dat$scientific_name %in% nm, ]
    
  # Spatial 
  dat <- sf::st_as_sf(
    dat, 
    coords = c("start_longitude","start_latitude"), 
    crs = 4236
  )
  
  
}