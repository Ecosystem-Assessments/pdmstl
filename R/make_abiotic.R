#' Prepare abiotic data 
#'
#' @export
make_abiotic <- function() {
  library(stars)
  out <- here::here("data","data-abiotic")
  chk_create(out)
  grd <- stars::read_stars("data/grid/grid.tif")
  
  # Functions 
  names_stars <- function(dat) {
    lapply(dat, names) |> 
    unlist() |> 
    unname()
  }
  
  # Abiotic data
  ## Bathymetry
  bathy <- importdat("e775900b")
  
  ## Bio-ORACLE
  biooracle <- here::here("data","data-raw","bio-oracle-4d4292ca") |>
               dir(pattern = ".tif$", full.names = TRUE) |>
               lapply(stars::read_stars)
  
  ## Temperature & Salinity
  dat <- here::here("data","data-integrated","bottom_temperature_salinity_egsl-6c724ee5") |>
               dir(pattern = ".tif$", full.names = TRUE) |>
               lapply(stars::read_stars)
  nmdat <- lapply(dat, names) |> 
           unlist() |>
           substr(43, 1e6)
  salinity <- dat[stringr::str_detect(nmdat, "salinity")]
  temperature <- dat[stringr::str_detect(nmdat, "temperature")]
  rm(dat)
  
  ## Dissolved oxygen
  oxygen <- here::here("data","data-integrated","bottom_oxygen_egsl-0d36cf5d") |>
            dir(pattern = ".tif$", full.names = TRUE) |>
            lapply(stars::read_stars)
                       
  ## Environment Beauchesne et al. 2020
  load("data/data-raw/beauchesne_thesis_abiotic_data/EnvironmentRasters.RData")
  temp <- env
  env <- list()
  for(i in 1:raster::nlayers(temp)) {
    env[[i]] <- stars::st_as_stars(temp[[i]])
  }
  rm(temp)
                
  ## All together
  abiotic <- c(
    bathy,
    biooracle,
    salinity,
    temperature,
    oxygen,
    env
  )

  # Warp 
  abiotic <- lapply(abiotic, stars::st_warp, dest = grd)
  
  # Change names
  nm <- names_stars(abiotic)
  nm <- gsub("_gebco_2021-e775900b-n90_s0_w-90_e0.tif","", nm)
  nm <- gsub("bottom_temperature_salinity_egsl-6c724ee5-","",nm)
  nm <- gsub("bottom_oxygen_egsl-0d36cf5d-","",nm)
  nm <- gsub(".Mean.tif","", nm)
  nm <- gsub(".Mean.BOv2_2.tif","", nm)
  nm <- gsub(".BOv2_2.tif","", nm)
  nm <- gsub("Light.bottom","Light", nm)
  nm <- gsub("bio-oracle-4d4292ca-Present.Benthic.Mean.Depth.","Bottom_", nm)
  nm <- gsub("bio-oracle-4d4292ca-Present.Surface.","Surface_", nm)
  nm <- gsub(".tif", "", nm)
  nm <- gsub("\\.", "_", nm)
  nm <- gsub("\\-", "_", nm)
  nm <- tolower(nm)
  nm
  
  # Verify bio-oracle ph and calcite bottom
  
  # Export
  for(i in 1:length(abiotic)) {
    stars::write_stars(
      abiotic[[i]],
      here::here(out, glue::glue("{nm[i]}.tif")),
      delete_dsn = TRUE
    )
  }
}