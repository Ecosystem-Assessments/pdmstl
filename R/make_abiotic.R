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
  bathy <- terra::rast("data/data-raw/bathymetry_gebco_2021-e775900b/bathymetry_gebco_2021-e775900b-n90_s0_w-90_e0.tif") |>
    terra::crop(x = _, 
                y = terra::vect(sf::st_read("data/aoi/aoi.gpkg")),
                mask = TRUE) |>
    terra::clamp(x = _, upper = 0) |>
    stars::st_as_stars() |>
    list()

  slope <- terra::terrain(terra::rast(bathy[[1]]), "slope", unit = "degrees") |>
             stars::st_as_stars() |>
             list()


  ## Bio-ORACLE
  biooracle <- here::here("data","data-raw","bio-oracle-4d4292ca") |>
               dir(pattern = ".tif$", full.names = TRUE) |>
               lapply(stars::read_stars) |>
               lapply(stars::st_warp, dest = grd)
  biooracle <- lapply(biooracle, function(x) {
                 tmp <- terra::mask(terra::rast(x), terra::vect(sf::st_read("data/aoi/aoi.gpkg")))
                 names(tmp) <- names(x)
                 return(tmp)
               }) |>
               lapply(stars::st_as_stars)

  
  ## Temperature & Salinity
  dat <- here::here("data","data-integrated","bottom_temperature_salinity_egsl-6c724ee5") |>
               dir(pattern = ".tif$", full.names = TRUE) |>
               lapply(terra::rast)
  nmdat <- lapply(dat, names) |> 
           unlist() |>
           substr(43, 1e6)
  salinity <- dat[stringr::str_detect(nmdat, "salinity")] |>
                terra::rast() |>
                  terra::app(x=_, fun=mean)
  names(salinity) <- "mean_salinity"
  salinity <- stars::st_as_stars(salinity) |>
                list()
  temperature <- dat[stringr::str_detect(nmdat, "temperature")] |>
                   terra::rast() |>
                     terra::app(x=_, fun=mean)
  names(temperature) <- "mean_temperature"
  temperature <- stars::st_as_stars(temperature) |>
                   list()
  rm(dat)
  
                       
  ## Environment Beauchesne et al. 2020
  load("data/data-raw/beauchesne_thesis_abiotic_data/EnvironmentRasters.RData")
  temp <- env[[c("ARAG", "sat")]]
  env <- list()
  for(i in 1:raster::nlayers(temp)) {
    env[[i]] <- stars::st_as_stars(temp[[i]])
  }
  rm(temp)


  ## Loring sediments data
  loring <- sf::st_read("data/data-raw/Seafloor_SubstratBenthique/Seafloor_SubstratBenthique.shp")
  loring$DEPOT_GRO <- as.factor(loring$DEPOT_GRO)
  loring[loring$DEPOT_GRO %in% "Gravier-Sable", "DEPOT_GRO"] <- "Sable-Gravier"
  loring$DEPOT_GRO <- droplevels(loring$DEPOT_GRO, "Gravier-Sable")
  loring <- terra::vect(loring) |>
              terra::project(x = _, y = terra::rast(grd)) |>
                terra::rasterize(x = _, y = terra::rast(grd), field = "DEPOT_GRO") |>
                  stars::st_as_stars() |>
                    list()


  ## All together
  abiotic <- c(
    bathy,
    biooracle,
    salinity,
    temperature,
    env,
    loring,
    slope
  )

  # Warp 
  abiotic <- lapply(abiotic, stars::st_warp, dest = grd)
  
  # Change names
  nm <- names_stars(abiotic)
  nm <- gsub("gebco_2022_sub_ice_n90.0_s0.0_w-90.0_e0.0","bathy", nm)
  nm <- gsub("bottom_temperature_salinity_egsl-6c724ee5-","mean_",nm)
  nm <- gsub("bottom_oxygen_egsl-0d36cf5d-","mean_",nm)
  nm <- gsub(".BOv2_1.tif","", nm)
  nm <- gsub(".BOv2_2.tif","", nm)
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