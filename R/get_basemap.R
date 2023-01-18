#' Download and format basemap data used for figures
#'
#' Accesses and formats spatial data that I use frequently for static mapping
#'
#'
#' @export

get_basemap <- function() {
  global_param()
  # Quebec
  canada <- getData('GADM', country = 'CAN', level = 1, path = 'data/data-basemap/')
  canada <- st_as_sf(canada)
  quebec <- canada[canada$NAME_1 %in% c('Québec', 'Nova Scotia','New Brunswick', 'Newfoundland and Labrador', 'Prince Edward Island'),]
  quebec <- suppressWarnings(st_simplify(quebec, dTolerance = 100, preserveTopology = F)) %>%
            st_transform(crs = param$crs)
  st_write(obj = quebec,
           dsn = "./data/data-basemap/quebec.geojson",
           delete_dsn = TRUE)

  # Canada
  canada <- getData('GADM', country = 'CAN', level = 0, path = 'data/data-basemap/')
  canada <- st_as_sf(canada)
  canada <- suppressWarnings(st_simplify(canada, dTolerance = 150, preserveTopology = F)) %>%
            st_transform(crs = param$crs)
  st_write(obj = canada,
           dsn = "./data/data-basemap/canada.geojson",
           delete_dsn = TRUE)
  # USA
  # Load needed data
  usa <- getData('GADM', country = 'USA', level = 0, path = 'data/data-basemap/')
  usa <- st_as_sf(usa)
  usa <- suppressWarnings(st_simplify(usa, dTolerance = 150, preserveTopology = F)) %>%
            st_transform(crs = param$crs)
  st_write(obj = usa,
           dsn = "./data/data-basemap/usa.geojson",
           delete_dsn = TRUE)

  # Delete loaded data
  file.remove("data/data-basemap/gadm36_CAN_1_sp.rds")
  file.remove("data/data-basemap/gadm36_CAN_0_sp.rds")
  file.remove("data/data-basemap/gadm36_USA_0_sp.rds")

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Simple EGSL outline from eDrivers
  # ---------------------------------------
  #
  # Import only for convenience, directly as package data
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # URL
  egsl <- 'https://github.com/eDrivers/eDriversGrids/raw/master/Data/RawData/egsl.zip'

  # Download
  download.file(egsl, destfile = "./data/data-basemap/egsl.zip")

  # Unzip
  unzip("./data/data-basemap/egsl.zip", exdir = "./data/data-basemap/egsl/")

  # Load
  library(sp)
  egsl <- st_read("./data/data-basemap/egsl/egsl.shp") %>%
          as("Spatial")

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #                               SIMPLIFIED CONTOUR
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Selected plygons in egsl (remove small islands that add many vertex to the dataset)
  j <- c(1, 9127, 8999, 9038, 9005, 8998, 131, 18, 17)
  p <- list()
  for(i in 1:length(j)) {
    p[[i]] <- Polygon(egsl@polygons[[1]]@Polygons[[j[i]]]@coords)
  }
  ps <- Polygons(p, 1)
  sps <- SpatialPolygons(list(ps))
  proj4string(sps) <- proj4string(egsl)

  # Transform to sf object and simplify
  egsl <- sps %>%
          SpatialPolygonsDataFrame(. , data.frame(ID = 1)) %>%
          st_as_sf() %>%
          st_transform(crs = 32198) %>%
          st_simplify(preserveTopology = T, dTolerance = 100)

  # -----
  st_write(obj = egsl,
           dsn = "./data/data-basemap/egsl.geojson",
           delete_dsn = TRUE)



  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #                                   CITIES
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  cities <- rbind(
    c("Baie-Comeau", 49.222670, -68.149355+.04, 0, 5000, -2500),
    c("Sept-Îles", 50.212456, -66.381044+.04, 0, 5000, -2500),
    c("Havre-Saint-Pierre", 50.242983, -63.598428+.04, 0, 5000, -2500),
    c("Blanc-Sablon", 51.426883, -57.132590+.04, 0, 5000, -2500),
    c("Gaspé", 48.831567, -64.487300+.04, 0, 5000, -2500),
    c("Caraquet", 47.777730, -64.953491+.04, 0, 5000, -2500),
    c("La Pocatière", 47.36885583905954, -70.02321997363107+.04, 0, 5000, -2500),
    c("La Baie", 48.34102261306717-.03, -70.88195643377449-.05, 1, -5000, 5000),
    c("Tadoussac", 48.1549463212358+.04, -69.7254261992016+.02,.5, -5000, 17500),
    c("Rivière-du-Loup", 47.84514499357216-.01, -69.5537057317311+.07, 0, 5000, -2500),
    c("Rimouski", 48.44414698303665-.03, -68.54511969494227+.05, 0, 5000, -2500),
    c("Baie-Comeau", 49.222495074896855+.02, -68.1603788485369-.04, 1, -5000, 5000),
    c("Matane", 48.847708031386716-.04, -67.52783889874046+.03, 0, 5000, -2500)
  ) %>%
  data.frame() %>%
  setNames(c("city","latitude","longitude","adjX","offX","offY")) %>%
  mutate(latitude = as.numeric(latitude),
         longitude = as.numeric(longitude),
         adjX = as.numeric(adjX),
         offX = as.numeric(offX),
         offY = as.numeric(offY)) %>%
  st_as_sf(coords = c("longitude","latitude"), crs = 4326) %>%
  st_transform(crs = param$crs) %>%
  cbind(st_coordinates(.)) %>%
  st_write(dsn = "./data/data-basemap/cities.geojson", delete_dsn = TRUE)


  # -----
  file.remove("data/data-basemap/egsl.zip")
  file.remove("data/data-basemap/egsl/egsl.dbf")
  file.remove("data/data-basemap/egsl/egsl.prj")
  file.remove("data/data-basemap/egsl/egsl.sbn")
  file.remove("data/data-basemap/egsl/egsl.sbx")
  file.remove("data/data-basemap/egsl/egsl.shp")
  file.remove("data/data-basemap/egsl/egsl.shx")
  file.remove("data/data-basemap/egsl/")
  # _____________________________________________________________________________ #
}


# =================================================================
#' @rdname load
#' @export
basemap <- function(basemap_name) {
  files <- dir('./data/data-basemap', full.names = TRUE)

  # Identify dataset to load
  uid <- stringr::str_detect(files, basemap_name)

  # Identify extensions
  ext <- data.table::last(stringr::str_split(files[uid], "\\.")[[1]])

  # Load according to extension type
  ## ---------------------------------------------
  ## GEOJSON
  if (ext == "geojson") {
    assign(x = basemap_name,
           value = sf::st_read(files[uid], quiet = TRUE),
           envir = globalenv())
  }
  ## ---------------------------------------------
  ## CSV
  if (ext == "csv") {
    dat <- read.csv(files[uid])
    if (colnames(dat)[1] == "X") {
      assign(x = data_name,
             value = read.csv(files[uid], row.names = 1),
             envir = globalenv())
    } else {
      assign(x = data_name,
             value = read.csv(files[uid]),
             envir = globalenv())
    }
  }
}
