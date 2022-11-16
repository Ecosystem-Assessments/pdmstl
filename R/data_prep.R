#' Script to prepare the data for analysis
#'
#' @export
data_prep <- function() {
  on.exit(sf::sf_use_s2(TRUE), add = TRUE)
  sf::sf_use_s2(FALSE)

  # ================================================================================================
  # Data import and format
  ## Geolocated placenames 
  placenames <- importdat("b5433840")[[1]] |>
                dplyr::select(pnuid = PNuid_NLidu, name = Name_en)
                
  ## First Nations location
  fn <- importdat("ce594316")
  
  ## Inuit communities location
  ic <- importdat("621e9a76")
  
  ## Covid data
  covid <- importdat("a56e753b")
  hr <- covid[["covid_timeline_canada-a56e753b-health_regions_wgs84.geojson"]]
  
  ## Proximity measures data 
  px <- importdat("ee7295d7")
  px <- px[["proximity_measures_database-ee7295d7-polygons.geojson"]] |>
        dplyr::select(
          -Shape_Length, -Shape_Area, -prov_name, -cd_name, -ccs_name, 
          -cs_name, -er_name, -fed_name, -cma_name, -db_pop
        )

  ## Statistics Canada census subdivisions
  # sc2016 <- importdat("000fd656")[[1]]
  sc2021 <- importdat("5e4be996")[[1]]

  # ## Census water treatment
  # water <- importdat("929a1773")[[1]]
  # water2 <- dplyr::filter(
  #   water, 
  # 
  # )

  ## Census 2021 housing suitability
  hs <- importdat("852db1a3")[[1]] |>
  hs <- dplyr::filter(
    hs,
    Housing.suitability..3. != "Total - Housing suitability" &
    Number.of.persons.per.room..3. == "Total - Number of persons per room" &
    Statistics..3C. == "Number of private households" &
    Tenure..4. == "Total - Tenure" &
    Number.of.rooms.and.number.of.bedrooms..12. == "Total - Number of rooms and number of bedrooms"
  ) |>
  dplyr::select(
    DGUID,
    housing_suitability = Housing.suitability..3.,
    household_size = Household.size..8..Total...Household.size.1.,
    average_household_size = Household.size..8..Average.household.size.8.
  ) |>
  tidyr::pivot_wider (
    id_cols = DGUID,
    names_from = housing_suitability,
    values_from = c(household_size, average_household_size)
  ) |>
  dplyr::rename(
    household_size_suitable = household_size_Suitable,
    household_size_not_suitable = "household_size_Not suitable",
    average_household_size_suitable = average_household_size_Suitable,
    average_household_size_not_suitable = "average_household_size_Not suitable"
  )

  ## Census 2021 dwelling condition
  dc <- importdat("b48b01d6")[[1]]
  dc <- dplyr::filter(
    dc,
    Period.of.construction..13. == "Total - Period of construction" &
    Structural.type.of.dwelling..10. == "Total - Structural type of dwelling" &
    Statistics..3C. == "Number of private households" &
    Dwelling.condition..4. != "Total - Dwelling condition"    
  ) |>
  dplyr::select(
    DGUID,
    dwelling_condition = Dwelling.condition..4.,
    tenure = Tenure..4..Total...Tenure.1.
  ) |>
  tidyr::pivot_wider(
    id_cols = DGUID,
    names_from = dwelling_condition,
    values_from = tenure
  ) |>
  dplyr::rename(
    regular_maintenance_needed = "Regular maintenance needed",
    minor_repairs_needed = "Minor repairs are needed",
    major_repairs_needed = "Major repairs needed"
  )
  
  ## Census 2021 acceptable housing
  ah <- importdat("f4abec86")[[1]]
  ah <- dplyr::filter(
    ah,
    Residence.on.or.off.reserve..3. != "Total - Residence on or off reserve" &
    Core.housing.need..5. == "Total - Core housing need" &
    Household.type.including.census.family.structure..16. == 
      "Total - Household type including census family structure" &
    Statistics..3C. == "Number of private households" &
    Acceptable.housing..9. != "Total - Acceptable housing"
  ) |>
  dplyr::select(
    DGUID,
    reserve = Residence.on.or.off.reserve..3.,
    acceptable_housing = Acceptable.housing..9.,
    tenure = Tenure..4..Total...Tenure.1.
  ) |>
  dplyr::mutate(
    reserve = tolower(stringr::str_replace(reserve," ","_")),
    acceptable_housing = tolower(stringr::str_replace(acceptable_housing," ","_"))
  ) |>
  tidyr::pivot_wider(
    id_cols = DGUID,
    names_from = c(reserve, acceptable_housing),
    values_from = tenure
  ) 
  
  ## National Pollutant Release Inventory
  npri <- importdat("d2f44fdf")[[1]] 
  npri <- dplyr::filter(
    npri,
    SectorDescriptionEn %in% c("Waste Treatment and Disposal", "Water and Wastewater Systems")
  ) |>
  dplyr::select(
    NpriID,
    SectorDescriptionEn    
  ) |>
  unique()
  
  ### Waste treatment and disposal vs Water and Wastewater Systems
  wtd <- dplyr::filter(npri, SectorDescriptionEn == "Waste Treatment and Disposal")
  wws <- dplyr::filter(npri, SectorDescriptionEn == "Water and Wastewater Systems")
  # ------------------------------------------------------------------------------------------------
  
  
  # ================================================================================================
  # Unique ids of features to geolocated placenames 
  ## Covid health regions: hruid
  # NOTE: using sf::st_nearest_feature because the coastline in the covid dataset is too coarse
  uid <- sf::st_nearest_feature(placenames, hr)
  placenames$hruid <- hr$hruid[uid]
  
  ## Statistics Canada census subdivisions 2016
  uid <- sf::st_nearest_feature(
    x = sf::st_transform(placenames, crs = sf::st_crs(sc2016)),
    y = sc2016
  )
  placenames$dguid_2016 <- sc2016$DGUID[uid]
  rm(sc2016) # Big dataset, remove to save memory

  ## Statistics Canada census subdivisions 2021
  uid <- sf::st_nearest_feature(
    x = sf::st_transform(placenames, crs = sf::st_crs(sc2021)),
    y = sc2021
  )
  placenames$dguid_2021 <- sc2021$DGUID[uid]
  rm(sc2021) # Big dataset, remove to save memory
  
  ## Proximity measures data 
  uid <- sf::st_nearest_feature(
    x = sf::st_transform(placenames, crs = sf::st_crs(px)),
    y = px
  )
  placenames$pxuid <- px$db_uid[uid]
  # ------------------------------------------------------------------------------------------------
  
    
  # ================================================================================================
  # Join data to placenames 
  ## Proximity dataset
  placenames <- dplyr::left_join(
    placenames, 
    sf::st_drop_geometry(px),
    by = c("pxuid" = "db_uid")
  )
  
  ## Housing suitability 
  placenames <- dplyr::left_join(placenames, hs, by = c("dguid_2021" = "DGUID_2021"))
  
  ## Dwelling condition
  placenames <- dplyr::left_join(placenames, dc, by = c("dguid_2021" = "DGUID_2021"))
  
  ## Acceptable housing
  placenames <- dplyr::left_join(placenames, ah, by = c("dguid_2021" = "DGUID_2021"))
  # ------------------------------------------------------------------------------------------------
  
  # ================================================================================================
  # Calculate distances 
  # WARNING: max_dist is completely arbitrary
  dist_calc <- function(pts, max_dist = 250) {
    dat <- sf::st_distance(placenames, wtd) |>
           units::set_units("km") |>
           apply(MARGIN = 1, FUN = min)
    # Cap to max dist
    dat <- ifelse(dat > max_dist, max_dist, dat) 
    
    # Transform as an index between 0 and 1, with 1 being the farthest, and 0 the closest
    dat <- dat/max_dist
  }
  
  ## Waste treatment and disposal vs Water and Wastewater Systems
  placenames$waste_treatment_disposal <- dist_calc(wtd)
  placenames$water_waster_systems <- dist_calc(wws)
  # ------------------------------------------------------------------------------------------------
  
}
