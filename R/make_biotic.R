#' Prepare biotic data 
#'
#' @export

make_biotic <- function() {
  library(stars)
  out <- here::here("data","data-biotic")
  chk_create(out)
  
  # Biotic data
  south <- vroom::vroom("data/data-raw/groundfish_survey_4t_dfo-5b61d05c/groundfish_survey_4t_dfo-5b61d05c.csv")
  north <- vroom::vroom("data/data-raw/groundfish_survey_teleost_dfo-d6b6f3fa/groundfish_survey_teleost_dfo-d6b6f3fa.csv")
  

  #---------- Standardize south ----------#

  # Change colnames
  cols_south <- data.frame(year__annee = "year",
                           month__mois = "month",
                           day__jour = "day",
                           start_hour__heure_de_depart = "start_hour",
                           start_minute__minute_de_depart = "start_minute",
                           latitude = "lat",
                           longitude = "lon",
                           gear__equipement = "gear",
                           species__espece = "species_code",
                           french_name__nom_francais = "fr_name",
                           english_name__nom_anglais = "en_name",
                           latin_name__nom_latin = "scientific_name",
                           weight_caught__poids_pris = "weight_caught",
                           number_caught__quantite__attrape = "number_caught")
  colnames(south)[colnames(south) %in% colnames(cols_south)] <- cols_south[1,]

  # make 'start_date' and 'start_time' column
  south[,"start_date"] <- as.Date(paste0(south$year,"-",
                                         south$month, "-",
                                         south$day),
                                  format = "%Y-%m-%d")
  south[,"start_time"] <- paste0(south$start_hour,":", 
                                 south$start_minute,":00")
  
  # Remove unnecessary column
  # Remove french name and english name columns because it contains a lot of unknown character (e.g. french name with accent)
  cols_rm <- c("start_hour", 
               "start_minute", 
               "year", 
               "month", 
               "day", 
               "fr_name", 
               "en_name")
  south <- south[,-which(colnames(south) %in% cols_rm)]

  # Correct small error with Rossia megaptera scientific_name (unknown character at the end of the name). We don't use this species for know but it needs to be clean for the code to run
  south[grep('Rossia megaptera', south$scientific_name),"scientific_name"] <- 'Rossia megaptera'
  # Make everything in lower case and replace " " with "_"
  south[,"gear"] <- gsub(" ", "_", tolower(south$gear))
  south[,"scientific_name"] <- gsub(" ", "_", tolower(south$scientific_name))


  #---------- Standardize north ----------#

  # Change format of start_time to 'HH:MM:SS'
  north$start_time <- paste0(hms::hms(north$start_time))

  # Get mean latitude and longitude to fit lat and lon of south
  north[,"lat"] <- rowMeans(north[,c("start_latitude", "end_latitude")])
  north[,"lon"] <- rowMeans(north[,c("start_longitude", "end_longitude")])
  
  # Remove unnecessary column
  # Remove french name and english name columns because it contains a lot of unknown character (e.g. french name with accent)
  cols_rm <- c("survey_no", 
               "ship_name", 
               "set_no", 
               "end_date", 
               "end_time", 
               "start_latitude", 
               "end_latitude",
               "start_longitude", 
               "end_longitude",
               "type_hre",
               "ship_speed",
               "nafo_division",
               "result_oper",
               "depth_min",
               "depth_max",
               "common_name_spe_f",
               "common_name_spe_a")
  north <- north[,-which(colnames(north) %in% cols_rm)]

  # Make everything in lower case and replace " " with "_"
  north[,"gear"] <- gsub(" ", "_", tolower(north$gear))
  north[,"scientific_name"] <- gsub(" ", "_", tolower(north$scientific_name))

  # Change capture_weight column name
  colnames(north)[colnames(north) == "capture_weight"] <- "weight_caught"


  #---------- Bind both dataframe ----------#

  # Bind both dataframe
  dat <- dplyr::bind_rows(north, south)

  # Standardize start_time with the same format
  dat$start_time <- format(as.POSIXct(gsub(" ", "", dat$start_time),
                                      format = "%H:%M:%S"), 
                           format = "%H:%M:%S")


  #---------- make absences for every species ----------#

  # Changes synonyms for accepted taxa name
  syn <- data.frame(syn = c("halipteris_finmarchica",
                            "pennatula_grandis",
                            "pennatulacea"),
                    acc = c("balticina_finmarchica",
                            "ptilella_grandis",
                            "pennatuloidea"))
  for(i in 1:nrow(syn)) {
    dat[dat$scientific_name %in% syn$syn[i], "scientific_name"] <- syn$acc[i]
  }

  # Select only target species
  species <- c("balticina_finmarchica",
               "pennatula_aculeata",
               "ptilella_grandis",
               "anthoptilum_grandiflorum")
  genus <- "pennatuloidea"

  sampling <- unique(dat[,c("start_date", "start_time", "lat", "lon")])
  sampling[,"id"] <- paste(1:nrow(sampling))
  dat <- dplyr::left_join(dat, sampling, by = c("start_date", "start_time", "lat", "lon"))

  cols_to_zero <- c("weight_caught", "number_caught")
  cols_to_na <- "species_code" #multiple code for a single species...

  for(i in unique(dat$id)) {
    cat("\r", paste0("Sampling point number ", i, " of ",length(unique(dat$id))))
    absences <- !species %in% dat[dat$id == i, "scientific_name"]
    genus_pr <- genus %in% dat[dat$id == i, "scientific_name"]
    if(any(absences) & !genus_pr) {
      line_to_add <- dat[dat$id == i, ][1,]
      for(j in species[absences]) {
        add_line <- line_to_add
        add_line[,cols_to_zero] <- 0
        add_line[,cols_to_na] <- NA
        add_line[,"scientific_name"] <- j
        dat <- rbind(dat, add_line)
      }
    }
  }

  #---------- Save one sf object for each species ----------#

  dat_sp <- lapply(species, function(x) {
    
    # Filter for each species
    dat_tmp <- dat[dat$scientific_name %in% x, ]
    absence_rows <- sapply(1:nrow(dat_tmp), function(x) {
      all(dat_tmp[x,cols_to_zero] == 0)
    })
    dat_tmp[absence_rows,"presence"] <- 0
    dat_tmp[is.na(dat_tmp$presence), "presence"] <- 1
    # Make dat a spatial object 
    dat_tmp <- sf::st_as_sf(
      dat_tmp,
      coords = c("lon","lat"),
      crs = 4326
    )
  
    # Save object in data-biotic
    sf::st_write(dat_tmp, paste0(out,"/",x,".gpkg"), append = FALSE)
  
  })
