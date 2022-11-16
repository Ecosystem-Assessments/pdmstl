# #' Script to prepare the data for analysis
# #'
# #' @export
# data_prep <- function() {
#   # ================================================================================================
#   # Covid data and health regions
#   covid <- importdat("a56e753b")
#   # hr <- importdat("c71da4d7") # not working...
#   hr <- sf::st_read("data/data-raw/health_region_cartographic_boundary-c71da4d7/raw/HR_000b18a_e/HR_000b18a_e.shp")
# 
#   # index of id correspondance between HR_UID from stats can data to covid timeline data (there are slight differences)
#   ## WARNING: ugly code up ahead, manual entries needed for this.
#   ## WARNING: some BC health regions are aggregated in the covid data 
#   ## WARNING: some MB health regions are not the same regions in the covid data, but it may be due to polygon simplification process, to check
#   ## !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#     hruid <- data.frame(
#       hruid_statscan = sort(unique(as.numeric(hr$HR_UID))),
#       hruid_covid = NA
#     ) 
#     uid <- hruid$hruid_statscan %in% covid[[3]]$hruid
#     hruid$hruid_covid[uid] <- hruid$hruid_statscan[uid]
# 
#     # Function to manually modify data frame
#     moddf <- function(from, to, hruid) hruid$hruid_statscan[hruid$hruid_covid == from] <- to
# 
#     hruid <- moddf(3554, )
#     hruid <- moddf(4701, )
#     hruid <- moddf(4702, )
#     hruid <- moddf(4703, )
#     hruid <- moddf(4704, )
#     hruid <- moddf(4705, )
#     hruid <- moddf(4706, )
#     hruid <- moddf(4707, )
#     hruid <- moddf(4708, )
#     hruid <- moddf(4709, )
#     hruid <- moddf(4710, )
#     hruid <- moddf(5911, 591)
#     hruid <- moddf(5912, 591)
#     hruid <- moddf(5913, 591)
#     hruid <- moddf(5914, 591)
#     hruid <- moddf(5921, 592) 
#     hruid <- moddf(5922, 592)
#     hruid <- moddf(5923, 592)
#     hruid <- moddf(5931, 592) # fucked up, spatially with 592
#     hruid <- moddf(5932, 592) # fucked up, spatially with 592 
#     hruid <- moddf(5933, 593)
#     hruid <- moddf(5941, 594)
#     hruid <- moddf(5942, 594)
#     hruid <- moddf(5943, 594)
#     hruid <- moddf(5951, 595)
#     hruid <- moddf(5952, 595)
#     hruid <- moddf(5953, 595)
#   ## !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# 
# 
# 
#            dplyr::left_join(hr$HR_UID, by = c("hruid" = "HR_UID"))
# 
#   # Identify wrong ids between covid data and stats can health regions
#   uid <- covid[[3]]$hruid %in% hr$HR_UID
#   wrguid <- covid[[3]][!uid, ]
#   df <- data.frame(
#     from = c(
# 
#     )
#     to = c(
# 
#     )
#   )
# 
#   # ================================================================================================
# 
# }