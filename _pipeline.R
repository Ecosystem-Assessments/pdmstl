library(devtools)
load_all()

pipeline <- function() {
  # Update global parameters
  global_param()
  
  # Get area of interest 
  get_aoi()
  
  # Make grid 
  # pipedat::pipegrid(x = pipedat:::basemap$can, cellsize = 1, crs = 4326)

  # Integrate data 
  pipedat::pipeflow("./data/data-config/pipedat.yml")
  
  # Prepare data 
  data_prep()
}