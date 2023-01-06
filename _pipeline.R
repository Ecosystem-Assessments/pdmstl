library(devtools)
load_all()

pipeline <- function() {
  # Update global parameters
  global_parameters()
  
  # Make grid 
  make_grid(cellsize = 0.01)

  # Integrate data 
  pipedat::pipeflow("./data/config/pipedat.yml")
  
  # Prepare assessment modules 
  make_abiotic()
  # make_biotic()
}