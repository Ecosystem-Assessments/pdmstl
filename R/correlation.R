#' Title
#' 
#' Description
#'
#' @param
#'
#' @return

env_occ_corr <- function(species) {

  # Species occurrences
  sp_occ <- terra::vect(sprintf("data/data-biotic/%s.gpkg", species))

  # Import all environmental variables
  env_files <- list.files("data/data-abiotic/")
  env <- lapply(env_files, function(x) {
    terra::rast(sprintf("data/data-abiotic/%s", x))
  })
  env <- terra::rast(env)

  # Extract values of nv for every points
  vals_extract <- terra::extract(env, 
                                 sp_occ,
                                 ID = FALSE)
  
  # Bind values extracted with presence/absence data
  dat <- cbind(sp_occ[,"presence"], vals_extract)
  dat <- dat[,-c(16)] # Temporary
  
  # Calculate correlation between presence and every environmental variable
  correlations <- sapply(names(dat[,2:ncol(dat)]), function(x) {
    tmp <- as.data.frame(dat[,c("presence",x)])
    tmp <- tmp[complete.cases(tmp),]
    cor(tmp[,1],
        tmp[,2],
        method = "pearson")
  })

  return(sort(correlations))
}

dat <- as.data.frame(dat)
dat <- dat[complete.cases(dat),]
coco <- prcomp(dat[,-1], scale = TRUE)
g <- ggbiplot(coco,
              obs.scale = 1,
              var.scale = 1,
              groups = dat[,"presence"],
              ellipse = TRUE,
              circle = TRUE,
              ellipse.prob = 0.68)

env <- rownames(coco$rotation[coco$rotation[,1]*coco$rotation[,2] < 0,])
vif(dat[,env])
env <- env[-which(env == "")];vif(dat[,env])