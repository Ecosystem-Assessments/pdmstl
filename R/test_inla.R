# Script just to test INLA model with a couple of covariables

#---------- SETUP ----------#

# Import libraries
## Use raster instead of terra for now
library(INLA)
library(raster)

# Import grid
grd <- raster::raster("data/grid/grid.tif")

# Import area of study
aoi <- sf::st_read("data/aoi/aoi.gpkg") |>
  as(object = _, Class = "Spatial")

# Import observations for every species
species <- c("balticina_finmarchica",
             "pennatula_aculeata",
             "ptilella_grandis",
             "anthoptilum_grandiflorum")

obs <- lapply(species, function(x) {
  tmp <- sf::st_read(paste0("data/data-biotic/",x,".gpkg"))
  return(as(tmp, "Spatial"))
})

pointcount <- function(obs, rast){
  # make a raster of zeroes like the input
  r = rast
  r[] = 0
  # get the cell index for each point and make a table:
  counts = table(raster::cellFromXY(rast,obs))
  # fill in the raster with the counts from the cell index:
  r[as.numeric(names(counts))] = counts
  return(r)
}

# Use grid and calculate number of presences/absences in every grid
obs_spdf <- lapply(obs, function(x) {
  # Number of trials
  observations <- pointcount(x, grd)
  # Number of success (presence)
  presences <- pointcount(x[x$presence == 1,], grd)
  # Make a stack with both
  obs <- raster::stack(observations, presences)
  names(obs) <- c("observations", "presences")
  # Transform in spdf
  tmp <- as(obs, "SpatialPointsDataFrame")
  tmp <- tmp[tmp$observations > 0,]
  return(tmp)
})
names(obs_spdf) <- species

# Import abiotic data
abiotic <- c("bathy",
             "bottom_temperature_mean",
             "bottom_dissolved_oxygen_mean",
             "bottom_salinity_mean",
             "bottom_phytoplankton_mean",
             "bottom_nitrate_mean",
             "bottom_silicate_mean",
             "bottom_light_mean",
             "slope",
             "bottom_current_velocity_max",
             "arag",
             "bottom_iron_mean")

covariables <- lapply(abiotic, function(x) {
  raster::raster(paste0("data/data-abiotic/",x,".tif")) # Use raster for INLA
})
covariables <- do.call(raster::stack, covariables)

#---------- Building Mesh ----------#

pedge <- as.numeric(0.03)
edge <- min(c(diff(raster::bbox(aoi)[1,])*pedge,
              diff(raster::bbox(aoi)[2,])*pedge))
aoi_line <- as(sf::st_read("data/aoi/aoi_line.gpkg"), "Spatial")
coords <- do.call(rbind,sp::coordinates(aoi_line)[[1]])
domain <- INLA::inla.nonconvex.hull(coords,
                                    concave = -.06,
                                    convex = -.03,
                                    resolution = c(42, 42))
mesh <- INLA::inla.mesh.2d(boundary = domain,
                           max.edge = c(edge, edge*5),
                           min.angle = 21,
                           cutoff = edge/2,
                           offset = c(edge, edge*2),
                           crs = raster::crs(aoi))


#---------- DEFINE STOCHASTIC PARTIAL DIFFERENTIAL EQUATION OBJECT ----------#

spde <- INLA::inla.spde2.pcmatern(mesh=mesh,
                                  alpha=2,
                                  prior.range=c(0.01, 0.01),
                                  prior.sigma=c(1, 0.01))


#---------- MAKE INDEX MATRIX ----------#

field <- INLA::inla.spde.make.index("field",n.spde=mesh$n)


#TODO: loop for species here
sp <- obs_spdf[[1]]
# extract env values for every point
dat <- raster::extract(covariables, 
                       sp::coordinates(sp))
# scale variables
dat <- as.data.frame(scale(dat))
dat[,"temp_sal_mean"] <- dat$bottom_temperature_mean * dat$bottom_salinity_mean

# extract env values for every point
pred <- raster::extract(covariables, 
                        mesh$loc[,1:2])
# scale variables
pred <- as.data.frame(scale(pred))
pred[,"temp_sal_mean"] <- pred$bottom_temperature_mean * pred$bottom_salinity_mean

#---------- MAKE A MATRIX ----------#
# For estimation
AEst<-INLA::inla.spde.make.A(mesh,loc=sp::coordinates(sp))
# For prediction
APred<-INLA::inla.spde.make.A(mesh)


#---------- ORGANISE THE A MATRIX INTO A LIST ----------#
# For estimation
AEstlist <- list(AEst,1)
# For prediction
APredlist <- list(APred,1)


#---------- ORGANISE THE EFFECTS (FIELD + EXPLANATORY VARIABLES) ----------#
# Estimation
effectEst <- list(field,
                  list(
                    oxy = dat[,"bottom_dissolved_oxygen_mean"],
                    bathy = dat[,"bathy"],
                    phyto = dat[,"bottom_phytoplankton_mean"],
                    nitrate = dat[,"bottom_nitrate_mean"],
                    silicate = dat[,"bottom_silicate_mean"],
                    light = dat[,"bottom_light_mean"],
                    slope = dat[,"slope"],
                    max_velocity = dat[,"bottom_current_velocity_max"],
                    aragonite = dat[,"arag"],
                    iron = dat[,"bottom_iron_mean"],
                    temp = dat[,"bottom_temperature_mean"],
                    sal = dat[,"bottom_salinity_mean"])
                  )
# Prediction
effectPred <- list(field,
                  list(
                    oxy = pred[,"bottom_dissolved_oxygen_mean"],
                    bathy = pred[,"bathy"],
                    phyto = pred[,"bottom_phytoplankton_mean"],
                    nitrate = pred[,"bottom_nitrate_mean"],
                    silicate = pred[,"bottom_silicate_mean"],
                    light = pred[,"bottom_light_mean"],
                    slope = pred[,"slope"],
                    max_velocity = pred[,"bottom_current_velocity_max"],
                    aragonite = pred[,"arag"],
                    iron = pred[,"bottom_iron_mean"],
                    temp = pred[,"bottom_temperature_mean"],
                    sal = pred[,"bottom_salinity_mean"])
                   )


#---------- BUILD THE STACK ----------#
# Stack for estimation
StackEst <- INLA::inla.stack(data=list(presences = sp$presences,
                                       observations = sp$observations),
                             A = AEstlist,
                             effects = effectEst,
                             tag="est")
# Stack for prediction
StackPred <- INLA::inla.stack(data=list(presences = NA,
                                        observations = NA),
                              A=APredlist,
                              effects=effectPred,
                              tag="pred")
# Organise StackEst and StackPred into a single stack
Stack <- inla.stack(StackEst,StackPred)


#---------- BUILD THE MODEL ----------#

make_model <- function(equation, Stack) {
  INLA::inla(equation,
             data = INLA::inla.stack.data(Stack),
             family="binomial",
             Ntrials=observations,
             control.family =list(link="logit"),
             control.predictor=list(A=INLA::inla.stack.A(Stack),
                                    compute=TRUE,
                                    link = 1),
             control.compute=list(waic=TRUE,
                                  openmp.strategy = "huge",
    				                      config = TRUE),
             control.inla=list(int.strategy = "ccd"),
             verbose = TRUE) 
}
eq <- list()
eq[[1]] <- presences ~ f(field, model = spde)
eq[[2]] <- presences ~ bathy + f(field, model = spde)
eq[[3]] <- presences ~ bathy + slope + f(field, model = spde)
eq[[4]] <- presences ~ sal + temp + oxy + slope + f(field, model = spde)
eq[[5]] <- presences ~ sal + temp + f(field, model = spde)
eq[[6]] <- presences ~ sal*temp + slope + f(field, model = spde)
eq[[7]] <- presences ~ sal + temp + oxy + slope + max_velocity + aragonite + phyto + f(field, model = spde)

models <- list()
models[[1]] <- make_model(eq1, Stack)
models[[2]] <- make_model(eq2, Stack)
models[[3]] <- make_model(eq3, Stack)
models[[4]] <- make_model(eq4, Stack)
models[[5]] <- make_model(eq5, Stack)
models[[6]] <- make_model(eq6, Stack)
models[[7]] <- make_model(eq7, Stack)

waic <- data.frame(model = NA ,waic = NA)

for(i in 1:length(models)) {
  waic[i, "model"] <- i
  waic[i, "waic"] <- models[[i]]$waic$waic
}

waic <- waic[order(waic$waic),]
best_model <- waic[1,"model"]

#---------- MAKE THE MAPS ----------#

# Define basis of the map
mapBasis <- INLA::inla.mesh.projector(mesh,
                                      dims = c(raster::nrow(grd), 
                                               raster::ncol(grd)),
                                      xlim = c(xmin(aoi), xmax(aoi)),
                                      ylim = c(ymin(aoi), ymax(aoi)),
                                      crs = crs(aoi))

# Find the mesh edges on which predictions should be made
ID <- INLA::inla.stack.index(Stack, tag="pred")$data

# Calculate prediction
mapMean <- INLA::inla.mesh.project(mapBasis, 
                                   models[[best_model]]$summary.fitted.values[["mean"]][ID])
map.025 <- INLA::inla.mesh.project(mapBasis, 
                                   models[[best_model]]$summary.fitted.values[["0.025quant"]][ID])
map.975 <- INLA::inla.mesh.project(mapBasis, 
                                   models[[best_model]]$summary.fitted.values[["0.975quant"]][ID])
  
# Transform map into a raster
rasterMean <- raster(t(mapMean[,ncol(mapMean):1]),
                     xmn = min(mapBasis$x), xmx = max(mapBasis$x), 
                     ymn = min(mapBasis$y), ymx = max(mapBasis$y))

raster.025 <- raster(t(map.025[,ncol(map.025):1]),
                     xmn = min(mapBasis$x), xmx = max(mapBasis$x), 
                     ymn = min(mapBasis$y), ymx = max(mapBasis$y))

raster.975 <- raster(t(map.975[,ncol(map.975):1]),
                     xmn = min(mapBasis$x), xmx = max(mapBasis$x), 
                     ymn = min(mapBasis$y), ymx = max(mapBasis$y))

rasterMean <- terra::crop(terra::rast(rasterMean), 
                          terra::vect(aoi), 
                          mask = TRUE)
raster.025 <- terra::crop(terra::rast(raster.025), 
                          terra::vect(aoi), 
                          mask = TRUE)
raster.975 <- terra::crop(terra::rast(raster.975), 
                          terra::vect(aoi), 
                          mask = TRUE)


# Plot the results
dev.new(height = 7, width = 19)
par(mfrow = c(1,3),
    oma = c(0,0,5,0))
terra::plot(raster.025,
            main = "2.5%",
            range = c(0,1))

plot(sp[sp$presences == 1, "presences"],
     pch = 16, 
     col = "red",
     add = TRUE,
     cex = 0.3)

plot(sp[sp$presences == 0, "presences"],
     pch = 16, 
     col = rgb(0,0,0,0.4),
     add = TRUE,
     cex = 0.2)

terra::plot(rasterMean,
            main = "Mean",
            range = c(0,1))

plot(sp[sp$presences == 1, "presences"],
     pch = 16, 
     col = "red",
     add = TRUE,
     cex = 0.3)

plot(sp[sp$presences == 0, "presences"],
     pch = 16, 
     col = rgb(0,0,0,0.4),
     add = TRUE,
     cex = 0.2)

terra::plot(raster.975,
            main = "97.5%",
            range = c(0,1))

plot(sp[sp$presences == 1, "presences"],
     pch = 16, 
     col = "red",
     add = TRUE,
     cex = 0.3)

plot(sp[sp$presences == 0, "presences"],
     pch = 16, 
     col = rgb(0,0,0,0.4),
     add = TRUE,
     cex = 0.2)

mtext(bquote(.(eq[[best_model]][[3]])),
      side = 3,
      line = 1,
      outer = TRUE,
      cex = 1.5)
