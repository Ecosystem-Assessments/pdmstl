# Using INLA SPDE models to extrapolate ph in the gulf ans estuary from ph data from relevés mutidisciplinaires d'août (NGSL) et de septembre 2017 (SGSL).

# Import functions
source("R/inla_models.R")

# Import libraries
## Use raster instead of terra for now
library(INLA)
library(raster)

# Import grid
grd <- raster::raster("data/grid/grid.tif")

# Import area of study
aoi <- sf::st_read("data/aoi/aoi.gpkg", quiet = TRUE) |>
  as(object = _, Class = "Spatial")
aoi_line <- sf::st_read("data/aoi/aoi_line.gpkg", quiet = TRUE) |>
  as(object = _, Class = "Spatial")


# Mesh
mesh <- make_mesh(pedge = 0.03, 
                  aoi_line,
                  resolution = c(60, 47), 
                  convex = -0.03, 
                  concave = -0.2)

# Define SPDE
spde <- INLA::inla.spde2.pcmatern(mesh=mesh,
                                  alpha=2,
                                  prior.range=c(5, 0.01),
                                  prior.sigma=c(1, 0.01))

# Make index matrix
field <- INLA::inla.spde.make.index("field",n.spde=spde$n.spde)

# Setup observations
ph <- readxl::read_xlsx("data/data-raw/ph/DATA_ReleveMulti2027_pH_Fond.xlsx") |>
        as.data.frame()
ph <- sp::SpatialPointsDataFrame(coords = ph[,c("LOND", "LATD")],
                                 data = ph[,"PH", drop = FALSE],
                                 proj4string = sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))

# Covariables (oxygen et temp)
temp <- terra::rast("data/data-abiotic/mean_temperature.tif") |>
          terra::scale()
oxy <- terra::rast("data/data-abiotic/sat.tif") |>
         terra::scale()
nitrate <- terra::rast("data/data-abiotic/bottom_nitrate_mean.tif") |>
             terra::scale()
iron <- terra::rast("data/data-abiotic/bottom_iron_mean.tif") |>
             terra::scale()
covariables <- c(temp, oxy, nitrate, iron, arag)

# Make stack
# For estimation
AEst<-INLA::inla.spde.make.A(mesh, 
                             loc=sp::coordinates(ph))
# For prediction
APred<-INLA::inla.spde.make.A(mesh)

# For estimation
AEstlist <- list(AEst,1)
# For prediction
APredlist <- list(APred,1)

# Estimation
covar_est <- terra::extract(covariables,  
                            terra::vect(ph),
                            ID = FALSE)
covar_pred <- terra::extract(covariables,
                             mesh$loc[,1:2])
effectEst <- list(field,
                  list(covar_est))
# Prediction
effectPred <- list(field,
                   list(covar_pred))

#---------- BUILD THE STACK ----------#
# Stack for estimation
StackEst <- INLA::inla.stack(data=list(ph = ph$PH),
                             A = AEstlist,
                             effects = effectEst,
                             tag="est")
# Stack for prediction
StackPred <- INLA::inla.stack(data=list(ph = NA),
                              A=APredlist,
                              effects=effectPred,
                              tag="pred")
# Organise StackEst and StackPred into a single stack
Stack <- inla.stack(StackEst,StackPred)


# Run model
model <- INLA::inla(ph ~ 0 + bottom_nitrate_mean + I(bottom_nitrate_mean^2) + bottom_iron_mean + mean_temperature + I(mean_temperature^2) + sat + I(sat^2) + f(field, model = spde),
                    data = INLA::inla.stack.data(Stack),
                    family="gaussian",
                    control.family = list(
                      control.link = list(model="identity")
                    ),
                    control.predictor=list(A=INLA::inla.stack.A(Stack),
                                           compute=TRUE,
                                           link = 1),
                    control.compute=list(waic=TRUE,
                                         openmp.strategy = "huge",
  				                               config = TRUE),
                    control.inla=list(int.strategy = "ccd"),
                    verbose = TRUE)

# Define basis of the map
mapBasis <- INLA::inla.mesh.projector(mesh,
                                      dims = c(raster::nrow(grd), 
                                               raster::ncol(grd)),
                                      xlim = c(xmin(aoi), xmax(aoi)),
                                      ylim = c(ymin(aoi), ymax(aoi)))

# Find the mesh edges on which predictions should be made
ID <- INLA::inla.stack.index(Stack, tag="pred")$data

# Calculate prediction
prediction <- INLA::inla.mesh.project(mapBasis, 
                                      model$summary.fitted.values[["mean"]][ID])

# Transform map into a raster
rast_pred <- raster(t(prediction[,ncol(prediction):1]),
                    xmn = min(mapBasis$x), xmx = max(mapBasis$x), 
                    ymn = min(mapBasis$y), ymx = max(mapBasis$y))

map <- terra::crop(terra::rast(rast_pred), 
                   terra::vect(aoi), 
                   mask = TRUE)

# Calculate prediction
prediction <- INLA::inla.mesh.project(mapBasis, 
                                      model$summary.fitted.values[["0.025quant"]][ID])

# Transform map into a raster
rast_pred <- raster(t(prediction[,ncol(prediction):1]),
                    xmn = min(mapBasis$x), xmx = max(mapBasis$x), 
                    ymn = min(mapBasis$y), ymx = max(mapBasis$y))

map025 <- terra::crop(terra::rast(rast_pred), 
                   terra::vect(aoi), 
                   mask = TRUE)


# Calculate prediction
prediction <- INLA::inla.mesh.project(mapBasis, 
                                      model$summary.fitted.values[["0.975quant"]][ID])

# Transform map into a raster
rast_pred <- raster(t(prediction[,ncol(prediction):1]),
                    xmn = min(mapBasis$x), xmx = max(mapBasis$x), 
                    ymn = min(mapBasis$y), ymx = max(mapBasis$y))

map975 <- terra::crop(terra::rast(rast_pred), 
                   terra::vect(aoi), 
                   mask = TRUE)



dev.new(width = 24, height = 8)
par(mfrow=c(1,3))
plot(map025,
     range=c(7.55,8.15))
terra::plot(terra::vect(ph),
            y = "PH",
            add = TRUE,
            pch = 20,
            col = rev(grDevices::terrain.colors(50)),
            legend="bottomright")
plot(map,
     range=c(7.55,8.15))
terra::plot(terra::vect(ph),
            add = TRUE,
            y = "PH",
            pch = 20,
            col = rev(grDevices::terrain.colors(50)),
            legend="bottomright")
plot(map975,
     range=c(7.55,8.15))
terra::plot(terra::vect(ph),
            y = "PH",
            add = TRUE,
            pch = 20,
            col = rev(grDevices::terrain.colors(50)),
            legend="bottomright")
