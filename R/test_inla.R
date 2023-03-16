# Script just to test INLA model with a couple of covariables

#---------- SETUP ----------#

# Import functions
source("R/inla_models.R")

# Import libraries
## Use raster instead of terra for now
library(INLA)
library(raster)

# Import grid
grd <- raster::raster("data/grid/grid.tif")

# Import area of study
aoi <- sf::st_read("data/aoi/aoi.gpkg") |>
  as(object = _, Class = "Spatial")
aoi_line <- sf::st_read("data/aoi/aoi_line.gpkg") |>
  as(object = _, Class = "Spatial")


# Import observations for every species
species <- c("balticina_finmarchica",
             "pennatula_aculeata",
             "ptilella_grandis",
             "anthoptilum_grandiflorum")
obs <- import_obs(species)
names(obs) <- species

# Use grid and calculate number of presences/absences in every grid
obs_spdf <- pres_abs(obs, grd, species)

# Import all abiotic data
covariables <- import_env()


#---------- Building Mesh ----------#

mesh <- make_mesh(pedge = 0.03, 
                  aoi_line,
                  resolution = c(60, 47), 
                  convex = -0.03, 
                  concave = -0.2)


#---------- DEFINE STOCHASTIC PARTIAL DIFFERENTIAL EQUATION OBJECT ----------#

spde <- INLA::inla.spde2.pcmatern(mesh=mesh,
                                  alpha=2,
                                  prior.range=c(100, 0.01),
                                  prior.sigma=c(1, 0.01))


#---------- MAKE INDEX MATRIX ----------#

field <- INLA::inla.spde.make.index("field",n.spde=spde$n.spde)


#---------- MAKE TRAINING AND TESTING DATASETS ----------#

dat <- prep_dat(obs_spdf[[1]],
                covariables,
                cols_as_factor = "depot_gro", 
                ratio = 0.75,
                final = FALSE)


#---------- MAKE STACK ----------#

Stack <- make_stack(mesh, dat, field, covariables, final = FALSE)


#---------- BUILD THE MODEL ----------#

# List of equations
eq <- list()
eq[[1]] <- presences ~ f(field, model = spde)
eq[[2]] <- presences ~ bathy
eq[[3]] <- presences ~ bathy + f(field, model = spde)
eq[[4]] <- presences ~ 0 + f(field, model = spde)
eq[[5]] <- presences ~ 0 + bathy
eq[[6]] <- presences ~ 0 + bathy + f(field, model = spde)

models <- lapply(eq, function(x) {
  make_model(x, Stack)
})


#---------- COMPARE MODELS WITH WAIC ----------#

waic <- compare_waic(models, eq)


#---------- CALCULATE AUC ----------#

auc <- calc_auc(models, dat, Stack, eq)


#---------- RUN THE FINAL MODEL ----------#

dat <- prep_dat(obs_spdf[[1]],
                covariables,
                cols_as_factor = "depot_gro", 
                ratio = 0.75,
                final = TRUE,
                mesh)

Stack <- make_stack(mesh, dat, field, covariables, final = TRUE)

eq <- presences ~ bathy + f(field, model = spde)

model <- make_model(eq, Stack)


#---------- MAKE THE MAPS ----------#

map_mean <- make_maps(model = model, 
                      type = "mean",
                      Stack, 
                      mesh,
                      grd, 
                      aoi,
                      tag = "pred")


#---------- EXTRACT PRESENCES TO PLOT ON THE MAP ----------#

presences <- obs_spdf[[1]][obs_spdf[[1]]$presences >= 1,] |>
               terra::vect()

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
     cex = 0.4)

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
     cex = 0.4)

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
     cex = 0.4)

plot(sp[sp$presences == 0, "presences"],
     pch = 16, 
     col = rgb(0,0,0,0.4),
     add = TRUE,
     cex = 0.2)

mtext(bquote(.(eq_test[[3]])),
      side = 3,
      line = 1,
      outer = TRUE,
      cex = 1.5)
