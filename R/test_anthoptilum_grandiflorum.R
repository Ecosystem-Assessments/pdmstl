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
aoi <- sf::st_read("data/aoi/aoi.gpkg", quiet = TRUE) |>
  as(object = _, Class = "Spatial")
aoi_line <- sf::st_read("data/aoi/aoi_line.gpkg", quiet = TRUE) |>
  as(object = _, Class = "Spatial")


# Import observations for every species
species <- "anthoptilum_grandiflorum"
obs <- import_obs(species)
names(obs) <- species

# Use grid and calculate number of presences/absences in every grid
obs_spdf <- pres_abs(obs, grd, species)

# Import all abiotic data
covariables <- import_env()



#---------- EXPLORE ABIOTIC VARIABLES ----------#

# Make absences from the data and join train and test data for the pca
dat <- prep_dat(obs_spdf[[1]],
                covariables,
                cols_as_factor = "depot_gro", 
                ratio = 0.75,
                final = FALSE)
dat <- do.call(rbind, dat)
# Makes presences > 1 == 1
dat[dat$presences >= 1,"presences"] <- 1

# Remove depot_gro from data to perform the pca since depot_gro is a factor
# Keep complete rows
tmp_dat <- dat[complete.cases(dat), -which(colnames(dat) %in% c("x", "y","depot_gro", "observations"))]
rownames(tmp_dat) <- NULL
# PCA
pca <- prcomp(x = tmp_dat, scale. = TRUE)

# Plot the pca
ggbiplot::ggbiplot(pca,
                   groups = as.factor(tmp_dat$presences),
                   points.size = 1,
                   ellipse = TRUE,
                   alpha = 0) +
  scale_color_discrete(name = '') +  
    geom_point(aes(colour=as.factor(tmp_dat$presences)), size = 0.01) + 
      xlim(-5.5, 4.5) + 
        ylim(-3, 5)

# From there, we can see that almost every surface variables except surface calcite seems to explain little presences.
# Does Anthoptilum have calcite skeleton?
# Let's remove surface variables since we won't use them

excluded_cols <- c("surface_chlorophyll_mean",
                   "surface_chlorophyll_min",
                   "surface_phytoplankton_mean",
                   "surface_phytoplankton_min",
                   "surface_primary_productivity_mean",
                   "surface_primary_productivity_min")

# On enlève encre depot_gro pour travailler sur la corrélation
dat <- dat[,-which(colnames(dat) %in% c(excluded_cols, "depot_gro"))]

# Certaines variables font forcément être corrélées entre elles.
# Afin d'éviter d'utiliser des covariables trop fortement corrélées, on fait une analyse des facteurs d'inflation de la variance (VIF), sert à identifier les variables les plus corrélées avec les autres.
# Valeur du VIF = 1/(1-R2), on obtient le R2 en faisant une régression d'Une variable sur toutes les autres.
# Commence avec toutes les variables et enlèvent la plus haute VIF stepwise jusqu'à avoir les VIFS sous un threshold (10)

# Première itération
vif <- usdm::vif(dat[,colnames(dat) %in% names(covariables)])
vif # Moyenne de biomasse de phyto est le plus corrélés
excluded_cols <- c(excluded_cols, "bottom_phytoplankton_mean")

# Deuxième itération
dat <- dat[,-which(colnames(dat) %in% c(excluded_cols, "depot_gro"))]
vif <- usdm::vif(dat[,colnames(dat) %in% names(covariables)])
vif # Minimum de silicate au fond de la mer
excluded_cols <- c(excluded_cols, "bottom_silicate_min")

# Troisième itération
dat <- dat[,-which(colnames(dat) %in% c(excluded_cols, "depot_gro"))]
vif <- usdm::vif(dat[,colnames(dat) %in% names(covariables)])
vif # Salinité moyenne au fond de la mer
excluded_cols <- c(excluded_cols, "mean_salinity")

# Quatrième itération
dat <- dat[,-which(colnames(dat) %in% c(excluded_cols, "depot_gro"))]
vif <- usdm::vif(dat[,colnames(dat) %in% names(covariables)])
vif # Plusieurs choix, mais comme on veut des covariables qui vont changer dans le temps, enlevont la bathymétrie
excluded_cols <- c(excluded_cols, "bathy")

# Cinquième itération
dat <- dat[,-which(colnames(dat) %in% c(excluded_cols, "depot_gro"))]
vif <- usdm::vif(dat[,colnames(dat) %in% names(covariables)])
vif # Phosphate au fond de la mer
excluded_cols <- c(excluded_cols, "bottom_phosphate_mean")

# Sixième itération
dat <- dat[,-which(colnames(dat) %in% c(excluded_cols, "depot_gro"))]
vif <- usdm::vif(dat[,colnames(dat) %in% names(covariables)])
vif # OK

# Vérifier la corrélation entre les variables
final_dat <- dat[,which(!colnames(dat) %in% c(excluded_cols, "depot_gro"))]
final_dat <- final_dat[complete.cases(final_dat),]
corr_covar <- cor(final_dat)

# Bottom chlorophyll-a is strongly correlated with nitrate and primary productivity. Let's remove it
excluded_cols <- c(excluded_cols, "bottom_chlorophyll_mean", "depot_gro")



#---------- EXPLORING MODELS ----------#

# Filter covariables
covariables <- covariables[[which(!names(covariables) %in% excluded_cols)]]

# Build mesh
mesh <- make_mesh(pedge = 0.03, 
                  aoi_line,
                  resolution = c(60, 47), 
                  convex = -0.03, 
                  concave = -0.2)

# Define SPDE
spde <- INLA::inla.spde2.pcmatern(mesh=mesh,
                                  alpha=2,
                                  prior.range=c(100, 0.01),
                                  prior.sigma=c(1, 0.01))

# Make index matrix
field <- INLA::inla.spde.make.index("field",n.spde=spde$n.spde)

# Training and testing datasets
dat <- prep_dat(obs_spdf[[1]],
                covariables,
                ratio = 0.75,
                final = FALSE)

# Make stack
Stack <- make_stack(mesh, dat, field, covariables, final = FALSE)

# Make a list of equations to test
eq <- list()
## Full model
eq[[1]] <- as.formula(paste0("presences ~ ", 
                             paste0(names(covariables), collapse = " + "),
                             " + f(field, model = spde)"))

# Run models
models <- lapply(eq, function(x) {
  make_model(x, Stack)
})

# If we look at fixed effects estimates, a couple of posterior a crossing 0,let's try other models by removing one variables at a time
# The estimates of bottom_current_velocity_max also seems weird, let's try a model without it
models[[1]]$summary.fixed[,c("0.025quant", "mean", "0.975quant")]

eq[[2]] <- update(eq[[1]], ~ . - bottom_current_velocity_max)
eq[[3]] <- update(eq[[1]], ~ . - bottom_light_mean)
eq[[4]] <- update(eq[[1]], ~ . - bottom_silicate_mean)
eq[[5]] <- update(eq[[1]], ~ . - mean_temperature)
eq[[6]] <- update(eq[[1]], ~ . - surface_calcite_mean)

models <- lapply(eq, function(x) {
  make_model(x, Stack)
})

# Compare models with WAIC
waic <- compare_waic(models, eq)
# Removing bottom_light_mean gives a better model, let's have a look
models[[3]]$summary.fixed[,c("0.025quant", "mean", "0.975quant")]
# We could try another model by removing either nitrate or surface calcite

eq <- list(eq[[1]], eq[[3]])
eq[[3]] <- update(eq[[2]], ~ . - bottom_nitrate_mean)
eq[[4]] <- update(eq[[2]], ~ . - surface_calcite_mean)

models <- lapply(eq, function(x) {
  make_model(x, Stack)
})

# Compare models with WAIC
waic <- compare_waic(models, eq)
# The one without surface calcite is slightly better
models[[4]]$summary.fixed[,c("0.025quant", "mean", "0.975quant")]
# Nitrate fixed effect estimate is still crossing 0, let's remove it too
eq[[5]] <- update(eq[[4]], ~ . - bottom_nitrate_mean)

models <- lapply(eq, function(x) {
  make_model(x, Stack)
})

# Compare models with WAIC
waic <- compare_waic(models, eq)
# The one without surface calcite is slightly better
models[[5]]$summary.fixed[,c("0.025quant", "mean", "0.975quant")]
# Remove iron
eq[[6]] <- update(eq[[5]], ~ . - bottom_iron_mean)
models[[6]] <- make_model(eq[[6]], Stack)

# Compare models with WAIC
waic <- compare_waic(models, eq)
models[[6]]$summary.fixed[,c("0.025quant", "mean", "0.975quant")]

# Calculate AUC of last model
auc <- calc_auc(models[6], dat, Stack, eq[6])
# Good AUC, now let's try to map it



#---------- FINAL MODEL ----------#

dat <- prep_dat(obs_spdf[[1]],
                covariables,
                final = TRUE,
                mesh = mesh)

Stack <- make_stack(mesh, dat, field, covariables, final = TRUE)

eq <- eq[[6]]

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


# Remove primary productivity

eq <- update(eq, ~ . -bottom_primary_productivity_mean)

model <- make_model(eq, Stack)

#---------- MAKE THE MAPS ----------#

map_mean <- make_maps(model = model, 
                      type = "mean",
                      Stack, 
                      mesh,
                      grd, 
                      aoi,
                      tag = "pred")


eq <- update(eq, ~ . -bottom_current_velocity_max)
model <- make_model(eq, Stack)

map_mean <- make_maps(model = model, 
                      type = "mean",
                      Stack, 
                      mesh,
                      grd, 
                      aoi,
                      tag = "pred")

#---------- binarize it ----------# 
ID <- INLA::inla.stack.index(Stack, tag="est")$data
pred <- model$summary.fitted.values[["mean"]][ID]
pred_pres <- pred[which(obs_spdf[[1]]$presences >= 1)]
pred_abs <- pred[which(obs_spdf[[1]]$presences < 1)]
eval_dismo <- dismo::evaluate(pred_pres, pred_abs)
thresh <- dismo::threshold(eval_dismo)$spec_sens

binary_map <- map_mean
binary_map[binary_map >= thresh] <- 1
binary_map[binary_map < thresh] <- 0

dev.new(heiht = 7, width = 15)
par(mfrow = c(1,2))
plot(map_mean)
points(presences, cex = 0.3)
plot(binary_map)
points(presences, cex = 0.3)
