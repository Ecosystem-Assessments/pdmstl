#' Title
#' 
#' Description
#'
#' @param
#'
#' @return

import_obs <- function(species) { 
  lapply(species, function(x) {
    tmp <- sf::st_read(
      sprintf("data/data-biotic/%s.gpkg", x),
      quiet = TRUE
    )
    return(as(tmp, "Spatial"))
  })
}


#' Title
#' 
#' Description
#'
#' @param
#'
#' @return

import_env <- function(path = "data/data-abiotic/") {
  abiotic <- list.files(path)
  covariables <- lapply(abiotic, function(x) {
    raster::raster(sprintf("%s%s",path, x))
  })
  covariables <- do.call(raster::stack, covariables)
  return(covariables)
}


#' Title
#' 
#' Description
#'
#' @param
#'
#' @return

pointcount <- function(obs, grd){
  # make a raster of zeroes like the input
  r = grd
  r[] = 0
  # get the cell index for each point and make a table:
  counts = table(raster::cellFromXY(grd, obs))
  # fill in the raster with the counts from the cell index:
  r[as.numeric(names(counts))] = counts
  return(r)
}


#' Title
#' 
#' Description
#'
#' @param
#'
#' @return

pres_abs <- function(obs, grd, species) {
  
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
  
  return(obs_spdf)

}

#' Title
#' 
#' Description
#'
#' @param
#'
#' @return

make_mesh <- function(pedge, aoi_line, resolution, convex, concave) {

  edge <- min(c(diff(raster::bbox(aoi_line)[1,])*pedge,
                diff(raster::bbox(aoi_line)[2,])*pedge))
  coords <- terra::crds(terra::vect(aoi_line)) # Use terra, sp just gives one set of coordinates
  domain <- INLA::inla.nonconvex.hull(coords,
                                      concave = concave,
                                      convex = convex,
                                      resolution = resolution)
  mesh <- INLA::inla.mesh.2d(boundary = domain,
                             max.edge = c(edge, edge*5),
                             min.angle = 21,
                             cutoff = edge/2,
                             offset = c(edge, edge*2),
                             crs = raster::crs(aoi_line))
  return(mesh)

}


#' Title
#' 
#' Description
#'
#' @param
#'
#' @return

prep_dat <- function(obs, covariables, cols_as_factor = "", ratio, final, mesh=NULL) {

  # Split between training and testing dataset
  if(!final) {
    train <- sample(c(TRUE, FALSE), 
                    nrow(obs), 
                    replace = TRUE, 
                    prob = c(ratio, 1-ratio))
    training <- obs[train,]
    testing <- obs[!train,]
  } else {
    training <- obs
    testing <- sp::SpatialPoints(coords = mesh$loc[,1:2], 
                                 proj4string = mesh$crs)
  }

  # Extract env values for every point
  dat <- raster::extract(covariables, 
                         sp::coordinates(training))
  # Scale variables
  dat <- scale(dat)
#  dat <- cbind(scale(dat[,which(!colnames(dat) %in% cols_as_factor)]),
#               dat[,cols_as_factor, drop = FALSE])
  dat <- cbind(training, dat) |>
    as.data.frame()
  # Change columns to factor
  if (!cols_as_factor %in% "") {
    for(i in cols_as_factor) {
      dat[,i] <- as.factor(dat[,i])
    }
  }

  # Extract env values for every point
  pred <- raster::extract(covariables, 
                          sp::coordinates(testing))
  # Scale variables
  pred <- scale(pred)
  #pred <- cbind(scale(pred[,which(!colnames(pred) %in% cols_as_factor)]),
  #              pred[,cols_as_factor, drop = FALSE])
  if(!final) pred <- cbind(testing, pred)
  pred <- as.data.frame(pred)
  # Change columns to factor
  if (!cols_as_factor %in% "") {
    for(i in cols_as_factor) {
      pred[,i] <- as.factor(pred[,i])
    }
  }

  return(list(train = dat, test = pred))
}


#' Title
#' 
#' Description
#'
#' @param
#'
#' @return

make_stack <- function(mesh, dat, field, covariables, final) {

  #---------- MAKE A MATRIX ----------#
  # For estimation
  AEst<-INLA::inla.spde.make.A(mesh, 
                               loc=as.matrix(dat[["train"]][,c("x", "y")]))
  # For prediction
  if(final) {
    APred<-INLA::inla.spde.make.A(mesh)
  } else {
    APred<-INLA::inla.spde.make.A(mesh,
                                  loc=as.matrix(dat[["test"]][,c("x", "y")]))
  }


  #---------- ORGANISE THE A MATRIX INTO A LIST ----------#
  # For estimation
  AEstlist <- list(AEst,1)
  # For prediction
  APredlist <- list(APred,1)

  #---------- ORGANISE THE EFFECTS (FIELD + EXPLANATORY VARIABLES) ----------#
  # Estimation
  effectEst <- list(field,
                    list(dat[["train"]][,names(covariables)]))
  # Prediction
  effectPred <- list(field,
                     list(dat[["test"]][,names(covariables)]))

  #---------- BUILD THE STACK ----------#
  # Stack for estimation
  presences <- dat[["train"]][,"presences"]
  observations <- dat[["train"]][,"observations"]
  StackEst <- INLA::inla.stack(data=list(presences = presences,
                                         observations = observations),
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

  return(Stack)
}


#' Title
#' 
#' Description
#'
#' @param
#'
#' @return 

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
             control.fixed=list(expand.factor.strategy="inla"),
             verbose = TRUE)
}


#' Title

#' Description
#'
#' @param
#'
#' @return

compare_waic <- function(models, eq) {

  waic <- data.frame(model = NA ,waic = NA)

  for(i in 1:length(models)) {
    waic[i, "model"] <- Reduce(paste0, deparse(eq[[i]]))
    waic[i, "waic"] <- models[[i]]$waic$waic
  }

  waic <- waic[order(waic$waic),]
  return(waic)

}


#' Title
#' 
#' Description
#'
#' @param
#'
#' @return

calc_auc <- function(models, dat, Stack, eq) {

  auc <- sapply(1:length(models), function(x) {
    # Get predictions for "pred" tag to get threshold and transform it to binary with treshold calculated from training dataset
    ID <- INLA::inla.stack.index(Stack, tag="pred")$data
    pred <- models[[x]]$summary.fitted.values[["mean"]][ID]

    # Calculate roc
    dat[["test"]]$presence[dat[["test"]]$presence >= 1] <- 1
    AUC <- pROC::auc(dat[["test"]]$presence, pred)
    return(c(Reduce(paste0, deparse(eq[[x]])), AUC))
  })

  auc <- t(auc) |>
    as.data.frame()
  colnames(auc) <- c("model", "auc")
  auc$auc <- as.numeric(auc$auc)

  return(auc[order(auc$auc, decreasing = TRUE),])
}

#' Title
#' 
#' Description
#'
#' @param
#'
#' @return 

make_maps <- function(model, type, Stack, mesh, grd, aoi, tag = "pred") {

  # Define basis of the map
  mapBasis <- INLA::inla.mesh.projector(mesh,
                                        dims = c(raster::nrow(grd), 
                                                 raster::ncol(grd)),
                                        xlim = c(xmin(aoi), xmax(aoi)),
                                        ylim = c(ymin(aoi), ymax(aoi)))

  # Find the mesh edges on which predictions should be made
  ID <- INLA::inla.stack.index(Stack, tag=tag)$data

  # Calculate prediction
  prediction <- INLA::inla.mesh.project(mapBasis, 
                                        model$summary.fitted.values[[type]][ID])

  # Transform map into a raster
  rast_pred <- raster(t(prediction[,ncol(prediction):1]),
                      xmn = min(mapBasis$x), xmx = max(mapBasis$x), 
                      ymn = min(mapBasis$y), ymx = max(mapBasis$y))

  map <- terra::crop(terra::rast(rast_pred), 
                     terra::vect(aoi), 
                     mask = TRUE)

  return(map)
}
