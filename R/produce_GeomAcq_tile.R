#' extracts geometry of acquisition for each plot
#'
#' @param list_aoi path for spectral bands required
#' @param collection_dir_geom collection.
#' @param geom_dir character.
#' @param nbCPU numeric.
#'
#' @return list of collections per plot
#' @importFrom terra rast writeRaster values
#' @importFrom progressr progressor with_progress handlers
#' @importFrom parallel makeCluster stopCluster
#' @importFrom future plan sequential
#' @importFrom future.apply future_lapply
#' @export
#'
produce_GeomAcq_tile <- function(list_aoi, collection_dir_geom, geom_dir, nbCPU){

  # define angles corresponding to gepometry of acquisition
  angle <- c('saa', 'sza', 'vaa', 'vza')
  # get collection corresponding to the full study area
  FileName <- file.path(collection_dir_geom,'collection_CDSE_geometry.rds')
  collection_plot <- readRDS(file = FileName)
  # download geometry of acquisition for full scene (1km resolution)
  script_geom <- system.file("scripts", "S2L2A_geometry.js", package = "CDSE")
  DatesofAcq <- as.list(unique(get_dateAcq(S2product = collection_plot$sourceId)))
  geom_subdir <- file.path(geom_dir,'angles_per_plot')
  dir.create(path = geom_subdir, showWarnings = F, recursive = T)
  nbPlots <- length(list_aoi)
  cl <- parallel::makeCluster(nbCPU)
  plan("cluster", workers = cl)  ## same as plan(multisession, workers = nbCPU)
  handlers(global = TRUE)
  handlers("cli")
  with_progress({
    p <- progressr::progressor(steps = length(DatesofAcq))
    future.apply::future_lapply(X = DatesofAcq,
                                FUN = write_geomplot,
                                geom_dir = geom_dir,
                                geom_subdir = geom_subdir,
                                list_aoi = list_aoi, p = p)


  })
  parallel::stopCluster(cl)
  plan(sequential)
  return(geom_subdir)
}

