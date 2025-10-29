#' download from CDSE geometry of acquisition for aoi
#'
#' @param aoi simple feature. vector including all S2 tiles intersecting with aoi
#' @param geom_dir character. directory where rasters for geometry are saved
#' @param nbCPU numeric. number of CPUs
#' @param collection_GeomAcq_S2 list. STAC collection
#'
#' @return list of collections per plot
#' @importFrom CDSE GetOAuthToken GetImage
#' @importFrom sf st_bbox
#' @importFrom terra writeRaster
#' @importFrom future plan sequential
#' @importFrom future.apply future_mapply
#' @importFrom parallel makeCluster stopCluster
#' @importFrom progressr with_progress progressor handlers
#' @export

download_s2_geom_acq <- function(aoi, geom_dir, collection_GeomAcq_S2, nbCPU=1){

  collection <- 'sentinel-2-l2a'
  # get token for authentication on CDSE
  OAuth_client <- get_OAuth_client()
  id <- OAuth_client$id
  pwd <- OAuth_client$pwd
  # id <- Sys.getenv("PREPROCS2_CDSE_ID")
  # pwd <- Sys.getenv("PREPROCS2_CDSE_SECRET")
  if (nchar(id)==0 | nchar(pwd)==0){
    fileNameAll <- NULL
    message('please follow install procedure for preprocS2 and provide OAuth')
    message('https://jbferet.gitlab.io/preprocs2/')
    message('no authentication possible to CDSE')
  } else {
    OAuthToken <- CDSE::GetOAuthToken(id = id,
                                      secret = pwd)
    # define angles corresponding to gepometry of acquisition
    angle <- c('saa', 'sza', 'vaa', 'vza')

    # download geometry of acquisition for full scene (1km resolution)
    script_geom <- system.file("extdata", "S2L2A_geometry.js", package = "preprocS2")

    DatesofAcq <- unique(get_dateAcq(S2product = collection_GeomAcq_S2$sourceId,
                                     stac_info = list('provider' = 'esa')))
    fileName <- fileNameAll <- list()
    if (nbCPU==1){
      for (i in seq_len(length(DatesofAcq))){
        for (ang in angle)
          fileName[[ang]] <- file.path(geom_dir,
                                       paste0(ang, '_', DatesofAcq[i], '.tiff'))
        fileNameAll[[as.character(DatesofAcq[i])]] <- fileName
        # check if angles already exist
        if (FALSE %in% file.exists(unlist(fileName))){
          if (Sys.time() >= attr(x = OAuthToken, which = 'expires')-30)
            OAuthToken <- CDSE::GetOAuthToken(id = id,
                                              secret = pwd)
          ras <- CDSE::GetImage(aoi = aoi, time_range = DatesofAcq[i],
                                script = script_geom, resolution = 1000,
                                collection = collection, format = "image/tiff",
                                mosaicking_order = "leastCC", token = OAuthToken)
          # mapply(FUN = terra::writeRaster, x = ras, filename = fileName)
          for (j in 1:4)
            terra::writeRaster(x = ras[[j]], filename = fileName[[j]])
        }
      }
    } else if (nbCPU>1){
      # check for existing files
      DatesofAcq2 <- DatesofAcq
      for (d in as.Date(DatesofAcq2)){
        dd <- as.character(as.Date(d))
        for (ang in angle)
          fileName[[ang]] <- file.path(geom_dir,
                                       paste0(ang, '_', dd, '.tiff'))
        if (! FALSE %in% file.exists(unlist(fileName)))
          DatesofAcq <- DatesofAcq[-which(DatesofAcq %in% dd)]
      }
      # define list of output names
      # fileName_geom <- list()
      i <- 0
      for (d in as.Date(DatesofAcq)){
        dd <- as.character(as.Date(d))
        i <- i +1
        for (ang in angle)
          fileName[[ang]] <- file.path(geom_dir,
                                       paste0(ang, '_', DatesofAcq[i], '.tiff'))
        fileNameAll[[as.character(DatesofAcq[i])]] <- fileName
      }
      if (length(DatesofAcq)>0){
        # check token
        if (Sys.time() >= attr(x = OAuthToken, which = 'expires')-60)
          OAuthToken <- CDSE::GetOAuthToken(id = id,
                                            secret = pwd)
        bbox <- sf::st_bbox(aoi)
        cl <- parallel::makeCluster(nbCPU)
        plan("cluster", workers = cl)
        handlers(global = TRUE)
        handlers("cli")
        with_progress({
          p <- progressr::progressor(steps = length(DatesofAcq))
          s2_file <- future.apply::future_mapply(FUN = get_images_parallel,
                                                 fileName = fileNameAll,
                                                 # fileName = fileName_geom,
                                                 time_range = DatesofAcq,
                                                 MoreArgs = list(bbox = bbox,
                                                                 collection = collection,
                                                                 script = script_geom,
                                                                 format = "image/tiff",
                                                                 mosaicking_order = "leastCC",
                                                                 resolution = 1000, buffer = 0,
                                                                 mask = F, token = OAuthToken,
                                                                 p = p),
                                                 future.seed = T, SIMPLIFY = F)
        })
        parallel::stopCluster(cl)
        plan(sequential)
      }
    }
  }
  return(fileNameAll)
}
