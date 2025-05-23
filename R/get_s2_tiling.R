#' return S2 tiles intersecting with the footprint of a vector file
#'
#' @param plots list.
#' @param aoi_path character. path for vector file defining aoi
#' @param datetime character or date or list defining date of acquisition or time range
#' @param output_dir character. path for output directory
#' @param cloudcover numeric.
#' @param overwrite boolean.
#' @param siteName character. name of the study site
#' @param path_S2tilinggrid character. path for the Sentinel-2_tiling_grid.kml file
#' @param collection character.
#' @param geomAcq boolean.
#' @param nbCPU numeric.
#' @param mask_path character.
#' @param resolution numeric.
#' @param fraction_vegetation numeric.
#' @param stac_url character.
#' @param doublecheckColl boolean.
#' @param offset numeric.
#' @param offset_B2 boolean.
#' @param corr_BRF boolean.
#' @param crs_target numeric.
#' @param RadiometricFilter list.
#' @param additional_process additional process to be applied to S2_items once downloaded
#' @param original_clouds boolean. should original cloud mask be used or not?
#' @param cellsize numeric. cell size
#' @param pursue_existing boolean. should previously downloaded data be accounted for?
#' @param argsin list. list of arguments for additional_process
#' @param writeoutput boolean. should output file be saved?
#' @param bypassDL boolean. the download of S2 data is not performed
#' @param bands2correct character. name of bands to correct from geometry
#'
#' @return plots list of plots
#' @export

get_s2_tiling <- function(plots = NULL, aoi_path, datetime, output_dir,
                          cloudcover = 99, overwrite = T, siteName = NULL,
                          path_S2tilinggrid = 'Sentinel-2_tiling_grid.kml',
                          collection = 'sentinel-2-l2a', geomAcq = F, nbCPU = 1,
                          mask_path = NULL, resolution = 10,
                          fraction_vegetation = 0, stac_url = NULL, doublecheckColl = T,
                          offset = 1000, offset_B2 = F, corr_BRF = F, crs_target = NULL,
                          RadiometricFilter = NULL, additional_process = NULL,
                          original_clouds = T, cellsize = 10000, pursue_existing = T,
                          argsin = NULL, writeoutput = T, bypassDL = F,
                          bands2correct = c('B8A', 'B11', 'B12')){

  # create proper datetime if only one date provided
  if (inherits(x = datetime, what = c('character', 'Date')))
    datetime <- list('from' = as.Date(datetime), 'to' = as.Date(datetime)+1)

  # define grid of tiles
  path_grid <- NULL
  if (cellsize>11000){
    message('cellsize > 11000 m will not ensure full acquisitions')
    message('reducing to "cellsize = 11000"')
    cellsize <- 11000
  }
  dsn_grid <- file.path(output_dir, paste0('aoi_grid_',cellsize,'.gpkg'))
  if (is.null(plots)){
    message('tiling area of interest')
    path_grid <- get_grid_aoi(aoi_path = aoi_path,
                              cellsize = cellsize,
                              output_dir = output_dir,
                              crs_target = crs_target)
    crs_target <- path_grid$crs_target
    plots <- path_grid$plots
    dsn_grid <- path_grid$dsn_grid
  }

  s2_raster_dir <- file.path(output_dir, 'raster_samples')
  dir.create(path = s2_raster_dir, showWarnings = F, recursive = T)
  if (!bypassDL){
    # check if already existing and list plots to process
    if (is.null(argsin)){
      missing <- get_missing_plots(plots = plots,
                                   pursue_existing = pursue_existing,
                                   datetime = datetime,
                                   output_dir = s2_raster_dir,
                                   pattern = siteName)
    } else {
      missing <- get_missing_plots(plots = plots,
                                   pursue_existing = pursue_existing,
                                   datetime = datetime,
                                   output_dir = argsin$output_path,
                                   pattern = argsin$siteName)
    }

    # define s2 tiles corresponding to aoi
    message('get S2 tiles corresponding to aoi')
    path_S2tilinggrid <- check_S2tilinggrid(path_S2tilinggrid = path_S2tilinggrid)
    S2_grid <- get_s2_tiles(plots = plots, dsn_bbox = aoi_path,
                            output_dir = output_dir, siteName = siteName,
                            path_S2tilinggrid = path_S2tilinggrid,
                            overwrite = overwrite)

    id <- Sys.getenv("PREPROCS2_CDSE_ID")
    pwd <- Sys.getenv("PREPROCS2_CDSE_SECRET")
    if (geomAcq & (nchar(id)==0 | nchar(pwd)==0)){
      message('Please provide authentication for CDSE if you want to get geometry of acquisition')
      message('Activate OAuth clients following this link')
      message('https://shapps.dataspace.copernicus.eu/dashboard/#/account/settings')
    } else if (geomAcq & nchar(id)>0 & nchar(pwd)>0){
      message('get S2 geometry of acquisition of tiles overlapping with aoi')
      # limit to 10 CPU for download using CDSE (errors raised when too many CPUs)
      nbCPU_CDSE <- min(c(8, nbCPU))
      get_GeomAcq_s2(dsn_S2tiles = S2_grid$dsn_S2tiles, datetime = datetime,
                     cloudcover = cloudcover,
                     output_dir = output_dir, overwrite = overwrite, nbCPU = nbCPU_CDSE)
    }
    # download S2 data
    message('download S2 collection')
    S2tiles <- S2_grid$S2tiles
    S2_items <- get_s2collection(plots = plots[missing],
                                 datetime = datetime,
                                 nbCPU = nbCPU,
                                 S2tiles = S2tiles,
                                 output_dir = output_dir,
                                 cloudcover = cloudcover,
                                 mask_path = mask_path,
                                 fraction_vegetation = fraction_vegetation,
                                 resolution = resolution,
                                 collection = collection,
                                 stac_url = stac_url,
                                 overwrite = overwrite,
                                 siteName = siteName,
                                 doublecheckColl = doublecheckColl,
                                 offset = offset,
                                 offset_B2 = offset_B2,
                                 corr_BRF = corr_BRF,
                                 RadiometricFilter = RadiometricFilter,
                                 rast_out = F,
                                 crs_target = crs_target,
                                 additional_process = additional_process,
                                 original_clouds = original_clouds,
                                 argsin = argsin, writeoutput = writeoutput,
                                 bands2correct = bands2correct)
  }
  tilingInfo <- list('plots' = plots,
                     'dsn_grid' = path_grid$dsn_grid,
                     'cellsize' = cellsize)
  return(tilingInfo)
}
