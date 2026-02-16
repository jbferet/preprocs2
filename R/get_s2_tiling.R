#' return S2 tiles intersecting with the footprint of a vector file
#'
#' @param plots list.
#' @param aoi_path character. path for vector file defining aoi
#' @param datetime character or date or list defining date of acquisition or time range
#' @param output_dir character. path for output directory
#' @param cloudcover numeric.
#' @param overwrite boolean.
#' @param site_name character. name of the study site
#' @param path_S2_tiling_grid character. path for the Sentinel-2_tiling_grid.kml file
#' @param stac_info list. stac provider, name of collection url for stac
#' @param geom_acq boolean.
#' @param nbCPU numeric.
#' @param mask_path character.
#' @param resolution numeric.
#' @param resampling character. resampling method for terra::resample
#' @param fraction_vegetation numeric.
#' @param overwrite_collection boolean.
#' @param offset numeric.
#' @param offset_B2 boolean.
#' @param corr_BRF boolean.
#' @param crs_target numeric.
#' @param radiometric_filter list.
#' @param additional_process additional process to be applied to S2_items once downloaded
#' @param original_clouds boolean. should original cloud mask be used or not?
#' @param cellsize numeric. cell size
#' @param resume boolean. should previously downloaded data be accounted for?
#' @param argsin list. list of arguments for additional_process
#' @param writeoutput boolean. should output file be saved?
#' @param bypassDL boolean. the download of S2 data is not performed
#' @param bands_to_correct character. name of bands to correct from geometry
#'
#' @return plots list of plots
#' @importFrom parallel detectCores
#' @export

get_s2_tiling <- function(plots = NULL, aoi_path, datetime, output_dir,
                          cloudcover = 99, overwrite = TRUE, site_name = NULL,
                          path_S2_tiling_grid = 'Sentinel-2_tiling_grid.kml',
                          stac_info, geom_acq = FALSE, nbCPU = 1,
                          mask_path = NULL, resolution = 10, resampling = 'near',
                          fraction_vegetation = 0, overwrite_collection = TRUE,
                          offset = 1000, offset_B2 = FALSE, corr_BRF = FALSE,
                          crs_target = NULL, radiometric_filter = NULL,
                          additional_process = NULL, original_clouds = TRUE,
                          cellsize = 10000, resume = TRUE,
                          argsin = NULL, writeoutput = TRUE, bypassDL = FALSE,
                          bands_to_correct = c('B8A', 'B11', 'B12')){


  nbCPU_max <- parallel::detectCores(all.tests = FALSE, logical = FALSE)
  if (nbCPU_max<nbCPU)
    nbCPU <- nbCPU_max

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
  dir.create(path = s2_raster_dir, showWarnings = FALSE, recursive = TRUE)
  if (!bypassDL){
    # check if already existing and list plots to process
    if (is.null(argsin)){
      missing <- get_missing_plots(plots = plots,
                                   resume = resume,
                                   datetime = datetime,
                                   output_dir = s2_raster_dir,
                                   pattern = site_name)
    } else {
      missing <- get_missing_plots(plots = plots,
                                   resume = resume,
                                   datetime = datetime,
                                   output_dir = argsin$output_path,
                                   pattern = argsin$site_name)
    }

    # define s2 tiles corresponding to aoi
    message('get S2 tiles corresponding to aoi')
    path_S2_tiling_grid <- check_s2_tiling_grid(path_S2_tiling_grid = path_S2_tiling_grid)
    S2_grid <- get_s2_tiles(plots = plots, dsn_bbox = aoi_path,
                            output_dir = output_dir, site_name = site_name,
                            path_S2_tiling_grid = path_S2_tiling_grid,
                            overwrite = overwrite)

    # get token for authentication on CDSE
    OAuth_client <- get_OAuth_client()
    id <- OAuth_client$id
    pwd <- OAuth_client$pwd
    # id <- Sys.getenv("PREPROCS2_CDSE_ID")
    # pwd <- Sys.getenv("PREPROCS2_CDSE_SECRET")

    if (geom_acq & (nchar(id)==0 | nchar(pwd)==0)){
      message('Please provide authentication for CDSE if you want to get geometry of acquisition')
      message('Activate OAuth clients following this link')
      message('https://shapps.dataspace.copernicus.eu/dashboard/#/account/settings')
    } else if (geom_acq & nchar(id)>0 & nchar(pwd)>0){
      message('get S2 geometry of acquisition of tiles overlapping with aoi')
      # limit to 10 CPU for download using CDSE (errors raised when too many CPUs)
      nbCPU_CDSE <- min(c(8, nbCPU))
      get_s2_geom_acq(dsn_s2_tiles = S2_grid$dsn_s2_tiles, datetime = datetime,
                     cloudcover = cloudcover,
                     output_dir = output_dir, overwrite = overwrite,
                     nbCPU = nbCPU_CDSE)
    }
    # download S2 data
    message('download S2 collection')
    stac_info <- get_stac_info(stac_info)
    s2_tiles <- S2_grid$s2_tiles
    rast_out <- FALSE
    S2_items <- get_s2_collection(plots = plots[missing],
                                  datetime = datetime,
                                  nbCPU = nbCPU,
                                  s2_tiles = s2_tiles,
                                  output_dir = output_dir,
                                  cloudcover = cloudcover,
                                  mask_path = mask_path,
                                  fraction_vegetation = fraction_vegetation,
                                  resolution = resolution,
                                  stac_info = stac_info,
                                  overwrite = overwrite,
                                  site_name = site_name,
                                  overwrite_collection = overwrite_collection,
                                  offset = offset,
                                  offset_B2 = offset_B2,
                                  corr_BRF = corr_BRF,
                                  radiometric_filter = radiometric_filter,
                                  rast_out = rast_out,
                                  crs_target = crs_target,
                                  resampling = resampling,
                                  additional_process = additional_process,
                                  original_clouds = original_clouds,
                                  argsin = argsin, writeoutput = writeoutput,
                                  bands_to_correct = bands_to_correct)
  }
  # produce a vrt if acquisition over a unique day
  if (datetime$from == (datetime$to-1)){
    if (writeoutput){
      message('writing VRT for reflectance data')
      listfiles <- list.files(s2_raster_dir,
                              pattern = paste0(datetime$from, '.tiff'),
                              full.names = TRUE)
      output_vrt_path <- file.path(output_dir,
                                   paste0(site_name, '_',
                                          datetime$from,'_mosaic.vrt'))
      message(output_vrt_path)
      v <- terra::vrt(x = listfiles, filename = output_vrt_path, overwrite = T)
    }
    if (!is.null(argsin$output_path)){
      if (dir.exists(argsin$output_path)){
        if (!is.null(argsin$SI_list)){
          message('writing VRT for spectral indices')
          for (si in argsin$SI_list){
            listfiles <- list.files(argsin$output_path,
                                    pattern = paste0('^', site_name, '_',
                                                     "\\d+", '_' , si,'.tiff'),
                                    full.names = TRUE)
            output_vrt_path <- file.path(output_dir, paste0(site_name, '_', si, '_', datetime$from,'_mosaic.vrt'))
            v <- terra::vrt(x = listfiles, filename = output_vrt_path, overwrite = T)
          }
        }
      }
    }
  }

  tilingInfo <- list('plots' = plots,
                     'dsn_grid' = path_grid$dsn_grid,
                     'cellsize' = cellsize)
  return(tilingInfo)
}
