#' This function reads a list of files corresponding to S2 bands
#' S2 bands are expected to have uniform spatial resolution and footprint
#' @param S2_Bands list. list of S2 bands obtained from get_S2_bands
#' @param aoi sf object.
#' @param resolution numeric. resolution of final product
#' @param interpolation character. method for resampling. default = 'nearest_neighbour'
#'
#' @return Stack_S2 list. contains stack of S2 bands
#'
#' @importFrom stars read_stars write_stars
#' @importFrom sf st_bbox st_read st_crop
#' @export

read_S2bands <- function(S2_Bands,
                         aoi = NULL,
                         resolution = 10,
                         interpolation = 'nearest_neighbour'){

  S2_rast <- list('S2Bands_10m' = S2_Bands$S2Bands_10m,
                  'S2Bands_20m' = S2_Bands$S2Bands_20m)
  whichOK <- lapply(lapply(S2_rast,is.null),isFALSE)
  for (band in names(whichOK))
    if (!whichOK[[band]]) S2_rast[[band]] <- NULL
  # get bounding box corresponding to footprint of image or image subset
  BB_XYcoords <- list()
  for (band in names(S2_rast)){
    BB_XYcoords[[band]] <- get_BB(path_raster = S2_rast[[band]][[1]],
                                  aoi = aoi)
  }
  BB_XYcoords <- adjust_BB_XYcoords(BB_XYcoords)
  # prepare reading data for extent defined by bounding box
  if (resolution==10) resampling <- list('S2Bands_10m' = 1, 'S2Bands_20m' = 2)
  if (resolution==20) resampling <- list('S2Bands_10m' = 0.5, 'S2Bands_20m' = 1)
  tmpDir <- tempdir()
  tmpfile <- list()

  pb <- progress::progress_bar$new(
    format = "Reading S2 bands [:bar] :percent in :elapsedfull",
    total = sum(unlist(lapply(S2_rast,length))), clear = FALSE, width= 100)

  for (res in names(S2_rast)){
    nXOff <- BB_XYcoords[[res]]$UL$col
    nYOff <- BB_XYcoords[[res]]$UL$row
    nXSize <- BB_XYcoords[[res]]$UR$col-BB_XYcoords[[res]]$UL$col+1
    nYSize <- BB_XYcoords[[res]]$LR$row-BB_XYcoords[[res]]$UR$row+1
    nBufXSize <- resampling[[res]]*nXSize
    nBufYSize <- resampling[[res]]*nYSize
    if (resampling[[res]]<=1) interpolation_tmp <- 'average'
    if (resampling[[res]]>1) interpolation_tmp <- interpolation
    # write interpolated individual bands in temp directory
    for (band in names(S2_Bands[[res]])){
      Stack_S2_tmp <- stars::read_stars(S2_Bands[[res]][[band]],
                                        RasterIO = list(nXOff = nXOff, nYOff = nYOff,
                                                        nXSize = nXSize, nYSize = nYSize,
                                                        nBufXSize = nBufXSize, nBufYSize = nBufYSize,
                                                        resample = interpolation_tmp),
                                        proxy = FALSE)

      if (!is.null(aoi)){
        Stack_S2_tmp <- sf::st_crop(x = Stack_S2_tmp, y = st_bbox(aoi))
      }
      tmpfile[[band]] <- file.path(tmpDir,
                                   tools::file_path_sans_ext(basename(S2_Bands[[res]][[band]])))
      if (band=='Cloud'){
        stars::write_stars(obj = Stack_S2_tmp, dsn=tmpfile[[band]],
                           driver =  "ENVI",type='Byte',overwrite = TRUE)
      } else {
        stars::write_stars(obj = Stack_S2_tmp, dsn=tmpfile[[band]],
                           driver =  "ENVI",type='Int16',overwrite = TRUE)
      }
      pb$tick()
      gc()
    }
  }
  Stack_S2 <- stars::read_stars(tmpfile, along = 'band',
                                driver =  "ENVI", proxy = TRUE)
  return(Stack_S2)
}
