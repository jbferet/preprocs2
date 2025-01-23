#' This function saves reflectance files
#'
#' @param S2_stars list. stars object containing raster data. Can be produced with function extract_from_S2_L2A
#' @param Refl_path character. path for reflectance file to be stored
#' @param Format character. file format for reflectance data
#' @param datatype character. data type (integer, float, 16bits, 32bits...)
#' @param S2Sat character. Sentinel-2 mission ('2A' or '2B')
#' @param tile_S2 character. S2 tile name (2 numbers + 3 letters)
#' @param dateAcq_S2 double. date of acquisition
#' @param MTD character. path for metadata file
#' @param MTD_MSI character. path for metadata MSI file
#' @param MTD_LaSRC character. path for metadata LaSRC file
#' @param MaxChunk numeric. Size of individual chunks to be written (in Mb)
#' @param s2mission character. Set NULL for online check, or '2A'/'2B' if mission known (or irrelevant)
#'
#' @return None
#' @importFrom stars read_stars
#' @export
save_reflectance_s2 <- function(S2_stars, Refl_path, Format = 'ENVI',
                                datatype = 'INT2S', S2Sat = NULL, tile_S2 = NULL,
                                dateAcq_S2 = NULL,
                                MTD = NULL, MTD_MSI = NULL, MTD_LaSRC = NULL,
                                MaxChunk = 256, s2mission = NULL){
  # identify if S2A or S2B, if possible
  if (is.null(s2mission)){
    s2mission <- check_S2mission(S2Sat = S2Sat, tile_S2 = tile_S2, dateAcq_S2 = dateAcq_S2)
  } else if (!s2mission== '2A' & !s2mission== '2B'){
    s2mission <- '2A'
  }
  
  # define central wavelength corresponding to each spectral band
  nameBands <- names(S2_stars$attr)
  if (!is.na(match('B2',nameBands))){
    if (s2mission=='2A'){
      WL_s2 <- list("B2"=496.6, "B3"=560.0, "B4"=664.5,
                    "B5"=703.9, "B6"=740.2, "B7"=782.5, "B8"=835.1,
                    "B8A"=864.8, "B11"=1613.7, "B12"=2202.4)
    } else if (s2mission=='2B'){
      WL_s2 <- list("B2"=492.1, "B3"=559.0, "B4"=665.0,
                    "B5"=703.8, "B6"=739.1, "B7"=779.7, "B8"=833.0,
                    "B8A"=864.0, "B11"=1610.4, "B12"=2185.7)
    }
  } else if (!is.na(match('B02',nameBands))){
    if (s2mission=='2A'){
      WL_s2 <- list("B02"=496.6, "B03"=560.0, "B04"=664.5,
                    "B05"=703.9, "B06"=740.2, "B07"=782.5, "B08"=835.1,
                    "B8A"=864.8, "B11"=1613.7, "B12"=2202.4)
    } else if (s2mission=='2B'){
      WL_s2 <- list("B02"=492.1, "B03"=559.0, "B04"=665.0,
                    "B05"=703.8, "B06"=739.1, "B07"=779.7, "B08"=833.0,
                    "B8A"=864.0, "B11"=1610.4, "B12"=2185.7)
    }
  }
  
  if (s2mission=='2A'){
    sensor <- 'Sentinel_2A'
  } else if (s2mission=='2B'){
    sensor <- 'Sentinel_2B'
  }
  
  S2_offset <- get_S2_offset(MTD_MSI, MTD_LaSRC)
  Offset <- S2_offset$Offset
  BOA_QuantVal <- S2_offset$BOA_QuantVal

  # identify where spectral bands are in the stars object
  Stars_Spectral <- list()
  starsnames <- names(S2_stars$attr)
  Stars_Spectral$bandname <- starsnames[which(!starsnames=='Cloud')]
  Stars_Spectral$wavelength <- WL_s2[Stars_Spectral$bandname]
  Stars_Spectral$`wavelength units` <- 'nanometers'
  
  SortedWL <- names(WL_s2)
  Reorder <- match(SortedWL,Stars_Spectral$bandname)
  Elim <- which(is.na(Reorder))
  if (length(Elim)>0){
    Reorder <- Reorder[-Elim]
  }
  pathR <- S2_stars$attr[Reorder]
  names_S2 <- names(pathR)
  names(pathR) <- NULL
  S2_stars2 <- stars::read_stars(pathR, along = 'band',
                                 proxy = TRUE, driver = 'ENVI')
  Stars_Spectral$bandname <- Stars_Spectral$bandname[Reorder]
  Stars_Spectral$wavelength <- Stars_Spectral$wavelength[Reorder]
  
  UniqueOffset <- as.numeric(unique(Offset$Offset))
  if (length(UniqueOffset)>1){
    message('Warning: BOA offset differs between bands.')
    message('Offset will not be applied to the final S2 reflectance raster')
    message('check metadata file to identify the offset applied on each band')
    print(MTD_MSI)
    UniqueOffset <- BOA_QuantVal <- 1
  }
  
  # write stack with bigRaster
  funct <- bigRaster::wrapperBig_Stack
  input_rasters <- as.list(S2_stars2$attr)
  names(input_rasters) <- names_S2
  input_args <- list('datatype' = datatype,
                     'offset' = UniqueOffset,
                     'BOA_QuantVal' = BOA_QuantVal)
  if (!is.null(MTD_LaSRC)) input_args$LaSRC <- TRUE
  output_rasters <- list('Stack' = Refl_path)
  bandNames <- list('Stack' = names_S2)
  
  bigRaster::apply_bigRaster(funct = funct,
                             input_rasters = input_rasters,
                             input_args = input_args,
                             output_rasters,
                             output_lyrs = length(input_rasters),
                             filetype = 'EHdr',
                             bandNames = bandNames)
  
  if (Format == 'ENVI') adjust_ENVI_hdr(dsn = Refl_path, Bands = Stars_Spectral,
                                        sensor = sensor, Stretch = TRUE)
  
  # save metadata file as well if available
  if (!is.null(MTD)){
    if (file.exists(MTD)) file.copy(from = MTD,
                                    to = file.path(dirname(Refl_path), basename(MTD)),
                                    overwrite = TRUE)}
  # save metadata file as well if available
  if (!is.null(MTD_MSI)){
    if (file.exists(MTD_MSI)) file.copy(from = MTD_MSI,
                                        to = file.path(dirname(Refl_path), basename(MTD_MSI)),
                                        overwrite = TRUE)}
  # save LaSRC metadata file as well if available
  if (!is.null(MTD_LaSRC)){
    if (file.exists(MTD_LaSRC)) file.copy(from = MTD_LaSRC,
                                          to = file.path(dirname(Refl_path), basename(MTD_LaSRC)),
                                          overwrite = TRUE)}
  # delete temporary file
  for (pathtemp in pathR){
    file.remove(pathtemp)
    if (file.exists(paste0(pathtemp,'.hdr'))) file.remove(paste0(pathtemp,'.hdr'))
  }
  gc()
  return(invisible())
}
