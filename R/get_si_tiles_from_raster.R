#' computes spectral index corresponding to a vector footprint from a raster
#' the raster is expecte to be sentinel-2 or landsat
#'
#' @param aoi spatVector.
#' @param aoi_ID character.
#' @param rast_path character.
#' @param mask_path character.
#' @param si_list character.
#' @param output_dir character.
#' @param overwrite boolean.
#' @param spectral_bands numeric.
#' @param sensor_name character. either sentinel-2 or landsat
#' @param site_name character.
#' @param ReflFactor numeric.
#' @param p object
#'
#' @return filename_si list of file paths produced
#' @importFrom terra vect crop res writeRaster values
#' @importFrom methods as
#' @importFrom sf st_sf
#' @export

get_si_tiles_from_raster <- function(aoi, aoi_ID, rast_path, mask_path = NULL,
                                     si_list, output_dir, overwrite = FALSE,
                                     spectral_bands, sensor_name = 'sentinel-2',
                                     site_name, ReflFactor = 10000, p = NULL){

  rast_obj <- terra::rast(rast_path)
  if (!is.null(mask_path)){
    mask_obj <- terra::rast(mask_path)
  } else {
    mask_obj <- NULL
  }

  # make sure directories are already created
  # output_dir_mask <- file.path(output_dir, 'mask')
  # dir.create(path = output_dir_mask, showWarnings = FALSE, recursive = TRUE)
  output_dir_si <- file.path(output_dir, 'spectral_indices')
  dir.create(path = output_dir_si, showWarnings = FALSE, recursive = TRUE)

  # which files are expected as outputs
  # filename_mask <- file.path(output_dir_mask, paste0('mask_',aoi_ID,'.tiff'))
  filename_si <- as.list(file.path(output_dir_si,
                                   paste0(site_name, '_', aoi_ID,
                                          '_', si_list, '.tiff')))
  names(filename_si) <- si_list

  # if overwrite or one of expected files does not exists
  if (! all(file.exists(unlist(filename_si))) | overwrite){
    aoi_plot <- methods::as(sf::st_sf(aoi), "Spatial")
    aoi_plot <- terra::vect(aoi_plot)
    sensor_refl <- terra::crop(x = rast_obj, y = aoi_plot)
    if (!is.null(mask_obj))
      mask_val <- terra::crop(x = mask_obj, y = aoi_plot)
    if (is.null(mask_obj))
      mask_val <- 1+0*sensor_refl[[1]]
    names(mask_val) <- 'mask'

    if (!is.null(spectral_bands)){
      SensorBands <- spectral_bands
    } else {
      if (!is.na(match(toupper(sensor_name), c('S2', 'SENTINEL2',
                                               'SENTINEL_2', 'SENTINEL-2')))){
        HDRpath <- system.file('extdata', 'SENTINEL_2.hdr',
                               package = 'preprocS2')
        sensor <- 'S2'
      } else if (!is.na(match(toupper(sensor_name), c('LANDSAT', 'LANDSAT7',
                                                      'LANDSAT_7', 'LANDSAT-7',
                                                      'LANDSAT8', 'LANDSAT_8',
                                                      'LANDSAT-8', 'LANDSAT9',
                                                      'LANDSAT_9', 'LANDSAT-9')))){
        HDRpath <- system.file('extdata', 'Landsat_7.hdr',
                               package = 'preprocS2')
        sensor <- 'landsat'
      }
      hdr <- read_envi_header(HDRpath = HDRpath)
      SensorBands <- hdr$wavelength
    }
    # if (sensor == 'landsat'){
    #   bandnames <- strsplit(x = hdr$`band names`, split = ', ')[[1]]
    #   sensor_refl$blue <- sensor_refl$B01
    #   sensor_refl$red <- sensor_refl$B03
    #   sensor_refl$nir <- sensor_refl$B04
    # } else if (sensor == 'S2'){
    #   sensor_refl$blue <- sensor_refl$B02
    #   sensor_refl$red <- sensor_refl$B04
    #   if (terra::res(sensor_refl)[1] == 10){
    #     sensor_refl$nir <- sensor_refl$B08
    #   } else if (terra::res(sensor_refl)[1] == 20){
    #     sensor_refl$nir <- sensor_refl$B8A
    #   }
    #   if (terra::res(sensor_refl)[1] == 10)
    #     bandnames <- c('B02', 'B03', 'B04', 'B05', 'B06', 'B07', 'B08', 'B8A', 'B11', 'B12')
    #   if (terra::res(sensor_refl)[1] == 20)
    #     bandnames <- c('B02', 'B03', 'B04', 'B05', 'B06', 'B07', 'B8A', 'B11', 'B12')
    # }
    # names(sensor_refl) <- bandnames
    # # compute radiometric mask
    # # set values out of range if NA to keep all pixels
    # if (! is.na(radiometric_filter$NDVIMask)){
    #   ndvi <- (sensor_refl$nir-sensor_refl$red)/(sensor_refl$nir+sensor_refl$red)
    # } else {
    #   ndvi <- 1 + 0*sensor_refl[[1]]
    # }
    #
    # if (is.na(radiometric_filter$cloudMask))
    #   sensor_refl$blue <- 0*sensor_refl[[1]]
    # if (is.na(radiometric_filter$shadeMask))
    #   sensor_refl$nir   <- 1e4 + 0*sensor_refl[[1]]
    #
    # sel <- sensor_refl$blue < radiometric_filter$cloudMask &
    #   sensor_refl$nir > radiometric_filter$shadeMask &
    #   ndvi > radiometric_filter$NDVIMask
    # # mainmask <- preprocS2::get_mainmask(mask_path = , sensor_refl, aoi_plot_sf)
    # bin_mask <- 0*sensor_refl[[1]]+1
    # bin_mask[is.na(bin_mask)] <- 0
    # names(bin_mask) <- 'mask'
    # bin_mask <- bin_mask*sel
    # # save mask
    # terra::writeRaster(x = bin_mask, filename = filename_mask, overwrite = TRUE)

    # compute SI
    SI <- spinR::compute_S2SI_Raster(Refl = sensor_refl,
                                     SensorBands = SensorBands,
                                     Sel_Indices = si_list,
                                     StackOut = F,
                                     ReflFactor = ReflFactor)
    # mask SI
    na_mask <- mask_val
    na_mask[which(terra::values(mask_val)==0)] <- NA
    # save SI
    for (si in si_list){
      sisel <- SI$SpectralIndices[[si]]
      names(sisel) <- si
      sisel <- sisel*na_mask
      terra::writeRaster(x = sisel,
                         filename = filename_si[[si]],
                         overwrite = overwrite)
    }
  }
  if (!is.null(p))
    p()
  return(filename_si)
}

