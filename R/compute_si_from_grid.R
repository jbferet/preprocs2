#' compute spectral indices for each cell of a grid defined over a raster
#'
#' @param rast_path character.
#' @param mask_path character.
#' @param plots list. list of plots (sf object)
#' @param site_name character.
#' @param si_list character.
#' @param output_dir character.
#' @param overwrite boolean.
#' @param spectral_bands numeric.
#' @param sensor_name character.
#' @param ReflFactor numeric.
#'
#'
#' @return filename_si list of file paths produced
#' @importFrom tools file_path_sans_ext
#' @importFrom terra vrt
#' @importFrom progressr handlers progressor with_progress
#' @export

compute_si_from_grid <- function(rast_path, mask_path = NULL, plots, site_name, si_list,
                                 output_dir, overwrite = FALSE,
                                 spectral_bands = NULL,
                                 sensor_name = 'sentinel-2', ReflFactor = 10000){

  if (!all(!is.na(match(si_list, names(spinR::listIndices_spinR()))))){
    sel <- which(is.na(match(si_list, names(spinR::listIndices_spinR()))))
    message('spectral index not available from spinR')
    message(si_list[sel])
    si_list <- si_list[-sel]
  }

  handlers("cli")
  suppressWarnings(with_progress({
    p <- progressr::progressor(steps = length(plots),
                               message = 'compute mask & SI')
    output_dir_si <- mapply(FUN = get_si_tiles_from_raster,
                            aoi = plots,
                            aoi_ID = as.list(names(plots)),
                            MoreArgs = list(rast_path = rast_path,
                                            mask_path = mask_path,
                                            si_list = si_list,
                                            output_dir = output_dir,
                                            spectral_bands = spectral_bands,
                                            sensor_name = sensor_name,
                                            site_name = site_name,
                                            overwrite = overwrite,
                                            ReflFactor = ReflFactor,
                                            p = p),
                            SIMPLIFY = F)}))

  # create vrt for spectral indices
  output_vrt <- file.path(output_dir, 'spectral_indices_vrt')
  dir.create(path = output_vrt, showWarnings = FALSE, recursive = TRUE)
  si_path_vrt <- list()
  raster_name <- tools::file_path_sans_ext(basename(rast_path))
  si_dir <- file.path(output_dir_si, 'spectral_indices')
  for (si in si_list){
    output_vrt_path <- file.path(output_vrt, paste0(raster_name, '_', si, '.vrt'))
    si_path_vrt[[si]] <- lapply(output_dir_si, '[[', si)
    v <- terra::vrt(x = unlist(si_path_vrt[[si]]),
                    filename = output_vrt_path, overwrite = TRUE)
  }


  return(output_dir_si)
}
