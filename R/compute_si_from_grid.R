#' compute spectral indices for each cell of a grid defined over a raster
#'
#' @param rast_path character.
#' @param mask_path character.
#' @param plots list. list of plots (sf object)
#' @param siteName character.
#' @param si_list character.
#' @param output_dir character.
#' @param overwrite boolean.
#' @param spectral_bands numeric.
#' @param sensor_name character.
#' @param ReflFactor numeric.
#'
#'
#' @return filename_si list of file paths produced
#' @export

compute_si_from_grid <- function(rast_path, mask_path = NULL, plots, siteName, si_list,
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
                                            siteName = siteName,
                                            overwrite = overwrite,
                                            p = p),
                            SIMPLIFY = F)}))
  return(output_dir_si)
}
