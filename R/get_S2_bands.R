#' This function returns path for the spectral bands to be used
#'
#' @param Path_dir_S2 character. Path for the directory containing S2 data. either L2A .SAFE S2 file or THEIA directory
#' @param S2source character. defines if data comes from SciHub as SAFE directory, from THEIA or from LaSRC
#' @param resolution numeric. spatial resolution of the final image: 10m or 20m
#' @param fre_sre character. SRE or FRE products from THEIA
#' @param Path_dir_Mask character. path for S2 mask directory
#'
#' @return ListBands list. contains path for spectral bands corresponding to 10m and 20m resolution
#' @export
#'
get_S2_bands <- function(Path_dir_S2, S2source = 'SAFE', resolution = 10,
                         fre_sre = 'FRE', Path_dir_Mask = NULL){

  if (S2source=='SAFE' | S2source=='Sen2Cor'){
    ListBands <- get_S2_bands_from_Sen2Cor(Path_dir_S2 = Path_dir_S2,resolution = resolution)
  } else if (S2source=='THEIA'){
    ListBands <- get_S2_bands_from_THEIA(Path_dir_S2 = Path_dir_S2,resolution = resolution, fre_sre = fre_sre)
  } else if (S2source=='LaSRC'){
    ListBands <- get_S2_bands_from_LaSRC(Path_dir_S2 = Path_dir_S2,resolution = resolution)
  # } else if (S2source == 'GEE'){
  #   ListBands <- get_S2_bands_from_RGEE(Path_dir_S2 = Path_dir_S2)
  # } else if (S2source == 'RAW'){
  #   ListBands <- get_S2_bands_from_RAW(Path_dir_S2 = Path_dir_S2,
  #                                      Path_dir_Mask = Path_dir_Mask)
  } else {
    message('The data source (Atmospheric correction) for Sentinel-2 image is unknown')
    message('Please provide S2 images from one of the following data sources:')
    message('- LaSRC (atmospheric correction: LaSRC)')
    message('- THEIA (atmospheric correction: MAJA)')
    message('- SAFE (atmospheric correction: Sen2Cor)')
    message('- GEE (atmospheric correction: Sen2Cor)')
    S2Bands_10m <- S2Bands_20m <- granule <- MTDfile <- metadata_MSI <- metadata_LaSRC <- NULL
    ListBands <- list('S2Bands_10m' = S2Bands_10m,'S2Bands_20m' = S2Bands_20m,'GRANULE' = granule,
                      'metadata'= MTDfile,'metadata_MSI' = metadata_MSI,
                      'metadata_LaSRC'= metadata_LaSRC)
  }
  return(ListBands)
}
