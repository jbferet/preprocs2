#' This function returns path for the spectral bands in LaSRC directory
#'
#' @param Path_dir_S2 character. Path for the SAFE directory containing S2 data
#' @param resolution numeric. spatial resolution of the final image: 10m or 20m
#'
#' @return ListBands list. contains path for spectral bands corresponding to 10m and 20m resolution, as well name of as granule
#' @importFrom stringr str_subset
#' @export
#'
get_S2_bands_from_LaSRC <- function(Path_dir_S2, resolution=10){

  # get granule directory & path for corresponding metadata XML file
  granule <- Path_dir_S2
  MTDfile <- file.path(granule,'MTD_TL.xml')
  if (file.exists(file.path(Path_dir_S2,'MTD_MSIL1C.xml'))){
    MTD_MSI_file <- file.path(Path_dir_S2,'MTD_MSIL1C.xml')
  } else {
    MTD_MSI_file <- NULL
  }

  # build path for all bands
  B10m <- c('band2','band3','band4','band5','band6','band7','band8','band8a','band11','band12')
  B10m_Standard <- c('B02','B03','B04','B05','B06','B07','B08','B8A','B11','B12')
  # Define path for bands
  S2Bands_10m <- S2Bands_20m <- list()
  for (i in 1:length(B10m)){
    S2Bands_10m[[B10m_Standard[i]]] <- file.path(Path_dir_S2,
                                                 list.files(Path_dir_S2,
                                                            pattern = paste(B10m[i],'.tif',sep = '')))
  }

  # get metadata file containing offset
  MTD_LaSRC <- stringr::str_subset(list.files(Path_dir_S2,pattern = 'S2'), ".xml$")
  if (file.exists(file.path(Path_dir_S2,MTD_LaSRC))){
    metadata_LaSRC <- file.path(Path_dir_S2,MTD_LaSRC)
  } else {
    metadata_LaSRC <- NULL
  }
  # get cloud mask
  Cloud <- 'CLM'
  S2Bands_10m[['Cloud']] <- file.path(Path_dir_S2,list.files(Path_dir_S2,pattern = Cloud))
  ListBands <- list('S2Bands_10m' = S2Bands_10m,
                    'S2Bands_20m' = S2Bands_20m,
                    'GRANULE' = granule,
                    'metadata' = MTDfile,
                    'metadata_MSI' = MTD_MSI_file,
                    'metadata_LaSRC' = metadata_LaSRC)
  return(ListBands)
}
