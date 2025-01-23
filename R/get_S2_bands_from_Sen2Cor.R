#' This function returns path for the spectral bands in SAFE / sen2Cor directory
#'
#' @param Path_dir_S2 character. Path for the SAFE directory containing S2 data
#' @param resolution numeric. spatial resolution of the final image: 10m or 20m
#'
#' @return ListBands list. contains path for spectral bands corresponding to 10m and 20m resolution, as well name of as granule
#' @export
#'
get_S2_bands_from_Sen2Cor <- function(Path_dir_S2, resolution = 10){
  # build path for all bands
  if (resolution == 10){
    B10m <- c('B02','B03','B04','B08')
    B20m <- c('B05','B06','B07','B8A','B11','B12')
    S2Bands_10m <- S2Bands_20m <- list()
  } else {
    B10m <- S2Bands_10m <- NULL
    B20m <- c('B02','B03','B04','B05','B06','B07','B8A','B11','B12')
    S2Bands_20m <- list()
  }
  # get granule directory & path for corresponding metadata XML file
  granule <- list.dirs(list.dirs(Path_dir_S2,recursive = FALSE)[grep(pattern = 'GRANULE',
                                                                     x = list.dirs(Path_dir_S2,recursive = FALSE))],recursive = FALSE)
  MTDfile <- file.path(granule,'MTD_TL.xml')
  if (file.exists(file.path(Path_dir_S2,'MTD_MSIL2A.xml'))){
    MTD_MSI_file <- file.path(Path_dir_S2,'MTD_MSIL2A.xml')
  } else {
    MTD_MSI_file <- NULL
  }

  # Define path for bands
  S2Bands_20m_dir <- file.path(granule,'IMG_DATA','R20m')
  S2Bands_10m_dir <- file.path(granule,'IMG_DATA','R10m')

  for (band in B20m){
    S2Bands_20m[[band]] <- file.path(S2Bands_20m_dir,list.files(S2Bands_20m_dir,pattern = band))
  }
  for (band in B10m){
    S2Bands_10m[[band]] <- file.path(S2Bands_10m_dir,list.files(S2Bands_10m_dir,pattern = band))
  }
  # get cloud mask
  Cloud <- 'MSK_CLDPRB_20m'
  Cloud_20m_dir <- file.path(granule,'QI_DATA')
  S2Bands_20m[['Cloud']] <- file.path(Cloud_20m_dir,list.files(Cloud_20m_dir,pattern = Cloud))
  ListBands <- list('S2Bands_10m' = S2Bands_10m,
                    'S2Bands_20m' = S2Bands_20m,
                    'GRANULE' = granule,
                    'metadata' = MTDfile,
                    'metadata_MSI' = MTD_MSI_file,
                    'metadata_LaSRC' = NULL)
  return(ListBands)
}
