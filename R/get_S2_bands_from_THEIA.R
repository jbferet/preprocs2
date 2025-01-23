#' This function returns path for the spectral bands in THEIA directory
#'
#' @param Path_dir_S2 character. Path for the SAFE directory containing S2 data
#' @param resolution numeric. spatial resolution of the final image: 10m or 20m
#' @param fre_sre character. SRE or FRE products from THEIA
#'
#' @return ListBands list. contains path for spectral bands corresponding to 10m and 20m resolution, as well name of as granule
#' @export
#'
get_S2_bands_from_THEIA <- function(Path_dir_S2, resolution=10, fre_sre='FRE'){

  # build path for all bands
  if (resolution == 10){
    B10m <- c('B02','B03','B04','B08')
    B20m <- c('B05','B06','B07','B8A','B11','B12')
  } else {
    B10m <- c()
    B20m <- c('B02','B03','B04','B05','B06','B07','B08','B8A','B11','B12')
  }

  # get Path_tile_S2 directory & path for corresponding metadata XML file
  Path_tile_S2 <- list.dirs(Path_dir_S2, recursive = FALSE)
  Files_tile_S2 <- list.files(Path_tile_S2, recursive = FALSE)
  MTDfile <- file.path(Path_tile_S2, Files_tile_S2[grep(pattern = 'MTD_ALL.xml', x = Files_tile_S2)])

  # Define path for bands
  S2Bands_10m_dir <- S2Bands_20m_dir <- Path_tile_S2
  S2Bands_10m <- S2Bands_20m <- list()
  for (band in B20m){
    band_20m_pattern <- paste0(gsub("0", "", band), '.tif') # for THEAI band 2 is 'B2' ('B02' for SAFE)
    list_files_20m <- list.files(S2Bands_20m_dir, pattern = band_20m_pattern)
    S2Bands_20m[[band]] <- file.path(S2Bands_20m_dir, list_files_20m)[grep(pattern = fre_sre,
                                                                           x = file.path(S2Bands_20m_dir, list_files_20m))]
  }
  for (band in B10m){
    band_10m_pattern <- paste0(gsub("0", "", band), '.tif') # for THEAI band 2 is 'B2' ('B02' for SAFE)
    list_files_10m <- list.files(S2Bands_10m_dir, pattern = band_10m_pattern)
    S2Bands_10m[[band]] <- file.path(S2Bands_10m_dir, list_files_10m)[grep(pattern = fre_sre,
                                                                           x = file.path(S2Bands_10m_dir, list_files_10m))]
  }

  # get cloud mask 10m
  Cloud_10m <- 'CLM_R1'
  Cloud_10m_dir <- file.path(Path_tile_S2, 'MASKS')
  S2Bands_10m[['Cloud']] <- file.path(Cloud_10m_dir, list.files(Cloud_10m_dir, pattern = Cloud_10m))

  # get cloud mask 20m
  Cloud_20m <- 'CLM_R2'
  Cloud_20m_dir <- file.path(Path_tile_S2, 'MASKS')
  S2Bands_20m[['Cloud']] <- file.path(Cloud_20m_dir, list.files(Cloud_20m_dir, pattern = Cloud_20m))

  # return list bands
  ListBands <- list('S2Bands_10m' = S2Bands_10m,
                    'S2Bands_20m' = S2Bands_20m,
                    'Path_tile_S2' = Path_tile_S2,
                    'metadata' = MTDfile)
  return(ListBands)
}
