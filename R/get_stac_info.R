#' define stac provider and collection based on a list of information provided
#' as input
#'
#' @param stac_info list. contains provider, stac_url, and collection

#' @return list of collections per plot
#' @importFrom rstac stac ext_filter post_request
#' @importFrom sf st_transform
#' @importFrom stringr str_split
#' @importFrom magrittr %>%
#' @export
#'
get_stac_info <- function(stac_info = NULL){

  if (!is.null(stac_info$collection))
    stac_info$collection <- tolower(stac_info$collection)
  if (!is.null(stac_info$provider))
    stac_info$provider <- tolower(stac_info$provider)

  # default provider : microsoft planetary computer
  if (is.null(stac_info)){
    stac_info <- list('provider' = 'mpc',
                      'stac_url' = 'https://planetarycomputer.microsoft.com/api/stac/v1',
                      'collection' = 'sentinel-2-l2a')

  # if microsoft planetary computer
  } else if (stac_info$provider %in% c('mpc', 'planetary_computer',
                                       'microsoft_planetary_computer')){
    stac_info$provider <- 'mpc'
    stac_info$stac_url <- 'https://planetarycomputer.microsoft.com/api/stac/v1'
    if (is.null(stac_info$collection))
      stac_info$collection <- 'sentinel-2-l2a'
    if (stac_info$collection %in% c('sentinel-2', 'sentinel2',
                                    's2', 'sentinel_2'))
      stac_info$collection <- 'sentinel-2-l2a'

    # if CDSE
  } else if (stac_info$provider %in% c('esa', 'cdse', 'copernicus')){
    stac_info$provider <- 'esa'
    stac_info$stac_url <- 'https://stac.dataspace.copernicus.eu/v1/'
    if (is.null(stac_info$collection))
      stac_info$collection <- 'sentinel-2-l2a'
    if (stac_info$collection %in% c('sentinel-2', 'sentinel2',
                                    's2', 'sentinel_2'))
      stac_info$collection <- 'sentinel-2-l2a'

  # # if google
  # } else if (stac_info$provider %in% c('gee', 'goggle', 'alphaearth')){
  #   stac_info$provider <- 'gee'
  #   stac_info$stac_url <- 'https://storage.googleapis.com/earthengine-stac/catalog/'
  #   if (is.null(stac_info$collection))
  #     stac_info$collection <- 'sentinel-2-l2a'
  #   if (stac_info$collection %in% c('sentinel-2', 'sentinel2',
  #                                   's2', 'sentinel_2'))
  #     stac_info$collection <- 'sentinel-2-l2a'

  # if cds theia / cdg MTD
  } else if (stac_info$provider %in% c('theia', 'teledetection', 'cds',
                                       'cdg', 'mtd', 'lasrc', 'mtd_esa')){
    stac_info$stac_url <- 'https://api.stac.teledetection.fr'
    if (stac_info$provider=='lasrc' &
        (is.null(stac_info$collection))){
      stac_info$collection <- 'sentinel2-l2a-sen2lasrc'
    } else if (stac_info$provider=='theia' &
               (is.null(stac_info$collection))){
      stac_info$collection <- 'sentinel2-l2a-theia'
    } else if (is.null(stac_info$collection)){
      stac_info$provider <- 'mtd_esa'
      stac_info$collection <- 'sentinel2-l2a-sen2cor'
    }
    # stac_info$provider <- 'theia'
  } else if (is.null(stac_info$stac_url)){
    message('please provide valid STAC catalog provider among')
    message('mpc, cdse, theia')
    stop()
  }
  return(stac_info)
}



