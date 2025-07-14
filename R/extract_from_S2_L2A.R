#' This function reads S2 data from L2A directories downloaded from
#' various data hubs including THEIA, PEPS & SCIHUB (SAFE format & LaSRC)
#' @param Path_dir_S2 character. path for S2 directory
#' @param Path_dir_Mask character. path for S2 mask directory
#' @param path_vector character. path for vector file
#' @param S2source character. type of directory format (depends on atmospheric correction: SAFE produced from Sen2Cor)
#' @param resolution numeric. buffer applied to vector file (in meters)
#' @param interpolation character. method for resampling. default = 'nearest_neighbour'
#' @param fre_sre character. SRE or FRE products from THEIA
#'
#' @return ListOut list.
#' - image stack
#' - path for individual band files corresponding to the stack
#' - path for vector (reprojected if needed)
#'
#' @importFrom terra rast
#' @importFrom tools file_path_sans_ext
#' @export

extract_from_S2_L2A <- function(Path_dir_S2,
                                Path_dir_Mask = NULL,
                                path_vector = NULL,
                                S2source = 'SAFE',
                                resolution = 10,
                                interpolation = 'nearest_neighbour',
                                fre_sre = 'FRE'){

  # Get list of paths corresponding to S2 bands and depending on S2 directory
  S2_Bands <- get_S2_bands(Path_dir_S2 = Path_dir_S2,
                           S2source = S2source,
                           resolution = resolution,
                           fre_sre = fre_sre,
                           Path_dir_Mask = Path_dir_Mask)

  elim <- which(unlist(lapply(S2_Bands,is.null)))
  if (length(elim)>0)
    S2_Bands[elim] <- NULL

  # check if vector and raster share the same projection. if not, re-project vector
  if (!is.null(path_vector)){
    SpatialObj <- terra::rast(x = S2_Bands[[1]][[1]])
    path_vector_reproj <- paste(tools::file_path_sans_ext(path_vector),'_reprojected.gpkg',sep = '')
    path_vector <- reproject_shp(path_vector_init = path_vector,
                                 SpatialObj = SpatialObj,
                                 path_vector_reproj = path_vector_reproj)
  }
  aoi <- sf::st_read(dsn = path_vector, quiet = T)
  # Extract data corresponding to the vector footprint (if provided) & resample data if needed
  S2_Stack <- read_S2bands(S2_Bands = S2_Bands,
                           aoi = aoi,
                           resolution = resolution,
                           interpolation = interpolation)

  # get full stack including 10m and 20m spatial resolution
  if (!is.null(S2_Bands$S2Bands_10m) & !is.null(S2_Bands$S2Bands_20m)){
    # reorder bands with increasing wavelength
    NameBands <- c(names(S2_Bands$S2Bands_10m),names(S2_Bands$S2Bands_20m))
    if (!is.na(match('B02',NameBands))) S2Bands <- c("B02", "B03", "B04", "B05", "B06", "B07", "B08", "B8A", "B11", "B12", "Cloud")
    if (!is.na(match('B2',NameBands))) S2Bands <- c("B2", "B3", "B4", "B5", "B6", "B7", "B8", "B8A", "B11", "B12", "Cloud")
    reorder_bands <- match(S2Bands,NameBands)
    NameBands <- NameBands[reorder_bands]
    S2_Stack$attr <- S2_Stack$attr[reorder_bands]
    names(S2_Stack$attr) <- NameBands
  } else if (length(S2_Bands$S2Bands_10m)>0){
    NameBands <- names(S2_Bands$S2Bands_10m)
    names(S2_Stack$attr) <- NameBands
  } else if (length(S2_Bands$S2Bands_20m)>0){
    NameBands <- names(S2_Bands$S2Bands_20m)
    names(S2_Stack$attr) <- NameBands
  }
  ListOut <- list('S2_Stack' = S2_Stack,
                  'S2_Bands' = S2_Bands,
                  'path_vector' = path_vector,
                  'NameBands' = NameBands)
  return(ListOut)
}
