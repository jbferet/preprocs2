# == == == == == == == == == == == == == == == == == == == == == == == == == == ==
# preprocS2
# Lib_preprocess_S2.R
# == == == == == == == == == == == == == == == == == == == == == == == == == == ==
# PROGRAMMERS:
# Jean-Baptiste FERET <jb.feret@teledetection.fr>
# Copyright 2021/08 Jean-Baptiste FERET
# == == == == == == == == == == == == == == == == == == == == == == == == == == ==
# This Library contains functions to preprocess Sentinel-2 images downloaded from
# different data hubs, such as THEIA, PEPS or SCIHUB
# == == == == == == == == == == == == == == == == == == == == == == == == == == ==

#' This function saves reflectance files
#'
#' @param S2Sat character. Sentinel-2 mission ('2A' or '2B')
#' @param tile_S2 character. S2 tile name (2 numbers + 3 letters)
#' @param dateAcq_S2 double. date of acquisition
#'
#' @return s2mission character. name of the S2 mission (2A or 2B)
#' @importFrom sen2r safe_getMetadata
#' @export
check_S2mission <- function(S2Sat, tile_S2, dateAcq_S2){

  # is mission already defined by user?
  if (!is.null(S2Sat)){
    if (S2Sat=='2A'){
      s2mission <- '2A'
    } else if (S2Sat=='2B'){
      s2mission <- '2B'
    }
  } else if (!is.null(tile_S2) & !is.null(dateAcq_S2)){
    tileOK <- sen2r::s2_list(tile = tile_S2,time_interval = as.Date(dateAcq_S2))
    s2mission <- sen2r::safe_getMetadata(tileOK,"mission")[[1]]
    if (is.null(s2mission)){
      message('Could not identify if image from Sentinel-2A or -2B')
      message('Defining central wavelength of spectral bands based on S2A')
      s2mission <- '2A'
    }
  } else {
    message('Could not identify if image from Sentinel-2A or -2B')
    message('Defining central wavelength of spectral bands based on S2A')
    s2mission <- '2A'
  }
  return(s2mission)
}

#' This function reads S2 data from L2A directories downloaded from
#' various data hubs including THEIA, PEPS & SCIHUB (SAFE format & LaSRC)
#' @param Path_dir_S2 character. path for S2 directory
#' @param path_vector character. path for vector file
#' @param S2source character. type of directory format (depends on atmospheric correction: SAFE produced from Sen2Cor)
#' @param resolution numeric. buffer applied to vector file (in meters)
#' @param interpolation character. method for resampling. default = 'bilinear'
#'
#' @return ListOut list.
#' - image stack
#' - path for individual band files corresponding to the stack
#' - path for vector (reprojected if needed)
#'
#' @importFrom raster raster
#' @importFrom tools file_path_sans_ext file_ext
#' @export
extract_from_S2_L2A <- function(Path_dir_S2, path_vector=NULL, S2source='SAFE', resolution=10, interpolation='bilinear'){
  # Get list of paths corresponding to S2 bands and depending on S2 directory
  S2_Bands <- get_S2_bands(Path_dir_S2 = Path_dir_S2,
                           S2source = S2source,
                           resolution = resolution)

  if (length(S2_Bands$S2Bands_10m)>0){
    rastmp <- raster::raster(S2_Bands$S2Bands_10m[[1]])
  } else if (length(S2_Bands$S2Bands_20m)>0){
    rastmp <- raster::raster(S2_Bands$S2Bands_20m[[1]])
  }
  # check if vector and raster share the same projection. if not, re-project vector
  if (!is.null(path_vector)){
    raster_proj <- raster::projection(rastmp)
    path_vector_reproj <- paste(file_path_sans_ext(path_vector),'_reprojected.',file_ext(path_vector),sep = '')
    path_vector <- reproject_shp(path_vector_init = path_vector,
                                 newprojection = raster_proj,
                                 path_vector_reproj = path_vector_reproj)
  }
  # Extract data corresponding to the vector footprint (if provided) & resample data if needed
  if (length(S2_Bands$S2Bands_10m)>0){
    Stack_10m <- read_S2bands(S2_Bands = S2_Bands$S2Bands_10m, path_vector = path_vector,
                              resampling = 1, interpolation = interpolation)
  }
  if (length(S2_Bands$S2Bands_20m)>0){
    if (resolution==10 && S2source!='LaSRC'){
      resampling <- 2
    } else {
      resampling <- 1
    }
    Stack_20m <- read_S2bands(S2_Bands = S2_Bands$S2Bands_20m, path_vector = path_vector,
                              resampling = resampling, interpolation = interpolation)
  }
  # get full stack including 10m and 20m spatial resolution
  if (length(S2_Bands$S2Bands_10m)>0){
    S2_Stack <- Stack_10m
    if (length(S2_Bands$S2Bands_20m)>0){
      for (band20 in names(S2_Bands$S2Bands_20m)){
        S2_Stack[[band20]] <- Stack_20m[[band20]]
      }
    }
  } else {
    S2_Stack <- Stack_20m
  }
  ListOut <- list('S2_Stack'=S2_Stack,'S2_Bands'=S2_Bands,'path_vector'=path_vector)
  return(ListOut)
}

#' This function gets coordinates of a bounding box defined by a vector (optional) and a raster
#'
#' @param path_raster character. path for raster file
#' @param path_vector character. path for vector file
#' @param Buffer numeric. buffer applied to vector file (in meters)
#'
#' @return BB_XYcoords list. Coordinates (in pixels) of the upper/lower right/left corners of bounding box
#' @export
get_BB <- function(path_raster,path_vector=NULL,Buffer = 0){

  if (!is.null(path_vector)){
    # get bounding box with a 50m buffer in order to allow for interpolation
    BB_XYcoords <- get_BB_from_Vector(path_raster = path_raster,
                                      path_vector = path_vector,
                                      Buffer = Buffer)
  } else if (is.null(path_vector)){
    BB_XYcoords <- get_BB_from_fullImage(path_raster)
  }
  return(BB_XYcoords)
}

#' This function gets extreme coordinates of a bounding box corresponding to a full image
#'
#' @param path_raster character. path for raster file
#'
#' @return BB_XYcoords list. Coordinates (in pixels) of the upper/lower right/left corners of bounding box
#' @importFrom raster raster
#' @export
get_BB_from_fullImage <- function(path_raster){
  # get raster coordinates corresponding to Full image
  rasterobj <- raster::raster(path_raster)
  BB_XYcoords <- list()
  BB_XYcoords[['UL']] <- data.frame('row'=1,'col'=1)
  BB_XYcoords[['UR']] <- data.frame('row'=1,'col'=dim(rasterobj)[2])
  BB_XYcoords[['LL']] <- data.frame('row'=dim(rasterobj)[1],'col'=1)
  BB_XYcoords[['LR']] <- data.frame('row'=dim(rasterobj)[1],'col'=dim(rasterobj)[2])
  return(BB_XYcoords)
}

#' This gets bounding box corresponding to a vector from a raster (UL, UR, LL, LR corners)
#'
#' @param path_raster character. path for raster file
#' @param path_vector character. path for vector file
#' @param Buffer numeric. buffer applied to vector file (in meters)
#'
#' @return BB_XYcoords list. Coordinates (in pixels) of the upper/lower right/left corners of bounding box
#' @importFrom sf st_read
#' @importFrom rgeos gBuffer
#' @importFrom sp SpatialPoints
#' @importFrom raster projection extract extent raster
#' @importFrom methods as
#' @export
get_BB_from_Vector <- function(path_raster,path_vector,Buffer = 0){

  Raster <- raster::raster(path_raster)
  # xmin <- xmax <- ymin <- ymax <- c()
  # extract BB coordinates from vector
  BB <- rgeos::gBuffer(spgeom = as(st_read(dsn = path_vector,quiet = T), "Spatial"),width=Buffer,byid = TRUE)
  BBext <- raster::extent(BB)
  xmin <- BBext[1]
  xmax <- BBext[2]
  ymin <- BBext[3]
  ymax <- BBext[4]
  # get coordinates of bounding box corresponding to vector
  Corners <- list()
  Corners[['UR']] <- sp::SpatialPoints(coords = cbind(xmax, ymax))
  Corners[['LR']] <- sp::SpatialPoints(coords = cbind(xmax, ymin))
  Corners[['UL']] <- sp::SpatialPoints(coords = cbind(xmin, ymax))
  Corners[['LL']] <- sp::SpatialPoints(coords = cbind(xmin, ymin))
  raster::projection(Corners[['UL']]) <- raster::projection(Corners[['UR']]) <-
    raster::projection(Corners[['LL']]) <- raster::projection(Corners[['LR']]) <- raster::projection(st_read(dsn = path_vector,quiet = T))
  # get coordinates for corners of bounding box
  BB_XYcoords <- list()
  for (corner in names(Corners)){
    ex.df <- as.data.frame(raster::extract(Raster,Corners[[corner]],cellnumbers=T))
    ColRow <- ind2sub(Raster,ex.df$cell)
    BB_XYcoords[[corner]] <- data.frame('row'=ColRow$row,'col'=ColRow$col)
  }
  return(BB_XYcoords)
}

#' get hdr name from image file name, assuming it is BIL format
#'
#' @param ImPath path of the image
#'
#' @return corresponding hdr
#' @import tools
#' @export
get_HDR_name <- function(ImPath) {
  if (file_ext(ImPath) == "") {
    ImPathHDR <- paste(ImPath, ".hdr", sep = "")
  } else if (file_ext(ImPath) == "bil") {
    ImPathHDR <- gsub(".bil", ".hdr", ImPath)
  } else if (file_ext(ImPath) == "zip") {
    ImPathHDR <- gsub(".zip", ".hdr", ImPath)
  } else {
    ImPathHDR <- paste(file_path_sans_ext(ImPath), ".hdr", sep = "")
  }

  if (!file.exists(ImPathHDR)) {
    message("WARNING : COULD NOT FIND HDR FILE")
    print(ImPathHDR)
    message("Process may stop")
  }
  return(ImPathHDR)
}

#' This function returns path for the spectral bands to be used
#'
#' @param Path_dir_S2 character. Path for the directory containing S2 data. either L2A .SAFE S2 file or THEIA directory
#' @param S2source character. defines if data comes from SciHub as SAFE directory, from THEIA or from LaSRC
#' @param resolution numeric. spatial resolution of the final image: 10m or 20m
#'
#' @return ListBands list. contains path for spectral bands corresponding to 10m and 20m resolution
#' @export
get_S2_bands <- function(Path_dir_S2, S2source = 'SAFE', resolution = 10){

  if (S2source=='SAFE' | S2source=='Sen2Cor'){
    ListBands <- get_S2_bands_from_Sen2Cor(Path_dir_S2 = Path_dir_S2,resolution = resolution)
  } else if (S2source=='THEIA'){
    ListBands <- get_S2_bands_from_THEIA(Path_dir_S2 = Path_dir_S2,resolution = resolution)
  } else if (S2source=='LaSRC'){
    ListBands <- get_S2_bands_from_LaSRC(Path_dir_S2 = Path_dir_S2,resolution = resolution)
  } else {
    message('The data source (Atmospheric correction) for Sentinel-2 image is unknown')
    message('Please provide S2 images from one of the following data sources:')
    message('- LaSRC (atmospheric correction: LaSRC)')
    message('- THEIA (atmospheric correction: MAJA)')
    message('- SAFE (atmospheric correction: Sen2Cor)')
    S2Bands_10m <- S2Bands_20m <- granule <- MTDfile <- NULL
    ListBands <- list('S2Bands_10m'=S2Bands_10m,'S2Bands_20m'=S2Bands_20m,'GRANULE'=granule,'metadata'=MTDfile)
  }
  return(ListBands)
}

#' This function returns path for the spectral bands in SAFE / sen2Cor directory
#'
#' @param Path_dir_S2 character. Path for the SAFE directory containing S2 data
#' @param resolution numeric. spatial resolution of the final image: 10m or 20m
#'
#' @return ListBands list. contains path for spectral bands corresponding to 10m and 20m resolution, as well name of as granule
#' @export
get_S2_bands_from_Sen2Cor <- function(Path_dir_S2, resolution=10){
  # build path for all bands
  if (resolution == 10){
    B10m <- c('B02','B03','B04','B08')
    B20m <- c('B05','B06','B07','B8A','B11','B12')
  } else {
    B10m <- c()
    B20m <- c('B02','B03','B04','B05','B06','B07','B08','B8A','B11','B12')
  }
  # get granule directory & path for corresponding metadata XML file
  granule <- list.dirs(list.dirs(Path_dir_S2,recursive = FALSE)[grep(pattern = 'GRANULE',x = list.dirs(Path_dir_S2,recursive = FALSE))],recursive = FALSE)
  MTDfile <- file.path(granule,'MTD_TL.xml')

  # Define path for bands
  S2Bands_20m_dir <- file.path(granule,'IMG_DATA','R20m')
  S2Bands_10m_dir <- file.path(granule,'IMG_DATA','R10m')
  S2Bands_10m <- S2Bands_20m <- list()
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
  # get metadata file
  granule <- list.dirs(list.dirs(Path_dir_S2,recursive = FALSE)[grep(pattern = 'GRANULE',x = list.dirs(Path_dir_S2,recursive = FALSE))],recursive = FALSE)


  ListBands <- list('S2Bands_10m'=S2Bands_10m,'S2Bands_20m'=S2Bands_20m,'GRANULE'=granule,'metadata'=MTDfile)
  return(ListBands)
}

#' This function returns path for the spectral bands in LaSRC directory
#'
#' @param Path_dir_S2 character. Path for the SAFE directory containing S2 data
#' @param resolution numeric. spatial resolution of the final image: 10m or 20m
#'
#' @return ListBands list. contains path for spectral bands corresponding to 10m and 20m resolution, as well name of as granule
#' @export
get_S2_bands_from_LaSRC <- function(Path_dir_S2, resolution=10){

  # get granule directory & path for corresponding metadata XML file
  granule <- Path_dir_S2
  MTDfile <- file.path(granule,'MTD_TL.xml')

  # build path for all bands
  B10m <- c('band2','band3','band4','band5','band6','band7','band8','band8a','band11','band12')
  B10m_Standard <- c('B02','B03','B04','B05','B06','B07','B08','B8A','B11','B12')
  # Define path for bands
  S2Bands_10m <- S2Bands_20m <- list()
  for (i in 1:length(B10m)){
    S2Bands_10m[[B10m_Standard[i]]] <- file.path(Path_dir_S2,list.files(Path_dir_S2,pattern = paste(B10m[i],'.tif',sep = '')))
  }
  # get cloud mask
  Cloud <- 'CLM'
  S2Bands_10m[['Cloud']] <- file.path(Path_dir_S2,list.files(Path_dir_S2,pattern = Cloud))
  ListBands <- list('S2Bands_10m'=S2Bands_10m,'S2Bands_20m'=S2Bands_20m,'GRANULE'=granule,'metadata'=MTDfile)
  return(ListBands)
}

#' This function returns path for the spectral bands in THEIA directory
#'
#' @param Path_dir_S2 character. Path for the SAFE directory containing S2 data
#' @param resolution numeric. spatial resolution of the final image: 10m or 20m
#'
#' @return ListBands list. contains path for spectral bands corresponding to 10m and 20m resolution, as well name of as granule
#' @export
get_S2_bands_from_THEIA <- function(Path_dir_S2, resolution=10){

  # param to deal with SRE or FRE products from THEIA
  fre_sre='FRE'

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
    S2Bands_20m[[band]] <- file.path(S2Bands_20m_dir, list_files_20m)[grep(pattern = fre_sre, x = file.path(S2Bands_20m_dir, list_files_20m))]
  }
  for (band in B10m){
    band_10m_pattern <- paste0(gsub("0", "", band), '.tif') # for THEAI band 2 is 'B2' ('B02' for SAFE)
    list_files_10m <- list.files(S2Bands_10m_dir, pattern = band_10m_pattern)
    S2Bands_10m[[band]] <- file.path(S2Bands_10m_dir, list_files_10m)[grep(pattern = fre_sre, x = file.path(S2Bands_10m_dir, list_files_10m))]
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

#' convert image coordinates from index to X-Y
#'
#' @param Raster image raster object
#' @param Image_Index coordinates corresponding to the raster
ind2sub <- function(Raster, Image_Index) {
  c <- ((Image_Index - 1) %% Raster@ncols) + 1
  r <- floor((Image_Index - 1) / Raster@ncols) + 1
  my_list <- list("col" = c, "row" = r)
  return(my_list)
}

#' Reads ENVI hdr file
#'
#' @param HDRpath Path of the hdr file
#'
#' @return list of the content of the hdr file
#' @export
read_ENVI_header <- function(HDRpath) {
  # header <- paste(header, collapse = "\n")
  if (!grepl(".hdr$", HDRpath)) {
    stop("File extension should be .hdr")
  }
  HDR <- readLines(HDRpath)
  ## check ENVI at beginning of file
  if (!grepl("ENVI", HDR[1])) {
    stop("Not an ENVI header (ENVI keyword missing)")
  } else {
    HDR <- HDR [-1]
  }
  ## remove curly braces and put multi-line key-value-pairs into one line
  HDR <- gsub("\\{([^}]*)\\}", "\\1", HDR)
  l <- grep("\\{", HDR)
  r <- grep("\\}", HDR)

  if (length(l) != length(r)) {
    stop("Error matching curly braces in header (differing numbers).")
  }

  if (any(r <= l)) {
    stop("Mismatch of curly braces in header.")
  }

  HDR[l] <- sub("\\{", "", HDR[l])
  HDR[r] <- sub("\\}", "", HDR[r])

  for (i in rev(seq_along(l))) {
    HDR <- c(
      HDR [seq_len(l [i] - 1)],
      paste(HDR [l [i]:r [i]], collapse = "\n"),
      HDR [-seq_len(r [i])]
    )
  }

  ## split key = value constructs into list with keys as names
  HDR <- sapply(HDR, split_line, "=", USE.NAMES = FALSE)
  names(HDR) <- tolower(names(HDR))

  ## process numeric values
  tmp <- names(HDR) %in% c(
    "samples", "lines", "bands", "header offset", "data type",
    "byte order", "default bands", "data ignore value",
    "wavelength", "fwhm", "data gain values"
  )
  HDR [tmp] <- lapply(HDR [tmp], function(x) {
    as.numeric(unlist(strsplit(x, ",")))
  })

  return(HDR)
}

#' This function reads a list of files corresponding to S2 bands
#' S2 bands are expected to have uniform spatial resolution and footprint
#' @param S2_Bands list. list of S2 bands obtained from get_S2_bands
#' @param path_vector path for a vector file
#' @param resampling numeric. resampling factor (default = 1, set to resampling = 2 to convert 20m into 10m resolution)
#' @param interpolation character. method for resampling. default = 'bilinear'
#'
#' @return Stack_S2 list. contains stack of S2 bands
#'
#' @importFrom stars read_stars
#' @importFrom sf st_bbox st_read st_crop
#' @export
read_S2bands <- function(S2_Bands, path_vector = NULL,
                         resampling = 1, interpolation = 'bilinear'){
  # get bounding box corresponding to footprint of image or image subset
  BB_XYcoords <- get_BB(path_raster = S2_Bands[[1]],
                        path_vector = path_vector, Buffer = 50)

  # prepare reading data for extent defined by bounding box
  nXOff <- BB_XYcoords$UL$col
  nYOff <- BB_XYcoords$UL$row
  nXSize <- BB_XYcoords$UR$col-BB_XYcoords$UL$col+1
  nYSize <- BB_XYcoords$LR$row-BB_XYcoords$UR$row+1
  nBufXSize <- resampling*nXSize
  nBufYSize <- resampling*nYSize
  if (resampling==1){
    interpolation = 'nearest_neighbour'
  }
  Stack_S2 <- stars::read_stars(S2_Bands,
                                RasterIO = list(nXOff = nXOff, nYOff = nYOff,
                                                nXSize = nXSize, nYSize = nYSize,
                                                nBufXSize = nBufXSize, nBufYSize = nBufYSize,
                                                resample=interpolation),proxy = FALSE)
  names(Stack_S2) <- names(S2_Bands)

  # adjust size to initial vector footprint without buffer
  # --> buffer is needed in order to ensure that extraction following
  # footprint of vector matches for images of different spatial resolution
  if (!is.null(path_vector)){
    Stack_S2 <- sf::st_crop(x = Stack_S2, y = st_bbox(st_read(dsn = path_vector,quiet = T)))
    # Stack_S2 <- stars::st_crop(x = Stack_S2, y = st_bbox(st_read(dsn = path_vector,quiet = T)),crop = TRUE)
  }
  return(Stack_S2)
}

#' This function reads a raster stack, and gets footprint as pixel coordinates or vector file as input
#' @param path_raster character. path for raster file
#' @param path_vector character. path for vector file
#' @param BBpix list. coordinates of pixels corresponding to a bounding box
#'
#' @return starsobj stars object corresponding to raster or raster subset
#'
#' @importFrom stars read_stars
#' @importFrom sf st_bbox st_read st_crop
#' @export
read_raster <- function(path_raster, path_vector = NULL, BBpix = NULL){
  # get bounding box corresponding to footprint of image or image subset
  if (is.null(BBpix)){
    BB_XYcoords <- get_BB(path_raster = path_raster,
                          path_vector = path_vector, Buffer = 0)
  } else {
    BB_XYcoords <- BBpix
  }
  # prepare reading data for extent defined by bounding box
  nXOff <- BB_XYcoords$UL$col
  nYOff <- BB_XYcoords$UL$row
  nXSize <- BB_XYcoords$UR$col-BB_XYcoords$UL$col+1
  nYSize <- BB_XYcoords$LR$row-BB_XYcoords$UR$row+1
  nBufXSize <- nXSize
  nBufYSize <- nYSize
  starsobj <- stars::read_stars(path_raster,
                                RasterIO = list(nXOff = nXOff, nYOff = nYOff,
                                                nXSize = nXSize, nYSize = nYSize,
                                                nBufXSize = nBufXSize, nBufYSize = nBufYSize),
                                proxy = FALSE)
  return(starsobj)
}

#' This function reprojects a shapefile and saves reprojected shapefile
#'
#' @param path_vector_init character. path for a shapefile to be reprojected
#' @param newprojection character. projection to be applied to path_vector_init
#' @param path_vector_reproj character. path for the reprojected shapefile
#'
#' @return path_vector character. path of the shapefile
#' - path_vector_init if the vector did not need reprojection
#' - path_vector_reproj if the vector needed reprojection
#'
#' @importFrom rgdal readOGR writeOGR
#' @importFrom sp spTransform
#' @importFrom raster projection
#' @export
reproject_shp = function(path_vector_init,newprojection,path_vector_reproj){

  dir_vector_init <- dirname(path_vector_init)
  name_vector_init <- file_path_sans_ext(basename(path_vector_init))
  vector_init_OGR <- rgdal::readOGR(dir_vector_init,name_vector_init,verbose = FALSE)
  vector_init_proj <- raster::projection(vector_init_OGR)

  if (!vector_init_proj==newprojection){
    dir_vector_reproj <- dirname(path_vector_reproj)
    name_vector_reproj <- file_path_sans_ext(basename(path_vector_reproj))
    vector_reproj <- sp::spTransform(vector_init_OGR, newprojection)
    rgdal::writeOGR(obj = vector_reproj, dsn = dir_vector_reproj,layer = name_vector_reproj, driver="ESRI Shapefile",overwrite_layer = TRUE)
    path_vector <- path_vector_reproj
  } else {
    path_vector <- path_vector_init
  }
  return(path_vector)
}

#' This function saves cloud masks.
#' 'CloudMask_Binary' is default binary mask with 0 where clouds are detected and 1 for clean pixels
#' 'CloudMask_RAW' is the original cloud layer produced by atmospheric correction algorithm
#' --> may be useful to refine cloud mask
#'
#' @param S2_stars list. stars object containing raster data. Can be produced with function extract_from_S2_L2A
#' @param Cloud_path character.
#' @param S2source character.
#' @param SaveRaw boolean. should the original cloud mask layer be saved?
#'
#' @return list of CloudMasks (binary mask, and raw mask if required)
#' @importFrom stars write_stars
#' @export
save_cloud_s2 <- function(S2_stars, Cloud_path, S2source = 'SAFE',SaveRaw = FALSE){

  # Save cloud mask
  if (SaveRaw==TRUE){
    Cloudraw <- file.path(Cloud_path,'CloudMask_RAW')
    WhichCloud <- which(names(S2_stars)=="Cloud")
    stars::write_stars(S2_stars, dsn=Cloudraw,layer=WhichCloud, driver =  "ENVI",type='Byte')
  } else {
    Cloudraw <- NULL
  }
  # Save cloud mask as in biodivMapR (0 = clouds, 1 = pixel ok)
  cloudmask <- S2_stars[['Cloud']]
  if (S2source=='SAFE' | S2source=='THEIA'){
    Cloudy <- which(cloudmask>0)
    Sunny <- which(cloudmask==0)
  } else if (S2source=='LaSRC'){
    Cloudy <- which(is.na(cloudmask))
    Sunny <- which(cloudmask==1)
  }
  cloudmask[Cloudy] <- 0
  cloudmask[Sunny] <- 1
  S2_stars[['Cloud']] <- cloudmask
  Cloudbin <- file.path(Cloud_path,'CloudMask_Binary')
  stars::write_stars(S2_stars, dsn=Cloudbin,layer=WhichCloud, driver =  "ENVI",type='Byte',overwrite = TRUE)
  CloudMasks <- list('BinaryMask' = Cloudbin, 'RawMask' = Cloudraw)

  return(CloudMasks)
}

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
#'
#' @return None
#' @importFrom stars write_stars
#' @export
save_reflectance_s2 <- function(S2_stars, Refl_path, Format='ENVI_BIL',datatype='INT2S',
                                S2Sat = NULL, tile_S2 = NULL, dateAcq_S2 = NULL, MTD = NULL){
  # identify if S2A or S2B, if possible
  s2mission <- check_S2mission(S2Sat = S2Sat, tile_S2 = tile_S2, dateAcq_S2 = dateAcq_S2)
  # define central wavelength corresponding to each spectral band
  if (s2mission=='2A'){
    WL_s2 <- list("B02"=496.6, "B03"=560.0, "B04"=664.5,
                  "B05"=703.9, "B06"=740.2, "B07"=782.5, "B08"=835.1,
                  "B8A"=864.8, "B11"=1613.7, "B12"=2202.4)
  } else if (s2mission=='2B'){
    WL_s2 <- list("B02"=492.1, "B03"=559.0, "B04"=665.0,
                  "B05"=703.8, "B06"=739.1, "B07"=779.7, "B08"=833.0,
                  "B8A"=864.0, "B11"=1610.4, "B12"=2185.7)
  }
  if (s2mission=='2A'){
    sensor <- 'Sentinel_2A'
  } else if (s2mission=='2B'){
    sensor <- 'Sentinel_2B'
  }
  # identify where spectral bands are in the stars object
  Stars_Spectral <- list()
  Stars_Spectral$bandname <- names(S2_stars)[which(!names(S2_stars)=='Cloud')]
  Stars_Spectral$wavelength <- WL_s2[Stars_Spectral$bandname]

  SortedWL <- names(WL_s2)
  Reorder <- match(SortedWL,Stars_Spectral$bandname)
  Elim <- which(is.na(Reorder))
  if (length(Elim)>0){
    Reorder <- Reorder[-Elim]
  }
  S2_stars <- S2_stars[Reorder]
  Stars_Spectral$bandname <- Stars_Spectral$bandname[Reorder]
  Stars_Spectral$wavelength <- Stars_Spectral$wavelength[Reorder]
  write_Stack_S2(Stars_S2 = S2_stars, Stars_Spectral = Stars_Spectral, Refl_path = Refl_path,
                 Format = Format, datatype = datatype,sensor=sensor)
  # save metadata file as well if available
  if (!is.null(MTD)){
    if (file.exists(MTD)){
      file.copy(from = MTD, to = file.path(dirname(Refl_path), basename(MTD)), overwrite = TRUE)
    }
  }
  return(invisible())
}

#' ENVI functions
#'
#' based on https://github.com/cran/hyperSpec/blob/master/R/read.ENVI.R
#' added wavelength, fwhm, ... to header reading
#' Title
#'
#' @param x character.
#' @param separator character
#' @param trim.blank boolean.
#'
#' @return list.
#' @export
split_line <- function(x, separator, trim.blank = TRUE) {
  tmp <- regexpr(separator, x)
  key <- substr(x, 1, tmp - 1)
  value <- substr(x, tmp + 1, nchar(x))
  if (trim.blank) {
    blank.pattern <- "^[[:blank:]]*([^[:blank:]]+.*[^[:blank:]]+)[[:blank:]]*$"
    key <- sub(blank.pattern, "\\1", key)
    value <- sub(blank.pattern, "\\1", value)
  }
  value <- as.list(value)
  names(value) <- key
  return(value)
}

#' save raster footprint as vector file
#'
#' @param path_raster character. path for a raster file
#' @param path_vector character. path for a vector file
#' @param driver character. driver for vector
#'
#' @return None
#' @importFrom raster raster extent
#' @importFrom sf st_as_sf st_write
#' @export
vectorize_raster_extent <- function(path_raster, path_vector, driver="ESRI Shapefile") {
  rast <- raster(path_raster)
  e <- extent(rast)
  # coerce to a SpatialPolygons object
  p <- as(e, 'SpatialPolygons')
  projection(p) <- projection(rast)
  p <- sf::st_as_sf(p)
  sf::st_write(obj = p, path_vector, driver=driver)  # create to a shapefile
  return(invisible())
}

#' writes ENVI hdr file
#'
#' @param HDR content to be written
#' @param HDRpath Path of the hdr file
#'
#' @return None
#' @importFrom stringr str_count
#' @export
write_ENVI_header <- function(HDR, HDRpath) {
  h <- lapply(HDR, function(x) {
    if (length(x) > 1 || (is.character(x) && str_count(x, "\\w+") > 1)) {
      x <- paste0("{", paste(x, collapse = ","), "}")
    }
    # convert last numerics
    x <- as.character(x)
  })
  writeLines(c("ENVI", paste(names(HDR), h, sep = " = ")), con = HDRpath)
  return(invisible())
}

#' This function writes a raster Stack object into a ENVI raster file
#'
#' @param StackObj list. raster stack
#' @param StackPath character. path where to store the stack
#' @param Bands list. should include 'bandname', and if possible 'wavelength'
#' @param datatype character. should be INT2S or FLT4S for example
#' @param sensor character. Name of the sensor used to acquire the image
#'
#' @return None
#' @importFrom utils read.table
#' @export
write_rasterStack_ENVI <- function(StackObj,StackPath,Bands,datatype='INT2S',sensor='Unknown'){

  r <- raster::writeRaster(x = StackObj, filename = StackPath, format = "EHdr", overwrite = TRUE, datatype = datatype)
  raster::hdr(r, format = "ENVI")
  # Edit HDR file to add metadata
  HDR <- read_ENVI_header(get_HDR_name(StackPath))
  HDR$`band names` <- Bands$bandname
  if (length(Bands$wavelength)==length(Bands$bandname)){
    HDR$wavelength <- Bands$wavelength
  } else {
    HDR$wavelength <- NULL
  }
  HDR$`default stretch` <- '0.000000 1000.000000 linear'
  HDR$`z plot range` <- NULL
  HDR$`data ignore value` <- '-Inf'
  HDR$`coordinate system string` <- read.table(paste(StackPath, ".prj", sep = ""))
  proj <- strsplit(x=strsplit(x =projection(StackObj),split = ' ' )[[1]][1],split = '=')[[1]][2]
  zone <- strsplit(x=strsplit(x =projection(StackObj),split = ' ' )[[1]][2],split = '=')[[1]][2]
  datum <- strsplit(x=strsplit(x =projection(StackObj),split = ' ' )[[1]][3],split = '=')[[1]][2]
  oldProj <- HDR$`map info`
  NewProj <- gsub(pattern = 'projection',replacement = proj,x = oldProj)
  NewProj <- paste(NewProj,zone,datum,sep = ', ')
  HDR$`map info` <- NewProj
  HDR$`sensor type` <- sensor
  write_ENVI_header(HDR = HDR,HDRpath = get_HDR_name(StackPath))

  # remove unnecessary files
  File2Remove <- paste(StackPath, ".aux.xml", sep = "")
  file.remove(File2Remove)
  File2Remove <- paste(StackPath, ".prj", sep = "")
  file.remove(File2Remove)
  File2Remove <- paste(StackPath, ".stx", sep = "")
  file.remove(File2Remove)
  return(invisible())
}

#' This function writes a stars object into a raster file
#'
#' @param Stars_S2 list. stars object containing raster data. Can be produced with function Crop_n_resample_S2
#' @param Stars_Spectral list. band name to be saved in the stack and spectral bands corresponding to the image
#' @param Refl_path character. path where to store the image
#' @param Format character. default = ENVI BIL. otheerwise use gdal drivers
#' @param datatype character. should be INT2S or FLT4S for example
#' @param sensor character. Name of the sensor used to acquire the image
#'
#' @return None
#' @export
write_Stack_S2 <- function(Stars_S2, Stars_Spectral, Refl_path, Format='ENVI_BIL',datatype='INT2S',sensor='Unknown'){

  Stars_S2 <- as(object = Stars_S2,Class = 'Raster')
  names(Stars_S2) <- Stars_Spectral$bandname
  if (Format == 'ENVI_BIL'){
    write_rasterStack_ENVI(StackObj=Stars_S2,StackPath = Refl_path,Bands=Stars_Spectral,datatype=datatype,sensor=sensor)
  } else {
    r <- raster::writeRaster(x = Stars_S2, filename = Refl_path, format = Format, overwrite = TRUE)
  }
  return(invisible())
}

#' #' This function crops and resamples S2 bands based on OGR from vector file
#' #' only resamples if CropExtent= FALSE
#' #' only crops if resample = FALSE
#' #'
#' #' @param S2_Bands list. list of S2 bands obtained from get_S2_bands
#' #' @param Vector_path path for a vector file
#' #' @param resample boolean. resample 20m bands to 10m if TRUE
#' #' @param interpolation character. method for resampling. default = 'bilinear'
#' #'
#' #' @return FullStack list. contains stack of S2 bands
#' #'
#' #' @importFrom raster raster
#' #' @export
#' Crop_n_resample_S2 <- function(S2_Bands,Vector_path = NULL, S2source = 'SAFE',
#'                                resampleRaster = FALSE, interpolation = 'bilinear'){
#'
#'   # if SAFE L2A product obtained from PEPS, SCIHUB, or
#'   if (S2source == 'SAFE') {
#'     # get bounding box for 10 and 20 m bands
#'     # if 10m bands available
#'     BB_XYcoordinates_10m <- list()
#'     if (length(S2_Bands$S2Bands_10m)>0){
#'       # get extent corresponding to CropExtent
#'       if (!is.null(Vector_path)){
#'         # get raster coordinates corresponding to CropExtent
#'         Raster <- raster::raster(S2_Bands$S2Bands_10m[[1]])
#'         BB_XYcoordinates_10m <- get_BB_from_OGR(Raster = Raster,Vector = Vector_path,
#'                                                 Buffer = 50)
#'       } else if (is.null(Vector_path)){
#'         # get raster coordinates corresponding to Full image
#'         Raster <- raster::raster(S2_Bands$S2Bands_10m[[1]])
#'         BB_XYcoordinates_10m[['UL']] <- data.frame('row'=1,'col'=1)
#'         BB_XYcoordinates_10m[['UR']] <- data.frame('row'=1,'col'=dim(Raster)[2])
#'         BB_XYcoordinates_10m[['LL']] <- data.frame('row'=dim(Raster)[1],'col'=1)
#'         BB_XYcoordinates_10m[['LR']] <- data.frame('row'=dim(Raster)[1],'col'=dim(Raster)[2])
#'       }
#'     }
#'     # get extent corresponding to CropExtent
#'     if (!is.null(Vector_path)){
#'       # get raster coordinates corresponding to CropExtent
#'       Raster <- raster::raster(S2_Bands$S2Bands_20m[[1]])
#'       BB_XYcoordinates_20m <- get_BB_from_OGR(Raster = Raster, Vector = Vector_path,
#'                                               Buffer = 50)
#'     } else if (is.null(Vector_path)){
#'       # get raster coordinates corresponding to Full image
#'
#'       Raster <- raster::raster(S2_Bands$S2Bands_20m[[1]])
#'       BB_XYcoordinates_20m <- list()
#'       BB_XYcoordinates_20m[['UL']] <- data.frame('row'=1,'col'=1)
#'       BB_XYcoordinates_20m[['UR']] <- data.frame('row'=1,'col'=dim(Raster)[2])
#'       BB_XYcoordinates_20m[['LL']] <- data.frame('row'=dim(Raster)[1],'col'=1)
#'       BB_XYcoordinates_20m[['LR']] <- data.frame('row'=dim(Raster)[1],'col'=dim(Raster)[2])
#'     }
#'
#'     # prepare reading data for extent defined by bounding box
#'     nXOff <- BB_XYcoordinates_20m$UL$col
#'     nYOff <- BB_XYcoordinates_20m$UL$row
#'     nXSize <- BB_XYcoordinates_20m$UR$col-BB_XYcoordinates_20m$UL$col+1
#'     nYSize <- BB_XYcoordinates_20m$LR$row-BB_XYcoordinates_20m$UR$row+1
#'     # extract data and resample if required
#'     if (resampleRaster==FALSE){
#'       nBufXSize <- nXSize
#'       nBufYSize <- nYSize
#'       Stack_20m <- stars::read_stars(S2_Bands$S2Bands_20m,
#'                                      RasterIO = list(nXOff = nXOff, nYOff = nYOff, nXSize = nXSize, nYSize = nYSize,
#'                                                      nBufXSize = nBufXSize, nBufYSize = nBufYSize),proxy = FALSE)
#'     } else {
#'       nBufXSize <- 2*nXSize
#'       nBufYSize <- 2*nYSize
#'       Stack_20m <- stars::read_stars(S2_Bands$S2Bands_20m,
#'                                      RasterIO = list(nXOff = nXOff, nYOff = nYOff, nXSize = nXSize, nYSize = nYSize,
#'                                                      nBufXSize = nBufXSize, nBufYSize = nBufYSize,resample=interpolation),proxy = FALSE)
#'     }
#'     names(Stack_20m) <- names(S2_Bands$S2Bands_20m)
#'
#'     # if 10m bands
#'     if (length(S2_Bands$S2Bands_10m)>0){
#'       # prepare reading data for extent defined by bounding box
#'       nXOff <- BB_XYcoordinates_10m$UL$col
#'       nYOff <- BB_XYcoordinates_10m$UL$row
#'       nXSize <- BB_XYcoordinates_10m$UR$col-BB_XYcoordinates_10m$UL$col+1
#'       nYSize <- BB_XYcoordinates_10m$LR$row-BB_XYcoordinates_10m$UR$row+1
#'       # extract data and resample if required
#'       nBufXSize <- nXSize
#'       nBufYSize <- nYSize
#'       Stack_10m <- stars::read_stars(S2_Bands$S2Bands_10m,
#'                                      RasterIO = list(nXOff = nXOff, nYOff = nYOff,
#'                                                      nXSize = nXSize, nYSize = nYSize),proxy = FALSE)
#'       names(Stack_10m) <- names(S2_Bands$S2Bands_10m)
#'     }
#'
#'     # adjust size to initial vector footprint without buffer
#'     if (!is.null(Vector_path)){
#'       Stack_20m <- st_crop(x = Stack_20m, y = st_bbox(st_read(dsn = Vector_path,quiet = T)),crop = TRUE)
#'       if (length(S2_Bands$S2Bands_10m)>0){
#'         Stack_10m <- st_crop(x = Stack_10m, y = st_bbox(st_read(dsn = Vector_path,quiet = T)),crop = TRUE)
#'       }
#'     }
#'
#'     # get full stack
#'     FullStack <- Stack_20m
#'     if (length(S2_Bands$S2Bands_10m)>0){
#'       for (band10 in names(S2_Bands$S2Bands_10m)){
#'         FullStack[[band10]] <- Stack_10m[[band10]]
#'       }
#'     }
#'
#'   } else if (S2source =='LaSRC') {
#'     # get extent corresponding to CropExtent
#'     BB_XYcoordinates_10m <- list()
#'     if (!is.null(Vector_path)){
#'       # get raster coordinates corresponding to CropExtent
#'       # Raster <- raster::raster(S2_Bands$S2Bands_10m[[1]])
#'       Raster <- raster(S2_Bands$S2Bands_10m[[1]])
#'       BB_XYcoordinates_10m <- get_BB_from_OGR(Raster = Raster,Vector = Vector_path,
#'                                               Buffer = 50)
#'     } else if (is.null(Vector_path)){
#'       # get raster coordinates corresponding to Full image
#'       Raster <- raster::raster(S2_Bands$S2Bands_10m[[1]])
#'       BB_XYcoordinates_10m[['UL']] <- data.frame('row'=1,'col'=1)
#'       BB_XYcoordinates_10m[['UR']] <- data.frame('row'=1,'col'=dim(Raster)[2])
#'       BB_XYcoordinates_10m[['LL']] <- data.frame('row'=dim(Raster)[1],'col'=1)
#'       BB_XYcoordinates_10m[['LR']] <- data.frame('row'=dim(Raster)[1],'col'=dim(Raster)[2])
#'     }
#'
#'     # prepare reading data for extent defined by bounding box
#'     nXOff <- BB_XYcoordinates_10m$UL$col
#'     nYOff <- BB_XYcoordinates_10m$UL$row
#'     nXSize <- BB_XYcoordinates_10m$UR$col-BB_XYcoordinates_10m$UL$col+1
#'     nYSize <- BB_XYcoordinates_10m$LR$row-BB_XYcoordinates_10m$UR$row+1
#'     # extract data and resample if required
#'     #nBufXSize <- nXSize
#'     #nBufYSize <- nYSize
#'
#'     Stack_10m <- stars::read_stars(S2_Bands$S2Bands_10m,
#'                                    RasterIO = list(nXOff = nXOff, nYOff = nYOff,
#'                                                    nXSize = nXSize, nYSize = nYSize),
#'                                                    proxy = FALSE)
#'     names(Stack_10m) <- names(S2_Bands$S2Bands_10m)
#'
#'     # adjust size to initial vector footprint without buffer
#'     if (!is.null(Vector_path)){
#'         Stack_10m <- st_crop(x = Stack_10m, y = st_bbox(st_read(dsn = Vector_path,quiet = T)),crop = TRUE)
#'     }
#'     # get full stack
#'     FullStack <- Stack_10m
#'   }
#'   return(FullStack)
#' }


#' #' Short description of the function
#' #'
#' #' @param raster_stack Typeof(raster_stack).
#' #' @param cloud_mask character. Path of the mask corresponding to the image
#' #'
#' #' @return raster_mask
#' #' @export
#'
#' CloudMask <- function(raster_stack, cloud_mask){
#'   # mask 1/ based on cloud mask layer 2/ based on Band02 (blue) 3/ based on band08
#'   cloud_mask[cloud_mask != 0] <- NA
#'   raster_mask <- mask(raster_stack, cloud_mask)
#'   raster_mask[raster_stack$B02 >500 ] <- NA
#'   raster_mask[raster_stack$B08 <1500 ] <- NA
#'   return(raster_mask)
#' }
#'
#'
#' #' Short description of the function
#' #'
#' #' @param bands Typeof().
#' #' @param study_frame_buffer character. Path of the mask corresponding to the image
#' #'
#' #' @return bands_buffer_crop
#' #' @export
#' Crop <- function(bands, study_frame_buffer){
#'   bands_buffer_crop <- crop(bands, study_frame_buffer)
#'   return(bands_buffer_crop)
#' }
#'
#'
#' #' Short description of the function
#' #'
#' #' @param dirs
#' #' @param list_band
#' #' @param fileend
#' #'
#' #' @return bands_buffer_crop
#' #' @export
#' FindFiles <- function(dirs,list_band, fileend){
#'   pattern <- file.path(list_band,fileend)
#'   path_B <- list.files(dirs, recursive=TRUE, pattern = pattern, full.names =TRUE)
#'   raster_band <- raster(path_B)
#'   return(raster_band)
#' }
#'
#' #' Short description of the function
#' #'
#' #' @param buffer_crop
#' #'
#' #' @return To_10m
#' #' @export
#' To10m <- function(buffer_crop){
#'   To_10m <- disaggregate(buffer_crop,2, method='bilinear')
#'   return(To_10m)
#' }


#' #' Extract areas corresponding to vector layer from Time series
#' #' the Time series is expected to be a list of individual files, or a stack with ENVI BIL format
#' #' @param TimeSeriesPath character. vector containing path for each element of time series
#' #' @param Dates character. vector corresponding to the date of each element of time series
#' #' @param Vector OGR from vector layer
#' #' If columns are not named, 1st=row, 2nd=col.
#' #' @param MaxRAM numeric. Maximum memory use at block reading.
#' #' It constrains the maximum number rows of a block
#' #'
#' #' @return ExtractTS list. list of elements extractted from vector layer for each date of the TS
#' #' @importFrom progress progress_bar
#' #' @export
#' Extract_from_TS_ENVI <- function(TimeSeriesPath, Dates,Vector){
#'   # explore time series using tools developed for biodivMapR
#'   # first get coordinates of pixels corresponding to samples
#'   XY0 <- extract_pixels_coordinates.From.OGR(TimeSeriesPath[1],Vector)
#'   # add each polygon in the shapefile to the XY list
#'   nbPolygons <- 0
#'   XY <- list()
#'   for (ii in 1:length(XY0)){
#'     nbPolygons <- nbPolygons+1
#'     XY[[nbPolygons]] <- XY0[[ii]]
#'   }
#'   Name_Plot <- Vector$id
#'   ExtractTS <- list()
#'
#'   pb <- progress_bar$new(
#'     format = "Extracting vector layer from Time Series [:bar] :percent in :elapsedfull",
#'     total = length(TimeSeriesPath), clear = FALSE, width= 100)
#'
#'   dd <- 0
#'   for (TSind in TimeSeriesPath){
#'     dd <- dd+1
#'     pb$tick()
#'     ExtractTS[[Dates[dd]]] <- list()
#'     for (ip in 1:nbPolygons){
#'       # if only one polygon in the shapefile and if the polyon is not included in the Raster_SpectralSpecies
#'       if (length(XY[[ip]]$col)==0){
#'         # if list of individual plots provided
#'         if (length(Name_Plot)==nbPolygons){
#'           message(paste('Polygon named',Name_Plot[ip],'is out of the raster'))
#'           # Set name to NA
#'           Name_Plot[ip] <- NA
#'         }
#'       } else {
#'         ExtractTS[[Dates[[dd]]]][[Name_Plot[ip]]] <- extract.big_raster(TSind, XY[[ip]])
#'       }
#'     }
#'   }
#'   return(ExtractTS)
#' }

# # Extracts pixels coordinates from raster corresponding to an area defined by a vector
# # @param Path.Raster path for the raster file. !! BIL expected
# # @param OGR.Vector  OGR for the vector file obtained from readOGR
# # @return ColRow list of coordinates of pixels corresponding to each polygon in shp
# extract_pixels_coordinates.From.OGR = function(Path.Raster,OGR.Vector){
#   # read raster info
#   Raster <- raster(Path.Raster, band = 1)
#   # for each polygon or point in the shapefile
#   ColRow <- list()
#   # extract pixel coordinates from raster based on vector
#   if (OGR.Vector@class[1]=='SpatialPointsDataFrame'){
#     XY <- raster::cellFromXY (Raster,OGR.Vector)
#     ColRow[[1]] <- ind2sub(Raster,XY)
#
#   } else if  (OGR.Vector@class[1]=='SpatialPolygonsDataFrame'){
#     XY <- raster::cellFromPolygon (Raster,OGR.Vector)
#     for (i in 1:length(XY)){
#       ColRow[[i]] <- ind2sub(Raster,XY[[i]])
#     }
#   }
#   return(ColRow)
# }
#


#' #' Extract bands of sparse pixels in image data cube
#' #' @param ImPath character. Path to the image cube
#' #' @param rowcol matrix or data.frame with two columns: row, col.
#' #' If columns are not named, 1st=row, 2nd=col.
#' #' @param MaxRAM numeric. Maximum memory use at block reading.
#' #' It constrains the maximum number rows of a block
#' #'
#' #' @return matrix. Rows are corresponding to the samples, columns are the bands.
#' #' @importFrom raster brick
#' #' @import stars
#' #' @export
#' extract.big_raster <- function(ImPath, rowcol, MaxRAM=.50){
#'
#'   if(!is.data.frame(rowcol)){
#'     rowcol <- as.data.frame(rowcol)
#'   }
#'
#'   if(!all(c('row', 'col') %in% colnames(rowcol))){
#'     warning('Columns row,col not found in rowcol argument. The two first columns are considered as row, col respectively.')
#'     colnames(rowcol)[1:2]= c('row', 'col')
#'   }
#'
#'   metarast <- raster(ImPath)
#'   # updated raster package: do not use brick with 2D raster
#'   if (nbands(metarast)>1){
#'     rasterInfo <- raster::brick(ImPath)
#'   } else{
#'     rasterInfo <- metarast
#'   }
#'
#'   # nbytes = as.numeric(substring(dataType(rasterInfo), 4, 4))
#'   # stars converts automatically values to numeric
#'   nbytes <- 8
#'   ImgSizeGb <- prod(dim(rasterInfo))*nbytes/2^30
#'   LineSizeGb <- prod(dim(rasterInfo)[2:3])*nbytes/2^30
#'   LinesBlock <- floor(MaxRAM/LineSizeGb)
#'   rowcol$rowInBlock <- ((rowcol$row-1) %% LinesBlock)+1  # row in block number
#'   rowcol$block <- floor((rowcol$row-1)/LinesBlock)+1  # block number
#'   rowcol$sampleIndex <- 1:nrow(rowcol)  # sample index to reorder result
#'
#'   sampleList = lapply(unique(rowcol$block), function(iblock){
#'     rc <- rowcol[rowcol$block==iblock,]
#'     rr <- range(rc$row)
#'     nYSize <- diff(rr)+1
#'     nXSize <- max(rc$col)
#'     # stars data cube dimension order is x*y*band
#'     ipix_stars <- (rc$rowInBlock-min(rc$rowInBlock))*nXSize+rc$col
#'     # get driver
#'     driver <- attr(rgdal::GDALinfo(ImPath,returnStats = FALSE), 'driver')
#'     values <- read_stars(ImPath, RasterIO =list(nXSize=nXSize, nYOff=rr[1], nYSize=nYSize),proxy = FALSE, driver=driver)[[1]]
#'     values <- matrix(values, nrow=nYSize*nXSize)
#'     res <- cbind(rc$sampleIndex, values[ipix_stars, ])
#'     rm('values')
#'     gc()
#'     return(res)
#'   })
#'
#'   samples = do.call(rbind, sampleList)
#'   samples = samples[order(samples[,1]),2:ncol(samples)]
#'
#'   return(samples)
#' }
