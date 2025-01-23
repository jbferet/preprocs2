#' This function saves cloud masks.
#' 'CloudMask_Binary' is default binary mask with 0 where clouds are detected and 1 for clean pixels
#' 'CloudMask_RAW' is the original cloud layer produced by atmospheric correction algorithm
#' --> may be useful to refine cloud mask
#'
#' @param S2_stars list. stars object containing raster data. Can be produced with function extract_from_S2_L2A
#' @param Cloud_path character.
#' @param S2source character.
#' @param footprint character. path for vector file defining footprint of interest in the image
#' @param SaveRaw boolean. should the original cloud mask layer be saved?
#' @param MaxChunk numeric. Size of individual chunks to be written (in Mb)
#'
#' @return list of CloudMasks (binary mask, and raw mask if required)
#' @importFrom stars write_stars read_stars
#' @importFrom terra rast vect rasterize
#' @export
save_cloud_s2 <- function(S2_stars, Cloud_path, S2source = 'SAFE',
                          footprint = NULL, SaveRaw = FALSE,MaxChunk = 256){
  
  WhichCloud <- which(names(S2_stars$attr)=="Cloud")
  # Save cloud mask
  if (SaveRaw==TRUE){
    Cloudraw <- file.path(Cloud_path,'CloudMask_RAW')
    obj <- stars::read_stars(S2_stars$attr[WhichCloud],proxy = TRUE)
    SizeObj <- dim(obj)[1]*dim(obj)[2]/(1024**2)
    nbChunks <- ceiling(SizeObj/MaxChunk)
    stars::write_stars(obj,
                       dsn=Cloudraw,
                       driver =  "ENVI",
                       type='Byte',
                       chunk_size = c(dim(obj)[1], dim(obj)[2]/nbChunks),
                       progress = TRUE)
  } else {
    Cloudraw <- NULL
  }
  # Save cloud mask as in biodivMapR (0 = clouds, 1 = pixel ok)
  cloudmask <- stars::read_stars(S2_stars$attr[WhichCloud],proxy = FALSE)
  if (S2source=='SAFE' | S2source=='THEIA' | S2source == 'GEE' | S2source == 'RAW'){
    Cloudy <- which(cloudmask[[1]]>0)
    Sunny <- which(cloudmask[[1]]==0)
  } else if (S2source=='LaSRC'){
    Cloudy <- which(is.na(cloudmask[[1]]))
    Sunny <- which(cloudmask[[1]]==1)
  }
  if (length(Cloudy)>0) cloudmask[[1]][Cloudy] <- 0
  if (length(Sunny)>0) cloudmask[[1]][Sunny] <- 1
  Cloudbin <- file.path(Cloud_path,'CloudMask_Binary')
  stars::write_stars(obj = cloudmask,
                     dsn = Cloudbin,
                     driver = "ENVI",
                     type = 'Byte',
                     overwrite = TRUE)
  # delete temporary file
  file.remove(S2_stars$attr[WhichCloud])
  if (file.exists(paste(S2_stars$attr[WhichCloud],'.hdr',sep=''))) file.remove(paste(S2_stars$attr[WhichCloud],'.hdr',sep=''))
  
  # apply footprint to binary mask
  if (!is.null(footprint)){
    footprint_Vect <- terra::vect(footprint)
    footprint_Rast <- terra::rast(Cloudbin)
    fp <- terra::rasterize(x = footprint_Vect,
                           y = footprint_Rast)
    fp[,,1] <- fp[,,1] * footprint_Rast[,,1]
    fp[,,1][which(is.na(fp[,,1]))] <- 0
    terra::writeRaster(x = fp,
                       filename = Cloudbin,
                       overwrite=TRUE,
                       wopt= list(gdal=c("COMPRESS=NONE"),
                                  datatype='INT1U',
                                  filetype = 'ENVI'))
  }
  CloudMasks <- list('BinaryMask' = Cloudbin, 'RawMask' = Cloudraw)
  rm(list = c('footprint_Rast', 'footprint_Vect', 'fp', 'cloudmask'))
  gc()
  return(CloudMasks)
}
