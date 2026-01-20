#' reads S2 data from SAFE data structure, crops if vector file provided
#' and writes reflectance data as well as cloud mask
#'
#' @param safe_path character. path for unalterated L2a SAFE product
#' @param path_aoi character. path for vector file defining aoi included in SAFE
#' @param resolution numeric. buffer applied to vector file (in meters)
#' @param output_dir character. path for output: reflectance, mask, metadata
#'
#' @return list. path for reflectance, cloud mask and metadata
#'
#' @importFrom tools file_ext
#' @export

extract_from_safe <- function(safe_path, path_aoi = NULL, resolution = 10,
                              output_dir){

  if (!tools::file_ext(safe_path)=='SAFE')
    stop('please provide SAFE L2A product as input')
  S2obj <- extract_from_S2_L2A(Path_dir_S2 = safe_path,
                               path_vector = path_aoi,
                               S2source = 'SAFE',
                               resolution = resolution)
  # create specific result directory corresponding to granule name
  output_dir_s2 <- file.path(output_dir,basename(S2obj$S2_Bands$GRANULE))
  # Write CLOUD MASK
  Cloud_path <- file.path(output_dir_s2,'CloudMask')
  dir.create(path = Cloud_path, showWarnings = FALSE, recursive = TRUE)
  # Filename for cloud mask
  cloudmasks <- save_s2_cloud(S2_stars = S2obj$S2_Stack,
                              Cloud_path = Cloud_path,
                              S2source = 'SAFE',
                              SaveRaw = T)
  # Write REFLECTANCE
  Refl_dir <- file.path(output_dir_s2,'Reflectance')
  dir.create(path = Refl_dir,showWarnings = FALSE,recursive = TRUE)
  # filename for Reflectance
  Refl_path <- file.path(Refl_dir,
                         paste0(basename(S2obj$S2_Bands$GRANULE),'_Refl'))

  # Save Reflectance file as ENVI image with BIL interleaves
  # metadata files are important to account for offset applied on S2 L2A products
  tile_S2 <- get_tile(S2obj$S2_Bands$GRANULE)
  stac_info <- list('provider' = 'esa')
  dateAcq_S2 <- get_dateAcq(S2product = dirname(dirname(S2obj$S2_Bands$GRANULE)),
                            stac_info = stac_info)
  # dateAcq_S2 <- get_dateAcq(S2product = dirname(dirname(S2obj$S2_Bands$GRANULE)))
  safename <- basename(safe_path)
  sensor <- strsplit(x = safename, split = '_')[[1]][1]
  s2mission <- gsub(pattern = 'S', x = sensor, replacement = '')
  save_s2_reflectance(S2_stars = S2obj$S2_Stack, Refl_path = Refl_path,
                      tile_S2 = tile_S2, s2mission = s2mission,
                      dateAcq_S2 = dateAcq_S2,
                      Format = 'ENVI', datatype = 'Int16',
                      MTD = S2obj$S2_Bands$metadata,
                      MTD_MSI = S2obj$S2_Bands$metadata_MSI)
  outlist <- list('Refl_path' = Refl_path,
                  'cloudmasks' = cloudmasks,
                  'MTD_TL' = S2obj$S2_Bands$metadata,
                  'MTD_MSIL2A' = S2obj$S2_Bands$metadata_MSI)
  return(outlist)
}

