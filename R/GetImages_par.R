#' function to call in parallel to get images from CDSE
#'
#' @param fileName character.
#' @param time_range list. define period 'from' / 'to' day of acquisition
#' @param bbox numeric. maximum cloud cover over tile
#' @param collection character. collection targeted with CDSE
#' @param script character. path for script
#' @param format character.
#' @param mosaicking_order character.
#' @param resolution numeric.
#' @param buffer numeric.
#' @param mask boolean.
#' @param token list.
#' @param p list.
#'
#' @return rast_out
#' @importFrom CDSE GetImageByTimerange
#' @importFrom terra writeRaster
#' @export

GetImages_par <- function(fileName, time_range, bbox, collection, script,
                          format, mosaicking_order, resolution, buffer, mask,
                          token, p = NULL){
  rast_out <- CDSE::GetImageByTimerange(time_range = time_range, bbox = bbox,
                                        collection = collection, script = script,
                                        format = format, resolution = resolution,
                                        mosaicking_order = mosaicking_order,
                                        buffer = buffer, mask = mask, token = token)
  for (j in 1:4)
    terra::writeRaster(x = rast_out[[j]], filename = fileName[[j]])
  if (!is.null(p)) p()
  return(rast_out)
}
