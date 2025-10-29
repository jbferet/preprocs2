#' return S2 tiles intersecting with the footprint of a vector file
#'
#' @param aoi_path character. path for vector file defining aoi
#' @param bbox bbox
#' @param output_dir character. path for output directory
#' @param overwrite boolean. if needs overwrite
#'
#' @return list of path corresponding to output files
#' @importFrom sf st_write st_bbox st_read st_crs st_transform
#' @export

get_footprint_from_bbox_aoi <- function(aoi_path = NULL, bbox = NULL,
                                        output_dir, overwrite = TRUE){
  input_dir <- NULL
  # if bbox not provided
  if (is.null(bbox) | ! inherits(x = bbox, 'bbox')){
    if (is.null(aoi_path)){
      message('provide path for valid vector file as "aoi_path", or "bbox" sf object as input for "get_s2_raster"')
      stop_quietly()
    } else if (file.exists(aoi_path)){
      bbox <- sf::st_bbox(sf::st_read(aoi_path, quiet= TRUE))
      input_dir <- dirname(aoi_path)
    }
  # if bbox provided
  } else if (inherits(x = bbox, 'bbox')){
    if (is.null(aoi_path)){
      input_dir <- output_dir
    } else if (file.exists(aoi_path)){
      message('both "aoi_path" and "bbox" defined as input for "get_s2_raster"')
      message('"aoi_path" will be used')
      bbox <- sf::st_bbox(sf::st_read(aoi_path, quiet= TRUE))
      input_dir <- dirname(aoi_path)
    } else {
      input_dir <- output_dir
    }
  }
  if (is.null(input_dir)){
    message('Please provide valid "aoi_path" and "bbox" as input for "get_s2_raster"')
    stop_quietly()
  }
  dir.create(path = input_dir, showWarnings = FALSE, recursive = TRUE)
  if (!is.null(aoi_path)){
    crs_final <- sf::st_crs(sf::st_read(aoi_path, quiet= TRUE))
  } else if (!is.null(bbox)){
    crs_final <- sf::st_crs(bbox)
  }
  # save bbox as individual plot
  bbox <- bbox |>
    sf::st_transform(crs_final)
  plots <- list('001' = bbox_to_poly(x = bbox, crs = crs_final))
  plots <- lapply(X = plots, FUN = sf::st_zm)
  footprint_path <- file.path(input_dir,'aoi_bbox.GPKG')
  if (!file.exists(footprint_path) | overwrite == TRUE)
    sf::st_write(obj = plots$`001`, dsn = footprint_path,
                 driver = 'GPKG', delete_dsn = TRUE, quiet = TRUE)

  return(list('footprint_path' = footprint_path, 'plots' = plots))
}
