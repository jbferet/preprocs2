#' convert image coordinates from index to X-Y
#'
#' @param Raster image raster object
#' @param Image_Index coordinates corresponding to the raster
#' @return list
#' 
ind2sub <- function(Raster, Image_Index) {
  c <- ((Image_Index - 1) %% ncol(Raster)) + 1
  r <- floor((Image_Index - 1) / ncol(Raster)) + 1
  my_list <- list("col" = c, "row" = r)
  return(my_list)
}
