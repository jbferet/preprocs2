#' This function adjusts coordinates of the bounding box for two sets of rasters
#' corresponding to 10 m and 20 m S2 bands
#'
#' @param BB_XYcoords list. includes pixel coordinates for UL, LL, UR, LR corners
#' of S2Bands_10m and S2Bands_20m
#'
#' @return BB_XYcoords
#' @export

adjust_BB_XYcoords <- function(BB_XYcoords){
  # adjust row cols for matching 10 m and 20 m bands: adjust corner size to
  # maximize extent
  if (!is.null(BB_XYcoords$S2Bands_10m) & !is.null(BB_XYcoords$S2Bands_20m)){
    # adjust corners:
    # upper left
    lb1 <- 2*(BB_XYcoords$S2Bands_20m$UL-1)+1
    BB_XYcoords$S2Bands_10m$UL$row <- min(BB_XYcoords$S2Bands_10m$UL$row, lb1$row)
    BB_XYcoords$S2Bands_10m$UL$col <- min(BB_XYcoords$S2Bands_10m$UL$col, lb1$col)
    lb2 <- floor((BB_XYcoords$S2Bands_10m$UL+1)/2)
    BB_XYcoords$S2Bands_20m$UL$row <- min(BB_XYcoords$S2Bands_20m$UL$row, lb2$row)
    BB_XYcoords$S2Bands_20m$UL$col <- min(BB_XYcoords$S2Bands_20m$UL$col, lb2$col)
    # upper right
    lb1 <- 2*(BB_XYcoords$S2Bands_20m$UR-1)+1
    BB_XYcoords$S2Bands_10m$UR$row <- min(BB_XYcoords$S2Bands_10m$UR$row, lb1$row)
    BB_XYcoords$S2Bands_10m$UR$col <- max(BB_XYcoords$S2Bands_10m$UR$col, lb1$col)
    lb2 <- floor((BB_XYcoords$S2Bands_10m$UR+1)/2)
    BB_XYcoords$S2Bands_20m$UR$row <- min(BB_XYcoords$S2Bands_20m$UR$row, lb2$row)
    BB_XYcoords$S2Bands_20m$UR$col <- max(BB_XYcoords$S2Bands_20m$UR$col, lb2$col)
    # lower left
    lb1 <- 2*(BB_XYcoords$S2Bands_20m$LL-1)+1
    BB_XYcoords$S2Bands_10m$LL$row <- max(BB_XYcoords$S2Bands_10m$LL$row, lb1$row)
    BB_XYcoords$S2Bands_10m$LL$col <- min(BB_XYcoords$S2Bands_10m$LL$col, lb1$col)
    lb2 <- floor((BB_XYcoords$S2Bands_10m$LL+1)/2)
    BB_XYcoords$S2Bands_20m$LL$row <- max(BB_XYcoords$S2Bands_20m$LL$row, lb2$row)
    BB_XYcoords$S2Bands_20m$LL$col <- min(BB_XYcoords$S2Bands_20m$LL$col, lb2$col)
    # lower right
    lb1 <- 2*(BB_XYcoords$S2Bands_20m$LR-1)+1
    BB_XYcoords$S2Bands_10m$LR$row <- max(BB_XYcoords$S2Bands_10m$LR$row, lb1$row)
    BB_XYcoords$S2Bands_10m$LR$col <- min(BB_XYcoords$S2Bands_10m$LR$col, lb1$col)
    lb2 <- floor((BB_XYcoords$S2Bands_10m$LR+1)/2)
    BB_XYcoords$S2Bands_20m$LR$row <- max(BB_XYcoords$S2Bands_20m$LR$row, lb2$row)
    BB_XYcoords$S2Bands_20m$LR$col <- min(BB_XYcoords$S2Bands_20m$LR$col, lb2$col)
  }
  return(BB_XYcoords)
}
