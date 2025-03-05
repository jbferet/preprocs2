#' identifies S2 tile fullly including a plot
#'
#' @param aoi sf object
#' @param S2_footprint sf object
#'
#' @return buffer size
#' @importFrom sf st_intersects st_covered_by st_area st_intersection
#' @importFrom tibble as_tibble
#' @export
#'
get_plot_tile <- function(aoi, S2_footprint){
  # get intersecting S2 tiles
  if (! sf::st_crs(S2_footprint) == sf::st_crs(aoi))
    aoi <- sf::st_transform(x = aoi, crs = sf::st_crs(S2_footprint))

  intersection <- sf::st_intersects(x = aoi, y = S2_footprint$geometry)
  subs2 <- list('Name' = S2_footprint$Name[intersection[[1]]],
                'geometry' = S2_footprint$geometry[intersection[[1]]])
  # check which tiles contain the full plot
  iscovered <- list()
  for (ii in seq_len(length(subs2$Name))){
    iscovered[[subs2$Name[ii]]] <- sf::st_covered_by(x = aoi, y = subs2$geometry[[ii]])[[1]]
  }
  tileselect <- names(which(unlist(iscovered)==1))
  # create a warning / error when is not fully included
  if (is.null(tileselect)){
    message('warning : a polygon is not fully included in a tile')
    message('please specify the tile of interest or ajust aoi')
    message('selecting tile with maximum overlapping area')
    int <- tibble::as_tibble(sf::st_intersection(x = aoi, y = subs2$geometry))
    int$area <- sf::st_area(int$geometry)
    sel <- which(int$area==max(int$area))
    iscovered[[sel]] <- 1
    tileselect <- names(which(unlist(iscovered)==1))
  }
  return(tileselect)
}
