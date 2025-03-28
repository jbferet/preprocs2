% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_s2_tiles.R
\name{get_s2_tiles}
\alias{get_s2_tiles}
\title{return S2 tiles intersecting with the footprint of a vector file}
\usage{
get_s2_tiles(
  plots,
  dsn_bbox,
  output_dir,
  siteName = NULL,
  overwrite = T,
  path_S2tilinggrid = "Sentinel-2_tiling_grid.kml"
)
}
\arguments{
\item{plots}{list. list of polygons}

\item{dsn_bbox}{character. path for vector file corresponding to bbox}

\item{output_dir}{character. output path}

\item{siteName}{character. name of the study site}

\item{overwrite}{boolean.}

\item{path_S2tilinggrid}{character. path for the Sentinel-2_tiling_grid.kml file}
}
\value{
S2tiles list of tiles corresponding to plots
}
\description{
return S2 tiles intersecting with the footprint of a vector file
}
