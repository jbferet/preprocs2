% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_s2_tiling.R
\name{get_s2_tiling}
\alias{get_s2_tiling}
\title{return S2 tiles intersecting with the footprint of a vector file}
\usage{
get_s2_tiling(
  plots = NULL,
  aoi_path,
  datetime,
  output_dir,
  cloudcover = 99,
  overwrite = T,
  siteName = NULL,
  path_S2tilinggrid = "Sentinel-2_tiling_grid.kml",
  collection = "sentinel-2-l2a",
  geomAcq = F,
  nbCPU = 1,
  mask_path = NULL,
  resolution = 10,
  fraction_vegetation = 0,
  stac_url = NULL,
  doublecheckColl = T,
  offset = 1000,
  offset_B2 = F,
  corr_BRF = F,
  crs_target = NULL,
  RadiometricFilter = NULL,
  additional_process = NULL,
  original_clouds = T,
  cellsize = 10000,
  pursue_existing = T,
  argsin = NULL,
  writeoutput = T,
  bypassDL = F,
  bands2correct = c("B8A", "B11", "B12")
)
}
\arguments{
\item{plots}{list.}

\item{aoi_path}{character. path for vector file defining aoi}

\item{datetime}{character or date or list defining date of acquisition or time range}

\item{output_dir}{character. path for output directory}

\item{cloudcover}{numeric.}

\item{overwrite}{boolean.}

\item{siteName}{character. name of the study site}

\item{path_S2tilinggrid}{character. path for the Sentinel-2_tiling_grid.kml file}

\item{collection}{character.}

\item{geomAcq}{boolean.}

\item{nbCPU}{numeric.}

\item{mask_path}{character.}

\item{resolution}{numeric.}

\item{fraction_vegetation}{numeric.}

\item{stac_url}{character.}

\item{doublecheckColl}{boolean.}

\item{offset}{numeric.}

\item{offset_B2}{boolean.}

\item{corr_BRF}{boolean.}

\item{crs_target}{numeric.}

\item{RadiometricFilter}{list.}

\item{additional_process}{additional process to be applied to S2_items once downloaded}

\item{original_clouds}{boolean. should original cloud mask be used or not?}

\item{cellsize}{numeric. cell size}

\item{pursue_existing}{boolean. should previously downloaded data be accounted for?}

\item{argsin}{list. list of arguments for additional_process}

\item{writeoutput}{boolean. should output file be saved?}

\item{bypassDL}{boolean. the download of S2 data is not performed}

\item{bands2correct}{character. name of bands to correct from geometry}
}
\value{
plots list of plots
}
\description{
return S2 tiles intersecting with the footprint of a vector file
}
