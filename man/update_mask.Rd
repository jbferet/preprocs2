% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/update_mask.R
\name{update_mask}
\alias{update_mask}
\title{update mask based on radiometric tresholds}
\usage{
update_mask(
  aoi,
  collection_path,
  iChar,
  raster_dir,
  cloudmasks,
  mask_path = NULL,
  fraction_vegetation = 10,
  collection = "sentinel-2-l2a",
  resolution = 10,
  offset = 1000,
  overwrite = FALSE,
  RadiometricFilter = NULL,
  siteName = NULL,
  crs_target = NULL,
  original_clouds = TRUE,
  S2_items = NULL,
  writeoutput = TRUE
)
}
\arguments{
\item{aoi}{geometry corresponding to one point}

\item{collection_path}{path for colection previously saved}

\item{iChar}{plot ID}

\item{raster_dir}{directory where rasters are stored}

\item{cloudmasks}{spatial raster}

\item{mask_path}{directory where mask is}

\item{fraction_vegetation}{numeric. minimum fraction vegetation over plot}

\item{collection}{character. collection targeted with CDSE}

\item{resolution}{numeric. spatial resolution (10 or 20)}

\item{offset}{numeric. offset}

\item{overwrite}{boolean.}

\item{RadiometricFilter}{list.}

\item{siteName}{character. name of the study site}

\item{crs_target}{numeric.}

\item{original_clouds}{boolean}

\item{S2_items}{list}

\item{writeoutput}{boolean. should output file be saved?}
}
\value{
list of collections per plot
}
\description{
update mask based on radiometric tresholds
}
