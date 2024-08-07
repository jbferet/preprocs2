% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Lib_preprocess_S2.R
\name{get_S2_L2A_Image}
\alias{get_S2_L2A_Image}
\title{download S2 L2A data from Copernicus hub or convert L1C to L2A}
\usage{
get_S2_L2A_Image(
  l2a_path,
  dateAcq,
  spatial_extent = NULL,
  tile = NULL,
  DeleteL1C = FALSE,
  Sen2Cor = TRUE,
  GoogleCloud = TRUE,
  level = "L2A"
)
}
\arguments{
\item{l2a_path}{character. path for storage of L2A image}

\item{dateAcq}{character. date of acquisition}

\item{spatial_extent}{path for a vector file}

\item{tile}{character. Sentinel-2 Tiles to be considered string (5-length character)}

\item{DeleteL1C}{Boolean. set TRUE to delete L1C images}

\item{Sen2Cor}{Boolean. set TRUE to automatically perform atmospheric corrections using sen2Cor}

\item{GoogleCloud}{boolean. set to TRUE if google cloud SDK is installed and}

\item{level}{character. define if L2A (default) or force L1C download
sen2r configured as an alternative hub for S2 download}
}
\value{
PathL2A character. Path for L2A image
}
\description{
download S2 L2A data from Copernicus hub or convert L1C to L2A
}
