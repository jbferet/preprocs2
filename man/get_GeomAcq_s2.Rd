% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_GeomAcq_s2.R
\name{get_GeomAcq_s2}
\alias{get_GeomAcq_s2}
\title{geometry of acquisition for aoi}
\usage{
get_GeomAcq_s2(
  dsn_S2tiles,
  datetime,
  cloudcover = 100,
  collection = "sentinel-2-l2a",
  output_dir,
  nbCPU = 1,
  overwrite = F
)
}
\arguments{
\item{dsn_S2tiles}{character. path for vector including all S2 tiles intersecting with aoi}

\item{datetime}{list. define period 'from' / 'to' day of acquisition}

\item{cloudcover}{numeric. maximum cloud cover over tile}

\item{collection}{character. collection targeted with CDSE}

\item{output_dir}{character. main output directory}

\item{nbCPU}{numeric. number of CPUs}

\item{overwrite}{boolean. should collection and S2 data be overwritten?}
}
\value{
list of collections per plot
}
\description{
geometry of acquisition for aoi
}
