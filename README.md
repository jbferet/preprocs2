# __preprocS2__ <img src="man/figures/Sentinel-2.gif" align="right" alt="" width="200" />

# An R package dedicated to basic preprocessing of Sentinel-2 Level-2A reflectance images

[![licence](https://img.shields.io/badge/Licence-GPL--3-blue.svg)](https://www.r-project.org/Licenses/GPL-3)
[![Build Status](https://gitlab.com/jbferet/preprocS2/badges/master/pipeline.svg)](https://gitlab.com/jbferet/preprocS2/pipelines/latest)


# preprocS2

`preprocS2` is a basic wrapper of the [`rstac`](https://brazil-data-cube.github.io/rstac/) package. 
It aims at downloading and preparing Sentinel-2 images for further processings using R packages such as 
[`biodivMapR`](https://jbferet.github.io/biodivMapR/index.html) for spectral diversity mapping, 
[`prosail`](https://jbferet.gitlab.io/prosail/index.html) for the computation of vegetation biophysical variables, and 
[`spinr`](https://gitlab.com/jbferet/spinr) for the computation of vegetation biophysical variables or spectral indices.

## Installation

```
devtools::install_github('jbferet/preprocS2')
```

## Access to CDSE STAC catalog

`preprocS2` uses the STAC API endpoint provided by 
[MPC](https://planetarycomputer.microsoft.com/docs/quickstarts/reading-stac/). 
However, some products optionally required by `preprocS2` are not provided by 
this STAC API. 
This is the case for raster data corresponding to the geometry of acquisition of 
Sentinel-2 images. 

These products are available from the STAC catalog provided by the 
[Sentinel-hub](https://dataspace.copernicus.eu/analyse/apis/sentinel-hub) 
Catalog API via [Copernicus Dataspace](https://dataspace.copernicus.eu/).

To be able to download data from the Sentinel-hub STAC Catalog API via 
[Copernicus Dataspace](https://dataspace.copernicus.eu/), 
create an account on the CDSE plateform, and activate the **OAuth clients**
following [this link](https://shapps.dataspace.copernicus.eu/dashboard/#/account/settings).
Then add your credentials to your `~/.Renviron`:

```r
usethis::edit_r_environ()
```

and add the following lines before saving the `~/.Renviron` file. 

```r
PREPROCS2_CDSE_ID = "sh-XXXXXXXX-XXXX-XXXX-XXXX-XXXXXXXXXXXX",
PREPROCS2_CDSE_SECRET = "XXXXXXXXXXXXXXXXXXXXXXXXXXXX"
```

Once the R session restarted, you can use all functionalities of `preprocS2`.


## Example

A tutorial vignette is available [here](https://jbferet.gitlab.io/preprocs2/articles/preprocS2.html).

## Acknowledgments / Fundings

This research was supported by the Agence Nationale de la Recherche 
([ANR](https://anr.fr/en/open-calls-and-preannouncements/), France) through the 
young researchers project **BioCop** (ANR-17-CE32-0001)
