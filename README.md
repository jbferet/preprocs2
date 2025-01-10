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

## Example

A tutorial vignette is available [here](https://jbferet.gitlab.io/preprocs2/articles/preprocS2.html).

## Acknowledgments / Fundings

This research was supported by the Agence Nationale de la Recherche 
([ANR](https://anr.fr/en/open-calls-and-preannouncements/), France) through the 
young researchers project **BioCop** (ANR-17-CE32-0001)
