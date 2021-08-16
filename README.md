# __preprocS2__ <img src="man/figures/Sentinel-2.gif" align="right" alt="" width="200" />

# A R package dedicated to basic preprocessing of Sentinel-2 Level-2A reflectance images

[![licence](https://img.shields.io/badge/Licence-GPL--3-blue.svg)](https://www.r-project.org/Licenses/GPL-3)
[![Build Status](https://gitlab.com/jbferet/preprocS2/badges/master/pipeline.svg)](https://gitlab.com/jbferet/preprocS2/pipelines/latest)


# preprocS2

The goal of preprocS2 is to provide a common framework for teh preprocessing of Level-2A Sentinel-2 images. 
Sentinel-2 L2A images can be produced or obtained from various data hubs or atmospheric correction methods. preprocS2 provides a unique function to read, crop, resample the original image directory, and write it as a raster stack. 


## Installation

After installing package `devtools`, you need to install the package `preprocS2` with the following command line in R session:
```
devtools::install_gitlab('jbferet/preprocS2')
```

## Example

A tutorial vignette is available [here](https://jbferet.gitlab.io/preprocS2/articles/preprocS21.html).

This is a basic example which shows you how to solve a common problem:

```{r example}
library(preprocS2)
## basic example code
```

What is special about using `README.Rmd` instead of just `README.md`? You can include R chunks like so:

```{r cars}
summary(cars)
```

You'll still need to render `README.Rmd` regularly, to keep `README.md` up-to-date. `devtools::build_readme()` is handy for this. You could also use GitHub Actions to re-render `README.Rmd` every time you push. An example workflow can be found here: <https://github.com/r-lib/actions/tree/master/examples>.

You can also embed plots, for example:

```{r pressure, echo = FALSE}
plot(pressure)
```

In that case, don't forget to commit and push the resulting figure files, so they display on GitHub and CRAN.
