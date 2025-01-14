# preprocS2 v2.2.0

## Fix
- systematically sign query for lasrc and planetary

# preprocS2 v2.1.0

## Addition
- added support for THEIA & LaSRC catalogs

# preprocS2 v2.0.1

## Addition
- added an error message when no data available on STAC catalog for the spatiotemporal request

# preprocS2 v2.0.0

## Fix
- use rstac

# preprocS2 v1.6.1

## Fix
- replace 'funct <- wrapperBig_Stack' with 'funct <- bigRaster::wrapperBig_Stack'

# preprocS2 v1.6.0

## Fix
- update tutorial

# preprocS2 v1.5.2

## Fix
- update tutorial

# preprocS2 v1.5.1

## Fix
- Use latest version of stars
- update tutorial for install

# preprocS2 v1.5.0

## Fix
- Copernicus hub not available from sen2r anymore. Google Cloud Sentinel-2 bucket defined as unique source for data download

# preprocS2 v1.4.0

## Fix
- remove dependency to rgdal and rgeos

# preprocS2 v1.3.1

## Fix
- introduction of terra

# preprocS2 v1.3.0

## Changes
- introduction of terra
- possibility to define s2mission in save_reflectance_s2 in order to avoid call to check_scihub_connection (and parametrization of apihub.txt with sen2r)

# preprocS2 v1.2.5

## Changes
- externalize function get_S2_offset independent from save_reflectance_s2


# preprocS2 v1.2.4

## Changes
- specify gdal driver ENVI when reading bands in tmp directory

# preprocS2 v1.2.3

## addition
- add parameter tile in function get_S2_L2A_Image

# preprocS2 v1.2.2

## addition
- added possibility to choose between L1C and L2A download when calling get_S2_L2A_Image (alternative to LTA)

## fix:
- fixed bug when no offset included in LaSRC data
- fixed date format when attempting to delete L1C image

# preprocS2 v1.2.1

## fix:
force install of stars version 0.5-5 as the next version currently available has bugs when dealing with writing large proxy objects

# preprocS2 v1.2.0

## addition
- included support with rgee: download S2 image subsets directly from Google Earth Engine

# preprocS2 v1.1.4

## fix
- fixed bug occuring when processing S2B images corrected with LaSRC: MTD_LaSRC was not found


# preprocS2 v1.1.3

## Change
- added suport to LaSRC L2A products and associated corection of offset
- updated vignettes

# preprocS2 v1.1.1

## addition
- added correction of BOA offset when writing reflectance data
- added saving of MTD_MSI into reflectance product directory

# preprocS2 v1.0.0

## fix
- fixed problem of RAM when writing large image: using stars proxy to write by chunk

## addition
- minor bugs and optimizations

# preprocS2 v0.1.1

## addition
- added a function to mosaic a set of rasters

# preprocS2 v0.1.0

## fix
Fixed bug:
- allows for multi-tile download
- allows for cropping even if vector is not fully included in raster

## addition
New functions
- Use Google cloud when properly configurated, and automatically request from gc when images are LTA on copernicus hub
- get tile from S2 name
- get date of acquisition from S2 name

