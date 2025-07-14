# preprocS2 v2.5.10

## fix
- import sf_use_s2 from sf in get_grid_aoi

# preprocS2 v2.5.9

## change
- started updating wording of code to follow good practices

## fix
- fixed initialization of mask_update <- NULL

# preprocS2 v2.5.8

## addition
- possibility to use aoi in degrees when calling get_grid_aoi
- borrowed meters_to_decdeg from https://github.com/mlammens/occUncertain

# preprocS2 v2.5.7

## change
- automatically creates output_dir if does not exists when calling get_s2_raster 

# preprocS2 v2.5.6

## change
- account for output of additional_process when continuing already started job 
- anticipate conflicts between prosail v3.0.0 and earlier versions

# preprocS2 v2.5.5

## change
- requires user to save CDSE OAuth in `~/.Renviron` 
- eliminate `Authentication` input variable from all functions

# preprocS2 v2.5.4

## addition
- added function 'extract_from_safe' to extract, crop and write reflectance, cloud mask and metadata from SAFE data

# preprocS2 v2.5.3

## Fix
- fix function 'get_s2_raster': add output_dir when calling 'get_s2_tiles'

# preprocS2 v2.5.2

## addition
- possibility to select spectral bands for which geometric correction will be applied
- possibility to save spectral indices for individual acquisitions

# preprocS2 v2.5.1

## addition
- function testing if spectral indices already computed when using get_s2_tiling

# preprocS2 v2.5.0

## addition
- added function get_s2_tiling to download S2 image from STAC catalog using tiles for multithread process
- added function get_quicklook to produce quicklooks from raster data
- added functions print_error_message_preprocS2 and get_HDR_name

## changes
- add crs_target as input to get_s2_tiling : possibility to define CRS for products saved (useful when downloading over large regions)
- add original_clouds as input to get_s2_tiling : possibility to bypass provider cloud mask
- radiometric filtering applied after reflectance correction (BRDF, B2)

# preprocS2 v2.4.0

## changes
- use package terra instead of sf::gdal_utils to download STAC data
- terra provides 10m/20m original bands with no reprojection
- writes raster data once all the process is finished to save reading / writing time
- added quiet = T when handling sf objects
- applied sf::st_zm to avoid warnings with XYZ vector files (s2 tiles)

# preprocS2 v2.3.1

## change
- add quiet = T when calling sf::st_read

# preprocS2 v2.3.0

## addition
- added support to extract information from SAFE and THEIA data, as provided in v1

# preprocS2 v2.2.7

## addition
- added input variable keepCRS in function get_s2_raster to preserve native projection provided by aoi_path or bbox
- modifications to avoid notes when building package
- adjust crs for aoi and bbox
- suppress warning when tmp files cannot be suppressed
- check online access to guarantee data download when S2 download required
- added message when downloading S2 tiling grid

# preprocS2 v2.2.6

## addition
- add geometry files as outputs for get_s2_raster when available
- updated vignettes
- add function get_s2_angles to get geometry of acquisition for an aoi

# preprocS2 v2.2.5

## fix
- properly signing for planetary collections
- deleting vignette #1

# preprocS2 v2.2.4

## addition
- accounts for vector files as masks in function "get_mainmask"

# preprocS2 v2.2.3

## addition
- added names for outputs rasters from get_s2_raster

# preprocS2 v2.2.2

## Fix
- get_s2_raster: systematically convert bbox to crs 4326
- remove item signature for planetary

# preprocS2 v2.2.1

## Fix
- check if (!is.null(out_dir)) at the end of get_cloudmask

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

