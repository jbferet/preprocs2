################################################################################
# This program aims at downloading and processing Sentinel-2 data
# This includes processing of S2 data
# - download full tile with sen2r
# - Crop, resample using interpolation, stack
# - compute spectral indices
# - Compute biophysical variables using PROSAIL inversion
################################################################################

rm(list=ls(all=TRUE));gc()
# Loading libraries
library(sen2r)
library(preprocS2)

########################################################################
##                      Download S2 data with sen2r                   ##
########################################################################
# get product name corresponding to Barrax
dateAcq <- '2021-05-13'
time_window <- as.Date(c(dateAcq, dateAcq))
path_vector <- 'D:/DEPOTS/01_MY_PACKAGES/preprocS2/DATA/Study_area_Barrax.shp'
list_safe <- s2_list(spatial_extent = sf::st_read(dsn = path_vector), time_interval = time_window)
DirWrite <- 'D:/DEPOTS/01_MY_PACKAGES/preprocS2/DATA'
s2_download(list_safe, outdir=DirWrite)

########################################################################
##                          PREPROCESS S2 DATA                        ##
########################################################################
##____________________________________________________________________##
##        Define where data is stored and where to write results      ##
##--------------------------------------------------------------------##
# define raster path
Path_S2 <- file.path(DirWrite,list.files(DirWrite,pattern = '.SAFE'))
# Result directory
result_path <- '../RESULTS'
dir.create(path = result_path,showWarnings = FALSE,recursive = TRUE)
##____________________________________________________________________##
##                  Extract, resample & stack data                    ##
##--------------------------------------------------------------------##
# define resolution
resolution <- 10
# define source of data
S2source <- 'SAFE'
S2obj <- preprocS2::extract_from_S2_L2A(Path_dir_S2 = Path_S2,
                                        path_vector = path_vector,
                                        S2source = S2source,
                                        resolution = resolution)
# update shapefile if needed (reprojection)
path_vector <- S2obj$path_vector

########################################################################
##                  Write CLOUD MASK & REFLECTANCE                    ##
########################################################################
# create specific result directory corresponding to granule name
results_site_path <- file.path(result_path,basename(S2obj$S2_Bands$GRANULE))
dir.create(path = results_site_path,showWarnings = FALSE,recursive = TRUE)
##____________________________________________________________________##
##                        Write CLOUD MASK                            ##
##--------------------------------------------------------------------##
# directory for cloud mask
Cloud_path <- file.path(results_site_path,'CloudMask')
dir.create(path = Cloud_path,showWarnings = FALSE,recursive = TRUE)
# Filename for cloud mask
cloudmasks <- preprocS2::save_cloud_s2(S2_stars = S2obj$S2_Stack,
                                       Cloud_path = Cloud_path,
                                       S2source = S2source, SaveRaw = T)
##____________________________________________________________________##
##                        Write REFLECTANCE                           ##
##--------------------------------------------------------------------##
# directory for Reflectance
Refl_dir <- file.path(results_site_path,'Reflectance')
dir.create(path = Refl_dir,showWarnings = FALSE,recursive = TRUE)
# filename for Reflectance
Refl_path <- file.path(Refl_dir,paste(basename(S2obj$S2_Bands$GRANULE),'_Refl',sep = ''))

# Save Reflectance file as ENVI image with BIL interleaves
tileS2 <- substring(strsplit(basename(S2obj$S2_Bands$GRANULE),'_')[[1]][2],2)
dateAcqS2 <- as.Date(substring(strsplit(basename(S2obj$S2_Bands$GRANULE),'_')[[1]][4],1,8),format="%Y%m%d")
preprocS2::save_reflectance_s2(S2_stars = S2obj$S2_Stack, Refl_path = Refl_path,
                               S2Sat = NULL, tile_S2 = tile_S2, dateAcq_S2 = dateAcq_S2,
                               Format = 'ENVI_BIL', datatype = 'INT2S', MTD = S2obj$S2_Bands$metadata)

########################################################################
##                      COMPUTE SPECTRAL INDEX                        ##
########################################################################
library(prosail)
library(raster)
library(stars)
# Read raster
Refl <- brick(Refl_path)
# get raster band name and clean format. Expecting band name and wavelength to be documented in image
HDR_Refl <- read_ENVI_header(get_HDR_name(Refl_path))
SensorBands <- HDR_Refl$wavelength
# compute a set of spectral indices defined by IndexList from S2 data
IndexList <- c('EVI','NDVI','CR_SWIR', 'MCARI','NDWI1','mNDVI705')
# ReflFactor = 10000 when reflectance is coded as INT16
Indices <- prosail::ComputeSpectralIndices_Raster(Refl = Refl, SensorBands = SensorBands,
                                                  Sel_Indices = IndexList,
                                                  ReflFactor = 10000, StackOut=F)

# create directory for Spectral indices
SI_path <- file.path(results_site_path,'SpectralIndices')
dir.create(path = SI_path,showWarnings = FALSE,recursive = TRUE)
# Save spectral indices
for (SpIndx in names(Indices$SpectralIndices)){
  Index_Path <- file.path(SI_path,paste(basename(S2obj$S2_Bands$GRANULE),'_',SpIndx,sep = ''))
  stars::write_stars(st_as_stars(Indices$SpectralIndices[[SpIndx]]), dsn=Index_Path, driver =  "ENVI",type='Float32')
  # write band name in HDR
  HDR <- read_ENVI_header(get_HDR_name(Index_Path))
  HDR$`band names` <- SpIndx
  write_ENVI_header(HDR = HDR,HDRpath = get_HDR_name(Index_Path))
}

########################################################################
##      COMPUTE BIOPHYSICAL VARIABLES BASED ON PROSAIL INVERSION      ##
########################################################################
# Update Cloud mask based on radiometric filtering
# eliminate pixels with NDVI < NDVI_Thresh because not enough vegetation
NDVI_Thresh <- 0.5
Elim <- which(values(Indices$SpectralIndices[['NDVI']])<NDVI_Thresh)
CloudInit <- stars::read_stars(cloudmasks$BinaryMask)
CloudInit$CloudMask_Binary[Elim] <- 0
# save updated cloud mask
Cloud_File <- file.path(Cloud_path,'CloudMask_Binary_Update')
stars::write_stars(CloudInit, dsn=Cloud_File,driver = "ENVI",type='Byte')

# get S2 geometry
# read metadata file from S2 image
xmlfile <- file.path(dirname(Refl_path),'MTD_TL.xml')
S2Geom <- get_S2geometry(MTD_TL_xml = xmlfile)

# Train PROSAIL inversion
minval <- data.frame('CHL'=10,'CAR'=0,'EWT' = 0.005,'ANT' = 0,'LMA' = 0.005,'N' = 1.5,'psoil' = 0.0, 'BROWN'=0.0,
                     'LIDFa' = 50, 'lai' = 0.5,'q'=0.1,'tto' = 0,'tts' = min(S2Geom$SZA), 'psi' = 5)
maxval <- data.frame('CHL'=90,'CAR'=20,'EWT' = 0.03,'ANT' = 3,'LMA' = 0.03,'N' = 2.0, 'psoil' = 1.0, 'BROWN'=0.5,
                     'LIDFa' = 70, 'lai' = 7,'q'=0.2,'tto' = 7,'tts' = max(S2Geom$SZA), 'psi' = 355)

# get sensor response for Sentinel-2
SensorName <- HDR_Refl$`sensor type`
SRF <- GetRadiometry(SensorName,Path_SensorResponse = NULL)
# adjust optical constants from 1nm sampling into spectral S2 spectral sampling
wvl <- SpecPROSPECT$lambda
SpecSensor <- PrepareSensorSimulation(SpecPROSPECT,SpecSOIL,SpecATM,SRF)
SpecPROSPECT_Sensor <- SpecSensor[[1]]$SpecPROSPECT_Sensor
SpecSOIL_Sensor <- SpecSensor[[1]]$SpecSOIL_Sensor
SpecATM_Sensor <- SpecSensor[[1]]$SpecATM_Sensor

# define spectral bands required to train SVR model for each variable
S2BandSelect <- list()
S2BandSelect$CHL <- S2BandSelect$lai <- S2BandSelect$EWT <- S2BandSelect$LMA <- c('B03','B04','B05','B06','B07','B08','B11','B12')
ImgBandNames <- strsplit(HDR_Refl$`band names`,split = ',')[[1]]
# get variable ID for train_prosail_inversion
Bands2Select <- list()
for (bpvar in names(S2BandSelect)){
  Bands2Select[[bpvar]] <- match(S2BandSelect[[bpvar]],ImgBandNames)
}

# define noise level for each variable
NoiseLevel <- list()
NoiseLevel$EWT <- 0.025
NoiseLevel$CHL <- 0.01
NoiseLevel$LMA <- NoiseLevel$lai <- 0.05

# where results will be stored
PROSAIL_ResPath <- file.path(results_site_path,'PROSAIL_INVERSION')
dir.create(path = PROSAIL_ResPath,showWarnings = FALSE,recursive = TRUE)

modelSVR <- train_prosail_inversion(minval=minval,maxval=maxval,Parms2Estimate=c('CHL','EWT','LMA','lai'),
                                    Bands2Select=Bands2Select,NoiseLevel=NoiseLevel, SAILversion = '4SAIL',
                                    SpecPROSPECT = SpecPROSPECT_Sensor, SpecSOIL = SpecSOIL_Sensor, SpecATM = SpecATM_Sensor,
                                    Path_Results=PROSAIL_ResPath,nbModels = 10,nbSamples = 1000,FigPlot = FALSE)

# Apply SVR model on Sentinel-2 data
Apply_prosail_inversion(raster_path = Refl_path, HybridModel = modelSVR, PathOut = PROSAIL_ResPath,
                        SelectedBands = S2BandSelect,bandname = ImgBandNames,
                        MaskRaster = Cloud_File, MultiplyingFactor = 10000)

