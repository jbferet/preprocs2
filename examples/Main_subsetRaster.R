################################################################################
# This program aims at processing Sentinel-2 data produced from Sen2R and LaSRC
# and stored as .SAFE directory or LaSRC directory (.tif files)
# This includes processing of S2 data
# - Crop, resample using interpolation, stack
# - compute spectral indices
# - Compute biophysical variables using PROSAIL inversion
################################################################################

rm(list=ls(all=TRUE));gc()
# Loading libraries
library(preprocS2)

########################################################################
##                          PREPROCESS S2 DATA                        ##
########################################################################
##____________________________________________________________________##
##        Define where data is stored and where to write results      ##
##--------------------------------------------------------------------##
# define raster path
Path_S2 <- '../RESULTS/L2A_T21NZF_A016552_20180823T141045/Reflectance/L2A_T21NZF_A016552_20180823T141045_Refl'
BB_XYcoords <- list()
BB_XYcoords[['UL']] <- data.frame('row'=1001,'col'=1001)
BB_XYcoords[['UR']] <- data.frame('row'=1001,'col'=2000)
BB_XYcoords[['LL']] <- data.frame('row'=2000,'col'=1001)
BB_XYcoords[['LR']] <- data.frame('row'=2000,'col'=2000)

stars_obj <- read_raster(path_raster = Path_S2, path_vector = NULL, BBpix = BB_XYcoords)

HDR <- read_ENVI_header(get_HDR_name(Path_S2))
Stars_Spectral <- list()
Stars_Spectral$bandname <- strsplit(HDR$`band names`,',')[[1]]
Stars_Spectral$wavelength <- HDR$wavelength
Refl_path <- paste(Path_S2,'_subset',sep = '')
write_Stack_S2(Stars_S2 = stars_obj, Stars_Spectral = Stars_Spectral, Refl_path = Refl_path,sensor=HDR$`sensor type`)

#
# ########################################################################
# ##      COMPUTE BIOPHYSICAL VARIABLES BASED ON PROSAIL INVERSION      ##
# ########################################################################
# # get S2 geometry
# # read metadata file from S2 image
# xmlfile <- file.path(dirname(Refl_path),'MTD_TL.xml')
# S2Geom <- get_S2geometry(MTD_TL_xml = xmlfile)
#
# # Train PROSAIL inversion
# minval <- data.frame('CHL'=10,'CAR'=0,'EWT' = 0.005,'ANT' = 0,'LMA' = 0.005,'N' = 1.5,'psoil' = 0.0, 'BROWN'=0.0,
#                      'LIDFa' = 50, 'lai' = 0.5,'q'=0.1,'tto' = 0,'tts' = min(S2Geom$SZA), 'psi' = 5)
# maxval <- data.frame('CHL'=90,'CAR'=20,'EWT' = 0.03,'ANT' = 3,'LMA' = 0.03,'N' = 2.0, 'psoil' = 1.0, 'BROWN'=0.5,
#                      'LIDFa' = 70, 'lai' = 7,'q'=0.2,'tto' = 7,'tts' = max(S2Geom$SZA), 'psi' = 355)
#
# # get sensor response for Sentinel-2
# SensorName <- HDR_Refl$`sensor type`
# SRF <- GetRadiometry(SensorName,Path_SensorResponse = NULL)
# # adjust optical constants from 1nm sampling into spectral S2 spectral sampling
# wvl <- SpecPROSPECT$lambda
# SpecSensor <- PrepareSensorSimulation(SpecPROSPECT,SpecSOIL,SpecATM,SRF)
# SpecPROSPECT_Sensor <- SpecSensor[[1]]$SpecPROSPECT_Sensor
# SpecSOIL_Sensor <- SpecSensor[[1]]$SpecSOIL_Sensor
# SpecATM_Sensor <- SpecSensor[[1]]$SpecATM_Sensor
#
# # define spectral bands required to train SVR model for each variable
# S2BandSelect <- list()
# S2BandSelect$CHL <- S2BandSelect$lai <- S2BandSelect$EWT <- S2BandSelect$LMA <- c('B03','B04','B05','B06','B07','B08','B11','B12')
# ImgBandNames <- strsplit(HDR_Refl$`band names`,split = ',')[[1]]
# # get variable ID for train_prosail_inversion
# Bands2Select <- list()
# for (bpvar in names(S2BandSelect)){
#   Bands2Select[[bpvar]] <- match(S2BandSelect[[bpvar]],ImgBandNames)
# }
#
# # define noise level for each variable
# NoiseLevel <- list()
# NoiseLevel$EWT <- 0.025
# NoiseLevel$CHL <- 0.01
# NoiseLevel$LMA <- NoiseLevel$lai <- 0.05
#
# # where results will be stored
# PROSAIL_ResPath <- file.path(results_site_path,'PROSAIL_INVERSION_4SAIL')
# dir.create(path = PROSAIL_ResPath,showWarnings = FALSE,recursive = TRUE)
#
# modelSVR <- train_prosail_inversion(minval=minval,maxval=maxval,Parms2Estimate=c('CHL','EWT','LMA','lai'),
#                                     Bands2Select=Bands2Select,NoiseLevel=NoiseLevel,
#                                     SpecPROSPECT = SpecPROSPECT_Sensor, SpecSOIL = SpecSOIL_Sensor, SpecATM = SpecATM_Sensor,
#                                     Path_Results=PROSAIL_ResPath,nbModels = 10,nbSamples = 1000,FigPlot = FALSE)
#
# # Apply SVR model on Sentinel-2 data
# Apply_prosail_inversion(raster_path = Refl_path, HybridModel = modelSVR, PathOut = PROSAIL_ResPath,
#                         SelectedBands = S2BandSelect,bandname = ImgBandNames,
#                         MaskRaster = cloudmasks$BinaryMask, MultiplyingFactor = 10000)

# ########################################################################
# ##                        4SAIL2 instead of 4SAIL                     ##
# ########################################################################
#
# # Train PROSAIL inversion
# minval <- data.frame('CHL'=10,'CAR'=0,'EWT' = 0.005,'ANT' = 0,'LMA' = 0.005,'N' = 1.5,'psoil' = 0.0, 'BROWN'=0.0,
#                      'LIDFa' = 50, 'lai' = 0.5,'q'=0.1,'tto' = 0,'tts' = min(S2Geom$SZA), 'psi' = 5)
# maxval <- data.frame('CHL'=90,'CAR'=20,'EWT' = 0.03,'ANT' = 3,'LMA' = 0.03,'N' = 2.0, 'psoil' = 1.0, 'BROWN'=0.5,
#                      'LIDFa' = 70, 'lai' = 7,'q'=0.2,'tto' = 7,'tts' = max(S2Geom$SZA), 'psi' = 355)
#
# # where results will be stored
# PROSAIL_ResPath <- file.path(results_site_path,'PROSAIL_INVERSION_4SAIL2bis')
# dir.create(path = PROSAIL_ResPath,showWarnings = FALSE,recursive = TRUE)
#
# modelSVR_4SAIL2 <- train_prosail_inversion(minval=minval,maxval=maxval,Parms2Estimate=c('CHL','EWT','LMA','lai'),
#                                            Bands2Select=Bands2Select,NoiseLevel=NoiseLevel,
#                                            SpecPROSPECT = SpecPROSPECT_Sensor, SpecSOIL = SpecSOIL_Sensor, SpecATM = SpecATM_Sensor,
#                                            Path_Results=PROSAIL_ResPath,nbModels = 1,nbSamples = 1000,FigPlot = FALSE,SAILversion = '4SAIL2')
#
# # Apply SVR model on Sentinel-2 data
# Apply_prosail_inversion(ImgFile = Refl_path,HybridModel = modelSVR_4SAIL2, PathOut = PROSAIL_ResPath,
#                         SelectedBands = S2BandSelect,bandname = Stars_Spectral$bandname,
#                         MaskRaster = Cloud_path, MultiplyingFactor = 10000)
