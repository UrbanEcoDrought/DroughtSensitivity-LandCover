# ==============================================================================
# 02_extract_Landsat_NDVI_L5-9_past.R
# ==============================================================================
# Extracts mean NDVI per land cover type from Landsat 5, 7, 8, and 9
# Collection 2 Level 2 surface reflectance imagery over the 7-county Chicago
# metropolitan region. Each satellite mission is processed independently so
# that mission identity is preserved as a grouping variable for the downstream
# mixed-effects models (scripts 03-05), where satellite mission serves as the
# random intercept to account for inter-sensor radiometric differences.
#
# For each Landsat mission, the workflow is:
#   1. Load imagery, clip to the Chicago region, and apply the QA bitmask to
#      remove clouds, shadows, snow, water, terrain occlusion, and saturated
#      pixels (see applyLandsatBitMask in script 00).
#   2. Apply Collection 2 Level 2 scale factors to convert from digital numbers
#      to physical units:
#        - Surface reflectance bands: DN * 0.0000275 + (-0.2) -> reflectance
#        - Thermal band: DN * 0.00341802 + 149 -> temperature in Kelvin
#   3. Rename bands to common names (blue, green, red, nir, swir1, swir2,
#      LST_K) and compute NDVI = (nir - red) / (nir + red).
#   4. Mosaic scenes within +/- 7 days using a median reducer to handle
#      path/row overlap (Chicago spans WRS-2 paths 22-23).
#   5. For each of the 7 land cover types, apply the year-matched NLCD mask
#      and compute the spatial mean NDVI across the region at 30 m resolution.
#   6. Export the date x NDVI time series to Google Drive as CSV.
#
# Note: Landsat 7 and 5 use different band numbering than 8/9:
#   L8/L9: SR_B2-B7 (blue-swir2), ST_B10 (thermal)
#   L7/L5: SR_B1-B5 + SR_B7 (blue-swir2), ST_B6 (thermal)
#
# This script only needs to be run once. After all GEE export tasks finish,
# the CSV outputs are copied from the personal Google Drive to the shared
# project drive.
# ==============================================================================

# Pull all available Landsat NDVI; save by product; do not merge products into same file
# TO BE RUN ONCE!
# Steps for each landsat product:
# 1. Pull in the date for the area and date range we want: Jan 1, 2000 - today; filter/clip to our region
# 2. Do data cleaning: apply the appropriate bitmask, do band-level corrections
# 3. Mosaic images together with a middle-centered 15 day window (image date +/- 7 days); using a median reducer function
#      -->  mosiacking of images within a week on either side of a single image *should* help reduce spatial-based noise in NDVI
#    -- CONSIDER SAVING THESE IMAGES FOR LATER!
# 4. Create landcover-specific collections using the existing nlcd-based masks (see script 01)
# 5. Reduce to the mean value for each image date
# 6. Save a file for each Landsat product (e.g. Landsat 7, Landsat 8) with the time series for each landcover class -- 1 value per landcover class per date

library(rgee); library(raster); library(terra)
ee_check() # For some reason, it's important to run this before initializing right now
# ee$data$createAssetHome("users/crollinson")
# ee$data$createAssetHome("projects/urbanecodrought")
# ee_Initialize(project="urbanecodrought")
# rgee::ee_Initialize(user = 'crollinson@mortonarb.org', drive=T, project="urbanecodrought")
rgee::ee_Initialize(user = 'crollinson@mortonarb.org', drive=T)
path.google.CR <- "~/Google Drive/My Drive/UrbanEcoDrought/"
path.google.share <- "~/Google Drive/Shared drives/Urban Ecological Drought/Manuscript - Urban Drought NDVI/"
NDVIsave <- "UrbanEcoDrought_NDVI-LandscapeSensitivity"
# GoogleFolderSave <- "UHI_Analysis_Output_Final_v2"
assetHome <- ee_get_assethome()

##################### 
# 0. Read in helper functions ----
##################### 
source("00_EarthEngine_HelperFunctions.R")
##################### 


#####################
# Color Palette etc. ----
#####################
# NDVI color ramp for interactive map visualization (white-to-dark-green).
# Used only for visual QA checks during development; not part of the data pipeline.

# Setting the center point for the Arb because I think we'll have more variation
Map$setCenter(-88.04526, 41.81513, 11);

ndviVis = list(
  min= 0.0,
  max= 1,
  palette= c(
    '#FFFFFF', '#CE7E45', '#DF923D', '#F1B555', '#FCD163', '#99B718', '#74A901',
    '#66A000', '#529400', '#3E8601', '#207401', '#056201', '#004C00', '#023B01',
    '#012E01', '#011D01', '#011301'
    )
  )
##################### 

#####################
# Read in Landcover Masks ----
#####################
# Load the 7-county Chicago region boundary and the NLCD-derived binary land
# cover masks created by script 01. Each mask is a multi-band GEE image asset
# with one band per year ("YR2001" through "YR2024"). When applied to a
# Landsat image, only pixels belonging to the target land cover class (in that
# image's year) are retained for NDVI extraction.
Chicago = ee$FeatureCollection("projects/breidyee/assets/SevenCntyChiReg")
ee_print(Chicago)

chiBounds <- Chicago$geometry()$bounds()
chiBBox <- ee$Geometry$BBox(-88.70738, 41.20155, -87.52453, 42.49575)

# Landcover names and mask ----
lcnames <- c("forest", "crop", "grassland", "urban-high", "urban-medium", "urban-low", "urban-open")

forMask <- ee$Image('users/crollinson/NLCD-Chicago_2000-2024_Forest')
# ee_print(forMask)
# Map$addLayer(forMask$select("YR2023"))

grassMask <- ee$Image('users/crollinson/NLCD-Chicago_2000-2024_Grass')
# Map$addLayer(grassMask$select("YR2023"))

cropMask <- ee$Image('users/crollinson/NLCD-Chicago_2000-2024_Crop')

urbOMask <- ee$Image('users/crollinson/NLCD-Chicago_2000-2024_Urban-Open')

urbLMask <- ee$Image('users/crollinson/NLCD-Chicago_2000-2024_Urban-Low')

urbMMask <- ee$Image('users/crollinson/NLCD-Chicago_2000-2024_Urban-Medium')

urbHMask <- ee$Image('users/crollinson/NLCD-Chicago_2000-2024_Urban-High')

# Map$addLayer(urbLMask$select("YR2023"))
# Map$addLayer(forMask$select("YR2023"))
##################### 

#####################
# Read in & Format Landsat 8 ----
#####################
# Landsat 8 OLI/TIRS (2013-present). Collection 2 Level 2 surface reflectance.
# Band mapping for L8/L9: SR_B2=blue, SR_B3=green, SR_B4=red, SR_B5=nir,
# SR_B6=swir1, SR_B7=swir2, ST_B10=thermal.
# Scale factors: reflectance = DN * 0.0000275 - 0.2; thermal = DN * 0.00341802 + 149 (Kelvin).

# "LANDSAT/LC08/C02/T1_RT"
# Load MODIS NDVI data; attach month & year
# https://developers.google.com/earth-engine/datasets/catalog/LANDSAT_LC08_C02_T1_L2
landsat8 <- ee$ImageCollection("LANDSAT/LC08/C02/T1_L2")$filterBounds(Chicago)$map(function(image){
  return(image$clip(Chicago))
})$map(function(img){
  d= ee$Date(img$get('system:time_start'));
  dy= d$get('day');    
  m= d$get('month');
  y= d$get('year');
  
  # # Add masks 
  img <- applyLandsatBitMask(img)
  
  # #scale correction; doing here & separating form NDVI so it gets saved on the image
  lAdj = img$select(c('SR_B1', 'SR_B2', 'SR_B3', 'SR_B4', 'SR_B5', 'SR_B6', 'SR_B7'))$multiply(0.0000275)$add(-0.2);
  lst_k = img$select('ST_B10')$multiply(0.00341802)$add(149);

  # img3 = img2$addBands(srcImg=lAdj, overwrite=T)$addBands(srcImg=lst_k, overwrite=T)$set('date',d, 'day',dy, 'month',m, 'year',y)
  return(img$addBands(srcImg=lAdj, overwrite=T)$addBands(srcImg=lst_k, overwrite=T)$set('date',d, 'day',dy, 'month',m, 'year',y))
})$select(c('SR_B2', 'SR_B3', 'SR_B4', 'SR_B5', 'SR_B6', 'SR_B7', 'ST_B10'),c('blue', 'green', 'red', 'nir', 'swir1', 'swir2', 'LST_K'))$map(addNDVI)
# Map$addLayer(landsat8$first()$select('NDVI'), ndviVis, "NDVI - First")
# ee_print(landsat8)
# Map$addLayer(landsat8$first()$select('NDVI'))

# Mosaic overlapping scenes within +/- 7 days using median. The median reducer
# bands get suffixed "_median", so rename back to common names for downstream use.
l8Mosaic = mosaicByDate(landsat8, 7)$select(c('blue_median', 'green_median', 'red_median', 'nir_median', 'swir1_median', 'swir2_median', 'LST_K_median', "NDVI_median"),c('blue', 'green', 'red', 'nir', 'swir1', 'swir2', 'LST_K', "NDVI"))$sort("date")
# ee_print(l8Mosaic, "landsat8-Mosaic")
# Map$addLayer(l8Mosaic$first()$select('NDVI'), ndviVis, "NDVI - First")

# Extract mean NDVI per land cover type and export to Google Drive.
# Each call masks the mosaic by one land cover class and saves a CSV with
# columns: date, NDVI (one row per composite date).
# Mask NDVI by Landcover & condense to regional means
for(LCTYPE in lcnames){
  # print(LCTYPE)
  extractByLC(imcol=l8Mosaic, landcover=LCTYPE, outfolder=NDVIsave, fileNamePrefix=paste0("Landsat8_", LCTYPE))
}

##################### 

#####################
# Read in & Format Landsat 9 ----
#####################
# Landsat 9 OLI-2/TIRS-2 (2021-present). Same band structure and scale factors
# as Landsat 8. Processed identically: QA mask, scale, rename, NDVI, mosaic, extract.

# "LANDSAT/LC09/C02/T1_L2"
# Load MODIS NDVI data; attach month & year
# https://developers.google.com/earth-engine/datasets/catalog/LANDSAT_LC09_C02_T1_L2
landsat9 <- ee$ImageCollection("LANDSAT/LC09/C02/T1_L2")$filterBounds(Chicago)$map(function(image){
  return(image$clip(Chicago))
})$map(function(img){
  d= ee$Date(img$get('system:time_start'));
  dy= d$get('day');    
  m= d$get('month');
  y= d$get('year');
  
  # # Add masks 
  img <- applyLandsatBitMask(img)
  
  # #scale correction; doing here & separating form NDVI so it gets saved on the image
  lAdj = img$select(c('SR_B1', 'SR_B2', 'SR_B3', 'SR_B4', 'SR_B5', 'SR_B6', 'SR_B7'))$multiply(0.0000275)$add(-0.2);
  lst_k = img$select('ST_B10')$multiply(0.00341802)$add(149);
  
  # img3 = img2$addBands(srcImg=lAdj, overwrite=T)$addBands(srcImg=lst_k, overwrite=T)$set('date',d, 'day',dy, 'month',m, 'year',y)
  return(img$addBands(srcImg=lAdj, overwrite=T)$addBands(srcImg=lst_k, overwrite=T)$set('date',d, 'day',dy, 'month',m, 'year',y))
})$select(c('SR_B2', 'SR_B3', 'SR_B4', 'SR_B5', 'SR_B6', 'SR_B7', 'ST_B10'),c('blue', 'green', 'red', 'nir', 'swir1', 'swir2', 'LST_K'))$map(addNDVI)
# Map$addLayer(landsat9$first()$select('NDVI'), ndviVis, "NDVI - First")
# ee_print(landsat9)
# Map$addLayer(landsat9$first()$select('NDVI'))

# Mosaic and rename bands (same as Landsat 8 above).
l9Mosaic = mosaicByDate(landsat9, 7)$select(c('blue_median', 'green_median', 'red_median', 'nir_median', 'swir1_median', 'swir2_median', 'LST_K_median', "NDVI_median"),c('blue', 'green', 'red', 'nir', 'swir1', 'swir2', 'LST_K', "NDVI"))$sort("date")
# ee_print(l9Mosaic, "landsat9-Mosaic")
# Map$addLayer(l9Mosaic$first()$select('NDVI'), ndviVis, "NDVI - First")

# Extract mean NDVI per land cover type and export to Google Drive.
# Mask NDVI by Landcover & condense to regional means
for(LCTYPE in lcnames){
  # print(LCTYPE)
  extractByLC(imcol=l9Mosaic, landcover=LCTYPE, outfolder=NDVIsave, fileNamePrefix=paste0("Landsat9_", LCTYPE))
}

##################### 

#####################
# Read in & Format Landsat 7 ----
#####################
# Landsat 7 ETM+ (2001-2023). Different band numbering than L8/L9:
#   SR_B1=blue, SR_B2=green, SR_B3=red, SR_B4=nir, SR_B5=swir1, SR_B7=swir2
#   ST_B6=thermal (not ST_B10 as in L8/L9).
# Same scale factors apply. Note: Landsat 7 developed a scan line corrector
# (SLC) failure in May 2003, causing striping in later images. The QA bitmask
# and mosaicking help mitigate this, but some data gaps remain.

# ""LANDSAT/LE07/C02/T1_L2""
# Load MODIS NDVI data; attach month & year
# https://developers.google.com/earth-engine/datasets/catalog/LANDSAT_LE07_C02_T1_L2
landsat7 <- ee$ImageCollection("LANDSAT/LE07/C02/T1_L2")$filterBounds(Chicago)$filterDate("2001-01-01", "2023-12-31")$map(function(image){
  return(image$clip(Chicago))
})$map(function(img){
  d= ee$Date(img$get('system:time_start'));
  dy= d$get('day');    
  m= d$get('month');
  y= d$get('year');
  
  # # Add masks 
  img <- applyLandsatBitMask(img)
  
  # #scale correction; doing here & separating form NDVI so it gets saved on the image
  lAdj = img$select(c('SR_B1', 'SR_B2', 'SR_B3', 'SR_B4', 'SR_B5', 'SR_B7'))$multiply(0.0000275)$add(-0.2);
  lst_k = img$select('ST_B6')$multiply(0.00341802)$add(149);
  
  # img3 = img2$addBands(srcImg=lAdj, overwrite=T)$addBands(srcImg=lst_k, overwrite=T)$set('date',d, 'day',dy, 'month',m, 'year',y)
  return(img$addBands(srcImg=lAdj, overwrite=T)$addBands(srcImg=lst_k, overwrite=T)$set('date',d, 'day',dy, 'month',m, 'year',y))
})$select(c('SR_B1', 'SR_B2', 'SR_B3', 'SR_B4', 'SR_B5', 'SR_B7', 'ST_B6'),c('blue', 'green', 'red', 'nir', 'swir1', 'swir2', 'LST_K'))$map(addNDVI)
# Map$addLayer(landsat7$first()$select('NDVI'), ndviVis, "NDVI - First")
# ee_print(landsat7)
# Map$addLayer(landsat7$first()$select('NDVI'))

# Mosaic and rename bands (same workflow as L8/L9).
l7Mosaic = mosaicByDate(landsat7, 7)$select(c('blue_median', 'green_median', 'red_median', 'nir_median', 'swir1_median', 'swir2_median', 'LST_K_median', "NDVI_median"),c('blue', 'green', 'red', 'nir', 'swir1', 'swir2', 'LST_K', "NDVI"))$sort("date")
# ee_print(l7Mosaic, "landsat7-Mosaic")
# Map$addLayer(l7Mosaic$first()$select('NDVI'), ndviVis, "NDVI - First")

# Extract mean NDVI per land cover type and export to Google Drive.
# Mask NDVI by Landcover & condense to regional means
for(LCTYPE in lcnames){
  # print(LCTYPE)
  extractByLC(imcol=l7Mosaic, landcover=LCTYPE, outfolder=NDVIsave, fileNamePrefix=paste0("Landsat7_", LCTYPE))
}

##################### 


#####################
# Read in & Format Landsat 5 ----
#####################
# Landsat 5 TM (2001-2012, decommissioned 2013). Same band naming as Landsat 7
# (SR_B1-B5, SR_B7, ST_B6). This is the earliest mission in the time series,
# providing NDVI observations from 2001 through ~2011 when combined with
# Landsat 7 to extend temporal coverage before Landsat 8 launched in 2013.

# "LANDSAT_LT05_C02_T1_L2"
# Load MODIS NDVI data; attach month & year
# https://developers.google.com/earth-engine/datasets/catalog/LANDSAT_LT05_C02_T1_L2
landsat5 <- ee$ImageCollection("LANDSAT/LT05/C02/T1_L2")$filterBounds(Chicago)$filterDate("2001-01-01", "2022-12-31")$map(function(image){
  return(image$clip(Chicago))
})$map(function(img){
  d= ee$Date(img$get('system:time_start'));
  dy= d$get('day');    
  m= d$get('month');
  y= d$get('year');
  
  # # Add masks 
  img <- applyLandsatBitMask(img)
  
  # #scale correction; doing here & separating form NDVI so it gets saved on the image
  lAdj = img$select(c('SR_B1', 'SR_B2', 'SR_B3', 'SR_B4', 'SR_B5', 'SR_B7'))$multiply(0.0000275)$add(-0.2);
  lst_k = img$select('ST_B6')$multiply(0.00341802)$add(149);
  
  # img3 = img2$addBands(srcImg=lAdj, overwrite=T)$addBands(srcImg=lst_k, overwrite=T)$set('date',d, 'day',dy, 'month',m, 'year',y)
  return(img$addBands(srcImg=lAdj, overwrite=T)$addBands(srcImg=lst_k, overwrite=T)$set('date',d, 'day',dy, 'month',m, 'year',y))
})$select(c('SR_B1', 'SR_B2', 'SR_B3', 'SR_B4', 'SR_B5', 'SR_B7', 'ST_B6'),c('blue', 'green', 'red', 'nir', 'swir1', 'swir2', 'LST_K'))$map(addNDVI)
# Map$addLayer(landsat5$first()$select('NDVI'), ndviVis, "NDVI - First")
# ee_print(landsat5)
# Map$addLayer(landsat5$first()$select('NDVI'))

# Mosaic and rename bands (same workflow as L7/L8/L9).
l5Mosaic = mosaicByDate(landsat5, 7)$select(c('blue_median', 'green_median', 'red_median', 'nir_median', 'swir1_median', 'swir2_median', 'LST_K_median', "NDVI_median"),c('blue', 'green', 'red', 'nir', 'swir1', 'swir2', 'LST_K', "NDVI"))$sort("date")
# ee_print(l5Mosaic, "landsat5-Mosaic")
# Map$addLayer(l5Mosaic$first()$select('NDVI'), ndviVis, "NDVI - First")

# Extract mean NDVI per land cover type and export to Google Drive.
# Mask NDVI by Landcover & condense to regional means
for(LCTYPE in lcnames){
  # print(LCTYPE)
  extractByLC(imcol=l5Mosaic, landcover=LCTYPE, outfolder=NDVIsave, fileNamePrefix=paste0("Landsat5_", LCTYPE))
}
#####################


# Copy outputs from personal Google Drive to shared project drive -----------
# GEE export tasks run asynchronously. Once all tasks have completed (check the
# GEE task manager), this block copies the per-mission, per-land-cover CSVs
# to the shared drive so collaborators can access the raw NDVI time series.
# After all tasks have run, move them to the shared drive
filesNOW <- dir(file.path("~/Google Drive/My Drive/", NDVIsave))
for(FILE in filesNOW){
  print(FILE)
  file.copy(from=file.path("~/Google Drive/My Drive/", NDVIsave, FILE), to=file.path(path.google.share, "data/data_raw", FILE), overwrite=T, copy.mode=T)
}
