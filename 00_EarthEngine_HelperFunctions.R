# ==============================================================================
# 00_EarthEngine_HelperFunctions.R
# ==============================================================================
# Google Earth Engine helper functions for the Urban Ecological Drought project.
# Called via source() by scripts 01 and 02 -- not run standalone.
#
# These functions handle common GEE operations needed to extract Landsat NDVI
# time series by land cover type across the Chicago metropolitan region.
# They are written using the rgee R-to-Python bridge, so GEE objects (ee$...)
# follow the Earth Engine Python API conventions.
# ==============================================================================

# Script with a bunch of commonly used helper functions to streamline code

# -- addTime ------------------------------------------------------------------
# Adds image acquisition time as a numeric band (fractional years since epoch).
# Used when building time series regressions or temporal joins in GEE.
addTime <- function(image){
  return(image$addBands(image$metadata('system:time_start')$divide(1000 * 60 * 60 * 24 * 365)))
}

# -- setYear / addYear --------------------------------------------------------
# Two implementations that both tag each image with its acquisition year as a
# metadata property. The year property is used downstream to match Landsat
# images to the corresponding annual NLCD land cover mask.
setYear <- function(img){
  return(img$set("year", img$date()$get("year")))
}

addYear = function(img) {
  d= ee$Date(ee$Number(img$get('system:time_start')));
  y= ee$Number(d$get('year'));
  return(img$set('year', y));
}

# -- bitwiseExtract -----------------------------------------------------------
# Extracts a range of bits (fromBit to toBit, inclusive) from an integer-valued
# QA band. Landsat Collection 2 Level 2 encodes quality flags (cloud, shadow,
# snow, water, saturation) as packed bits in QA_PIXEL and QA_RADSAT bands.
# This utility isolates the relevant bits so they can be tested against
# confidence thresholds.
bitwiseExtract <- function(input, fromBit, toBit) {
  maskSize <- ee$Number(1)$add(toBit)$subtract(fromBit)
  mask <- ee$Number(1)$leftShift(maskSize)$subtract(1)
  return(input$rightShift(fromBit)$bitwiseAnd(mask))
}

# -- addNDVI ------------------------------------------------------------------
# Computes the Normalized Difference Vegetation Index: NDVI = (NIR - Red) / (NIR + Red).
# NDVI is the primary response variable in the drought sensitivity analysis.
# Expects bands already renamed to common names ('nir', 'red').
addNDVI <- function(img){
  return( img$addBands(img$normalizedDifference(c('nir','red'))$rename('NDVI')));
}


# -- applyLandsatBitMask ------------------------------------------------------
# Applies the Landsat Collection 2 Level 2 QA bitmask to remove contaminated
# pixels. Masks out: clouds, cloud shadows, snow/ice, water, terrain-occluded
# pixels, and saturated pixels in the red/NIR bands used for NDVI.
#
# Uses LOW confidence thresholds (lte(1)) for cloud, shadow, and snow flags
# because stricter thresholds (medium/high confidence) remove too many
# observations in the Chicago region, leaving insufficient data for analysis.
applyLandsatBitMask = function(img){
  qaPix <- img$select('QA_PIXEL');
  qaRad <- img$select('QA_RADSAT');
  terrMask <- qaRad$bitwiseAnd(11)$eq(0); ## get rid of any terrain occlusion
  # satMask <- qaRad$bitwiseAnd(3 << 4)$eq(0); ## get rid of any saturated bands we use to calculate NDVI
  satMask <- bitwiseExtract(qaRad, 3, 4)$eq(0) ## get rid of any saturated bands we use to calculate NDVI 
  # clearMask <- qaPix$bitwiseAnd(1<<7)$eq(0)
  clearMask <- bitwiseExtract(qaPix, 1, 5)$eq(0)
  waterMask <- bitwiseExtract(qaPix, 7, 7)$eq(0)
  cloudConf = bitwiseExtract(qaPix, 8, 9)$lte(1) ## we can only go with low confidence; doing finer leads to NOTHING making the cut
  shadowConf <- bitwiseExtract(qaPix, 10, 11)$lte(1) ## we can only go with low confidence; doing finer leads to NOTHING making the cut
  snowConf <- bitwiseExtract(qaPix, 12, 13)$lte(1) ## we can only go with low confidence; doing finer leads to NOTHING making the cut
  
  
  img <- img$updateMask(clearMask$And(waterMask)$And(cloudConf)$And(shadowConf)$And(snowConf)$And(terrMask)$And(satMask));
  
  return(img)
  
}

# -- mosaicByDate --------------------------------------------------------------
# Combines overlapping Landsat scenes acquired on or near the same date using
# a median reducer. Chicago spans two Landsat path/row tiles (WRS-2 paths
# 22-23), so multiple scenes can cover the region on the same day. Mosaicking
# with a median within a +/- dayWindow (typically 7 days) reduces noise from
# path/row overlap and produces a single composite image per date.
#
# Function for combining images with the same date
# 2nd response from here: https:#gis.stackexchange.com/questions/280156/mosaicking-image-collection-by-date-day-in-google-earth-engine
mosaicByDate <- function(imcol, dayWindow){
  # imcol: An image collection
  # returns: An image collection
  imlist = imcol$toList(imcol$size())
  
  # Note: needed to specify the ee_utils_pyfunc since it's not an image collection
  unique_dates <- imlist$map(ee_utils_pyfunc(function(img){
    return(ee$Image(img)$date()$format("YYYY-MM-dd"))
  }))$distinct()
  
  # Same as above: what we're mappign through is a List, so need to call python
  mosaic_imlist = unique_dates$map(ee_utils_pyfunc(function(d){
    d = ee$Date(d)
    dy= d$get('day');    
    m= d$get('month');
    y= d$get('year');
    
    im = imcol$filterDate(d$advance(-dayWindow, "day"), d$advance(dayWindow, "day"))$reduce(ee$Reducer$median()) # shoudl influence the window for image aggregation
    
    return(im$set("system:time_start", d$millis(), 
                  "system:id", d$format("YYYY-MM-dd"),
                  'date', d, 'day', dy, 'month', m, 'year', y))
  }))
  
  # testOUT <- ee$ImageCollection(mosaic_imlist)
  # ee_print(testOUT)
  return (ee$ImageCollection(mosaic_imlist))
}

# -- maskByLC ------------------------------------------------------------------
# Applies a year-specific NLCD land cover mask to an image collection. Each
# Landsat image is matched to the corresponding annual band in the multi-band
# mask image (e.g., band "YR2012" for a 2012 image). This ensures that land
# cover changes over time are accounted for -- a pixel classified as forest in
# 2004 but developed by 2016 will only be included in the forest analysis for
# the years it was actually forested.
maskByLC <- function(imcol, MASK){
  imcol <- imcol$map(function(img){
    # Note: This is very slow, but I don't know how else to match the bands
    yrNow = ee$Number(img$get('year'))$format()$slice(0) # May be able to work around the slice, but I kept getting format issues
    yrStr = ee$String("YR")$cat(yrNow) # Need to figure out how to pull the right band
    
    maskNow = MASK$select(yrStr); # Very clunky, but it works!
    
    return(img$updateMask(maskNow))
    
  })
  return(imcol)
}

# -- regionNDVIMean ------------------------------------------------------------
# Computes the spatial mean NDVI across the 7-county Chicago region at 30 m
# (native Landsat) resolution. Returns an ee$Feature with the mean NDVI value
# and the image acquisition date. This spatial averaging produces a single
# regional NDVI value per date per land cover type, which becomes one
# observation in the downstream mixed-effects models.
regionNDVIMean <- function(img){
  RedMn = img$select("NDVI")$reduceRegion(reducer= ee$Reducer$mean(), geometry=Chicago$geometry(),
                                          scale=30, # hard-coded, but it's what has to happen to work
                                          maxPixels=1e13)
  return(ee$Feature(NULL, RedMn)$set('system:time_start', img$get('system:time_start'))$set('date', ee$Date(img$get('system:time_start'))$format("YYYY-MM-dd")))
}

# -- extractByLC ---------------------------------------------------------------
# Full extraction pipeline: masks an image collection by a specified land cover
# type, computes regional mean NDVI for each date, and exports the resulting
# time series to Google Drive as a CSV. This is the main entry point called
# in script 02 for each combination of Landsat mission x land cover class.
#
# Workflow: (1) select the appropriate binary mask for the land cover class,
# (2) apply the year-matched mask via maskByLC(), (3) compute spatial mean
# NDVI via regionNDVIMean(), (4) export date + NDVI columns to Google Drive.
#
# Function to extract things by landcover type; note: there are some not soft-coded options here,
#   so you'll need to make sure that landcover and mask names match what Christy is using in this repo
extractByLC <- function(imcol, landcover, outfolder, fileNamePrefix, ...){
  lcnames <- c("forest", "crop", "grassland", "urban-high", "urban-medium", "urban-low", "urban-open")
  if(!landcover %in% lcnames){
    stop(paste("Invalid landcover type.  Must match one of the following:", paste(lcnames, collapse = ", ")))
  }
  if(landcover=="forest") lcMask = forMask
  if(landcover=="crop") lcMask = cropMask
  if(landcover=="grassland") lcMask = grassMask
  if(landcover=="urban-high") lcMask = urbHMask
  if(landcover=="urban-medium") lcMask = urbMMask
  if(landcover=="urban-low") lcMask = urbLMask
  if(landcover=="urban-open") lcMask = urbOMask

  ndviLCYear <- maskByLC(imcol, lcMask)

  # regionNDVIMean is a function defied above
  LCMeans = ee$FeatureCollection(ndviLCYear$map(regionNDVIMean))

  LCMeansSave <- ee_table_to_drive(collection=LCMeans,
                                    description=paste0("Save_", fileNamePrefix),
                                    folder=outfolder,
                                    fileNamePrefix=fileNamePrefix,
                                    timePrefix=F,
                                    fileFormat="CSV",
                                    selectors=c("date", "NDVI"))
  LCMeansSave$start()

  return(print(paste0(fileNamePrefix, " processed! Check Earth Engine queue for status")))
}
