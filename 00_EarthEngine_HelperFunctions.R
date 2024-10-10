# Script with a bunch of commonly used helper functions to streamline code

addTime <- function(image){ 
  return(image$addBands(image$metadata('system:time_start')$divide(1000 * 60 * 60 * 24 * 365)))
}

setYear <- function(img){
  return(img$set("year", img$date()$get("year")))
}

addYear = function(img) {
  d= ee$Date(ee$Number(img$get('system:time_start')));
  y= ee$Number(d$get('year'));
  return(img$set('year', y));
}

bitwiseExtract <- function(input, fromBit, toBit) {
  maskSize <- ee$Number(1)$add(toBit)$subtract(fromBit)
  mask <- ee$Number(1)$leftShift(maskSize)$subtract(1)
  return(input$rightShift(fromBit)$bitwiseAnd(mask))
}

addNDVI <- function(img){
  return( img$addBands(img$normalizedDifference(c('nir','red'))$rename('NDVI')));
}


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

regionNDVIMean <- function(img){
  RedMn = img$select("NDVI")$reduceRegion(reducer= ee$Reducer$mean(), geometry=Chicago$geometry(),
                                          scale=30, # hard-coded, but it's what has to happen to work
                                          maxPixels=1e13)
  return(ee$Feature(NULL, RedMn)$set('system:time_start', img$get('system:time_start'))$set('date', ee$Date(img$get('system:time_start'))$format("YYYY-MM-dd")))
}

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
