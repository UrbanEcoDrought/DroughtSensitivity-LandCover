# Script to create a harmonized nlcd layer with landcover info from 2000 - 2023 and then individual landcover class masks to apply
# TO BE RUN ONCE!
# Steps: 
# 1. Read in nlcd layer
# 2. Set up dupe layers for years in between NLCD product years
# 3. Create & save annual-year masks for each set of landcover classes


library(rgee); library(raster); library(terra)
ee_check() # For some reason, it's important to run this before initializing right now
# ee_Authenticate(user="crollinson@mortonarb.org")
rgee::ee_Initialize(user = 'crollinson@mortonarb.org', drive=T, project="UrbanEcoDrought")
path.google.CR <- "~/Google Drive/My Drive/UrbanEcoDrought/"
path.google.share <- "~/Google Drive/Shared drives/Urban Ecological Drought/"
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
# Setting the center point for the Arb because I think we'll have more variation
Map$setCenter(-88.04526, 41.81513, 11);

# Adding Landcover Classes!
nlcdPalette = c(
    '#5475A8', # Open Water (11)
    # '#d1def8', # Ice/Snow (12)
    '#dec5c5', # Developed, Open Space (21)
    '#d99282', # Developed, Low Intensity (22)
    '#eb0000', # Developed, Medium Intensity (23)
    '#ab0000', # Developed High Intensity (24)
    '#b3ac9f', # Barren Land (31)
    '#68ab5f', # Deciduous Forest (41)
    '#1c5f2c', # Evergreen Forest (42)
    '#b5c58f', # Mixed Forest (43)
    # '#af963c', # Dwarf Shrub/Scrub (51); Alaska Only
    '#ccb879', # Shrub/Scrub (52)
    '#dfdfc2', # Grassland/Herbaceous (71)
    # '#d1d182', # Sedge/herbaceous (72); Alaska Only
    # '#a3cc51', # lichens (73); Alaska Only
    # '#82ba9e', # Moss (74); Alaska Only
    '#dcd939', # Pasture/Hay (81)
    '#ab6c28', # Cultivated Crops (82)
    '#b8d9eb', # Woody Wetlands (90)
    '#6c9fb8' # Emergent Herbaceous Wetlands (95)
  );

nlcdvis = list(
  min= 0,
  max= 95,
  palette= nlcdPalette
);
##################### 

##################### 
# Read in base layers ----
##################### 
Chicago = ee$FeatureCollection("projects/breidyee/assets/SevenCntyChiReg") 
ee_print(Chicago)

chiBounds <- Chicago$geometry()$bounds()
chiBBox <- ee$Geometry$BBox(-88.70738, 41.20155, -87.52453, 42.49575)

# https://developers.google.com/earth-engine/datasets/catalog/USGS_NLCD_RELEASES_2019_REL_NLCD
nlcdChi <- ee$ImageCollection('USGS/NLCD_RELEASES/2019_REL/NLCD')$select('landcover')$map(function(img){
    d <- ee$Date(ee$Number(img$get('system:time_start')));
    y <- ee$Number(d$get('year'));
  return(img$clip(Chicago)$set('year', y))
})

# https://developers.google.com/earth-engine/datasets/catalog/USGS_NLCD_RELEASES_2021_REL_NLCD#description
nlcdChi21 <- ee$ImageCollection('USGS/NLCD_RELEASES/2021_REL/NLCD')$select('landcover')$map(function(img){
  d <- ee$Date(ee$Number(img$get('system:time_start')));
  y <- ee$Number(d$get('year'));
  return(img$clip(Chicago)$set('year', y))
})

# ee_print(nlcdChi21)

lcVals = nlcdChi$first()$reduceRegion(ee$Reducer$frequencyHistogram(), Chicago, maxPixels=1e12)
# ee_print(lcVals)
lcVals$getInfo()

# ee_print(nlcdChi) # Note: the nlcd is giving me a strsplit code error, but it does map!
Map$addLayer(nlcdChi$first()$select('landcover'), nlcdvis, 'NLCD Land Cover',);
Map$addLayer(nlcdChi21$first()$select('landcover'), nlcdvis, 'NLCD Land Cover',);


projNLCD = nlcdChi$select("landcover")$first()$projection()
projNLCD$getInfo() # Note that this is really messy
# projCRS = projNLCD$crs()
projCRS = "EPSG:4326" # This seems to be what works
projTransform <- unlist(projNLCD$getInfo()$transform) # I saved this, but using it in the export causes really weird things

# Chicago <- Chicago$map(function(FEAT){return(FEAT$reproject(projNLCD))})

# chiGeom <- Chicago$geometry()
# 
# chiTransform = c(1,0,0,0,1,0)



nlcdProj = "EPSG:4326"
nlcdTransform = c(30, 0, -2493045, 0, -30, 3310005)
##################### 

##################### 
# Create annually-resolved image ----
##################### 
lcChi2001 <- nlcdChi$filter(ee$Filter$eq('system:index', '2001'))$first();
# ee_print(lcChi2001)
# Map$addLayer(lcChi2001, nlcdvis, 'NLCD Land Cover');
lcChi2004 <- nlcdChi$filter(ee$Filter$eq('system:index', '2004'))$first();
lcChi2006 <- nlcdChi$filter(ee$Filter$eq('system:index', '2006'))$first();
lcChi2008 <- nlcdChi$filter(ee$Filter$eq('system:index', '2008'))$first();
lcChi2011 <- nlcdChi$filter(ee$Filter$eq('system:index', '2011'))$first();
lcChi2013 <- nlcdChi$filter(ee$Filter$eq('system:index', '2013'))$first();
lcChi2016 <- nlcdChi$filter(ee$Filter$eq('system:index', '2016'))$first();
lcChi2019 <- nlcdChi$filter(ee$Filter$eq('system:index', '2019'))$first();
lcChi2021 <- nlcdChi21$filter(ee$Filter$eq('system:index', '2021'))$first();
# ee_print(lcChi2019)
# Map$addLayer(lcChi2019, nlcdvis, 'NLCD Land Cover 2019');
# Map$addLayer(lcChi2021, nlcdvis, 'NLCD Land Cover 2021');

# Creating duped layers for each year for our sanity
lcChi2000 <- lcChi2001$set('system:time_start', ee$Date$fromYMD(2000, 1, 1))$set('year',2000);
# lcChi2001 <- nlcdChi$filter(ee$Filter$eq('system:index', '2001'))$first();
lcChi2002 <- lcChi2001$set('system:time_start', ee$Date$fromYMD(2002, 1, 1))$set('year',2002);
lcChi2003 <- lcChi2004$set('system:time_start', ee$Date$fromYMD(2003, 1, 1))$set('year',2003);
# lcChi2004 <- nlcdChi$filter(ee$Filter$eq('system:index', '2004'))$first();
lcChi2005 <- lcChi2006$set('system:time_start', ee$Date$fromYMD(2005, 1, 1))$set('year',2005);
# lcChi2006 <- nlcdChi$filter(ee$Filter$eq('system:index', '2006'))$first();
lcChi2007 <- lcChi2008$set('system:time_start', ee$Date$fromYMD(2007, 1, 1))$set('year',2007);
# lcChi2008 <- nlcdChi$filter(ee$Filter$eq('system:index', '2008'))$first();
lcChi2009 <- lcChi2008$set('system:time_start', ee$Date$fromYMD(2009, 1, 1))$set('year',2009);
lcChi2010 <- lcChi2011$set('system:time_start', ee$Date$fromYMD(2010, 1, 1))$set('year',2010);
# lcChi2011 <- nlcdChi$filter(ee$Filter$eq('system:index', '2011'))$first();
lcChi2012 <- lcChi2013$set('system:time_start', ee$Date$fromYMD(2012, 1, 1))$set('year',2012);
# lcChi2013 <- nlcdChi$filter(ee$Filter$eq('system:index', '2013'))$first();
lcChi2014 <- lcChi2013$set('system:time_start', ee$Date$fromYMD(2014, 1, 1))$set('year',2014);
lcChi2015 <- lcChi2016$set('system:time_start', ee$Date$fromYMD(2015, 1, 1))$set('year',2015);
# lcChi2016 <- nlcdChi$filter(ee$Filter$eq('system:index', '2016'))$first();
lcChi2017 <- lcChi2016$set('system:time_start', ee$Date$fromYMD(2017, 1, 1))$set('year',2017);
lcChi2018 <- lcChi2019$set('system:time_start', ee$Date$fromYMD(2018, 1, 1))$set('year',2018);
# lcChi2019 <- nlcdChi$filter(ee$Filter$eq('system:index', '2019'))$first();
lcChi2020 <- lcChi2019$set('system:time_start', ee$Date$fromYMD(2020, 1, 1))$set('year',2020);
lcChi2021 <- lcChi2021$set('system:time_start', ee$Date$fromYMD(2021, 1, 1))$set('year',2021);
lcChi2022 <- lcChi2021$set('system:time_start', ee$Date$fromYMD(2022, 1, 1))$set('year',2022);
lcChi2023 <- lcChi2021$set('system:time_start', ee$Date$fromYMD(2023, 1, 1))$set('year',2023);
lcChi2024 <- lcChi2021$set('system:time_start', ee$Date$fromYMD(2024, 1, 1))$set('year',2024);


collAnn <- ee$ImageCollection(c(lcChi2001, lcChi2002, lcChi2003, lcChi2004, lcChi2005, lcChi2006, lcChi2007, lcChi2008, lcChi2009, lcChi2010, lcChi2011, lcChi2012, lcChi2013, lcChi2014, lcChi2015, lcChi2016, lcChi2017, lcChi2018, lcChi2019, lcChi2020, lcChi2021, lcChi2022, lcChi2023, lcChi2024))

# Saving will be much easier if it's a single year with multiple bands
yrLC <- ee$List(collAnn$aggregate_array("year"))$distinct()
# yrLC$getInfo()
yrString <- ee$List(paste0("YR", yrLC$getInfo()))

lcChiAnn <- ee$ImageCollection$toBands(collAnn)$rename(yrString)
# ee_print(lcChiAnn)
Map$addLayer(lcChiAnn$select("YR2012"), nlcdvis, 'NLCD Land Cover');

saveLandCover <- ee_image_to_asset(lcChiAnn, description="Save_NLCD-Chicago_AnnualDupe_2000-2024", assetId=file.path(assetHome, "NLCD-Chicago_AnnualDupe_2000-2024"), maxPixels = 10e9, scale=30, region = chiBounds, crs=projCRS, overwrite=T)
saveLandCover$start()

##################### 

##################### 
# Set up masks for our key landcover classes here! ----
# Note: This will use our Collection rather than the single image that has bands by years
# # Forest: 41,42,43
# # Shrub/Savanna/Grass: 51,52,53,71,72
# # Crop = 81,82
# # Open Urban: 21
# # Low Intensity Urban: 22
# # Medium Intensity Urban: 23
# # High Intensity Urban: 24
##################### 

# # Define the land cover classes and their corresponding values

# # Forest: 41,42,43 ----
classFor = collAnn$map(function(image) {
  d = ee$Date(ee$Number(image$get('system:time_start')))
  lcMask = image$select('landcover')$eq(41)$Or(image$select('landcover')$eq(42))$Or(image$select('landcover')$eq(43));
  return(image$updateMask(lcMask)$set('class', 'Forest')$set('year', d$get('year')));
});

forMask <- ee$ImageCollection$toBands(classFor)$rename(yrString)
# ee_print(forMask)
# Map$addLayer(forMask$select("YR2012"));

saveForMask <- ee_image_to_asset(forMask, description="Save_lcMask-Forest_2000-2024", assetId=file.path(assetHome, "NLCD-Chicago_2000-2024_Forest"), maxPixels = 10e9, scale=30, region = chiBounds, crs="EPSG:4326", overwrite=T)
saveForMask$start()
# nlcdProj



# # Grassland/Savanna/Grass: 51,52,71,72 ----
classGrass = collAnn$map(function(image) {
  d = ee$Date(ee$Number(image$get('system:time_start')))
  lcMask = image$select('landcover')$eq(51)$Or(image$select('landcover')$eq(52))$Or(image$select('landcover')$eq(71))$Or(image$select('landcover')$eq(72));
  return(image$updateMask(lcMask)$set('class', 'Grassland')$set('year', d$get('year')));
});

grassMask <- ee$ImageCollection$toBands(classGrass)$rename(yrString)

saveGrassMask <- ee_image_to_asset(grassMask, description="Save_lcMask-Grass_2000-2024", assetId=file.path(assetHome, "NLCD-Chicago_2000-2024_Grass"), maxPixels = 10e9, scale=30, region = chiBounds, crs="EPSG:4326", overwrite=T)
saveGrassMask$start()



# # Crop = 81,82 ----
classCrop = collAnn$map(function(image) {
  d = ee$Date(ee$Number(image$get('system:time_start')))
  lcMask = image$select('landcover')$eq(81)$Or(image$select('landcover')$eq(82));
  return(image$updateMask(lcMask)$set('class', 'Crop')$set('year', d$get('year')));
});

cropMask <- ee$ImageCollection$toBands(classCrop)$rename(yrString)

saveCropMask <- ee_image_to_asset(cropMask, description="Save_lcMask-Crop_2000-2024", assetId=file.path(assetHome, "NLCD-Chicago_2000-2024_Crop"), maxPixels = 10e9, scale=30, region = chiBounds, crs="EPSG:4326", overwrite=T)
saveCropMask$start()


# # Open Urban: 21 ----
classUrbO = collAnn$map(function(image) {
  d = ee$Date(ee$Number(image$get('system:time_start')))
  lcMask = image$select('landcover')$eq(21);
  return(image$updateMask(lcMask)$set('class', 'Urban-Open')$set('year', d$get('year')));
});

urbOMask <- ee$ImageCollection$toBands(classUrbO)$rename(yrString)

saveUrbOMask <- ee_image_to_asset(urbOMask, description="Save_lcMask-Urban-Open_2000-2024", assetId=file.path(assetHome, "NLCD-Chicago_2000-2024_Urban-Open"), maxPixels = 10e9, scale=30, region = chiBounds, crs="EPSG:4326", overwrite=T)
saveUrbOMask$start()


# # Low Intensity Urban: 22 ----
classUrbL = collAnn$map(function(image) {
  d = ee$Date(ee$Number(image$get('system:time_start')))
  lcMask = image$select('landcover')$eq(22);
  return(image$updateMask(lcMask)$set('class', 'Urban-Low')$set('year', d$get('year')));
});

urbLMask <- ee$ImageCollection$toBands(classUrbL)$rename(yrString)

saveUrbLMask <- ee_image_to_asset(urbLMask, description="Save_lcMask-Urban-Low_2000-2024", assetId=file.path(assetHome, "NLCD-Chicago_2000-2024_Urban-Low"), maxPixels = 10e9, scale=30, region = chiBounds, crs="EPSG:4326", overwrite=T)
saveUrbLMask$start()



# # Medium Intensity Urban: 23 ----
classUrbM = collAnn$map(function(image) {
  d = ee$Date(ee$Number(image$get('system:time_start')))
  lcMask = image$select('landcover')$eq(23);
  return(image$updateMask(lcMask)$set('class', 'Urban-Medium')$set('year', d$get('year')));
});

urbMMask <- ee$ImageCollection$toBands(classUrbM)$rename(yrString)

saveUrbMMask <- ee_image_to_asset(urbMMask, description="Save_lcMask-Urban-Medium_2000-2024", assetId=file.path(assetHome, "NLCD-Chicago_2000-2024_Urban-Medium"), maxPixels = 10e9, scale=30, region = chiBounds, crs="EPSG:4326", overwrite=T)
saveUrbMMask$start()


# # High Intenstity Urban: 24 ----
classUrbH = collAnn$map(function(image) {
  d = ee$Date(ee$Number(image$get('system:time_start')))
  lcMask = image$select('landcover')$eq(24);
  return(image$updateMask(lcMask)$set('class', 'Urban-Low')$set('year', d$get('year')));
});

urbHMask <- ee$ImageCollection$toBands(classUrbH)$rename(yrString)

saveUrbHMask <- ee_image_to_asset(urbHMask, description="Save_lcMask-Urban-High_2000-2024", assetId=file.path(assetHome, "NLCD-Chicago_2000-2024_Urban-High"), maxPixels = 10e9, scale=30, region = chiBounds, crs="EPSG:4326", overwrite=T)
saveUrbHMask$start()
##################### 

