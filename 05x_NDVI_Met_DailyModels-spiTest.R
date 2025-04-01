# Re-running the partial effects script with the leading SPI model

# Leading SPI Additive Model
# SPI30day; Tmax_30day

# Creating a clean script to re-do the daily correlation modeling and do some prediction from it
library(ggplot2)
library(lubridate)
library(ggcorrplot)
library(dplyr)

# Settindbplyr# Setting the file paths. This may be different for your computer.
# Sys.setenv(GOOGLE_DRIVE = "G:/Shared drives/Urban Ecological Drought/Manuscript - Urban Drought NDVI Daily Corrs/")
Sys.setenv(GOOGLE_DRIVE = "~/Google Drive/Shared drives/Urban Ecological Drought/Manuscript - Urban Drought NDVI/")
google.drive <- Sys.getenv("GOOGLE_DRIVE")

path.NDVI <- file.path(google.drive, "data", "data_raw")
path.figs <- file.path(google.drive, "exploratory figures/FinalDailyModel")
pathSave <- file.path(google.drive, "data/processed_files/FinalDailyModel")

# Read in the two key data frames
ndviMet <- read.csv(file.path(google.drive,"data/processed_files/landsat_ndvi_metVars_combined.csv"))
ndviMet$date <- as.Date(ndviMet$date)
ndviMet$mission <- as.factor(ndviMet$mission) 
ndviMet$landcover <- as.factor(ndviMet$landcover)
summary(ndviMet)

# # # # # # # # # # # # # # # # # # # # # # # # # 
# Follow-up Analysis----
# Alternate way of visualizing -- partial effects (coeficient x (mean?)value -- give impact in NDVI space) --> that would get away from stat. sig. and into acctual effect space
# Partial effects will be the model coefficient multiplied by predictor value; For met partial effects, it will be the climatic norm
# # # # # # # # # # # # # # # # # # # # # # # # # 
# Reading in our met vars
ChicagolandSPI <- read.csv(file.path(google.drive, "../data/GRIDMET_data/gridmet_aggregated_data/Chicagoland_Daily_Aggregated_SPI.csv"))
ChicagolandSPEI <- read.csv(file.path(google.drive, "../data/GRIDMET_data/gridmet_aggregated_data/Chicagoland_Daily_Aggregated_SPEI.csv"))
ChicagolandTmin <- read.csv(file.path(google.drive, "../data/GRIDMET_data/gridmet_aggregated_data/Chicagoland_Daily_Aggregated_Tmin.csv"))
ChicagolandTmax <- read.csv(file.path(google.drive, "../data/GRIDMET_data/gridmet_aggregated_data/Chicagoland_Daily_Aggregated_Tmax.csv"))

ChicagolandSPI$date <- as.Date(paste(ChicagolandSPI$Year, ChicagolandSPI$Month, ChicagolandSPI$Day, sep="-"), "%Y-%m-%d")
ChicagolandSPEI$date <- as.Date(ChicagolandSPEI$Date, "%m/%d/%Y")
ChicagolandTmin$date <- as.Date(paste(ChicagolandTmin$Year, ChicagolandTmin$Month, ChicagolandTmin$Day, sep="-"), "%Y-%m-%d")
ChicagolandTmax$date <- as.Date(paste(ChicagolandTmax$Year, ChicagolandTmax$Month, ChicagolandTmax$Day, sep="-"), "%Y-%m-%d")

# making day, month and yday vars for SPEI
ChicagolandSPEI$Day <- lubridate::day(ChicagolandSPEI$date)
ChicagolandSPEI$Month <- lubridate::month(ChicagolandSPEI$date)
ChicagolandSPEI$DOY <- lubridate::yday(ChicagolandSPEI$date)


summary(ChicagolandSPI)
summary(ChicagolandSPEI)
summary(ChicagolandTmin)
summary(ChicagolandTmax)

# cleaning up the dataframes a bit so that the merges go through easier
names(ChicagolandSPI)
ChicagolandSPI2 <- ChicagolandSPI[,!names(ChicagolandSPI) %in% c("Year", "Month", "Day", "DOY")]

names(ChicagolandSPEI)
ChicagolandSPEI2 <- ChicagolandSPEI[,!names(ChicagolandSPEI) %in% c("Date", "Month", "Day", "DOY")]

names(ChicagolandTmin)
ChicagolandTmin2 <- ChicagolandTmin[,!names(ChicagolandTmin) %in% c("Year", "Month", "Day", "DOY")]

names(ChicagolandTmax)
ChicagolandTmax2 <- ChicagolandTmax[,!names(ChicagolandTmax) %in% c("Year", "Month", "Day", "DOY")]

dim(ChicagolandSPI2); dim(ChicagolandSPEI2); dim(ChicagolandTmin2); dim(ChicagolandTmax2)
summary(ChicagolandSPI2)
summary(ChicagolandSPEI2)
summary(ChicagolandTmin2)
summary(ChicagolandTmax2)
# Combining met data together in a single data frame
# chiMetpre1 <- merge(ChicagolandTmin, ChicagolandSPI, all=T, by=c("date"))
# chiMetpre2 <- merge(chiMetpre1, ChicagolandSPEI , all=T)
# chiMet <- merge(chiMetpre2, ChicagolandTmax, all=T)
# summary(chiMet)
library(dplyr)
library(purrr)
df_list <- list(ChicagolandSPI2, ChicagolandSPEI2, ChicagolandTmax2, ChicagolandTmin2)
chiMet <- reduce(df_list, full_join, by="date")
summary(chiMet)

# removing the 7day variable that snuck in there
head(chiMet)
chiMet <- chiMet[,!names(chiMet) %in% c("Tmax_7day", "Tmin_7day")]
chiMet$yday <- lubridate::yday(chiMet$date)

head(chiMet[chiMet$yday>80 & chiMet$yday<90,])
# want to hang on to chi met so that we can use it to calculate the yearly partial effects later on.

# Gettign the climatic norm to get our met partial effects
# calculating normals just over the period that we have Satellite data
chiMetNorms <- aggregate(cbind(SPEI14, Tmax_30day) ~ yday, data=chiMet[chiMet$date>=as.Date("2000-01-01"),], FUN=mean, na.rm=T)
summary(chiMetNorms)

dim(chiMetNorms)

# # # # # # # # # # # # # # # # # # # # # # # # # 
# Partial Effects----
# generating code to calculate the partial effects throughout the different DOY for the different land cover models.
# need to bring in the mean conditions for DOY for temp, SPEI, and for NDVI
library(dplyr)
library(zoo)

# loading the model data frames so that we can merge in the met data and create partial effects.
# loading the SPEI14 and TMAX14 models as a place to start. Need to check wiht @crollinson to make sure this is the correct model form. 
crop.add3 <- read.csv(file.path(pathSave, paste0("DailyModel_FinalModel_Stats_Additive_SPI30-TMAX30_crop.csv")))
forest.add3 <- read.csv(file.path(pathSave, paste0("DailyModel_FinalModel_Stats_Additive_SPI30-TMAX30_forest.csv")))
grassland.add3 <- read.csv(file.path(pathSave, paste0("DailyModel_FinalModel_Stats_Additive_SPI30-TMAX30_grassland.csv")))
urbanHigh.add3 <- read.csv(file.path(pathSave, paste0("DailyModel_FinalModel_Stats_Additive_SPI30-TMAX30_urban-high.csv")))
urbanLow.add3 <- read.csv(file.path(pathSave, paste0("DailyModel_FinalModel_Stats_Additive_SPI30-TMAX30_urban-low.csv")))
urbanOpen.add3 <- read.csv(file.path(pathSave, paste0("DailyModel_FinalModel_Stats_Additive_SPI30-TMAX30_urban-open.csv")))
urbanMedium.add3 <- read.csv(file.path(pathSave, paste0("DailyModel_FinalModel_Stats_Additive_SPI30-TMAX30_urban-medium.csv")))

modOutAll <- rbind(crop.add3, forest.add3, grassland.add3, urbanHigh.add3, urbanLow.add3, urbanOpen.add3, urbanMedium.add3)
modOutAll$modelType <- as.factor("additive")
summary(modOutAll)                      


summary(ndviMet)  

# creating the lagged dataset
datLC <- ndviMet[,c("date", "NDVI", "landcover")]

datLC <- datLC %>%
  arrange(landcover, date)  # Ensure correct order

datLC <- datLC %>%
  arrange(landcover, date) %>%  # Ensure data is ordered by land cover type and date
  group_by(landcover) %>%
  mutate(NDVI.Lag14d = rollapply(NDVI, width = 14, align = "right", FUN = mean, fill = NA, na.rm = TRUE)) %>%
  ungroup()
summary(datLC)

# # plotting to check that it looks somewhat right
# ggplot(data=datLC) + facet_wrap(landcover~.) +
#   geom_line(aes(x=date, y=NDVI.Lag14d, col=landcover))

# # checking that the lagged calculation that ran in the forloop is the same as the one that was created here.
# forest.lag <- datLC # created in line 113 above
# 
# head(forest.lag)
# 
# ross.forest <- datLC[datLC$landcover %in% "forest",]
# head(ross.forest)
# 
# meow <- forest.lag$NDVI.Lag14d - ross.forest$NDVI.Lag14d
# summary(meow) # zeroes are good!

summary(ndviMet)# data frame with met vars
summary(datLC) # data frame with lags
summary(modOutAll) # data frame with coefficients.

#merging lag into ndvimet data frame
datLC2 <- merge(ndviMet, datLC, by=c("date", "landcover", "NDVI"))
summary(datLC2)

# creating a yday column
datLC2$yday <- yday(datLC2$date)

# had to separate, because there were some differnences in the dates that are available. Arises from teh NDVI lagged data.
lag.dailyNorm <- aggregate(NDVI.Lag14d ~ yday+landcover, data= datLC2, FUN=mean, na.rm=T) # landcover shouldn't make a difference for the metVars, but will for the lagged time series.
spi.dailyNorm<- aggregate(SPI30day ~ yday+landcover, data= datLC2, FUN=mean, na.rm=T) # landcover shouldn't make a difference for the metVars, but will for the lagged time series.
tmax.dailyNorm <- aggregate(Tmax_30day ~ yday+landcover, data= datLC2, FUN=mean, na.rm=T) # landcover shouldn't make a difference for the metVars, but will for the lagged time series.
ndvi.dailyNorm <- aggregate(NDVI ~ yday+landcover, data= datLC2, FUN=mean, na.rm=T) # landcover shouldn't make a difference for the metVars, but will for the lagged time series.
# merging daily mean data  together
# start with metVars

dailyMeans <- merge(spi.dailyNorm, tmax.dailyNorm, by=c("yday", "landcover"), all=T)
dailyMeans2 <- merge(dailyMeans, lag.dailyNorm, by=c("yday", "landcover"), all=T)
dailyMeans3 <- merge(dailyMeans2, ndvi.dailyNorm, by=c("yday", "landcover"), all=T)
summary(dailyMeans3)
names(dailyMeans3) <- c("yday", "landcover", "spi30d.norm", "tmax30d.norm", "ndviLag.norm", "NDVI.norm")


# merging in these daily means with the model output data
summary(modOutAll) # data frame with coefficients.

modOutAll2 <- merge(modOutAll, dailyMeans3, by=c("yday", "landcover"))
summary(modOutAll2)

# calculating partial effects----

# calculating mean Climate partial effects
# partial effect = coefficient * daily mean Var

modOutAll2$partial.Drought.climateNorm <- modOutAll2$coef.Drought* modOutAll2$spi30d.norm
modOutAll2$partial.Temp.climateNorm<- modOutAll2$coef.Temp * modOutAll2$tmax30d.norm
modOutAll2$partial.Lag.climateNorm <- modOutAll2$coef.Lag * modOutAll2$ndviLag.norm

summary(modOutAll2)

# saving data frame
write.csv(modOutAll2, file.path(pathSave, paste0("DailyModel_FinalModel_modOutAdd3_Stats_climateNormPartialEffects_AllLandcovers.csv")), row.names=F)

####################
# Calculating Partial Effects for individual years----

summary(ndviMet)
summary(chiMet)
summary(modOutAll2) # wanting this data frame for just the coefficients
head(modOutAll2)

# limiting chiMet to the date range we have data for
chiMet2 <- chiMet[chiMet$date >= as.Date("2000-01-01"),]

ggplot(data = chiMet2, aes(x=date, y = 1, fill=Tmax_30day)) +
  geom_tile()

ggplot(data = chiMet2[chiMet2$date >="2009-01-01" & chiMet2$date<="2013-01-01",], aes(x=date, y = Tmax_30day)) +
  geom_line()
ggplot(data = chiMet2[chiMet2$date >="2000-01-01",], aes(x=date, y = 1, fill=SPEI14)) +
  geom_tile()
ggplot(data = ndviMet[ndviMet$date >="2000-01-01",], aes(x=date, y=SPEI14)) +
  geom_line()

# need to calculate partial effects for individual years.
# pull out the daily coefficients

drought.coeff <- modOutAll2[,names(modOutAll2) %in% c("yday", "landcover", "DroughtVar", "coef.Drought")]
temp.coeff <- modOutAll2[,names(modOutAll2) %in% c("yday", "landcover", "TempVar", "coef.Temp")]
lag.coeff <- modOutAll2[,names(modOutAll2) %in% c("yday", "landcover", "coef.Lag")]

# building a dataframe to house the date-driven partial effects
lc.list <- unique(modOutAll2$landcover)

pe.date.df <- data.frame(date = chiMet2$date,
                         SPI.30d = chiMet2$SPI30day,
                         TMAX.30d = chiMet2$Tmax_30day)
lcDate.df <- expand.grid(date = chiMet2$date,
                         landcover = lc.list)

pe.date.df <- merge(pe.date.df, lcDate.df, by="date", all.y=T)
summary(pe.date.df)
# merging in the NDVI lag. This will give us replicatino across the land covers
summary(datLC)

# want to keep all of the climate date, so doing all=T
# limiting the chi.met merge to just the date range of the NDVI data
pe.date.df2 <- merge(datLC, pe.date.df, by=c("date","landcover"), all=T)
pe.date.df2 <- datLC %>%
  full_join(pe.date.df, by=c("date", "landcover"))

summary(pe.date.df2)

# creating a yday variable to merge in the coefficients
pe.date.df2$yday <- yday(pe.date.df2$date)
summary(pe.date.df2)

# merging in coefficients
pe.date.df3 <- merge(pe.date.df2, modOutAll2[, c("yday", "landcover", "coef.Drought", "coef.Temp", "coef.Lag", "coef.Int")], 
                     by=c("landcover", "yday"), all.x=T)
pe.date.df3 <- pe.date.df2 %>%
  full_join(modOutAll2[, c("yday", "landcover", "coef.Drought", "coef.Temp", "coef.Lag", "coef.Int")],
            by=c("landcover", "yday"))

summary(pe.date.df3)

# calculating partial effects
pe.date.df3$partial.Drought.date <- pe.date.df3$SPI.30d*pe.date.df3$coef.Drought
pe.date.df3$partial.Temp.date <- pe.date.df3$TMAX.30d*pe.date.df3$coef.Temp
pe.date.df3$partial.Lag.date <- pe.date.df3$NDVI.Lag14d*pe.date.df3$coef.Lag

# # Calculating residual for partial effect standardization process
# # NDVI - intercept - partialEffect.Lag = Residual
# pe.date.df3$resid <- pe.date.df3$NDVI - pe.date.df3$coef.Int - pe.date.df3$partial.Lag.date
# head(pe.date.df3)
# summary(pe.date.df3)
# 
# ggplot(data=pe.date.df3) +
#   geom_vline(xintercept=0, linetype="dashed") +
#   geom_density(aes(x=resid))
# 
# ggplot(data=pe.date.df3) +
#   geom_vline(xintercept=0, linetype="dashed") +
#   geom_density(aes(x=partial.Drought.date), col="orange2")+
#   geom_density(aes(x=partial.Temp.date), col="navy")+
#   geom_density(aes(x=partial.Lag.date), col="black")


# creating standardized variables for plotting later
# note: standardizing by NDVI leaves too many gaps becasue of NDVI's irregular passes

# pe.date.df3$peTempStd <- pe.date.df3$partial.Temp.date/pe.date.df3$NDVI
# pe.date.df3$peDroughtStd <- pe.date.df3$partial.Drought.date/pe.date.df3$NDVI

######
# Alternative PE standardizing----
# adding in a month column
pe.date.df3$month <- month(pe.date.df3$date)
mean.gs.ndvi <- aggregate(NDVI~landcover, data=pe.date.df3[pe.date.df3$month %in% c(4:10),], FUN=mean, na.rm=T)

for(i in unique(mean.gs.ndvi$landcover)){
  temp <- pe.date.df3[pe.date.df3$landcover==i,]
  ndvi.lc.mean <- mean.gs.ndvi[mean.gs.ndvi$landcover==i,"NDVI"]
  
  pe.date.df3[pe.date.df3$landcover==i, "peDroughtStd.gsMean"] <- temp$partial.Drought.date/ndvi.lc.mean
  pe.date.df3[pe.date.df3$landcover==i, "peTempStd.gsMean"] <- temp$partial.Temp.date/ndvi.lc.mean
  pe.date.df3[pe.date.df3$landcover==i, "peLagStd.gsMean"] <- temp$partial.Lag.date/ndvi.lc.mean
}
head(pe.date.df3)

# saving data frame-Additive model 3
write.csv(pe.date.df3, file.path(pathSave, paste0("DailyModel_FinalModel_modOutAdd3_Stats_dailyPartialEffects_AllLandcovers.csv")), row.names=F)


