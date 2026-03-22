# Checkign the netcdf data 
# Creating a clean script to re-do the daily correlation modeling and do some prediction from it
library(ggplot2)
library(lubridate)
library(ggcorrplot)

# Setting the file paths. This may be different for your computer.
# Sys.setenv(GOOGLE_DRIVE = "G:/Shared drives/Urban Ecological Drought/Manuscript - Urban Drought NDVI/")
Sys.setenv(GOOGLE_DRIVE = "~/Google Drive/Shared drives/Urban Ecological Drought/Manuscript - Urban Drought NDVI/")
google.drive <- Sys.getenv("GOOGLE_DRIVE")

path.NDVI <- file.path(google.drive, "data", "data_raw")
path.figs <- file.path(google.drive, "exploratory figures/FinalDailyModel")
pathSave <- file.path(google.drive, "data/processed_files/FinalDailyModel")


ChicagolandSPI <- read.csv(file.path(google.drive, "../data/data_sets/Daily Meteorological Data/Chicagoland_Daily_SPI.csv"))
ChicagolandSPEI <- read.csv(file.path(google.drive, "../data/data_sets/Daily Meteorological Data/Chicagoland_Daily_SPEI.csv"))
ChicagolandTemp <- read.csv(file.path(google.drive, "../data/data_sets/Daily Meteorological Data/Chicagoland_Daily_Temps.csv"))

# create column with date in ISO format; making it lowercase "date" so that it merges easier
ChicagolandSPI$date <- as.Date(ChicagolandSPI$Date, "%m/%d/%Y")
ChicagolandSPI$yday <- lubridate::yday(ChicagolandSPI$date)

ChicagolandSPEI$date <- as.Date(ChicagolandSPEI$Date, "%m/%d/%Y")
ChicagolandSPEI$yday <- lubridate::yday(ChicagolandSPEI$date)

ChicagolandTemp$date <- as.Date(ChicagolandTemp$Date, "%m/%d/%Y")
ChicagolandTemp$yday <- lubridate::yday(ChicagolandTemp$date)

summary(ChicagolandSPI)
summary(ChicagolandSPEI)
summary(ChicagolandTemp)
length(unique(ChicagolandSPEI$yday)); length(unique(ChicagolandSPI$yday)); length(unique(ChicagolandTemp$yday))


# Checking the nc files
# file.exists(file.path(google.drive, "../data/data_sets/Daily Meteorological Data/Daily_SPEI14day"))
spei14nc <- ncdf4::nc_open(file.path(google.drive, "../data/data_sets/Daily Meteorological Data/Daily_SPEI14day.nc"))
speiTime <- as.Date(ncdf4::ncvar_get(spei14nc, "time") + as.Date("0000-01-01")-1) # Looks like the origin date is probably 0
spei14 <- apply(ncdf4::ncvar_get(spei14nc, "spei"), 3, mean, na.rm=T)
dim(spei14)
summary(speiTime)
(spei14[1:100])
summary(spei14)
summary(spei14nc$dim$time)
spei14nc$dim$time
spei14nc$var$spei
summary(spei14nc$var)
dfSPEI14 <- data.frame(date=speiTime, spei14=spei14)
dfSPEI14$yday <- lubridate::yday(dfSPEI14$date)
summary(dfSPEI14[is.na(dfSPEI14$spei14),])
summary(dfSPEI14[!is.na(dfSPEI14$spei14) & dfSPEI14$spei14>10,])
summary(dfSPEI14)
