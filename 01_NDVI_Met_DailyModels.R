# Creating a clean skip to re-do the daily correlation modeling and do some prediction from it



# script to explore NDVI time series for the land cover classes of the Chicago region
library(ggplot2)
library(lubridate)

# Setting the file paths. This may be different for your computer.
Sys.setenv(GOOGLE_DRIVE = "G:/Shared drives/Urban Ecological Drought")
Sys.setenv(GOOGLE_DRIVE = "~/Google Drive/Shared drives/Urban Ecological Drought")
google.drive <- Sys.getenv("GOOGLE_DRIVE")


path.figs <- file.path(google.drive, "data/exploratory figures/daily models")
if(!dir.exists(path.figs)) dir.create(path.figs)
# path.landsat <- file.path(google.drive, "Neighborhood remote sensing analysis/Landsat NDVI")
# dir(path.landsat)
pathSave <- file.path(google.drive, "data/r_files/processed_files")

# Going back to the raw landsat data -- lookgin for somethign that has the satellite attached to know it's raw.
# # I don't know where this was created, but it's what we have -- we'll need to go back and figure this out at some point
ndvi.all <- readRDS(file.path(google.drive, "data/r_files/processed_files/landsat_ndvi_all.RDS"))
head(ndvi.all)
summary(ndvi.all)
summary(ndvi.all[ndvi.all$year==2023,])


# reading in Trent's SPI
ChicagolandSPI <- read.csv(file.path(google.drive, "data/data_sets/Daily Meteorological Data/Chicagoland_Daily_SPI.csv"))
ChicagolandSPEI <- read.csv(file.path(google.drive, "data/data_sets/Daily Meteorological Data/Chicagoland_Daily_SPEI.csv"))
ChicagolandTemp <- read.csv(file.path(google.drive, "data/data_sets/Daily Meteorological Data/Chicagoland_Daily_Temps.csv"))

# create column with date in ISO format; making it lowercase "date" so that it merges easier
ChicagolandSPI$date <- as.Date(ChicagolandSPI$Date, "%m/%d/%Y")
ChicagolandSPEI$date <- as.Date(ChicagolandSPEI$Date, "%m/%d/%Y")
ChicagolandTemp$date <- as.Date(ChicagolandTemp$Date, "%m/%d/%Y")
summary(ChicagolandSPI)
summary(ChicagolandSPEI)
summary(ChicagolandTemp)

dim(ChicagolandSPI); dim(ChicagolandSPEI); dim(ChicagolandTemp)


ndviMet <- merge(ndvi.all, ChicagolandTemp, all.x=T, all.y=F)
ndviMet <- merge(ndviMet, ChicagolandSPI, all.x=T, all.y=F)
ndviMet <- merge(ndviMet, ChicagolandSPEI, all.x=T, all.y=F)
summary(ndviMet)

# saving ndviMet to the data drive so that predictors are paired together with the NDVI data
saveRDS(ndviMet, file = file.path(google.drive, "data/r_files/processed_files/landsat_ndvi_metVars_combined.RDS"))
write.csv(ndviMet, file = file.path(google.drive, "data/r_files/processed_files/landsat_ndvi_metVars_combined.csv"), row.names=F)
#########################################
# Daily model MEGAloop ----
#########################################
for(LC in unique(ndviMet$type)){
  print(LC)
  # Subset the data 
  datLC <- ndviMet[ndviMet$type==LC,]
  
  # Checking the autocorrelation in NDVI
  head(datLC)
  # acf(datLC$NDVI[!is.na(datLC$NDVI)])
  # acf(datLC$resid[!is.na(datLC$resid)]) # note: need to run below for this to work!
  
  
  # Creating a 14-day NDVI lag (day -14), that goes across satellites to try to bring in autocorrleation
  # May need a longer window, but we'll see
  datLC$NDVI.Lag14d <- NA
  for(i in 1:nrow(datLC)){
    rowLag <- which(datLC$date>=(datLC$date[i]-14) & datLC$date<datLC$date[i])
    
    if(length(rowLag)<1 ) next
    if(length(rowLag)==1) datLC$NDVI.Lag14d[i] <- datLC$NDVI[rowLag]
    if(length(rowLag)>1) datLC$NDVI.Lag14d[i] <- mean(datLC$NDVI[rowLag], na.rm=T)
    
  }
  summary(datLC)
  
  # ggplot(data=datLC[datLC$year==2020,]) +
  # ggtitle(paste0(LC, "; Year = 2020")) +
  #   # geom_line(aes(group=year, color=satellite), linewidth=0.5)  +
  #   geom_line(aes(x=doy, y=NDVI, group=satellite), color="black") +
  #   geom_line(aes(x=doy, y=NDVI.Lag14d), color="gray50")
  
  
  # Do some quick graphs to check to make sure we can see a signal we're looking for
  # showing 2012 in black
  png(file.path(path.figs, paste0("NDVI_", LC, "_Raw_bySatellite_2012.png")), height=6, width=6, units="in", res=220)
  ggplot(data=datLC, aes(x=doy, y=NDVI)) +
    ggtitle(paste0(LC, "; Year = 2012")) +
    geom_line(aes(group=year, color=satellite), linewidth=0.5)  +
    geom_line(data=datLC[datLC$year==2012,], aes(group=satellite), color="black", linewidth=0.9)
  dev.off()
  
  png(file.path(path.figs,  paste0("NDVI_", LC, "_Raw_bySatellite_2005.png")), height=6, width=6, units="in", res=220)
  print(
    ggplot(data=datLC, aes(x=doy, y=NDVI)) +
    ggtitle(paste0(LC, "; Year = 2005")) +
    geom_line(aes(group=year, color=satellite), linewidth=0.5)  +
    geom_line(data=datLC[datLC$year==2005,], aes(group=satellite), color="black", linewidth=0.9)
  )
  dev.off()
  
  png(file.path(path.figs,  paste0("NDVI_", LC, "_Raw_bySatellite_2023.png")), height=6, width=6, units="in", res=220)
  print(
    ggplot(data=datLC, aes(x=doy, y=NDVI)) +
    ggtitle(paste0(LC, "; Year = 2023")) +
    geom_line(aes(group=year, color=satellite), linewidth=0.5)  +
    geom_line(data=datLC[datLC$year==2023,], aes(group=satellite), color="black", linewidth=0.9)
    )
  dev.off()
  
  
  # Starting with doing a simple model of NDVI ~ 
  days.use <- 1:365
  modsList <- list()
  modsLag <- list()
  modsNorm <- list()
  modsTemp <- list()
  modsMoist <- list()
  modsCombo <- list()
  
  mod.out <- data.frame(landcover=LC, yday=1:365, intercept=NA, coef.Lag=NA, coef.SPEI30=NA, coef.Tmin30=NA, pVal.Lag=NA, pVal.SPEI30=NA, pVal.Tmin30=NA, rSq.Process=NA, rSq.Lag=NA, rSq.Norm=NA) 
  mod.outTemp <- data.frame(landcover=LC, yday=1:365, intercept=NA, coef.Lag=NA, coef.Tmin14=NA, pVal.Lag=NA, pVal.Tmin14=NA, rSq.Process=NA, rSq.Lag=NA, rSq.Norm=NA) 
  mod.outMoist <- data.frame(landcover=LC, yday=1:365, intercept=NA, coef.Lag=NA, coef.SPI30=NA,  pVal.Lag=NA, pVal.SPI30=NA, rSq.Process=NA, rSq.Lag=NA, rSq.Norm=NA) 
  mod.outCombo <- data.frame(landcover=LC, yday=1:365, intercept=NA, coef.Lag=NA, coef.SPI30=NA, coef.Tmin14=NA, pVal.Lag=NA, pVal.SPI30=NA, pVal.Tmin14=NA, rSq.Process=NA, rSq.Lag=NA, rSq.Norm=NA) 
  
  
  datLC$NDVI.pred <- NA # Setting up a palceholder for predicted values
  datLC$NDVI.predLag <- NA # Setting up a palceholder for predicted values
  datLC$NDVI.predNorm <- NA # Setting up a palceholder for predicted values
  datLC$NDVI.predTemp <- NA # Setting up a palceholder for predicted values
  datLC$NDVI.predMoist <- NA # Setting up a palceholder for predicted values
  datLC$NDVI.predCombo <- NA # Setting up a palceholder for predicted values
  
  
  # row.ind = 0 # Setting up an index that will tell us what row to save things in; we should start with 0 because we haven't done anything yet
  pb <- txtProgressBar(min=0, max=nrow(mod.out), style=3)
  for(i in 1:nrow(mod.out)){
    setTxtProgressBar(pb, i)
    # For testing using i=185 (which is July 4; yday(as.Date("2021-07-04"))) -- this is a period that should have a decent SPI relationship based off of the initial corr plots
    # dayNOW <- days.use[i] # This is almost exactly the same as above, but now i will go from 1 to 215 (the number of unique days.use we have)
    dayNOW = i # Repurposing old code, so doing a clunky approach here
    
    ## Using an even-sided window to train the model for now to understand the relationships
    # Here we're subsetting our big data frame to the SMALL temporal window we want --> this should help with temporal stationarity in the effects of our predictors
    rowNow <- which(datLC$doy>=dayNOW-7 & datLC$doy<=dayNOW+7 )
    dat.tmp <- datLC[rowNow,] # Subsets things to a particular window (not just a single day); otherwise we were working with just 5 years of data, which isn't helpful
    # summary(dat.tmp)
    
    # Doing some graphing that we're not saving for our own sanity
    # ggplot(data=dat.tmp) + geom_violin(aes(x=as.factor(year), y=NDVI, fill=satellite), scale="width")
    # ggplot(data=dat.tmp) + geom_violin(aes(x=as.factor(year), y=X30d.SPI, fill=satellite), scale="width")
    # ggplot(data=dat.tmp, aes(x=X30d.SPI, y=NDVI)) + geom_point(aes(color=satellite)) + stat_smooth(method="lm")
     
    #Set up a normal (intercept-only) model
    modN <- nlme::lme(NDVI ~ 1, random=list(satellite=~1), data=dat.tmp[,], na.action=na.omit)
    modsNorm[[i]] <- modN
    summary(modN)
    
    #Set up a lag-only model
    modL <- nlme::lme(NDVI ~ NDVI.Lag14d, random=list(satellite=~1), data=dat.tmp[,], na.action=na.omit)
    modsLag[[i]] <- modL
    
    # This is running a pretty basic model --> TMIN30d shouldn't have a big impact in the summer, but we'll keep it to see what happens
    modDay <- nlme::lme(NDVI ~ X30d.SPEI + TMIN30d + NDVI.Lag14d, random=list(satellite=~1), data=dat.tmp[,], na.action=na.omit)
    modsList[[i]] <- modDay
    sumMod <- summary(modDay)
    # MuMIn::r.squaredGLMM(modDay)[,"R2m"]
    
    # Storing key stats about the model
    mod.out[i,"intercept"] <- modDay$coefficients$fixed["(Intercept)"]
    mod.out[i,"coef.Lag"] <- modDay$coefficients$fixed["NDVI.Lag14d"]
    mod.out[i,"coef.SPEI30"] <- modDay$coefficients$fixed["X30d.SPEI"]
    mod.out[i,"coef.Tmin30"] <- modDay$coefficients$fixed["TMIN30d"]
    mod.out[i,"tStat.Lag"] <- sumMod$tTable["NDVI.Lag14d", "t-value"]
    mod.out[i,"tStat.SPEI30"] <- sumMod$tTable["X30d.SPEI", "t-value"]
    mod.out[i,"tStat.Tmin30"] <- sumMod$tTable["TMIN30d", "t-value"]
    mod.out[i,"pVal.Lag"] <- sumMod$tTable["NDVI.Lag14d", "p-value"]
    mod.out[i,"pVal.SPEI30"] <- sumMod$tTable["X30d.SPEI", "p-value"]
    mod.out[i,"pVal.Tmin30"] <-sumMod$tTable["TMIN30d", "p-value"]
    mod.out[i, "rSq.Process"] <- MuMIn::r.squaredGLMM(modDay)[,"R2m"]
    mod.out[i, "rSq.Lag"] <- MuMIn::r.squaredGLMM(modL)[,"R2m"]
    mod.out[i, "rSq.Norm"] <- MuMIn::r.squaredGLMM(modN)[,"R2m"]
    
    
    ###############################
    # using what we've learned from the ML series, building some basic models and stepping through RMSE and AIC checks
    
    modTemp <- nlme::lme(NDVI ~ TMIN14d + NDVI.Lag14d, random=list(satellite=~1), data=dat.tmp[,], na.action=na.omit)
    modsTemp[[i]] <- modTemp
    sumModTemp <- summary(modTemp)
    
    modMoist <- nlme::lme(NDVI ~ X30d.SPI + NDVI.Lag14d, random=list(satellite=~1), data=dat.tmp[,], na.action=na.omit)
    modsMoist[[i]] <- modMoist
    sumModMoist <- summary(modMoist)
    
    modBoth <- nlme::lme(NDVI ~ X30d.SPI + TMIN14d + NDVI.Lag14d, random=list(satellite=~1), data=dat.tmp[,], na.action=na.omit)
    modsCombo[[i]] <- modBoth
    sumModBoth <- summary(modBoth)
    
    
    
    # Storing key stats about the model
    mod.outTemp[i,"intercept"] <- modTemp$coefficients$fixed["(Intercept)"]
    mod.outTemp[i,"coef.Lag"] <- modTemp$coefficients$fixed["NDVI.Lag14d"]
    # mod.outTemp[i,"coef.SPI30"] <- modTemp$coefficients$fixed["X30d.SPI"]
    mod.outTemp[i,"coef.Tmin14"] <- modTemp$coefficients$fixed["TMIN14d"]
    mod.outTemp[i,"tStat.Lag"] <- sumModTemp$tTable["NDVI.Lag14d", "t-value"]
    # mod.outTemp[i,"tStat.SPI30"] <- sumModTemp$tTable["X30d.SPI", "t-value"]
    mod.outTemp[i,"tStat.Tmin14"] <- sumModTemp$tTable["TMIN14d", "t-value"]
    mod.outTemp[i,"pVal.Lag"] <- sumModTemp$tTable["NDVI.Lag14d", "p-value"]
    # mod.outTemp[i,"pVal.SPI30"] <- sumModTemp$tTable["X30d.SPI", "p-value"]
    mod.outTemp[i,"pVal.Tmin14"] <-sumModTemp$tTable["TMIN14d", "p-value"]
    mod.outTemp[i, "rSq.Process"] <- MuMIn::r.squaredGLMM(modTemp)[,"R2m"]
    mod.outTemp[i, "rSq.Lag"] <- MuMIn::r.squaredGLMM(modL)[,"R2m"]
    mod.outTemp[i, "rSq.Norm"] <- MuMIn::r.squaredGLMM(modN)[,"R2m"]
    
    mod.outMoist[i,"intercept"] <- modMoist$coefficients$fixed["(Intercept)"]
    mod.outMoist[i,"coef.Lag"] <- modMoist$coefficients$fixed["NDVI.Lag14d"]
    mod.outMoist[i,"coef.SPI30"] <- modMoist$coefficients$fixed["X30d.SPI"]
    # mod.outMoist[i,"coef.Tmin14"] <- modMoist$coefficients$fixed["TMIN14d"]
    mod.outMoist[i,"tStat.Lag"] <- sumModMoist$tTable["NDVI.Lag14d", "t-value"]
    mod.outMoist[i,"tStat.SPI30"] <- sumModMoist$tTable["X30d.SPI", "t-value"]
    # mod.outMoist[i,"tStat.Tmin14"] <- sumModMoist$tTable["TMIN14d", "t-value"]
    mod.outMoist[i,"pVal.Lag"] <- sumModMoist$tTable["NDVI.Lag14d", "p-value"]
    mod.outMoist[i,"pVal.SPI30"] <- sumModMoist$tTable["X30d.SPI", "p-value"]
    # mod.outMoist[i,"pVal.Tmin14"] <-sumModMoist$tTable["TMIN14d", "p-value"]
    mod.outMoist[i, "rSq.Process"] <- MuMIn::r.squaredGLMM(modMoist)[,"R2m"]
    mod.outMoist[i, "rSq.Lag"] <- MuMIn::r.squaredGLMM(modL)[,"R2m"]
    mod.outMoist[i, "rSq.Norm"] <- MuMIn::r.squaredGLMM(modN)[,"R2m"]
    
    mod.outCombo[i,"intercept"] <- modBoth$coefficients$fixed["(Intercept)"]
    mod.outCombo[i,"coef.Lag"] <- modBoth$coefficients$fixed["NDVI.Lag14d"]
    mod.outCombo[i,"coef.SPI30"] <- modBoth$coefficients$fixed["X30d.SPI"]
    mod.outCombo[i,"coef.Tmin14"] <- modBoth$coefficients$fixed["TMIN14d"]
    mod.outCombo[i,"tStat.Lag"] <- sumModBoth$tTable["NDVI.Lag14d", "t-value"]
    mod.outCombo[i,"tStat.SPI30"] <- sumModBoth$tTable["X30d.SPI", "t-value"]
    mod.outCombo[i,"tStat.Tmin14"] <- sumModBoth$tTable["TMIN14d", "t-value"]
    mod.outCombo[i,"pVal.Lag"] <- sumModBoth$tTable["NDVI.Lag14d", "p-value"]
    mod.outCombo[i,"pVal.SPI30"] <- sumModBoth$tTable["X30d.SPI", "p-value"]
    mod.outCombo[i,"pVal.Tmin14"] <-sumModBoth$tTable["TMIN14d", "p-value"]
    mod.outCombo[i, "rSq.Process"] <- MuMIn::r.squaredGLMM(modBoth)[,"R2m"]
    mod.outCombo[i, "rSq.Lag"] <- MuMIn::r.squaredGLMM(modL)[,"R2m"]
    mod.outCombo[i, "rSq.Norm"] <- MuMIn::r.squaredGLMM(modN)[,"R2m"]
    
    
    
  }
  summary(mod.out)
  head(mod.out)
  
  write.csv(mod.out, file.path(pathSave, paste0("daily_models/DailyModel_OG_Stats_", LC, ".csv")), row.names=F)
  write.csv(mod.outTemp, file.path(pathSave, paste0("daily_models/DailyModel_Temp_Stats_", LC, ".csv")), row.names=F)
  write.csv(mod.outMoist, file.path(pathSave, paste0("daily_models/DailyModel_Moisture_Stats_", LC, ".csv")), row.names=F)
  write.csv(mod.outCombo, file.path(pathSave, paste0("daily_models/DailyModel_Combo_Stats_", LC, ".csv")), row.names=F)
  
  
  # Stacking so we can do a new daily corr figure
  # OG Model
  effectStack <- stack(mod.out[,grep("tStat", names(mod.out))])
  names(effectStack) <- c("tStat", "effect")
  effectStack$doy <- mod.out$yday
  effectStack$effect <- gsub("tStat.", "", effectStack$effect) # making clean names
  effectStack$pVal <- stack(mod.out[,grep("pVal", names(mod.out))])[,"values"]
  effectStack$coef <- stack(mod.out[,grep("coef", names(mod.out))])[,"values"]
  
  summary(effectStack)
  
  # Temp only Model
  effectStackTemp <- stack(mod.outTemp[,grep("tStat", names(mod.outTemp))])
  names(effectStackTemp) <- c("tStat", "effect")
  effectStackTemp$doy <- mod.outTemp$yday
  effectStackTemp$effect <- gsub("tStat.", "", effectStackTemp$effect) # making clean names
  effectStackTemp$pVal <- stack(mod.outTemp[,grep("pVal", names(mod.outTemp))])[,"values"]
  effectStackTemp$coef <- stack(mod.outTemp[,grep("coef", names(mod.outTemp))])[,"values"]
  
  summary(effectStackTemp)
  
  # SPI only Model
  effectStackMoist <- stack(mod.outMoist[,grep("tStat", names(mod.outMoist))])
  names(effectStackMoist) <- c("tStat", "effect")
  effectStackMoist$doy <- mod.outMoist$yday
  effectStackMoist$effect <- gsub("tStat.", "", effectStackMoist$effect) # making clean names
  effectStackMoist$pVal <- stack(mod.outMoist[,grep("pVal", names(mod.outMoist))])[,"values"]
  effectStackMoist$coef <- stack(mod.outMoist[,grep("coef", names(mod.outMoist))])[,"values"]
  
  summary(effectStackMoist)
  
  # Combo Model
  effectStackCombo <- stack(mod.outCombo[,grep("tStat", names(mod.outCombo))])
  names(effectStackCombo) <- c("tStat", "effect")
  effectStackCombo$doy <- mod.outCombo$yday
  effectStackCombo$effect <- gsub("tStat.", "", effectStackCombo$effect) # making clean names
  effectStackCombo$pVal <- stack(mod.outCombo[,grep("pVal", names(mod.outCombo))])[,"values"]
  effectStackCombo$coef <- stack(mod.outCombo[,grep("coef", names(mod.outCombo))])[,"values"]
  
  summary(effectStackCombo)
  
  
  
  plotEffSig <- ggplot(data=effectStack[effectStack$pVal<0.05,]) +
    ggtitle(LC) +
    geom_tile(aes(x=doy, y=effect, fill=tStat)) +
    scale_fill_gradient2(low="orange2", high="green4", mid="gray80", midpoint=0) +
    theme_bw()
  
  png(file.path(path.figs, paste0("NDVI-Model_", LC, "_Effects_SigOnly.png")), height=6, width=10, units="in", res=220)
  print(plotEffSig)
  dev.off()
  
  plotEffAll <- ggplot(data=effectStack[,]) +
    ggtitle(LC) +
    geom_tile(aes(x=doy, y=effect, fill=tStat)) +
    scale_fill_gradient2(low="orange2", high="green4", mid="gray80", midpoint=0) +
    theme_bw()
  
  png(file.path(path.figs, paste0("NDVI-Model_", LC, "_Effects_All.png")), height=6, width=10, units="in", res=220)
  print(plotEffAll)
  dev.off()
  
  # Temp Only
  plotEffSigTemp <- ggplot(data=effectStackTemp[effectStackTemp$pVal<0.05,]) +
    ggtitle(LC) +
    geom_tile(aes(x=doy, y=effect, fill=tStat)) +
    scale_fill_gradient2(low="orange2", high="green4", mid="gray80", midpoint=0) +
    theme_bw()
  
  png(file.path(path.figs, paste0("NDVI-Temp-Model_", LC, "_Effects_SigOnly.png")), height=6, width=10, units="in", res=220)
  print(plotEffSigTemp)
  dev.off()
  
  plotEffAllTemp <- ggplot(data=effectStackTemp[,]) +
    ggtitle(LC) +
    geom_tile(aes(x=doy, y=effect, fill=tStat)) +
    scale_fill_gradient2(low="orange2", high="green4", mid="gray80", midpoint=0) +
    theme_bw()
  
  png(file.path(path.figs, paste0("NDVI-Temp-Model_", LC, "_Effects_All.png")), height=6, width=10, units="in", res=220)
  print(plotEffAllTemp)
  dev.off()
  
  # Mositure Only
  plotEffSigMoist <- ggplot(data=effectStackMoist[effectStackMoist$pVal<0.05,]) +
    ggtitle(LC) +
    geom_tile(aes(x=doy, y=effect, fill=tStat)) +
    scale_fill_gradient2(low="orange2", high="green4", mid="gray80", midpoint=0) +
    theme_bw()
  
  png(file.path(path.figs, paste0("NDVI-Moisture-Model_", LC, "_Effects_SigOnly.png")), height=6, width=10, units="in", res=220)
  print(plotEffSigMoist)
  dev.off()
  
  plotEffAllMoist <- ggplot(data=effectStackMoist[,]) +
    ggtitle(LC) +
    geom_tile(aes(x=doy, y=effect, fill=tStat)) +
    scale_fill_gradient2(low="orange2", high="green4", mid="gray80", midpoint=0) +
    theme_bw()
  
  png(file.path(path.figs, paste0("NDVI-Moisture-Model_", LC, "_Effects_All.png")), height=6, width=10, units="in", res=220)
  print(plotEffAllMoist)
  dev.off()
  
  # Combo
  plotEffSigCombo <- ggplot(data=effectStackCombo[effectStackCombo$pVal<0.05,]) +
    ggtitle(LC) +
    geom_tile(aes(x=doy, y=effect, fill=tStat)) +
    scale_fill_gradient2(low="orange2", high="green4", mid="gray80", midpoint=0) +
    theme_bw()
  
  png(file.path(path.figs, paste0("NDVI-Combo-Model_", LC, "_Effects_SigOnly.png")), height=6, width=10, units="in", res=220)
  print(plotEffSigCombo)
  dev.off()
  
  plotEffAllCombo <- ggplot(data=effectStackCombo[,]) +
    ggtitle(LC) +
    geom_tile(aes(x=doy, y=effect, fill=tStat)) +
    scale_fill_gradient2(low="orange2", high="green4", mid="gray80", midpoint=0) +
    theme_bw()
  
  png(file.path(path.figs, paste0("NDVI-Combo-Model_", LC, "_Effects_All.png")), height=6, width=10, units="in", res=220)
  print(plotEffAllCombo)
  dev.off()
  
  
  
  
  # Now predicting from the models --> we need to do this separately from fitting because we want ONE prediction per obs
  # This could be made more efficient 
  for(DAY in unique(datLC$doy)){
    if(DAY == 366) next # Skip leap day
    rowNowBase <- which(datLC$doy==DAY)
    rowNow <- which(datLC$doy==DAY & !is.na(datLC$X30d.SPI) & !is.na(datLC$TMIN14d) & !is.na(datLC$NDVI.Lag14d))
    
    if(length(rowNowBase)==0) next # Skip this row if we don't have the predictors we need
    datLC$NDVI.predNorm[rowNowBase] <- predict(modsNorm[[DAY]], newdata=datLC[rowNowBase,])
    
    if(length(rowNow)==0) next # Skip this row if we don't have the predictors we need
    datLC$NDVI.pred[rowNow] <- predict(modsList[[DAY]], newdata=datLC[rowNow,])
    datLC$NDVI.predLag[rowNow] <- predict(modsLag[[DAY]], newdata=datLC[rowNow,])
    datLC$NDVI.predTemp[rowNow] <- predict(modsTemp[[DAY]], newdata=datLC[rowNow,])
    datLC$NDVI.predMoist[rowNow] <- predict(modsMoist[[DAY]], newdata=datLC[rowNow,])
    datLC$NDVI.predCombo[rowNow] <- predict(modsCombo[[DAY]], newdata=datLC[rowNow,])
    
    
  }
  
  # Now looking at the output
  datLC$month <- lubridate::month(datLC$date)
  datLC$resid <- datLC$NDVI - datLC$NDVI.pred
  hist(datLC$resid)
  
  datLC$resid.temp <- datLC$NDVI - datLC$NDVI.predTemp
  hist(datLC$resid.temp)
  
  datLC$resid.moist <- datLC$NDVI - datLC$NDVI.predMoist
  hist(datLC$resid.moist)
  
  datLC$resid.combo <- datLC$NDVI - datLC$NDVI.predCombo
  hist(datLC$resid.combo)
  
  
  summary(datLC)
  
  write.csv(datLC, file.path(pathSave, paste0("daily_models/DailyModel_NDVI-predict_", LC, ".csv")), row.names=F)
  
  # stacking residuals for some diagnistic plotting
  names.resid <- names(datLC[grep("resid",names(datLC))])
  names.pred <- names(datLC[grep("pred",names(datLC))])
  
  resid.stack <- stack(datLC[,names(datLC) %in% names.resid])
  names(resid.stack) <- c("residuals", "resid.type")
  resid.stack$month <- datLC$month
  resid.stack$doy <- datLC$doy
  resid.stack$date <- datLC$date
  resid.stack$year <- datLC$year
  
  pred.stack <- stack(datLC[,names(datLC) %in% names.pred])
  names(pred.stack) <- c("prediction", "pred.type")
  pred.stack$month <- datLC$month
  pred.stack$doy <- datLC$doy
  pred.stack$date <- datLC$date
  pred.stack$NDVI <- datLC$NDVI
  pred.stack$NDVI.Lag14d <- datLC$NDVI.Lag14d
  pred.stack$TMIN14d <- datLC$TMIN14d
  pred.stack$X30d.SPI <- datLC$X30d.SPI
  
  pred.res.stack <- merge(resid.stack, pred.stack, by=c("date", "doy", "month"), all=T)
  
  # Doing some diagnostic plotting
  png(file.path(path.figs, paste0("NDVI-Model_", LC, "_Residuals_byMonth.png")), height=6, width=6, units="in", res=220)
  print(ggplot(data=pred.res.stack) +
    ggtitle(LC) +
    facet_wrap(~month) +
    geom_density(aes(x=residuals, color=resid.type)) +
    geom_vline(xintercept = 0, col="red2"))
  dev.off()
  
  
  png(file.path(path.figs, paste0("NDVI-Model_", LC, "_Pred-Obs_byMonth.png")), height=6, width=6, units="in", res=220)
  print(ggplot(data=pred.res.stack) +
    ggtitle(LC) +
    facet_grid(month~pred.type) +
    geom_point(aes(x=prediction, y=NDVI, col=pred.type)) +
    geom_abline(slope=1, intercept = 0, col="red2")) 
  dev.off()
  
  png(file.path(path.figs, paste0("NDVI-Model_", LC, "_SPEI30-Resid_byMonth.png")), height=6, width=6, units="in", res=220)
  print(ggplot(data=pred.res.stack) +
    ggtitle(LC) +
      facet_grid(month~resid.type) +
    geom_point(aes(x=X30d.SPI, y=residuals)) +
    geom_hline(yintercept = 0, col="red2"))
  dev.off()
  
  ndvi2005 <-  ggplot(data=pred.res.stack[pred.res.stack$year==2005,]) +
                  ggtitle(paste0(LC, " NDVI in Year 2005 (drought year)")) +              
                  stat_smooth(aes(x=doy, y=prediction, color=pred.type), method="gam") +
                  geom_point(aes(x = doy, y=NDVI), color="orange") +
                  geom_point(aes(x=doy, y=prediction, color=pred.type)) +
                  scale_color_manual(values=
                                       c(NDVI.pred = "tan",
                                         NDVI.predLag = "forestgreen",
                                        NDVI.predNorm = "black",
                                        NDVI.PredTemp = "red3",
                                        NDVI.PredMoist = "lightblue",
                                        NDVI.predCombo = "purple2")) +
                  scale_y_continuous(name="NDVI", limits=c(0, max(datLC$NDVI, na.rm=T))) +
                  theme_bw()
    
    
  png(file.path(path.figs, paste0("NDVI-Model_", LC, "_NDVI_2005.png")), height=6, width=6, units="in", res=220)
  print(ndvi2005)
  dev.off()
  
  # ggplot(data=datLC[datLC$year==2021,]) +
  #   ggtitle(paste0(LC, " NDVI in Year 2021 (drought year)")) +
  #   stat_smooth(aes(x=doy, y=NDVI.predNorm, color="normal"), method="gam") +
  #   geom_point(aes(x=doy, y=NDVI, color="observed")) +
  #   geom_point(aes(x=doy, y=NDVI.pred, color="predicted-process")) +
  #   stat_smooth(aes(x=doy, y=NDVI, color="observed"), method="gam") +
  #   stat_smooth(aes(x=doy, y=NDVI.predLag, color="predicted-lag only"), method="gam") +
  #   stat_smooth(aes(x=doy, y=NDVI.pred, color="predicted-process"), method="gam") +
  #   scale_color_manual(values=c("observed"="red4", "predicted-lag only"="salmon2", "predicted-process"="orange2", normal="black")) +
  #   scale_y_continuous(name="NDVI", limits=c(0, max(datLC$NDVI, na.rm=T))) +
  #   theme_bw()
  
  ndvi2012 <-  ggplot(data=pred.res.stack[pred.res.stack$year==2012,]) +
    ggtitle(paste0(LC, " NDVI in Year 2012 (drought year)")) +
    stat_smooth(aes(x=doy, y=prediction, color=pred.type), method="gam") +
    geom_point(aes(x = doy, y=NDVI), color="orange") +
    geom_point(aes(x=doy, y=prediction, color=pred.type)) +
    
    scale_color_manual(values=
                         c(NDVI.pred = "tan",
                           NDVI.predLag = "forestgreen",
                           NDVI.predNorm = "black",
                           NDVI.PredTemp = "red3",
                           NDVI.PredMoist = "lightblue",
                           NDVI.predCombo = "purple2")) +
    scale_y_continuous(name="NDVI", limits=c(0, max(datLC$NDVI, na.rm=T))) +
    theme_bw()
  
  
  # ndvi2012 <- ggplot(data=datLC[datLC$year==2012,]) +
  #   ggtitle(paste0(LC, " NDVI in Year 2012 (drought year)")) +
  #   stat_smooth(aes(x=doy, y=NDVI.predNorm, color="normal"), method="gam") +
  #   geom_point(aes(x=doy, y=NDVI, color="observed")) +
  #   geom_point(aes(x=doy, y=NDVI.pred, color="predicted-process")) +
  #   stat_smooth(aes(x=doy, y=NDVI, color="observed"), method="gam") +
  #   stat_smooth(aes(x=doy, y=NDVI.predLag, color="predicted-lag only"), method="gam") +
  #   stat_smooth(aes(x=doy, y=NDVI.pred, color="predicted-process"), method="gam") +
  #   scale_color_manual(values=c("observed"="red4", "predicted-lag only"="salmon2", "predicted-process"="orange2", normal="black")) +
  #   scale_y_continuous(name="NDVI", limits=c(0, max(datLC$NDVI, na.rm=T))) +
  #   theme_bw()
  
  png(file.path(path.figs, paste0("NDVI-Model_", LC, "_NDVI_2012.png")), height=6, width=6, units="in", res=220)
  print(ndvi2012)
  dev.off()

  
  ndvi2020 <-  ggplot(data=pred.res.stack[pred.res.stack$year==2020,]) +
    ggtitle(paste0(LC, " NDVI in Year 2020 (non-drought year)")) +
    stat_smooth(aes(x=doy, y=prediction, color=pred.type), method="gam") +
    geom_point(aes(x = doy, y=NDVI), color="orange") +
    geom_point(aes(x=doy, y=prediction, color=pred.type)) +
    
    scale_color_manual(values=
                         c(NDVI.pred = "tan",
                           NDVI.predLag = "forestgreen",
                           NDVI.predNorm = "black",
                           NDVI.PredTemp = "red3",
                           NDVI.PredMoist = "lightblue",
                           NDVI.predCombo = "purple2")) +
    scale_y_continuous(name="NDVI", limits=c(0, max(datLC$NDVI, na.rm=T))) +
    theme_bw()
  # ndvi2020 <- ggplot(data=datLC[datLC$year==2020,]) +
  #         ggtitle(paste0(LC, " NDVI in Year 2020 (non-drought year)")) +
  #         stat_smooth(aes(x=doy, y=NDVI.predNorm, color="normal"), method="gam") +
  #         geom_point(aes(x=doy, y=NDVI, color="observed")) +
  #         geom_point(aes(x=doy, y=NDVI.pred, color="predicted-process")) +
  #         stat_smooth(aes(x=doy, y=NDVI, color="observed"), method="gam") +
  #         stat_smooth(aes(x=doy, y=NDVI.predLag, color="predicted-lag only"), method="gam") +
  #         stat_smooth(aes(x=doy, y=NDVI.pred, color="predicted-process"), method="gam") +
  #         scale_color_manual(values=c("observed"="red4", "predicted-lag only"="salmon2", "predicted-process"="orange2", normal="black")) +
  #         scale_y_continuous(name="NDVI", limits=c(0, max(datLC$NDVI, na.rm=T))) +
  #         theme_bw()
  
  png(file.path(path.figs, paste0("NDVI-Model_", LC, "_NDVI_2020.png")), height=6, width=6, units="in", res=220)
  print(ndvi2020)
  dev.off()
  
  png(file.path(path.figs, paste0("NDVI-Model_", LC, "_NDVI-2005_MetDaily.png")), height=6, width=6, units="in", res=220)
  print(cowplot::plot_grid(ndvi2005 + theme(legend.title = element_blank(), plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "lines")),  plotEffSig+ theme(plot.margin = unit(c(0.5, 5.0, 0.5, 0.5), "lines"), axis.text.y=element_text(angle=90, hjust=0.5)), ncol=1))
  dev.off()
  
  corPredObsJJA <- lm(NDVI ~ NDVI.pred, data=datLC[datLC$doy>=yday(as.Date("2001-06-01")) & datLC$doy<yday(as.Date("2001-09-01")),])
  summary(corPredObsJJA)$r.squared
  
  corPredObsJJATemp <- lm(NDVI ~ NDVI.predTemp, data=datLC[datLC$doy>=yday(as.Date("2001-06-01")) & datLC$doy<yday(as.Date("2001-09-01")),])
  summary(corPredObsJJATemp)$r.squared
  
  corPredObsJJAMoist <- lm(NDVI ~ NDVI.predMoist, data=datLC[datLC$doy>=yday(as.Date("2001-06-01")) & datLC$doy<yday(as.Date("2001-09-01")),])
  summary(corPredObsJJAMoist)$r.squared
  
  corPredObsJJACombo <- lm(NDVI ~ NDVI.predCombo, data=datLC[datLC$doy>=yday(as.Date("2001-06-01")) & datLC$doy<yday(as.Date("2001-09-01")),])
  summary(corPredObsJJACombo)$r.squared
  
  png(file.path(path.figs, paste0("NDVI-Model_OG_", LC, "_Pred-Obs_JuneJulAug.png")), height=6, width=6, units="in", res=220)
  print(ggplot(data=datLC[datLC$doy>=yday(as.Date("2001-06-01")) & datLC$doy<yday(as.Date("2001-09-01")),], aes(x=NDVI.pred, y=NDVI)) +
    ggtitle(paste0(LC, ": June-July-August NDVI with 1:1 line (pseudo-R2=", round(summary(corPredObsJJA)$r.squared, 2), ")")) +
    geom_point() +
    geom_abline(slope=1, intercept=0, color="red2") +
    theme_bw())
  dev.off()
  
  png(file.path(path.figs, paste0("NDVI-Model_Temp_", LC, "_Pred-Obs_JuneJulAug.png")), height=6, width=6, units="in", res=220)
  print(ggplot(data=datLC[datLC$doy>=yday(as.Date("2001-06-01")) & datLC$doy<yday(as.Date("2001-09-01")),], aes(x=NDVI.predTemp, y=NDVI)) +
          ggtitle(paste0(LC, ": June-July-August NDVI with 1:1 line (pseudo-R2=", round(summary(corPredObsJJATemp)$r.squared, 2), ")")) +
          geom_point() +
          geom_abline(slope=1, intercept=0, color="red2") +
          theme_bw())
  dev.off()
  
  png(file.path(path.figs, paste0("NDVI-Model_Moist_", LC, "_Pred-Obs_JuneJulAug.png")), height=6, width=6, units="in", res=220)
  print(ggplot(data=datLC[datLC$doy>=yday(as.Date("2001-06-01")) & datLC$doy<yday(as.Date("2001-09-01")),], aes(x=NDVI.predMoist, y=NDVI)) +
          ggtitle(paste0(LC, ": June-July-August NDVI with 1:1 line (pseudo-R2=", round(summary(corPredObsJJAMoist)$r.squared, 2), ")")) +
          geom_point() +
          geom_abline(slope=1, intercept=0, color="red2") +
          theme_bw())
  dev.off()
  
  png(file.path(path.figs, paste0("NDVI-Model_Combo_", LC, "_Pred-Obs_JuneJulAug.png")), height=6, width=6, units="in", res=220)
  print(ggplot(data=datLC[datLC$doy>=yday(as.Date("2001-06-01")) & datLC$doy<yday(as.Date("2001-09-01")),], aes(x=NDVI.predCombo, y=NDVI)) +
          ggtitle(paste0(LC, ": June-July-August NDVI with 1:1 line (pseudo-R2=", round(summary(corPredObsJJACombo)$r.squared, 2), ")")) +
          geom_point() +
          geom_abline(slope=1, intercept=0, color="red2") +
          theme_bw())
  dev.off()
  
  print("") # Just kicking the label to a new line to make things cleaner
} 
#########################################


# RMSE Calculations----
library(caret)
# loading .csv files
all.files <- list.files(file.path(pathSave, "daily_models"))
rmse.files <- all.files[grep("DailyModel_NDVI-predict", all.files)]

# looking at urban-medium first

dat.all <- NULL
for(i in rmse.files){
  dat.temp <- read.csv(file.path(pathSave, "daily_models", i), header=T)
  
  if(is.null(dat.all)) dat.all <- dat.temp else dat.all <- rbind(dat.all, dat.temp)
}

head(dat.all)
summary(as.factor(dat.all$type))


# creating data frame to house RMSE results

rmse.df <- data.frame(model.type = c("NULL", "TMIN14_only", "SPI30_only", "COMBO"),
                      full.range = NA,
                      rmse.jan = NA,
                      rmse.feb = NA,
                      rmse.mar = NA,
                      rmse.apr = NA,
                      rmse.may = NA,
                      rmse.jun = NA,
                      rmse.jul = NA,
                      rmse.aug = NA,
                      rmse.sep = NA,
                      rmse.oct = NA,
                      rmse.nov = NA,
                      rmse.dec = NA)


jan.dat <- dat.all[dat.all$month==1,]
feb.dat <- dat.all[dat.all$month==2,]
mar.dat <- dat.all[dat.all$month==3,]
apr.dat <- dat.all[dat.all$month==4,]
may.dat <- dat.all[dat.all$month==5,]
jun.dat <- dat.all[dat.all$month==6,]
jul.dat <- dat.all[dat.all$month==7,]
aug.dat <- dat.all[dat.all$month==8,]
sep.dat <- dat.all[dat.all$month==9,]
oct.dat <- dat.all[dat.all$month==10,]
nov.dat <- dat.all[dat.all$month==11,]
dec.dat <- dat.all[dat.all$month==12,]

# NULL

rmse.df[rmse.df$model.type=="NULL","full.range"] <- RMSE(dat.all$NDVI.predLag, dat.all$NDVI, na.rm=T)
rmse.df[rmse.df$model.type=="NULL","rmse.jan"] <- RMSE(jan.dat$NDVI.predLag, jan.dat$NDVI, na.rm=T)
rmse.df[rmse.df$model.type=="NULL","rmse.feb"] <- RMSE(feb.dat$NDVI.predLag, feb.dat$NDVI, na.rm=T)
rmse.df[rmse.df$model.type=="NULL","rmse.mar"] <- RMSE(mar.dat$NDVI.predLag, mar.dat$NDVI, na.rm=T)
rmse.df[rmse.df$model.type=="NULL","rmse.apr"] <- RMSE(apr.dat$NDVI.predLag, apr.dat$NDVI, na.rm=T)
rmse.df[rmse.df$model.type=="NULL","rmse.may"] <- RMSE(may.dat$NDVI.predLag, may.dat$NDVI, na.rm=T)
rmse.df[rmse.df$model.type=="NULL","rmse.jun"] <- RMSE(jun.dat$NDVI.predLag, jun.dat$NDVI, na.rm=T)
rmse.df[rmse.df$model.type=="NULL","rmse.jul"] <- RMSE(jul.dat$NDVI.predLag, jul.dat$NDVI, na.rm=T)
rmse.df[rmse.df$model.type=="NULL","rmse.aug"] <- RMSE(aug.dat$NDVI.predLag, aug.dat$NDVI, na.rm=T)
rmse.df[rmse.df$model.type=="NULL","rmse.sep"] <- RMSE(sep.dat$NDVI.predLag, sep.dat$NDVI, na.rm=T)
rmse.df[rmse.df$model.type=="NULL","rmse.oct"] <- RMSE(oct.dat$NDVI.predLag, oct.dat$NDVI, na.rm=T)
rmse.df[rmse.df$model.type=="NULL","rmse.nov"] <- RMSE(nov.dat$NDVI.predLag, nov.dat$NDVI, na.rm=T)
rmse.df[rmse.df$model.type=="NULL","rmse.dec"] <- RMSE(dec.dat$NDVI.predLag, dec.dat$NDVI, na.rm=T)

# Tmin14day only

rmse.df[rmse.df$model.type=="TMIN14_only","full.range"] <- RMSE(dat.all$NDVI.predTemp, dat.all$NDVI, na.rm=T)
rmse.df[rmse.df$model.type=="TMIN14_only","rmse.jan"] <- RMSE(jan.dat$NDVI.predTemp, jan.dat$NDVI, na.rm=T)
rmse.df[rmse.df$model.type=="TMIN14_only","rmse.feb"] <- RMSE(feb.dat$NDVI.predTemp, feb.dat$NDVI, na.rm=T)
rmse.df[rmse.df$model.type=="TMIN14_only","rmse.mar"] <- RMSE(mar.dat$NDVI.predTemp, mar.dat$NDVI, na.rm=T)
rmse.df[rmse.df$model.type=="TMIN14_only","rmse.apr"] <- RMSE(apr.dat$NDVI.predTemp, apr.dat$NDVI, na.rm=T)
rmse.df[rmse.df$model.type=="TMIN14_only","rmse.may"] <- RMSE(may.dat$NDVI.predTemp, may.dat$NDVI, na.rm=T)
rmse.df[rmse.df$model.type=="TMIN14_only","rmse.jun"] <- RMSE(jun.dat$NDVI.predTemp, jun.dat$NDVI, na.rm=T)
rmse.df[rmse.df$model.type=="TMIN14_only","rmse.jul"] <- RMSE(jul.dat$NDVI.predTemp, jul.dat$NDVI, na.rm=T)
rmse.df[rmse.df$model.type=="TMIN14_only","rmse.aug"] <- RMSE(aug.dat$NDVI.predTemp, aug.dat$NDVI, na.rm=T)
rmse.df[rmse.df$model.type=="TMIN14_only","rmse.sep"] <- RMSE(sep.dat$NDVI.predTemp, sep.dat$NDVI, na.rm=T)
rmse.df[rmse.df$model.type=="TMIN14_only","rmse.oct"] <- RMSE(oct.dat$NDVI.predTemp, oct.dat$NDVI, na.rm=T)
rmse.df[rmse.df$model.type=="TMIN14_only","rmse.nov"] <- RMSE(nov.dat$NDVI.predTemp, nov.dat$NDVI, na.rm=T)
rmse.df[rmse.df$model.type=="TMIN14_only","rmse.dec"] <- RMSE(dec.dat$NDVI.predTemp, dec.dat$NDVI, na.rm=T)

# 30dSPI only
rmse.df[rmse.df$model.type=="SPI30_only","full.range"] <- RMSE(dat.all$NDVI.predMoist, dat.all$NDVI, na.rm=T)
rmse.df[rmse.df$model.type=="SPI30_only","rmse.jan"] <- RMSE(jan.dat$NDVI.predMoist, jan.dat$NDVI, na.rm=T)
rmse.df[rmse.df$model.type=="SPI30_only","rmse.feb"] <- RMSE(feb.dat$NDVI.predMoist, feb.dat$NDVI, na.rm=T)
rmse.df[rmse.df$model.type=="SPI30_only","rmse.mar"] <- RMSE(mar.dat$NDVI.predMoist, mar.dat$NDVI, na.rm=T)
rmse.df[rmse.df$model.type=="SPI30_only","rmse.apr"] <- RMSE(apr.dat$NDVI.predMoist, apr.dat$NDVI, na.rm=T)
rmse.df[rmse.df$model.type=="SPI30_only","rmse.may"] <- RMSE(may.dat$NDVI.predMoist, may.dat$NDVI, na.rm=T)
rmse.df[rmse.df$model.type=="SPI30_only","rmse.jun"] <- RMSE(jun.dat$NDVI.predMoist, jun.dat$NDVI, na.rm=T)
rmse.df[rmse.df$model.type=="SPI30_only","rmse.jul"] <- RMSE(jul.dat$NDVI.predMoist, jul.dat$NDVI, na.rm=T)
rmse.df[rmse.df$model.type=="SPI30_only","rmse.aug"] <- RMSE(aug.dat$NDVI.predMoist, aug.dat$NDVI, na.rm=T)
rmse.df[rmse.df$model.type=="SPI30_only","rmse.sep"] <- RMSE(sep.dat$NDVI.predMoist, sep.dat$NDVI, na.rm=T)
rmse.df[rmse.df$model.type=="SPI30_only","rmse.oct"] <- RMSE(oct.dat$NDVI.predMoist, oct.dat$NDVI, na.rm=T)
rmse.df[rmse.df$model.type=="SPI30_only","rmse.nov"] <- RMSE(nov.dat$NDVI.predMoist, nov.dat$NDVI, na.rm=T)
rmse.df[rmse.df$model.type=="SPI30_only","rmse.dec"] <- RMSE(dec.dat$NDVI.predMoist, dec.dat$NDVI, na.rm=T)

# Combo

rmse.df[rmse.df$model.type=="COMBO","full.range"] <- RMSE(dat.all$NDVI.predCombo, dat.all$NDVI, na.rm=T)
rmse.df[rmse.df$model.type=="COMBO","rmse.jan"] <- RMSE(jan.dat$NDVI.predCombo, jan.dat$NDVI, na.rm=T)
rmse.df[rmse.df$model.type=="COMBO","rmse.feb"] <- RMSE(feb.dat$NDVI.predCombo, feb.dat$NDVI, na.rm=T)
rmse.df[rmse.df$model.type=="COMBO","rmse.mar"] <- RMSE(mar.dat$NDVI.predCombo, mar.dat$NDVI, na.rm=T)
rmse.df[rmse.df$model.type=="COMBO","rmse.apr"] <- RMSE(apr.dat$NDVI.predCombo, apr.dat$NDVI, na.rm=T)
rmse.df[rmse.df$model.type=="COMBO","rmse.may"] <- RMSE(may.dat$NDVI.predCombo, may.dat$NDVI, na.rm=T)
rmse.df[rmse.df$model.type=="COMBO","rmse.jun"] <- RMSE(jun.dat$NDVI.predCombo, jun.dat$NDVI, na.rm=T)
rmse.df[rmse.df$model.type=="COMBO","rmse.jul"] <- RMSE(jul.dat$NDVI.predCombo, jul.dat$NDVI, na.rm=T)
rmse.df[rmse.df$model.type=="COMBO","rmse.aug"] <- RMSE(aug.dat$NDVI.predCombo, aug.dat$NDVI, na.rm=T)
rmse.df[rmse.df$model.type=="COMBO","rmse.sep"] <- RMSE(sep.dat$NDVI.predCombo, sep.dat$NDVI, na.rm=T)
rmse.df[rmse.df$model.type=="COMBO","rmse.oct"] <- RMSE(oct.dat$NDVI.predCombo, oct.dat$NDVI, na.rm=T)
rmse.df[rmse.df$model.type=="COMBO","rmse.nov"] <- RMSE(nov.dat$NDVI.predCombo, nov.dat$NDVI, na.rm=T)
rmse.df[rmse.df$model.type=="COMBO","rmse.dec"] <- RMSE(dec.dat$NDVI.predCombo, dec.dat$NDVI, na.rm=T)

rmse.df

# Now looking at deciles of the TMIN14d
tdec.rmse.df <- data.frame(model.type = c("NULL", "TMIN14_only", "SPI30_only", "COMBO"),
                      full.range = NA,
                      rmse.p00 = NA,
                      rmse.p10 = NA,
                      rmse.p20 = NA,
                      rmse.p30 = NA,
                      rmse.p40 = NA,
                      rmse.p50 = NA,
                      rmse.p60 = NA,
                      rmse.p70 = NA,
                      rmse.p80 = NA,
                      rmse.p90 = NA)

dat.all$Temp14d.bins <- cut(dat.all$TMIN14d, breaks = 10, labels=1:10, na.rm=T)


tdec.rmse.df[tdec.rmse.df$model.type=="NULL","full.range"] <- RMSE(dat.all$NDVI.predLag, dat.all$NDVI, na.rm=T)
tdec.rmse.df[tdec.rmse.df$model.type=="NULL","rmse.p00"] <- RMSE(dat.all[dat.all$Temp14d.bins==1, "NDVI.predLag"], dat.all[dat.all$Temp14d.bins==1, "NDVI"], na.rm=T)
tdec.rmse.df[tdec.rmse.df$model.type=="NULL","rmse.p10"] <- RMSE(dat.all[dat.all$Temp14d.bins==2, "NDVI.predLag"], dat.all[dat.all$Temp14d.bins==1, "NDVI"], na.rm=T)
tdec.rmse.df[tdec.rmse.df$model.type=="NULL","rmse.p20"] <- RMSE(dat.all[dat.all$Temp14d.bins==3, "NDVI.predLag"], dat.all[dat.all$Temp14d.bins==1, "NDVI"], na.rm=T)
tdec.rmse.df[tdec.rmse.df$model.type=="NULL","rmse.p30"] <- RMSE(dat.all[dat.all$Temp14d.bins==4, "NDVI.predLag"], dat.all[dat.all$Temp14d.bins==1, "NDVI"], na.rm=T)
tdec.rmse.df[tdec.rmse.df$model.type=="NULL","rmse.p40"] <- RMSE(dat.all[dat.all$Temp14d.bins==5, "NDVI.predLag"], dat.all[dat.all$Temp14d.bins==1, "NDVI"], na.rm=T)
tdec.rmse.df[tdec.rmse.df$model.type=="NULL","rmse.p50"] <- RMSE(dat.all[dat.all$Temp14d.bins==6, "NDVI.predLag"], dat.all[dat.all$Temp14d.bins==1, "NDVI"], na.rm=T)
tdec.rmse.df[tdec.rmse.df$model.type=="NULL","rmse.p60"] <- RMSE(dat.all[dat.all$Temp14d.bins==7, "NDVI.predLag"], dat.all[dat.all$Temp14d.bins==1, "NDVI"], na.rm=T)
tdec.rmse.df[tdec.rmse.df$model.type=="NULL","rmse.p70"] <- RMSE(dat.all[dat.all$Temp14d.bins==8, "NDVI.predLag"], dat.all[dat.all$Temp14d.bins==1, "NDVI"], na.rm=T)
tdec.rmse.df[tdec.rmse.df$model.type=="NULL","rmse.p80"] <- RMSE(dat.all[dat.all$Temp14d.bins==9, "NDVI.predLag"], dat.all[dat.all$Temp14d.bins==1, "NDVI"], na.rm=T)
tdec.rmse.df[tdec.rmse.df$model.type=="NULL","rmse.p90"] <- RMSE(dat.all[dat.all$Temp14d.bins==10, "NDVI.predLag"], dat.all[dat.all$Temp14d.bins==1, "NDVI"], na.rm=T)


tdec.rmse.df[tdec.rmse.df$model.type=="TMIN14_only","full.range"] <- RMSE(dat.all$NDVI.predTemp, dat.all$NDVI, na.rm=T)
tdec.rmse.df[tdec.rmse.df$model.type=="TMIN14_only","rmse.p00"] <- RMSE(dat.all[dat.all$Temp14d.bins==1, "NDVI.predTemp"], dat.all[dat.all$Temp14d.bins==1, "NDVI"], na.rm=T)
tdec.rmse.df[tdec.rmse.df$model.type=="TMIN14_only","rmse.p10"] <- RMSE(dat.all[dat.all$Temp14d.bins==2, "NDVI.predTemp"], dat.all[dat.all$Temp14d.bins==1, "NDVI"], na.rm=T)
tdec.rmse.df[tdec.rmse.df$model.type=="TMIN14_only","rmse.p20"] <- RMSE(dat.all[dat.all$Temp14d.bins==3, "NDVI.predTemp"], dat.all[dat.all$Temp14d.bins==1, "NDVI"], na.rm=T)
tdec.rmse.df[tdec.rmse.df$model.type=="TMIN14_only","rmse.p30"] <- RMSE(dat.all[dat.all$Temp14d.bins==4, "NDVI.predTemp"], dat.all[dat.all$Temp14d.bins==1, "NDVI"], na.rm=T)
tdec.rmse.df[tdec.rmse.df$model.type=="TMIN14_only","rmse.p40"] <- RMSE(dat.all[dat.all$Temp14d.bins==5, "NDVI.predTemp"], dat.all[dat.all$Temp14d.bins==1, "NDVI"], na.rm=T)
tdec.rmse.df[tdec.rmse.df$model.type=="TMIN14_only","rmse.p50"] <- RMSE(dat.all[dat.all$Temp14d.bins==6, "NDVI.predTemp"], dat.all[dat.all$Temp14d.bins==1, "NDVI"], na.rm=T)
tdec.rmse.df[tdec.rmse.df$model.type=="TMIN14_only","rmse.p60"] <- RMSE(dat.all[dat.all$Temp14d.bins==7, "NDVI.predTemp"], dat.all[dat.all$Temp14d.bins==1, "NDVI"], na.rm=T)
tdec.rmse.df[tdec.rmse.df$model.type=="TMIN14_only","rmse.p70"] <- RMSE(dat.all[dat.all$Temp14d.bins==8, "NDVI.predTemp"], dat.all[dat.all$Temp14d.bins==1, "NDVI"], na.rm=T)
tdec.rmse.df[tdec.rmse.df$model.type=="TMIN14_only","rmse.p80"] <- RMSE(dat.all[dat.all$Temp14d.bins==9, "NDVI.predTemp"], dat.all[dat.all$Temp14d.bins==1, "NDVI"], na.rm=T)
tdec.rmse.df[tdec.rmse.df$model.type=="TMIN14_only","rmse.p90"] <- RMSE(dat.all[dat.all$Temp14d.bins==10, "NDVI.predTemp"], dat.all[dat.all$Temp14d.bins==1, "NDVI"], na.rm=T)

tdec.rmse.df[tdec.rmse.df$model.type=="SPI30_only","full.range"] <- RMSE(dat.all$NDVI.predMoist, dat.all$NDVI, na.rm=T)
tdec.rmse.df[tdec.rmse.df$model.type=="SPI30_only","rmse.p00"] <- RMSE(dat.all[dat.all$Temp14d.bins==1, "NDVI.predMoist"], dat.all[dat.all$Temp14d.bins==1, "NDVI"], na.rm=T)
tdec.rmse.df[tdec.rmse.df$model.type=="SPI30_only","rmse.p10"] <- RMSE(dat.all[dat.all$Temp14d.bins==2, "NDVI.predMoist"], dat.all[dat.all$Temp14d.bins==1, "NDVI"], na.rm=T)
tdec.rmse.df[tdec.rmse.df$model.type=="SPI30_only","rmse.p20"] <- RMSE(dat.all[dat.all$Temp14d.bins==3, "NDVI.predMoist"], dat.all[dat.all$Temp14d.bins==1, "NDVI"], na.rm=T)
tdec.rmse.df[tdec.rmse.df$model.type=="SPI30_only","rmse.p30"] <- RMSE(dat.all[dat.all$Temp14d.bins==4, "NDVI.predMoist"], dat.all[dat.all$Temp14d.bins==1, "NDVI"], na.rm=T)
tdec.rmse.df[tdec.rmse.df$model.type=="SPI30_only","rmse.p40"] <- RMSE(dat.all[dat.all$Temp14d.bins==5, "NDVI.predMoist"], dat.all[dat.all$Temp14d.bins==1, "NDVI"], na.rm=T)
tdec.rmse.df[tdec.rmse.df$model.type=="SPI30_only","rmse.p50"] <- RMSE(dat.all[dat.all$Temp14d.bins==6, "NDVI.predMoist"], dat.all[dat.all$Temp14d.bins==1, "NDVI"], na.rm=T)
tdec.rmse.df[tdec.rmse.df$model.type=="SPI30_only","rmse.p60"] <- RMSE(dat.all[dat.all$Temp14d.bins==7, "NDVI.predMoist"], dat.all[dat.all$Temp14d.bins==1, "NDVI"], na.rm=T)
tdec.rmse.df[tdec.rmse.df$model.type=="SPI30_only","rmse.p70"] <- RMSE(dat.all[dat.all$Temp14d.bins==8, "NDVI.predMoist"], dat.all[dat.all$Temp14d.bins==1, "NDVI"], na.rm=T)
tdec.rmse.df[tdec.rmse.df$model.type=="SPI30_only","rmse.p80"] <- RMSE(dat.all[dat.all$Temp14d.bins==9, "NDVI.predMoist"], dat.all[dat.all$Temp14d.bins==1, "NDVI"], na.rm=T)
tdec.rmse.df[tdec.rmse.df$model.type=="SPI30_only","rmse.p90"] <- RMSE(dat.all[dat.all$Temp14d.bins==10, "NDVI.predMoist"], dat.all[dat.all$Temp14d.bins==1, "NDVI"], na.rm=T)

tdec.rmse.df[tdec.rmse.df$model.type=="COMBO","full.range"] <- RMSE(dat.all$NDVI.predCombo, dat.all$NDVI, na.rm=T)
tdec.rmse.df[tdec.rmse.df$model.type=="COMBO","rmse.p00"] <- RMSE(dat.all[dat.all$Temp14d.bins==1, "NDVI.predCombo"], dat.all[dat.all$Temp14d.bins==1, "NDVI"], na.rm=T)
tdec.rmse.df[tdec.rmse.df$model.type=="COMBO","rmse.p10"] <- RMSE(dat.all[dat.all$Temp14d.bins==2, "NDVI.predCombo"], dat.all[dat.all$Temp14d.bins==1, "NDVI"], na.rm=T)
tdec.rmse.df[tdec.rmse.df$model.type=="COMBO","rmse.p20"] <- RMSE(dat.all[dat.all$Temp14d.bins==3, "NDVI.predCombo"], dat.all[dat.all$Temp14d.bins==1, "NDVI"], na.rm=T)
tdec.rmse.df[tdec.rmse.df$model.type=="COMBO","rmse.p30"] <- RMSE(dat.all[dat.all$Temp14d.bins==4, "NDVI.predCombo"], dat.all[dat.all$Temp14d.bins==1, "NDVI"], na.rm=T)
tdec.rmse.df[tdec.rmse.df$model.type=="COMBO","rmse.p40"] <- RMSE(dat.all[dat.all$Temp14d.bins==5, "NDVI.predCombo"], dat.all[dat.all$Temp14d.bins==1, "NDVI"], na.rm=T)
tdec.rmse.df[tdec.rmse.df$model.type=="COMBO","rmse.p50"] <- RMSE(dat.all[dat.all$Temp14d.bins==6, "NDVI.predCombo"], dat.all[dat.all$Temp14d.bins==1, "NDVI"], na.rm=T)
tdec.rmse.df[tdec.rmse.df$model.type=="COMBO","rmse.p60"] <- RMSE(dat.all[dat.all$Temp14d.bins==7, "NDVI.predCombo"], dat.all[dat.all$Temp14d.bins==1, "NDVI"], na.rm=T)
tdec.rmse.df[tdec.rmse.df$model.type=="COMBO","rmse.p70"] <- RMSE(dat.all[dat.all$Temp14d.bins==8, "NDVI.predCombo"], dat.all[dat.all$Temp14d.bins==1, "NDVI"], na.rm=T)
tdec.rmse.df[tdec.rmse.df$model.type=="COMBO","rmse.p80"] <- RMSE(dat.all[dat.all$Temp14d.bins==9, "NDVI.predCombo"], dat.all[dat.all$Temp14d.bins==1, "NDVI"], na.rm=T)
tdec.rmse.df[tdec.rmse.df$model.type=="COMBO","rmse.p90"] <- RMSE(dat.all[dat.all$Temp14d.bins==10, "NDVI.predCombo"], dat.all[dat.all$Temp14d.bins==1, "NDVI"], na.rm=T)


tdec.rmse.df

# deciles of 30day SPI
mdec.rmse.df <- data.frame(model.type = c("NULL", "TMIN14_only", "SPI30_only", "COMBO"),
                           full.range = NA,
                           rmse.p00 = NA,
                           rmse.p10 = NA,
                           rmse.p20 = NA,
                           rmse.p30 = NA,
                           rmse.p40 = NA,
                           rmse.p50 = NA,
                           rmse.p60 = NA,
                           rmse.p70 = NA,
                           rmse.p80 = NA,
                           rmse.p90 = NA)

dat.all$SPI30.bins <- cut(dat.all$X30d.SPI, breaks = 10, labels=1:10, na.rm=T)

mdec.rmse.df[mdec.rmse.df$model.type=="NULL","full.range"] <- RMSE(dat.all$NDVI.predLag, dat.all$NDVI, na.rm=T)
mdec.rmse.df[mdec.rmse.df$model.type=="NULL","rmse.p00"] <- RMSE(dat.all[dat.all$SPI30.bins==1, "NDVI.predLag"], dat.all[dat.all$SPI30.bins==1, "NDVI"], na.rm=T)
mdec.rmse.df[mdec.rmse.df$model.type=="NULL","rmse.p10"] <- RMSE(dat.all[dat.all$SPI30.bins==2, "NDVI.predLag"], dat.all[dat.all$SPI30.bins==1, "NDVI"], na.rm=T)
mdec.rmse.df[mdec.rmse.df$model.type=="NULL","rmse.p20"] <- RMSE(dat.all[dat.all$SPI30.bins==3, "NDVI.predLag"], dat.all[dat.all$SPI30.bins==1, "NDVI"], na.rm=T)
mdec.rmse.df[mdec.rmse.df$model.type=="NULL","rmse.p30"] <- RMSE(dat.all[dat.all$SPI30.bins==4, "NDVI.predLag"], dat.all[dat.all$SPI30.bins==1, "NDVI"], na.rm=T)
mdec.rmse.df[mdec.rmse.df$model.type=="NULL","rmse.p40"] <- RMSE(dat.all[dat.all$SPI30.bins==5, "NDVI.predLag"], dat.all[dat.all$SPI30.bins==1, "NDVI"], na.rm=T)
mdec.rmse.df[mdec.rmse.df$model.type=="NULL","rmse.p50"] <- RMSE(dat.all[dat.all$SPI30.bins==6, "NDVI.predLag"], dat.all[dat.all$SPI30.bins==1, "NDVI"], na.rm=T)
mdec.rmse.df[mdec.rmse.df$model.type=="NULL","rmse.p60"] <- RMSE(dat.all[dat.all$SPI30.bins==7, "NDVI.predLag"], dat.all[dat.all$SPI30.bins==1, "NDVI"], na.rm=T)
mdec.rmse.df[mdec.rmse.df$model.type=="NULL","rmse.p70"] <- RMSE(dat.all[dat.all$SPI30.bins==8, "NDVI.predLag"], dat.all[dat.all$SPI30.bins==1, "NDVI"], na.rm=T)
mdec.rmse.df[mdec.rmse.df$model.type=="NULL","rmse.p80"] <- RMSE(dat.all[dat.all$SPI30.bins==9, "NDVI.predLag"], dat.all[dat.all$SPI30.bins==1, "NDVI"], na.rm=T)
mdec.rmse.df[mdec.rmse.df$model.type=="NULL","rmse.p90"] <- RMSE(dat.all[dat.all$SPI30.bins==10, "NDVI.predLag"], dat.all[dat.all$SPI30.bins==1, "NDVI"], na.rm=T)


mdec.rmse.df[mdec.rmse.df$model.type=="TMIN14_only","full.range"] <- RMSE(dat.all$NDVI.predTemp, dat.all$NDVI, na.rm=T)
mdec.rmse.df[mdec.rmse.df$model.type=="TMIN14_only","rmse.p00"] <- RMSE(dat.all[dat.all$SPI30.bins==1, "NDVI.predTemp"], dat.all[dat.all$SPI30.bins==1, "NDVI"], na.rm=T)
mdec.rmse.df[mdec.rmse.df$model.type=="TMIN14_only","rmse.p10"] <- RMSE(dat.all[dat.all$SPI30.bins==2, "NDVI.predTemp"], dat.all[dat.all$SPI30.bins==1, "NDVI"], na.rm=T)
mdec.rmse.df[mdec.rmse.df$model.type=="TMIN14_only","rmse.p20"] <- RMSE(dat.all[dat.all$SPI30.bins==3, "NDVI.predTemp"], dat.all[dat.all$SPI30.bins==1, "NDVI"], na.rm=T)
mdec.rmse.df[mdec.rmse.df$model.type=="TMIN14_only","rmse.p30"] <- RMSE(dat.all[dat.all$SPI30.bins==4, "NDVI.predTemp"], dat.all[dat.all$SPI30.bins==1, "NDVI"], na.rm=T)
mdec.rmse.df[mdec.rmse.df$model.type=="TMIN14_only","rmse.p40"] <- RMSE(dat.all[dat.all$SPI30.bins==5, "NDVI.predTemp"], dat.all[dat.all$SPI30.bins==1, "NDVI"], na.rm=T)
mdec.rmse.df[mdec.rmse.df$model.type=="TMIN14_only","rmse.p50"] <- RMSE(dat.all[dat.all$SPI30.bins==6, "NDVI.predTemp"], dat.all[dat.all$SPI30.bins==1, "NDVI"], na.rm=T)
mdec.rmse.df[mdec.rmse.df$model.type=="TMIN14_only","rmse.p60"] <- RMSE(dat.all[dat.all$SPI30.bins==7, "NDVI.predTemp"], dat.all[dat.all$SPI30.bins==1, "NDVI"], na.rm=T)
mdec.rmse.df[mdec.rmse.df$model.type=="TMIN14_only","rmse.p70"] <- RMSE(dat.all[dat.all$SPI30.bins==8, "NDVI.predTemp"], dat.all[dat.all$SPI30.bins==1, "NDVI"], na.rm=T)
mdec.rmse.df[mdec.rmse.df$model.type=="TMIN14_only","rmse.p80"] <- RMSE(dat.all[dat.all$SPI30.bins==9, "NDVI.predTemp"], dat.all[dat.all$SPI30.bins==1, "NDVI"], na.rm=T)
mdec.rmse.df[mdec.rmse.df$model.type=="TMIN14_only","rmse.p90"] <- RMSE(dat.all[dat.all$SPI30.bins==10, "NDVI.predTemp"], dat.all[dat.all$SPI30.bins==1, "NDVI"], na.rm=T)

mdec.rmse.df[mdec.rmse.df$model.type=="SPI30_only","full.range"] <- RMSE(dat.all$NDVI.predMoist, dat.all$NDVI, na.rm=T)
mdec.rmse.df[mdec.rmse.df$model.type=="SPI30_only","rmse.p00"] <- RMSE(dat.all[dat.all$SPI30.bins==1, "NDVI.predMoist"], dat.all[dat.all$SPI30.bins==1, "NDVI"], na.rm=T)
mdec.rmse.df[mdec.rmse.df$model.type=="SPI30_only","rmse.p10"] <- RMSE(dat.all[dat.all$SPI30.bins==2, "NDVI.predMoist"], dat.all[dat.all$SPI30.bins==1, "NDVI"], na.rm=T)
mdec.rmse.df[mdec.rmse.df$model.type=="SPI30_only","rmse.p20"] <- RMSE(dat.all[dat.all$SPI30.bins==3, "NDVI.predMoist"], dat.all[dat.all$SPI30.bins==1, "NDVI"], na.rm=T)
mdec.rmse.df[mdec.rmse.df$model.type=="SPI30_only","rmse.p30"] <- RMSE(dat.all[dat.all$SPI30.bins==4, "NDVI.predMoist"], dat.all[dat.all$SPI30.bins==1, "NDVI"], na.rm=T)
mdec.rmse.df[mdec.rmse.df$model.type=="SPI30_only","rmse.p40"] <- RMSE(dat.all[dat.all$SPI30.bins==5, "NDVI.predMoist"], dat.all[dat.all$SPI30.bins==1, "NDVI"], na.rm=T)
mdec.rmse.df[mdec.rmse.df$model.type=="SPI30_only","rmse.p50"] <- RMSE(dat.all[dat.all$SPI30.bins==6, "NDVI.predMoist"], dat.all[dat.all$SPI30.bins==1, "NDVI"], na.rm=T)
mdec.rmse.df[mdec.rmse.df$model.type=="SPI30_only","rmse.p60"] <- RMSE(dat.all[dat.all$SPI30.bins==7, "NDVI.predMoist"], dat.all[dat.all$SPI30.bins==1, "NDVI"], na.rm=T)
mdec.rmse.df[mdec.rmse.df$model.type=="SPI30_only","rmse.p70"] <- RMSE(dat.all[dat.all$SPI30.bins==8, "NDVI.predMoist"], dat.all[dat.all$SPI30.bins==1, "NDVI"], na.rm=T)
mdec.rmse.df[mdec.rmse.df$model.type=="SPI30_only","rmse.p80"] <- RMSE(dat.all[dat.all$SPI30.bins==9, "NDVI.predMoist"], dat.all[dat.all$SPI30.bins==1, "NDVI"], na.rm=T)
mdec.rmse.df[mdec.rmse.df$model.type=="SPI30_only","rmse.p90"] <- RMSE(dat.all[dat.all$SPI30.bins==10, "NDVI.predMoist"], dat.all[dat.all$SPI30.bins==1, "NDVI"], na.rm=T)

mdec.rmse.df[mdec.rmse.df$model.type=="COMBO","full.range"] <- RMSE(dat.all$NDVI.predCombo, dat.all$NDVI, na.rm=T)
mdec.rmse.df[mdec.rmse.df$model.type=="COMBO","rmse.p00"] <- RMSE(dat.all[dat.all$SPI30.bins==1, "NDVI.predCombo"], dat.all[dat.all$SPI30.bins==1, "NDVI"], na.rm=T)
mdec.rmse.df[mdec.rmse.df$model.type=="COMBO","rmse.p10"] <- RMSE(dat.all[dat.all$SPI30.bins==2, "NDVI.predCombo"], dat.all[dat.all$SPI30.bins==1, "NDVI"], na.rm=T)
mdec.rmse.df[mdec.rmse.df$model.type=="COMBO","rmse.p20"] <- RMSE(dat.all[dat.all$SPI30.bins==3, "NDVI.predCombo"], dat.all[dat.all$SPI30.bins==1, "NDVI"], na.rm=T)
mdec.rmse.df[mdec.rmse.df$model.type=="COMBO","rmse.p30"] <- RMSE(dat.all[dat.all$SPI30.bins==4, "NDVI.predCombo"], dat.all[dat.all$SPI30.bins==1, "NDVI"], na.rm=T)
mdec.rmse.df[mdec.rmse.df$model.type=="COMBO","rmse.p40"] <- RMSE(dat.all[dat.all$SPI30.bins==5, "NDVI.predCombo"], dat.all[dat.all$SPI30.bins==1, "NDVI"], na.rm=T)
mdec.rmse.df[mdec.rmse.df$model.type=="COMBO","rmse.p50"] <- RMSE(dat.all[dat.all$SPI30.bins==6, "NDVI.predCombo"], dat.all[dat.all$SPI30.bins==1, "NDVI"], na.rm=T)
mdec.rmse.df[mdec.rmse.df$model.type=="COMBO","rmse.p60"] <- RMSE(dat.all[dat.all$SPI30.bins==7, "NDVI.predCombo"], dat.all[dat.all$SPI30.bins==1, "NDVI"], na.rm=T)
mdec.rmse.df[mdec.rmse.df$model.type=="COMBO","rmse.p70"] <- RMSE(dat.all[dat.all$SPI30.bins==8, "NDVI.predCombo"], dat.all[dat.all$SPI30.bins==1, "NDVI"], na.rm=T)
mdec.rmse.df[mdec.rmse.df$model.type=="COMBO","rmse.p80"] <- RMSE(dat.all[dat.all$SPI30.bins==9, "NDVI.predCombo"], dat.all[dat.all$SPI30.bins==1, "NDVI"], na.rm=T)
mdec.rmse.df[mdec.rmse.df$model.type=="COMBO","rmse.p90"] <- RMSE(dat.all[dat.all$SPI30.bins==10, "NDVI.predCombo"], dat.all[dat.all$SPI30.bins==1, "NDVI"], na.rm=T)

mdec.rmse.df
