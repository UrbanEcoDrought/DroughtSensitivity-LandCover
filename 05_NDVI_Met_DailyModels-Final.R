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

if(!dir.exists(path.figs)) dir.create(path.figs, recursive = T)
if(!dir.exists(pathSave)) dir.create(pathSave, recursive = T)

# Read in the two key data frames
ndviMet <- read.csv(file.path(google.drive,"data/processed_files/landsat_ndvi_metVars_combined.csv"))
ndviMet$date <- as.Date(ndviMet$date)
ndviMet$mission <- as.factor(ndviMet$mission) 
ndviMet$landcover <- as.factor(ndviMet$landcover)

modStatsAll <- read.csv(file.path(google.drive, "data/processed_files/ModelSelection-Multivariate", paste0("DailyModel_VarSelection-Multivariate_ModelStats-ALL.csv")))
modStatsAll$landcover <- as.factor(modStatsAll$landcover)
modStatsAll$model <- as.factor(modStatsAll$model)
modStatsAll$DroughtVar <- as.factor(modStatsAll$DroughtVar)
modStatsAll$TempVar <- as.factor(modStatsAll$TempVar)
modStatsAll$modelType <- as.factor(modStatsAll$modelType)
summary(modStatsAll)

# Aggregating data to mean by model, drought var, temp var, model type, adn land cover
aggLC <- aggregate(cbind(dAIC, dR2, dRMSE) ~ model + DroughtVar + TempVar + modelType + landcover, data=modStatsAll[!modStatsAll$model %in% c("modLag"),], FUN=mean, na.rm=T)
aggLC2 <- aggregate(cbind(dAIC, dR2, dRMSE) ~ model + DroughtVar + TempVar + modelType, data=aggLC, FUN=mean, na.rm=T)
aggLC2$dAIC.rank[order(aggLC2$dAIC, decreasing=F)] <- 1:nrow(aggLC2)
aggLC2$dR2.rank[order(aggLC2$dR2, decreasing=T)] <- 1:nrow(aggLC2)
aggLC2$dRMSE.rank[order(aggLC2$dRMSE, decreasing=F)] <- 1:nrow(aggLC2)
aggLC2

aggDroughtVar <- aggregate(cbind(dAIC, dR2, dRMSE) ~ DroughtVar + modelType, data=aggLC, FUN=mean, na.rm=T)
aggDroughtVar$dAIC.rank[order(aggDroughtVar$dAIC, decreasing=F)] <- 1:nrow(aggDroughtVar)
aggDroughtVar$dR2.rank[order(aggDroughtVar$dR2, decreasing=T)] <- 1:nrow(aggDroughtVar)
aggDroughtVar$dRMSE.rank[order(aggDroughtVar$dRMSE, decreasing=F)] <- 1:nrow(aggDroughtVar)
aggDroughtVar

aggTempVar <- aggregate(cbind(dAIC, dR2, dRMSE) ~ TempVar + modelType, data=aggLC, FUN=mean, na.rm=T)
aggTempVar$dAIC.rank[order(aggTempVar$dAIC, decreasing=F)] <- 1:nrow(aggTempVar)
aggTempVar$dR2.rank[order(aggTempVar$dR2, decreasing=T)] <- 1:nrow(aggTempVar)
aggTempVar$dRMSE.rank[order(aggTempVar$dRMSE, decreasing=F)] <- 1:nrow(aggTempVar)
aggTempVar

aggXn <- aggregate(cbind(dAIC.xn, dR2.xn, dRMSE.xn) ~ model + DroughtVar + TempVar + modelType + landcover, data=modStatsAll[!modStatsAll$model %in% c("modLag"),], FUN=mean, na.rm=T)
aggXn2 <- aggregate(cbind(dAIC.xn, dR2.xn, dRMSE.xn) ~ model + DroughtVar + TempVar + modelType, data=aggXn, FUN=mean, na.rm=T)
aggXn2$dAIC.rank[order(aggXn2$dAIC, decreasing=F)] <- 1:nrow(aggXn2)
aggXn2$dR2.rank[order(aggXn2$dR2, decreasing=T)] <- 1:nrow(aggXn2)
aggXn2$dRMSE.rank[order(aggXn2$dRMSE, decreasing=F)] <- 1:nrow(aggXn2)
aggXn2

aggAdd <- aggregate(cbind(dAIC, dR2, dRMSE) ~ model + DroughtVar + TempVar + modelType + landcover, data=modStatsAll[!modStatsAll$model %in% c("modLag") & !modStatsAll$modelType=="interaction",], FUN=mean, na.rm=T)
aggAdd2 <- aggregate(cbind(dAIC, dR2, dRMSE) ~ model + DroughtVar + TempVar + modelType, data=aggAdd, FUN=mean, na.rm=T)
aggAdd2$dAIC.rank[order(aggAdd2$dAIC, decreasing=F)] <- 1:nrow(aggAdd2)
aggAdd2$dR2.rank[order(aggAdd2$dR2, decreasing=T)] <- 1:nrow(aggAdd2)
aggAdd2$dRMSE.rank[order(aggAdd2$dRMSE, decreasing=F)] <- 1:nrow(aggAdd2)
aggAdd2

aggAdd2[grep("SPI", aggAdd2$DroughtVar),]
aggAdd2[grep("SPEI", aggAdd2$DroughtVar),]

# Of interactive models, 30dSPEI x TMIN60d is the best by many metrics, but not strong support for improvement based on AIC; R2 improvements rel to additive in the ballpark of 0.025
# Of additive models: 14dSPEI x TMAX30d ranks best for dR2 & dRMSE; next best is 14dSPEI x TMAX60dl not strong support via AIC, but R2 improvements rel to lag-only in ballpark of 0.05
# The best performing SPI model is actually 30-d SPI with 60-d TMAX



# # # # # # # # # # # # # # # # # # # # # # # # # 
# Running the models! ----
# # # # # # # # # # # # # # # # # # # # # # # # # 
LCtypes <- unique(ndviMet$landcover)
modOutList <- list()
modOutListSPI <- list()
for(LC in LCtypes){
  print(LC)
  # Subset the data to a single land cover type
  datLC <- ndviMet[ndviMet$landcover==LC,]
  
  # Checking the autocorrelation in NDVI
  head(datLC)
  # acf(datLC$NDVI[!is.na(datLC$NDVI)])
  # acf(datLC$resid[!is.na(datLC$resid)]) # note: need to run below for this to work!
  
  
  # Creating a 14-day NDVI lag (day -14), that goes across missions to try to bring in autocorrleation
  # May need a longer window, but we'll see
  datLC$NDVI.Lag14d <- NA
  for(i in 1:nrow(datLC)){
    rowLag <- which(datLC$date>=(datLC$date[i]-14) & datLC$date<datLC$date[i])
    
    if(length(rowLag)<1 ) next
    if(length(rowLag)==1) datLC$NDVI.Lag14d[i] <- datLC$NDVI[rowLag]
    if(length(rowLag)>1) datLC$NDVI.Lag14d[i] <- mean(datLC$NDVI[rowLag], na.rm=T)
    
  }
  summary(datLC)
  
  # Starting with doing a simple model of NDVI ~ 
  days.use <- 1:365
  
  modsList <- list()
  mod.out <- data.frame(landcover=LC, yday=1:365, Rsq=NA, RMSE=NA, 
                        coef.Int=NA, coef.Lag=NA, coef.SPEI14=NA, coef.TMAX30=NA, 
                        err.Int=NA, err.Lag=NA, err.SPEI14=NA, err.TMAX30=NA, 
                        tVal.Int=NA, tVal.Lag=NA, tVal.SPEI14=NA, tVal.TMAX30=NA, 
                        pVal.Int=NA, pVal.Lag=NA, pVal.SPEI14=NA, pVal.TMAX30=NA) 
  
  # row.ind = 0 # Setting up an index that will tell us what row to save things in; we should start with 0 because we haven't done anything yet
  pb <- txtProgressBar(min=min(days.use), max=max(days.use), style=3)
  for(i in days.use){
    setTxtProgressBar(pb, i)
    # For testing using i=185 (which is July 4; yday(as.Date("2021-07-04"))) -- this is a period that should have a decent SPI relationship based off of the initial corr plots
    # dayNOW <- days.use[i] # This is almost exactly the same as above, but now i will go from 1 to 215 (the number of unique days.use we have)
    # dayNOW = i # Repurposing old code, so doing a clunky approach here
    
    ## Using an even-sided window to train the model for now to understand the relationships
    # Here we're subsetting our big data frame to the SMALL temporal window we want --> this should help with temporal stationarity in the effects of our predictors
    rowNow <- which(datLC$yday>=i-7 & datLC$yday<=i+7 )
    dat.tmp <- datLC[rowNow,] # Subsets things to a particular window (not just a single day); otherwise we were working with just 5 years of data, which isn't helpful
    # summary(dat.tmp)
    
    # Doing some graphing that we're not saving for our own sanity
    # ggplot(data=dat.tmp) + geom_violin(aes(x=as.factor(year), y=NDVI, fill=mission), scale="width")
    # ggplot(data=dat.tmp) + geom_violin(aes(x=as.factor(year), y=X30d.SPI, fill=mission), scale="width")
    # ggplot(data=dat.tmp, aes(x=X30d.SPI, y=NDVI)) + geom_point(aes(color=mission)) + stat_smooth(method="lm")
    
    #Set up a lag-only model
    modFinal <- nlme::lme(NDVI ~ X14d.SPEI + TMAX30d + NDVI.Lag14d, random=list(mission=~1), data=dat.tmp[,], na.action=na.omit)
    sumFinal <- summary(modFinal)
    modsList[[i]] <- modFinal
    mod.out$Rsq[i] <- MuMIn::r.squaredGLMM(modFinal)[2]
    mod.out$RMSE[i] <- sqrt(mean(resid(modFinal)^2))
    
    mod.out[i,c("coef.Int", "coef.SPEI14", "coef.TMAX30", "coef.Lag")] <- sumFinal$tTable[,"Value"]
    mod.out[i,c("err.Int", "err.SPEI14", "err.TMAX30", "err.Lag")] <- sumFinal$tTable[,"Std.Error"]
    mod.out[i,c("tVal.Int", "tVal.SPEI14", "tVal.TMAX30", "tVal.Lag")] <- sumFinal$tTable[,"t-value"]
    mod.out[i,c("pVal.Int", "pVal.SPEI14", "pVal.TMAX30", "pVal.Lag")] <- sumFinal$tTable[,"p-value"]
  } # End day of year loop
  summary(mod.out)
  modOutList[[LC]] <- mod.out

  write.csv(mod.out, file.path(pathSave, paste0("DailyModel_FinalModel_Stats_", LC, ".csv")), row.names=F)
  saveRDS(modsList, file.path(pathSave, paste0("DailyModel_FinalModels_", LC, ".RDS")))
  
  effectStack <- stack(mod.out[,grep("tVal", names(mod.out))])
  names(effectStack) <- c("tVal", "effect")
  effectStack$doy <- mod.out$yday
  effectStack$effect <- gsub("tVal.", "", effectStack$effect) # making clean names
  effectStack$pVal <- stack(mod.out[,grep("pVal", names(mod.out))])[,"values"]
  effectStack$coef <- stack(mod.out[,grep("coef", names(mod.out))])[,"values"]
  
  plotEffSig <- ggplot(data=effectStack[effectStack$pVal<0.05,]) +
    ggtitle(LC) +
    geom_tile(aes(x=doy, y=effect, fill=tVal)) +
    scale_fill_gradient2(low="orange2", high="green4", mid="gray80", midpoint=0) +
    theme_bw()
  
  png(file.path(path.figs, paste0("NDVI-Model_", LC, "_Effects_SigOnly.png")), height=6, width=10, units="in", res=220)
  print(plotEffSig)
  dev.off()
  
  plotEffAll <- ggplot(data=effectStack[,]) +
    ggtitle(LC) +
    geom_tile(aes(x=doy, y=effect, fill=tVal)) +
    scale_fill_gradient2(low="orange2", high="green4", mid="gray80", midpoint=0) +
    theme_bw()
  
  png(file.path(path.figs, paste0("NDVI-Model_", LC, "_Effects_All.png")), height=6, width=10, units="in", res=220)
  print(plotEffAll)
  dev.off()
  
  # # Now predicting from the models --> we need to do this separately from fitting because we want ONE prediction per obs
  # # This could be made more efficient 
  # for(DAY in unique(datLC$doy)){
  #   if(DAY == 366) next # Skip leap day
  #   rowNowBase <- which(datLC$doy==DAY)
  #   rowNow <- which(datLC$doy==DAY & !is.na(datLC$X30d.SPI) & !is.na(datLC$TMIN14d) & !is.na(datLC$NDVI.Lag14d))
  #   
  #   if(length(rowNowBase)==0) next # Skip this row if we don't have the predictors we need
  #   datLC$NDVI.predNorm[rowNowBase] <- predict(modsNorm[[DAY]], newdata=datLC[rowNowBase,])
  #   
  #   if(length(rowNow)==0) next # Skip this row if we don't have the predictors we need
  #   datLC$NDVI.pred[rowNow] <- predict(modsList[[DAY]], newdata=datLC[rowNow,])
  #   datLC$NDVI.predLag[rowNow] <- predict(modsLag[[DAY]], newdata=datLC[rowNow,])
  #   datLC$NDVI.predTemp[rowNow] <- predict(modsTemp[[DAY]], newdata=datLC[rowNow,])
  #   datLC$NDVI.predMoist[rowNow] <- predict(modsMoist[[DAY]], newdata=datLC[rowNow,])
  #   datLC$NDVI.predCombo[rowNow] <- predict(modsCombo[[DAY]], newdata=datLC[rowNow,])
  #   
  #   
  # }
  
  print("") # Just kicking the label to a new line to make things cleaner
} # End LC loop

modOutAll <- dplyr::bind_rows(modOutList)
summary(modOutAll)


effectStack <- stack(modOutAll[,grep("tVal", names(mod.out))])
names(effectStack) <- c("tVal", "effect")
effectStack[,c("doy", "landcover")] <- modOutAll[,c("yday", "landcover")]
effectStack$effect <- gsub("tVal.", "", effectStack$effect) # making clean names
effectStack$pVal <- stack(modOutAll[,grep("pVal", names(modOutAll))])[,"values"]
effectStack$coef <- stack(modOutAll[,grep("coef", names(modOutAll))])[,"values"]

# Plotting results----

# establishing seasons for plotting simplicity
# defining general seasonal windows
seasons <- data.frame(
  season = c("Winter", "Winter","Spring", "Summer", "Fall"),
  xmin = c(355,1, 80, 172, 264),   # Approx start of seasons
  xmax = c(365, 80, 172, 264, 355)   # Approx end of seasons
)

# defining rough growing season for plotting purposes; we can always change this.
grow.seas <- data.frame(
  gs.start = 91, # beginning of april
  gs.end = 304 # end of october
)

# establishing factor for significance
effectStack$sig <- ifelse(effectStack$pVal<0.05, "yes", "no")


# Remove the last row since it has NA for date_end and value_end
df_segment <- df[!is.na(df$date_end), ]

plotEffSig1 <- ggplot(data=effectStack[effectStack$pVal<0.05,]) +
  facet_wrap(~landcover) +
  geom_tile(aes(x=doy, y=effect, fill=tVal)) +
  scale_fill_gradient2(low="orange2", high="green4", mid="gray80", midpoint=0) +
  theme_bw()

png(file.path(path.figs, paste0("NDVI-Model_FinalCombined_Effects_SigOnly_wrapLC.png")), height=6, width=10, units="in", res=220)
print(plotEffSig1)
dev.off()

# testing some things with a polar projectino but haven't saved it yet.
plotEffSig1.polar <- ggplot(data=effectStack[!effectStack$effect %in% c("Int", "Lag"),]) +
  facet_wrap(~landcover) +
  # adding in gs lines
  geom_vline(data = grow.seas, aes(xintercept = gs.start), col="red3", linetype="dashed") +
  geom_vline(data = grow.seas, aes(xintercept = gs.end), col="red3", linetype="dashed") +
  geom_point(aes(x=doy, y=tVal, col=effect, pch=sig, size=sig)) +
  geom_rect(data = seasons,
            aes(xmin = xmin, xmax = xmax, ymin = max(effectStack$tVal[!effectStack$effect %in% c("Int", "Lag")]), ymax = max(effectStack$tVal[!effectStack$effect %in% c("Int", "Lag")])+2, fill = season),
            inherit.aes = FALSE, alpha = 0.2) +
  scale_color_manual(values = c("TMAX30"="orange2", "SPEI14"="dodgerblue", "Lag" = "green4")) +
  scale_fill_manual(values = c("Winter" = "#1f78b4", "Spring" = "#b2df8a", 
                                 "Summer" = "#33a02c", "Fall" = "#a6cee3")) +
  scale_shape_manual(values=c("yes" = 19, "no" = 4)) +
  scale_size_manual(values=c("yes" = 3, "no" = 0.1)) +
  theme_bw() +
  coord_polar()



plotEffAll1 <- ggplot(data=effectStack[,]) +
  facet_wrap(~landcover) +
  geom_tile(aes(x=doy, y=effect, fill=tVal)) +
  scale_fill_gradient2(low="orange2", high="green4", mid="gray80", midpoint=0) +
  theme_bw()

png(file.path(path.figs, paste0("NDVI-Model_FinalCombined_Effects_All_wrapLC.png")), height=6, width=10, units="in", res=220)
print(plotEffAll1)
dev.off()


plotEffSig2 <- ggplot(data=effectStack[effectStack$pVal<0.05,]) +
  facet_wrap(~effect) +
  geom_tile(aes(x=doy, y=landcover, fill=tVal)) +
  scale_fill_gradient2(low="orange2", high="green4", mid="gray80", midpoint=0) +
  theme_bw()

png(file.path(path.figs, paste0("NDVI-Model_FinalCombined_Effects_SigOnly_wrapVar.png")), height=6, width=10, units="in", res=220)
print(plotEffSig2)
dev.off()

plotEffAll2 <- ggplot(data=effectStack[,]) +
  facet_wrap(~effect) +
  geom_tile(aes(x=doy, y=landcover, fill=tVal)) +
  scale_fill_gradient2(low="orange2", high="green4", mid="gray80", midpoint=0) +
  theme_bw()

png(file.path(path.figs, paste0("NDVI-Model_FinalCombined_Effects_All_wrapVar.png")), height=6, width=10, units="in", res=220)
print(plotEffAll2)
dev.off()

plotEffSig3 <- ggplot(data=effectStack[effectStack$effect %in% c("SPEI14", "TMAX30") & effectStack$pVal<0.05,]) +
  facet_wrap(~effect) +
  geom_tile(aes(x=doy, y=landcover, fill=tVal)) +
  scale_fill_gradient2(low="orange2", high="green4", mid="gray80", midpoint=0) +
  theme_bw()

png(file.path(path.figs, paste0("NDVI-Model_FinalCombined_Effects_SigOnly_Met_wrapVar.png")), height=6, width=10, units="in", res=220)
print(plotEffSig3)
dev.off()
# # # # # # # # # # # # # # # # # # # # # # # # # 

# # # # # # # # # # # # # # # # # # # # # # # # # 
# Follow-up Analysis----
# Alternate way of visualizing -- partial effects (coeficient x (mean?)value -- give impact in NDVI space) --> that would get away from stat. sig. and into acctual effect space
# Partial effects will be the model coefficient multiplied by predictor value; For met partial effects, it will be the climatic norm
# # # # # # # # # # # # # # # # # # # # # # # # # 
# Reading in our met vars
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

dim(ChicagolandSPI); dim(ChicagolandSPEI); dim(ChicagolandTemp)

# Combining met data together in a single data frame
chiMet <- merge(ChicagolandTemp, ChicagolandSPI, all=T)
chiMet <- merge(chiMet, ChicagolandSPEI , all=T)
# chiMet$yday <- lubridate::yday(chiMet$date)
# chiMet$year <- lubridate::year(chiMet$date)
chiMet <- chiMet[!is.na(chiMet$date),]
summary(chiMet)
length(unique(chiMet$yday))
head(chiMet[is.na(chiMet$date),])

head(chiMet[chiMet$yday>80 & chiMet$yday<90,])

# Gettign the climatic norm to get our met partial effects
chiMetNorms <- aggregate(cbind(X14d.SPEI, TMAX30d) ~ yday, data=chiMet[chiMet$date>=as.Date("2000-01-01"),], FUN=mean, na.rm=T)
summary(chiMetNorms)

dim(chiMetNorms); dim(modOutAll)

# Once we have the met partial effects, we'll merge the climate norm data frames in to mod.out and then multiple coef.X14dSPEI by X14dSPEI to get the partial effect in NDVI units; (same thing for temp partial effects)

write.csv(modOutAll, file.path(pathSave, paste0("DailyModel_FinalModel_Stats_AllLandcovers.csv")), row.names=F)

# # # # # # # # # # # # # # # # # # # # # # # # # 

