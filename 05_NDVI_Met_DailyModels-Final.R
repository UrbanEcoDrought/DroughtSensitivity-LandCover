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

if(!dir.exists(path.figs)) dir.create(path.figs, recursive = T)
if(!dir.exists(pathSave)) dir.create(pathSave, recursive = T)

# Read in the two key data frames
ndviMet <- read.csv(file.path(google.drive,"data/processed_files/landsat_ndvi_metVars_combined.csv"))
ndviMet$date <- as.Date(ndviMet$date)
ndviMet$mission <- as.factor(ndviMet$mission) 
ndviMet$landcover <- as.factor(ndviMet$landcover)
summary(ndviMet)

modStatsAll <- read.csv(file.path(google.drive, "data/processed_files/ModelSelection-Multivariate", paste0("DailyModel_VarSelection-Multivariate_ModelStats-ALL.csv")))
modStatsAll$landcover <- as.factor(modStatsAll$landcover)
modStatsAll$model <- as.factor(modStatsAll$model)
modStatsAll$DroughtVar <- as.factor(modStatsAll$DroughtVar)
modStatsAll$TempVar <- as.factor(modStatsAll$TempVar)
modStatsAll$modelType <- as.factor(modStatsAll$modelType)
summary(modStatsAll)

aggLC <- aggregate(cbind(dAIC, dR2, dRMSE) ~ model + DroughtVar + TempVar + modelType + landcover, data=modStatsAll[!modStatsAll$model %in% c("modLag") & modStatsAll$yday>=90 & modStatsAll$yday<=300,], FUN=mean, na.rm=T)
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
aggXn2$rank.avg <- apply(aggXn2[,c("dR2.rank", "dRMSE.rank")], 1, mean)
aggXn2$RankComb[order(aggXn2$rank.avg, decreasing=F)] <- 1:nrow(aggXn2)
aggXn2

aggAdd <- aggregate(cbind(dAIC, dR2, dRMSE) ~ model + DroughtVar + TempVar + modelType + landcover, data=modStatsAll[!modStatsAll$model %in% c("modLag") & !modStatsAll$modelType=="interaction",], FUN=mean, na.rm=T)
aggAdd2 <- aggregate(cbind(dAIC, dR2, dRMSE) ~ model + DroughtVar + TempVar + modelType, data=aggAdd, FUN=mean, na.rm=T)
aggAdd2$dAIC.rank[order(aggAdd2$dAIC, decreasing=F)] <- 1:nrow(aggAdd2)
aggAdd2$dR2.rank[order(aggAdd2$dR2, decreasing=T)] <- 1:nrow(aggAdd2)
aggAdd2$dRMSE.rank[order(aggAdd2$dRMSE, decreasing=F)] <- 1:nrow(aggAdd2)
aggAdd2$rank.avg <- apply(aggAdd2[,c("dR2.rank", "dRMSE.rank")], 1, mean)
aggAdd2$RankComb[order(aggAdd2$rank.avg, decreasing=F)] <- 1:nrow(aggAdd2)
aggAdd2

aggAdd2[grep("SPI", aggAdd2$DroughtVar),]
aggAdd2[grep("SPEI", aggAdd2$DroughtVar),]
mean(aggAdd2$RankComb[grep("SPI", aggAdd2$DroughtVar)])
mean(aggAdd2$RankComb[grep("SPEI", aggAdd2$DroughtVar)])



aggXn2[grep("SPI", aggXn2$DroughtVar),]
aggXn2[grep("SPEI", aggXn2$DroughtVar),]
mean(aggXn2$RankComb[grep("SPI", aggXn2$DroughtVar)])
mean(aggXn2$RankComb[grep("SPEI", aggXn2$DroughtVar)])


# Most models have very similar performance metrics; lets run 4 different models
# add1: 14dSPEI + 14d TMAX (#2)
# add2: 30dSPEI + 30d TMAX (#2)
# int1: 14d SPI x 14d TMAX (#1)
# int2: 30d SPI x 14d TMAX (#2)

# When no interaction, SPEI *very* consistently outperforms SPI (but diffs. very minor); when have an interaction, timescale is a bigger factor for splitting haris


# # # # # # # # # # # # # # # # # # # # # # # # # 
# Running the models! ----
# previously: picked 2 sets of variables for further investigation (e.g. aggXn2)
# Purpose: How much of the power is actual sensitivity to variable selection as opposed to noise and parameter trade off?
# # # # # # # # # # # # # # # # # # # # # # # # # 
LCtypes <- unique(ndviMet$landcover)
modOutListAdd1 <- list()
modOutListAdd2 <- list()
modOutListInt1 <- list()
modOutListInt2 <- list()
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
  
  modsListAdd1 <- list()
  modsListAdd2 <- list()
  modsListInt1 <- list()
  modsListInt2 <- list()
  
  # add1: 14dSPEI + 14d TMAX (#2)
  # add2: 30dSPEI + 30d TMAX (#2)
  # int1: 14d SPI x 14d TMAX (#1)
  # int2: 30d SPI x 14d TMAX (#2)
  
  mod.outAdd1 <- data.frame(landcover=LC, yday=1:365, DroughtVar="X14dSPEI", TempVar="TMAX14d", Rsq=NA, RMSE=NA, 
                            coef.Int=NA, coef.Lag=NA, coef.Drought=NA, coef.Temp=NA, 
                            err.Int=NA , err.Lag=NA , err.Drought=NA , err.Temp=NA , 
                            tVal.Int=NA, tVal.Lag=NA, tVal.Drought=NA, tVal.Temp=NA, 
                            pVal.Int=NA, pVal.Lag=NA, pVal.Drought=NA, pVal.Temp=NA) 
  mod.outAdd2 <- data.frame(landcover=LC, yday=1:365, DroughtVar="X30dSPEI", TempVar="TMA30d",  Rsq=NA, RMSE=NA, 
                            coef.Int=NA, coef.Lag=NA, coef.Drought=NA, coef.Temp=NA, 
                            err.Int=NA , err.Lag=NA , err.Drought=NA , err.Temp=NA , 
                            tVal.Int=NA, tVal.Lag=NA, tVal.Drought=NA, tVal.Temp=NA, 
                            pVal.Int=NA, pVal.Lag=NA, pVal.Drought=NA, pVal.Temp=NA) 
  mod.outInt1 <- data.frame(landcover=LC, yday=1:365, DroughtVar="X14dSPI", TempVar="MAX14d", Rsq=NA, RMSE=NA, 
                            coef.Int=NA, coef.Lag=NA, coef.Drought=NA, coef.Temp=NA, coef.TxD=NA, coef.LagxD=NA, coef.LagxT=NA, coef.DxTxLag=NA, 
                            err.Int=NA , err.Lag=NA , err.Drought=NA , err.Temp=NA , err.TxD=NA , err.LagxD=NA , err.LagxT=NA , err.DxTxLag=NA , 
                            tVal.Int=NA, tVal.Lag=NA, tVal.Drought=NA, tVal.Temp=NA, tVal.TxD=NA, tVal.LagxD=NA, tVal.LagxT=NA, tVal.DxTxLag=NA, 
                            pVal.Int=NA, pVal.Lag=NA, pVal.Drought=NA, pVal.Temp=NA, pVal.TxD=NA, pVal.LagxD=NA, pVal.LagxT=NA, pVal.DxTxLag=NA) 

  mod.outInt2 <- data.frame(landcover=LC, yday=1:365, DroughtVar="X30dSPI", TempVar="TMAX",  Rsq=NA, RMSE=NA, 
                            coef.Int=NA, coef.Lag=NA, coef.Drought=NA, coef.Temp=NA, coef.TxD=NA, coef.LagxD=NA, coef.LagxT=NA, coef.DxTxLag=NA, 
                            err.Int=NA , err.Lag=NA , err.Drought=NA , err.Temp=NA , err.TxD=NA , err.LagxD=NA , err.LagxT=NA , err.DxTxLag=NA , 
                            tVal.Int=NA, tVal.Lag=NA, tVal.Drought=NA, tVal.Temp=NA, tVal.TxD=NA, tVal.LagxD=NA, tVal.LagxT=NA, tVal.DxTxLag=NA, 
                            pVal.Int=NA, pVal.Lag=NA, pVal.Drought=NA, pVal.Temp=NA, pVal.TxD=NA, pVal.LagxD=NA, pVal.LagxT=NA, pVal.DxTxLag=NA) 
  
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
    
    # Trying 4 different models to compare
    # add1: 14dSPEI + 14d TMAX (#2)
    # add2: 30dSPEI + 30d TMAX (#2)
    # int1: 14d SPI x 14d TMAX (#1)
    # int2: 30d SPI x 14d TMAX (#2)
    
    
    #Set up a lag-only model
    modAdd1 <- nlme::lme(NDVI ~ X14d.SPEI + TMAX14d + NDVI.Lag14d, random=list(mission=~1), data=dat.tmp[,], na.action=na.omit)
    sumAdd1 <- summary(modAdd1)
    modsListAdd1[[i]] <- modAdd1
    mod.outAdd1$Rsq[i] <- MuMIn::r.squaredGLMM(modAdd1)[2]
    mod.outAdd1$RMSE[i] <- sqrt(mean(resid(modAdd1)^2))
    
    mod.outAdd1[i,c("coef.Int", "coef.Drought", "coef.Temp", "coef.Lag")] <- sumAdd1$tTable[,"Value"]
    mod.outAdd1[i,c("err.Int", "err.Drought", "err.Temp", "err.Lag")] <- sumAdd1$tTable[,"Std.Error"]
    mod.outAdd1[i,c("tVal.Int", "tVal.Drought", "tVal.Temp", "tVal.Lag")] <- sumAdd1$tTable[,"t-value"]
    mod.outAdd1[i,c("pVal.Int", "pVal.Drought", "pVal.Temp", "pVal.Lag")] <- sumAdd1$tTable[,"p-value"]
    
    modAdd2 <- nlme::lme(NDVI ~ X30d.SPEI + TMAX30d + NDVI.Lag14d, random=list(mission=~1), data=dat.tmp[,], na.action=na.omit)
    sumAdd2 <- summary(modAdd2)
    modsListAdd2[[i]] <- modAdd2
    mod.outAdd2$Rsq[i] <- MuMIn::r.squaredGLMM(modAdd2)[2]
    mod.outAdd2$RMSE[i] <- sqrt(mean(resid(modAdd2)^2))
    
    mod.outAdd2[i,c("coef.Int", "coef.Drought", "coef.Temp", "coef.Lag")] <- sumAdd2$tTable[,"Value"]
    mod.outAdd2[i,c("err.Int", "err.Drought", "err.Temp", "err.Lag")] <- sumAdd2$tTable[,"Std.Error"]
    mod.outAdd2[i,c("tVal.Int", "tVal.Drought", "tVal.Temp", "tVal.Lag")] <- sumAdd2$tTable[,"t-value"]
    mod.outAdd2[i,c("pVal.Int", "pVal.Drought", "pVal.Temp", "pVal.Lag")] <- sumAdd2$tTable[,"p-value"]

    modInt1 <- nlme::lme(NDVI ~ X14d.SPI*TMAX14d*NDVI.Lag14d, random=list(mission=~1), data=dat.tmp[,], na.action=na.omit)
    sumInt1 <- summary(modInt1)
    modsListInt1[[i]] <- modInt1
    mod.outInt1$Rsq[i] <- MuMIn::r.squaredGLMM(modInt1)[2]
    mod.outInt1$RMSE[i] <- sqrt(mean(resid(modInt1)^2))
    
    mod.outInt1[i,c("coef.Int", "coef.Drought", "coef.Temp", "coef.Lag", "coef.TxD", "coef.LagxD", "coef.LagxT", "coef.DxTxLag")] <- sumInt1$tTable[,"Value"]
    mod.outInt1[i,c("err.Int" , "err.Drought" , "err.Temp" , "err.Lag" , "err.TxD" , "err.LagxD" , "err.LagxT" , "err.DxTxLag" )] <- sumInt1$tTable[,"Std.Error"]
    mod.outInt1[i,c("tVal.Int", "tVal.Drought", "tVal.Temp", "tVal.Lag", "tVal.TxD", "tVal.LagxD", "tVal.LagxT", "tVal.DxTxLag")] <- sumInt1$tTable[,"t-value"]
    mod.outInt1[i,c("pVal.Int", "pVal.Drought", "pVal.Temp", "pVal.Lag", "pVal.TxD", "pVal.LagxD", "pVal.LagxT", "pVal.DxTxLag")] <- sumInt1$tTable[,"p-value"]
    

    modInt2 <- nlme::lme(NDVI ~ X30d.SPI*TMAX14d*NDVI.Lag14d, random=list(mission=~1), data=dat.tmp[,], na.action=na.omit)
    sumInt2 <- summary(modInt2)
    modsListInt2[[i]] <- modInt2
    mod.outInt2$Rsq[i] <- MuMIn::r.squaredGLMM(modInt2)[2]
    mod.outInt2$RMSE[i] <- sqrt(mean(resid(modInt2)^2))
    
    mod.outInt2[i,c("coef.Int", "coef.Drought", "coef.Temp", "coef.Lag", "coef.TxD", "coef.LagxD", "coef.LagxT", "coef.DxTxLag")] <- sumInt2$tTable[,"Value"]
    mod.outInt2[i,c("err.Int" , "err.Drought" , "err.Temp" , "err.Lag" , "err.TxD" , "err.LagxD" , "err.LagxT" , "err.DxTxLag" )] <- sumInt2$tTable[,"Std.Error"]
    mod.outInt2[i,c("tVal.Int", "tVal.Drought", "tVal.Temp", "tVal.Lag", "tVal.TxD", "tVal.LagxD", "tVal.LagxT", "tVal.DxTxLag")] <- sumInt2$tTable[,"t-value"]
    mod.outInt2[i,c("pVal.Int", "pVal.Drought", "pVal.Temp", "pVal.Lag", "pVal.TxD", "pVal.LagxD", "pVal.LagxT", "pVal.DxTxLag")] <- sumInt2$tTable[,"p-value"]
    
  } # End day of year loop
  # summary(mod.out)
  modOutListAdd1[[LC]] <- mod.outAdd1
  modOutListAdd2[[LC]] <- mod.outAdd2
  modOutListInt1[[LC]] <- mod.outInt1
  modOutListInt2[[LC]] <- mod.outInt2
  
  write.csv(mod.outAdd1, file.path(pathSave, paste0("DailyModel_FinalModel_Stats_Additive_SPEI14-TMAX14_", LC, ".csv")), row.names=F)
  saveRDS(modsListAdd1, file.path(pathSave, paste0("DailyModel_FinalModels_Additive_SPEI14-TMAX14_", LC, ".RDS")))
  
  write.csv(mod.outAdd2, file.path(pathSave, paste0("DailyModel_FinalModel_Stats_Additive_SPEI30-TMAX30_", LC, ".csv")), row.names=F)
  saveRDS(modsListAdd2, file.path(pathSave, paste0("DailyModel_FinalModels_Additive_SPEI30-TMAX30_", LC, ".RDS")))
  
  write.csv(mod.outInt1, file.path(pathSave, paste0("DailyModel_FinalModel_Stats_Interactive_SPI14-TMAX14_", LC, ".csv")), row.names=F)
  saveRDS(modsListInt1, file.path(pathSave, paste0("DailyModel_FinalModels_Interactive_SPI14-TMAX14_", LC, ".RDS")))
  
  write.csv(mod.outInt2, file.path(pathSave, paste0("DailyModel_FinalModel_Stats_Interactive_SPI30-TMAX14_", LC, ".csv")), row.names=F)
  saveRDS(modsListInt2, file.path(pathSave, paste0("DailyModel_FinalModels_Interactive_SPI30-TMAX14_", LC, ".RDS")))
  
  
  effectStackAdd1 <- stack(mod.outAdd1[,grep("tVal", names(mod.outAdd1))])
  names(effectStackAdd1) <- c("tVal", "effect")
  effectStackAdd1$model <- c("SPEI14-TMAX14")
  effectStackAdd1$doy <- mod.outAdd1$yday
  effectStackAdd1$effect <- gsub("tVal.", "", effectStackAdd1$effect) # making clean names
  effectStackAdd1$effect <- factor(effectStackAdd1$effect, rev( c("Drought", "Temp", "Lag", "Int")))
  effectStackAdd1$pVal <- stack(mod.outAdd1[,grep("pVal", names(mod.outAdd1))])[,"values"]
  effectStackAdd1$coef <- stack(mod.outAdd1[,grep("coef", names(mod.outAdd1))])[,"values"]
  
  effectStackAdd2 <- stack(mod.outAdd2[,grep("tVal", names(mod.outAdd2))])
  names(effectStackAdd2) <- c("tVal", "effect")
  effectStackAdd2$model <- c("SPEI30-TMAX30")
  effectStackAdd2$doy <- mod.outAdd2$yday
  effectStackAdd2$effect <- gsub("tVal.", "", effectStackAdd2$effect) # making clean names
  effectStackAdd2$effect <- factor(effectStackAdd2$effect, rev( c("Drought", "Temp", "Lag", "Int")))
  effectStackAdd2$pVal <- stack(mod.outAdd2[,grep("pVal", names(mod.outAdd2))])[,"values"]
  effectStackAdd2$coef <- stack(mod.outAdd2[,grep("coef", names(mod.outAdd2))])[,"values"]
  

  effectStackInt1 <- stack(mod.outInt1[,grep("tVal", names(mod.outInt1))])
  names(effectStackInt1) <- c("tVal", "effect")
  effectStackInt1$model <- c("SPI14-TMAX14")
  effectStackInt1$doy <- mod.outInt1$yday
  effectStackInt1$effect <- gsub("tVal.", "", effectStackInt1$effect) # making clean names
  effectStackInt1$effect <- factor(effectStackInt1$effect, rev( c("Int","Lag", "Drought", "Temp", "TxD", "LagxD", "LagxT", "DxTxLag")))
  effectStackInt1$pVal <- stack(mod.outInt1[,grep("pVal", names(mod.outInt1))])[,"values"]
  effectStackInt1$coef <- stack(mod.outInt1[,grep("coef", names(mod.outInt1))])[,"values"]
  
  # ggplot(data=effectStackInt1[effectStackInt1$pVal<0.05 & !is.na(effectStackInt1$tVal),]) +
  #   ggtitle(LC) +
  #   # facet_wrap(~model, scales="free_y") +
  #   geom_tile(aes(x=doy, y=effect, fill=tVal)) +
  #   scale_fill_gradient2(low="orange2", high="green4", mid="gray80", midpoint=0) +
  #   theme_bw()
  
  effectStackInt2 <- stack(mod.outInt2[,grep("tVal", names(mod.outInt2))])
  names(effectStackInt2) <- c("tVal", "effect")
  effectStackInt2$model <- c("SPI30-TMAX14")
  effectStackInt2$doy <- mod.outInt2$yday
  effectStackInt2$effect <- gsub("tVal.", "", effectStackInt2$effect) # making clean names
  effectStackInt1$effect <- factor(effectStackInt2$effect, rev( c("Int","Lag", "Drought", "Temp", "TxD", "LagxD", "LagxT", "DxTxLag")))
  effectStackInt2$pVal <- stack(mod.outInt2[,grep("pVal", names(mod.outInt2))])[,"values"]
  effectStackInt2$coef <- stack(mod.outInt2[,grep("coef", names(mod.outInt2))])[,"values"]
  
  effectStack <- rbind(effectStackAdd1, effectStackAdd2, effectStackInt1, effectStackInt2)
  summary(effectStack)
  
  plotEffSig <- ggplot(data=effectStack[effectStack$pVal<0.05,]) +
    ggtitle(LC) +
    facet_wrap(~model, scales="free_y") +
    geom_tile(aes(x=doy, y=effect, fill=tVal)) +
    scale_fill_gradient2(low="orange2", high="green4", mid="gray80", midpoint=0) +
    theme_bw()
  
  png(file.path(path.figs, paste0("NDVI-Model_", LC, "_Effects_SigOnly.png")), height=6, width=10, units="in", res=220)
  print(plotEffSig)
  dev.off()
  
  plotEffAll <- ggplot(data=effectStack[,]) +
    ggtitle(LC) +
    facet_wrap(~model, scales="free_y") +
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

modOutAllAdd1 <- dplyr::bind_rows(modOutListAdd1)
modOutAllAdd2 <- dplyr::bind_rows(modOutListAdd2)
modOutAllInt1 <- dplyr::bind_rows(modOutListInt1)
modOutAllInt2 <- dplyr::bind_rows(modOutListInt2)


# want to use modOutAllAdd1 for now (03/06/2025)
# It is the best performing of the additive models
# eases interpretation
# no major performance boost from an interactive model.

effectStack <- stack(modOutAllAdd1[,grep("tVal", names(modOutAllAdd1))])
names(effectStack) <- c("tVal", "effect")
effectStack[,c("doy", "landcover", "DroughtVar", "TempVar")] <- modOutAllAdd1[,c("yday", "landcover", "DroughtVar", "TempVar")]
effectStack$model <- c("SPEI14-TMAX14")
effectStack$effect <- gsub("tVal.", "", effectStack$effect) # making clean names
effectStack$effect <- factor(effectStack$effect, rev( c("Drought", "Temp", "Lag", "Int")))
effectStack$pVal <- stack(modOutAllAdd1[,grep("pVal", names(modOutAllAdd1))])[,"values"]
effectStack$coef <- stack(modOutAllAdd1[,grep("coef", names(modOutAllAdd1))])[,"values"]

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
# df_segment <- df[!is.na(df$date_end), ]

modName <- unique(effectStack$model)
plotEffSig1 <- ggplot(data=effectStack[effectStack$pVal<0.05,]) +
  ggtitle(modName) +
  facet_wrap(~landcover) +
  geom_tile(aes(x=doy, y=effect, fill=tVal)) +
  scale_fill_gradient2(low="orange2", high="green4", mid="gray80", midpoint=0) +
  theme_bw()

png(file.path(path.figs, paste0("NDVI-Model_FinalCombined_Effects_BestAdditive_SigOnly_wrapLC.png")), height=6, width=10, units="in", res=220)
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
  ggtitle(modName) +
  facet_wrap(~landcover) +
  geom_tile(aes(x=doy, y=effect, fill=tVal)) +
  scale_fill_gradient2(low="orange2", high="green4", mid="gray80", midpoint=0) +
  theme_bw()

png(file.path(path.figs, paste0("NDVI-Model_FinalCombined_Effects_All_wrapLC.png")), height=6, width=10, units="in", res=220)
print(plotEffAll1)
dev.off()


plotEffSig2 <- ggplot(data=effectStack[effectStack$pVal<0.05,]) +
  ggtitle(modName) +
  facet_wrap(~effect) +
  geom_tile(aes(x=doy, y=landcover, fill=tVal)) +
  scale_fill_gradient2(low="orange2", high="green4", mid="gray80", midpoint=0) +
  theme_bw()

png(file.path(path.figs, paste0("NDVI-Model_FinalCombined_Effects_SigOnly_wrapVar.png")), height=6, width=10, units="in", res=220)
print(plotEffSig2)
dev.off()

plotEffAll2 <- ggplot(data=effectStack[,]) +
  ggtitle(modName) +
  facet_wrap(~effect) +
  geom_tile(aes(x=doy, y=landcover, fill=tVal)) +
  scale_fill_gradient2(low="orange2", high="green4", mid="gray80", midpoint=0) +
  theme_bw()

png(file.path(path.figs, paste0("NDVI-Model_FinalCombined_Effects_All_wrapVar.png")), height=6, width=10, units="in", res=220)
print(plotEffAll2)
dev.off()

plotEffSig3 <- ggplot(data=effectStack[effectStack$effect %in% c("Drought", "Temp") & effectStack$pVal<0.05,]) +
  ggtitle(modName) +
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
ChicagolandSPEI <- read.csv(file.path(google.drive, "../data/GRIDMET_data/gridmet_aggregated_data/Chicagoland_Daily_Aggregated_SPEI_1991_2024.csv"))
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
# want to hang on to chi met so that we can use it to calculate the yearly partial effects later on.

# Gettign the climatic norm to get our met partial effects
# calculating normals just over the period that we have Satellite data
chiMetNorms <- aggregate(cbind(SPEI14, TMAX14d) ~ yday, data=chiMet[chiMet$date>=as.Date("2000-01-01"),], FUN=mean, na.rm=T)
summary(chiMetNorms)

dim(chiMetNorms); dim(modOutAll)

# # # # # # # # # # # # # # # # # # # # # # # # # 
# Partial Effects----
# generating code to calculate the partial effects throughout the different DOY for the different land cover models.
# need to bring in the mean conditions for DOY for temp, SPEI, and for NDVI
library(dplyr)
library(zoo)

# loading the model data frames so that we can merge in the met data and create partial effects.
# loading the SPEI14 and TMAX14 models as a place to start. Need to check wiht @crollinson to make sure this is the correct model form. 
crop.add1 <- read.csv(file.path(pathSave, paste0("DailyModel_FinalModel_Stats_Additive_SPEI14-TMAX14_crop.csv")))
forest.add1 <- read.csv(file.path(pathSave, paste0("DailyModel_FinalModel_Stats_Additive_SPEI14-TMAX14_forest.csv")))
grassland.add1 <- read.csv(file.path(pathSave, paste0("DailyModel_FinalModel_Stats_Additive_SPEI14-TMAX14_grassland.csv")))
urbanHigh.add1 <- read.csv(file.path(pathSave, paste0("DailyModel_FinalModel_Stats_Additive_SPEI14-TMAX14_urban-high.csv")))
urbanLow.add1 <- read.csv(file.path(pathSave, paste0("DailyModel_FinalModel_Stats_Additive_SPEI14-TMAX14_urban-low.csv")))
urbanOpen.add1 <- read.csv(file.path(pathSave, paste0("DailyModel_FinalModel_Stats_Additive_SPEI14-TMAX14_urban-open.csv")))
urbanMedium.add1 <- read.csv(file.path(pathSave, paste0("DailyModel_FinalModel_Stats_Additive_SPEI14-TMAX14_urban-medium.csv")))

modOutAll <- rbind(crop.add1, forest.add1, grassland.add1, urbanHigh.add1, urbanLow.add1, urbanOpen.add1, urbanMedium.add1)
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
spei.dailyNorm<- aggregate(X14d.SPEI ~ yday+landcover, data= datLC2, FUN=mean, na.rm=T) # landcover shouldn't make a difference for the metVars, but will for the lagged time series.
tmax.dailyNorm <- aggregate(TMAX14d ~ yday+landcover, data= datLC2, FUN=mean, na.rm=T) # landcover shouldn't make a difference for the metVars, but will for the lagged time series.
ndvi.dailyNorm <- aggregate(NDVI ~ yday+landcover, data= datLC2, FUN=mean, na.rm=T) # landcover shouldn't make a difference for the metVars, but will for the lagged time series.
# merging daily mean data  together
# start with metVars

dailyMeans <- merge(spei.dailyNorm, tmax.dailyNorm, by=c("yday", "landcover"), all=T)
dailyMeans2 <- merge(dailyMeans, lag.dailyNorm, by=c("yday", "landcover"), all=T)
dailyMeans3 <- merge(dailyMeans2, ndvi.dailyNorm, by=c("yday", "landcover"), all=T)
summary(dailyMeans3)
names(dailyMeans3) <- c("yday", "landcover", "spei14d.norm", "tmax14d.norm", "ndviLag.norm", "NDVI.norm")


# merging in these daily means with the model output data
summary(modOutAll) # data frame with coefficients.

modOutAll2 <- merge(modOutAll, dailyMeans3, by=c("yday", "landcover"))
summary(modOutAll2)

# calculating partial effects----

# calculating mean Climate partial effects
# partial effect = coefficient * daily mean Var

modOutAll2$partial.Drought.climateNorm <- modOutAll2$coef.Drought* modOutAll2$spei14d.norm
modOutAll2$partial.Temp.climateNorm<- modOutAll2$coef.Temp * modOutAll2$tmax14d.norm
modOutAll2$partial.Lag.climateNorm <- modOutAll2$coef.Lag * modOutAll2$ndviLag.norm

summary(modOutAll2)

# saving data frame
write.csv(modOutAll2, file.path(pathSave, paste0("DailyModel_FinalModel_modOutAdd1_Stats_climateNormPartialEffects_AllLandcovers.csv")), row.names=F)

####################
# Calculating Partial Effects for individual years----

summary(ndviMet)
summary(chiMet)
summary(modOutAll2) # wanting this data frame for just the coefficients
head(modOutAll2)

ggplot(data = chiMet[chiMet$date >="2000-01-01",], aes(x=date, y = 1, fill=TMAX14d)) +
  geom_tile()

ggplot(data = chiMet[chiMet$date >="2009-01-01" & chiMet$date<="2013-01-01",], aes(x=date, y = TMAX14d)) +
  geom_line()
ggplot(data = chiMet[chiMet$date >="2000-01-01",], aes(x=date, y = 1, fill=SPEI14)) +
  geom_tile()
ggplot(data = ndviMet[ndviMet$date >="2000-01-01",], aes(x=date, y=X14d.SPEI)) +
  geom_line()

# need to calculate partial effects for individual years.
# pull out the daily coefficients

drought.coeff <- modOutAll2[,names(modOutAll2) %in% c("yday", "landcover", "DroughtVar", "coef.Drought")]
temp.coeff <- modOutAll2[,names(modOutAll2) %in% c("yday", "landcover", "TempVar", "coef.Temp")]
lag.coeff <- modOutAll2[,names(modOutAll2) %in% c("yday", "landcover", "coef.Lag")]

# building a dataframe to house the date-driven partial effects
pe.date.df <- data.frame(date = chiMet$date,
                         SPEI.14d = chiMet$SPEI14,
                         TMAX.14d = chiMet$TMAX14d)
# merging in the NDVI lag. This will give us replicatino across the land covers
summary(datLC)

pe.date.df2 <- merge(datLC, pe.date.df, by="date", all.x=T)
summary(pe.date.df2)

# creating a yday variable to merge in the coefficients
pe.date.df2$yday <- yday(pe.date.df2$date)
summary(pe.date.df2)

# merging in coefficients
pe.date.df3 <- merge(pe.date.df2, modOutAll2[, c("yday", "landcover", "coef.Drought", "coef.Temp", "coef.Lag", "coef.Int")], 
                     by=c("landcover", "yday"))
summary(pe.date.df3)

# calculating partial effects
pe.date.df3$partial.Drought.date <- pe.date.df3$SPEI.14d*pe.date.df3$coef.Drought
pe.date.df3$partial.Temp.date <- pe.date.df3$TMAX.14d*pe.date.df3$coef.Temp
pe.date.df3$partial.Lag.date <- pe.date.df3$NDVI.Lag14d*pe.date.df3$coef.Lag

# Calculating residual for partial effect standardization process
# NDVI - intercept - partialEffect.Lag = Residual
pe.date.df3$resid <- pe.date.df3$NDVI - pe.date.df3$coef.Int - pe.date.df3$partial.Lag.date
head(pe.date.df3)
summary(pe.date.df3)

ggplot(data=pe.date.df3) +
  geom_vline(xintercept=0, linetype="dashed") +
  geom_density(aes(x=resid))

ggplot(data=pe.date.df3) +
  geom_vline(xintercept=0, linetype="dashed") +
  geom_density(aes(x=partial.Drought.date), col="orange2")+
  geom_density(aes(x=partial.Temp.date), col="navy")+
  geom_density(aes(x=partial.Lag.date), col="black")


# creating standardized variables for plotting later
pe.date.df3$peTempStd <- pe.date.df3$partial.Temp.date/pe.date.df3$NDVI
pe.date.df3$peDroughtStd <- pe.date.df3$partial.Drought.date/pe.date.df3$NDVI

# saving data frame
write.csv(pe.date.df3, file.path(pathSave, paste0("DailyModel_FinalModel_modOutAdd1_Stats_dailyPartialEffects_AllLandcovers.csv")), row.names=F)


agg.test <- aggregate(partial.Drought.date~yday+landcover, data=pe.date.df3, FUN=mean)
agg.test2 <- merge(agg.test, modOutAll2[,names(modOutAll2) %in% c("yday","landcover", "partial.Drought.climateNorm")], by=c("landcover", "yday"))
head(agg.test2)

agg.test2$partial.diff <- agg.test2$partial.Drought.climateNorm-agg.test2$partial.Drought.date
summary(agg.test2)
ggplot(agg.test2) + facet_grid(landcover~.) +
  geom_line(aes(x=yday, y=partial.Drought.date), col="blue") +
  geom_line(aes(x=yday, y=partial.Drought.climateNorm), col="forestgreen")

ggplot(agg.test2) + facet_grid(landcover~.) +
  geom_hline(yintercept=0, linetype="dashed") +
  geom_line(aes(x=yday, y=partial.diff), col="blue") 

3.266e-02/0.8815 # checking the max difference in teh partial effects agains the max value for NDVI to get a picture of the percent of NDVI we're talkign about with the different ways of calculating the partial effects
