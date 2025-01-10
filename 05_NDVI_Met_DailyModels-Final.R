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


# Most models have very similar performance metrics; lets run 3 different models
# add1: 14dSPEI + 14d TMAX (#2)
# add2: 30dSPEI + 30d TMAX (#2)
# int1: 14d SPI x 14d TMAX (#1)
# int2: 30d SPI x 14d TMAX (#2)

# When no interaction, SPEI *very* consistently outperforms SPI (but diffs. very minor); when have an interaction, timescale is a bigger factor for splitting haris


# # # # # # # # # # # # # # # # # # # # # # # # # 
# Running the models! ----
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
summary(modOutAll)


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
df_segment <- df[!is.na(df$date_end), ]

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

