# Creating a clean script to re-do the daily correlation modeling and do some prediction from it
library(ggplot2)
library(lubridate)
library(ggcorrplot)

# Setting the file paths. This may be different for your computer.
# Sys.setenv(GOOGLE_DRIVE = "G:/Shared drives/Urban Ecological Drought/Manuscript - Urban Drought NDVI Daily Corrs/")
Sys.setenv(GOOGLE_DRIVE = "~/Google Drive/Shared drives/Urban Ecological Drought/Manuscript - Urban Drought NDVI/")
google.drive <- Sys.getenv("GOOGLE_DRIVE")

path.NDVI <- file.path("G:/Shared drives/Urban Ecological Drought/", "data", "UrbanEcoDrought_NDVI_LocalExtract")
path.figs <- file.path(google.drive, "exploratory figures/ModelSelection-Multivariate")
pathSave <- file.path(google.drive, "data/processed_files/ModelSelection-Multivariate")

if(!dir.exists(path.figs)) dir.create(path.figs, recursive = T)
if(!dir.exists(pathSave)) dir.create(pathSave, recursive = T)

# Read in the ndviMet data file from script 3
ndviMet <- read.csv(file.path(google.drive, "data/processed_files/landsat_ndvi_metVars_combined.csv"), header=T)
ndviMet$date <- as.Date(ndviMet$date)
ndviMet$mission <- as.factor(ndviMet$mission) 
ndviMet$landcover <- as.factor(ndviMet$landcover)
summary(ndviMet)

modStatsAll <- read.csv(file.path(google.drive, "data/processed_files/ModelSelection-Univariate", paste0("DailyModel_VarSelection-Univariate_ModelStats-ALL.csv")))
modStatsAll$landcover <- as.factor(modStatsAll$landcover)
modStatsAll$model <- as.factor(modStatsAll$model)
summary(modStatsAll)


# Based on the corr plots, temperature variables are fairly highly correlated with each other as are SPI/SPEI.
# 90 temp vars correlate less with shorter-term
# Checking the stats for each var to figure out where to start
aggLC <- aggregate(cbind(dAIC, dR2, dRMSE, dRMSEper) ~ model + landcover, data=modStatsAll[!modStatsAll$model %in% c("modIntOnly", "modLag") & modStatsAll$yday>=90 & modStatsAll$yday<=300,], FUN=mean)
aggYDAY <- aggregate(cbind(dAIC, dR2, dRMSE, dRMSEper) ~ model + yday, data=modStatsAll[!modStatsAll$model %in% c("modIntOnly", "modLag") & modStatsAll$yday>=90 & modStatsAll$yday<=300,], FUN=mean)


aggLC2 <- aggregate(cbind(dAIC, dR2, dRMSE, dRMSEper) ~ model, data=aggLC, FUN=mean)
aggYDAY2 <- aggregate(cbind(dAIC, dR2, dRMSE, dRMSEper) ~ model, data=aggYDAY, FUN=mean)

aggLC2$dR2.rank[order(aggLC2$dR2, decreasing=T)] <- 1:nrow(aggLC2)
aggLC2$dRMSE.rank[order(aggLC2$dRMSE, decreasing=F)] <- 1:nrow(aggLC2)
aggLC2

aggYDAY2$dR2.rank[order(aggYDAY2$dR2, decreasing=T)] <- 1:nrow(aggYDAY2)
aggYDAY2$dRMSE.rank[order(aggYDAY2$dRMSE, decreasing=F)] <- 1:nrow(aggYDAY2)
aggYDAY2

# Looks like both orders of aggregating give you similar, but not the same answers.
aggLC2$rank.avg <- apply(aggLC2[,c("dR2.rank", "dRMSE.rank")], 1, mean)
aggLC2$RankComb[order(aggLC2$rank.avg, decreasing=F)] <- 1:nrow(aggLC2)
aggLC2

mean(aggLC2$RankComb[grep("Tmax", aggLC2$model)])
mean(aggLC2$RankComb[grep("Tmin", aggLC2$model)])
mean(aggLC2$RankComb[grep("SPEI", aggLC2$model)])
mean(aggLC2$RankComb[grep("SPI", aggLC2$model)])

# Looking across variables and temporal scales, SPEI and SPI are closer with timescale having a stronger "single"; TMAX & TMIN behave similarly
# 30-day SPEI is the best in both R2 & RMSE with 30-day SPI in a close second; 14 days, so lets definitely start with that in our model and combine it with 30-day Tmin & Tmax; TMAX, which is the highest temp predictor; we'll test by itself as well as interactions


LCtypes <- unique(ndviMet$landcover)
varsDrought <- c("SPEI14", "SPEI30", "SPI30day", "SPI60day")
varsTemp <- c("Tmax_14day", "Tmax_30day", "Tmax_60day", "Tmin_60day")
modType <- c("additive", "interaction")

# Creating a dataframe with all permutations and giving it names
dfModComb <- data.frame(varDrought = rep(varsDrought, each=length(varsTemp)*length(modType)),
                        varsTemp = rep(rep(varsTemp, each=length(modType)), times=length(varsDrought)),
                        modType=modType)
dfModComb$modName <- apply(dfModComb, 1, FUN=function(x){paste(x, collapse="-")})

modNames <- dfModComb$modName
length(modNames); length(unique(modNames))

listAIC <- listError <- listRMSE <- listR2 <- list()
listAICd <- listError <- listRMSEd <- listR2d <- list()

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
  
  modsOut <- list()
  modsOut$modLag <- list()
  for(VAR in dfModComb$modName){
    modsOut[[VAR]] <- list()
  }
  
  # Right now we only care about the model sumary stats
  # For R2, we'll use the marginal R2 --> the part described by the fixed effects
  modOutAIC <- modOutError <-  modOutR2 <- modOutRMSE <- data.frame(landcover=LC, yday=1:365)
  modOutAIC[, c("modLag", modNames)] <- modOutError[, c("modLag", modNames)] <- modOutR2[, c("modLag", modNames)] <- modOutRMSE[, c("modLag", modNames)] <- NA
  # mod.out <- data.frame(landcover=LC, yday=1:365, intercept=NA, coef.Lag=NA, coef.SPEI30=NA, coef
  
  
  # row.ind = 0 # Setting up an index that will tell us what row to save things in; we should start with 0 because we haven't done anything yet
  pb <- txtProgressBar(min=0, max=nrow(modOutAIC), style=3)
  for(i in 1:nrow(modOutAIC)){
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
    modL <- nlme::lme(NDVI ~ NDVI.Lag14d, random=list(mission=~1), data=dat.tmp[,], na.action=na.omit)
    modsOut$modLag[[i]] <- modL
    modOutAIC$modLag[i] <- AIC(modL)
    modOutR2$modLag[i] <- MuMIn::r.squaredGLMM(modL)[2]
    modOutRMSE$modLag[i] <- sqrt(mean(resid(modL)^2))
    modOutError$modLag[i] <- mean(resid(modL))
    
    for(VAR1 in varsDrought){
      for(VAR2 in varsTemp){
        # print(VAR)
        dat.tmp$VAR1 <- dat.tmp[,VAR1]
        dat.tmp$VAR2 <- dat.tmp[,VAR2]
        
        modAdd <-  nlme::lme(NDVI ~ VAR1 + VAR2 + NDVI.Lag14d, random=list(mission=~1), data=dat.tmp[,], na.action=na.omit)
        summary(modAdd)
        modOutAIC[i, paste(VAR1, VAR2, "additive", sep="-")] <- AIC(modAdd)
        modOutR2[i,  paste(VAR1, VAR2, "additive", sep="-")] <- MuMIn::r.squaredGLMM(modAdd)[2]
        modOutRMSE[i,  paste(VAR1, VAR2, "additive", sep="-")] <- sqrt(mean(resid(modAdd)^2))
        modOutError[i,  paste(VAR1, VAR2, "additive", sep="-")] <- mean(resid(modAdd))
        
        modInt <-  nlme::lme(NDVI ~ VAR1*VAR2*NDVI.Lag14d, random=list(mission=~1), data=dat.tmp[,], na.action=na.omit)
        summary(modInt)
        modOutAIC[i, paste(VAR1, VAR2, "interaction", sep="-")] <- AIC(modInt)
        modOutR2[i,  paste(VAR1, VAR2, "interaction", sep="-")] <- MuMIn::r.squaredGLMM(modInt)[2]
        modOutRMSE[i,  paste(VAR1, VAR2, "interaction", sep="-")] <- sqrt(mean(resid(modInt)^2))
        modOutError[i,  paste(VAR1, VAR2, "interaction", sep="-")] <- mean(resid(modInt))
        
      }
    }
    
    # summary(modOutAIC)
    
  } # End day of year loop
  # summary(modOutAIC)
  # head(modOutAIC)
  # summary(modOutRMSE)
  # summary(modOutR2)
  
  write.csv(modOutAIC, file.path(pathSave, paste0("DailyModel_MultiVarSelection_AIC_", LC, ".csv")), row.names=F)
  write.csv(modOutR2, file.path(pathSave, paste0("DailyModel_MultiVarSelection_R2c_", LC, ".csv")), row.names=F)
  write.csv(modOutRMSE, file.path(pathSave, paste0("DailyModel_MultiVarSelection_RMSE_", LC, ".csv")), row.names=F)
  write.csv(modOutError, file.path(pathSave, paste0("DailyModel_MultiVarSelection_Error_", LC, ".csv")), row.names=F)
  
  # saving local copies
  # write.csv(modOutAIC, file.path(google.drive, "data/processed_files", paste0("DailyModel_MultiVarSelection_AIC_", LC, ".csv")), row.names=F)
  # write.csv(modOutR2, file.path(google.drive, "data/processed_files", paste0("DailyModel_MultiVarSelection_R2c_", LC, ".csv")), row.names=F)
  # write.csv(modOutRMSE, file.path(google.drive, "data/processed_files", paste0("DailyModel_MultiVarSelection_RMSE_", LC, ".csv")), row.names=F)
  
  listAIC[[LC]] <- modOutAIC
  listRMSE[[LC]] <- modOutRMSE
  listR2[[LC]] <- modOutR2
  listError[[LC]] <- modOutError
  
  # Calculating dAIC
  dAIC <- modOutAIC
  # Negative values indicate improvement; greater than -2 is typically significant.
  dAIC[,c("modLag", modNames)] <- modOutAIC[,c("modLag", modNames)] - modOutAIC$modLag 
  summary(dAIC)
  
  # Stacking so we can do a new daily corr figure
  # OG Model
  aicStack <- stack(dAIC[,c("modLag", modNames)])
  names(aicStack) <- c("dAIC", "model")
  aicStack$yday <- dAIC$yday
  summary(aicStack)
  
  
  plot.dAIC <- ggplot(data=aicStack[,]) +
    ggtitle(LC) +
    geom_tile(aes(x=yday, y=model, fill=dAIC)) +
    scale_fill_gradient2(low="green4", high="orange2", mid="gray80", midpoint=0) +
    scale_x_continuous(expand=c(0,0)) +
    theme_bw()
  
  
  # png(file.path(path.figs, paste0("NDVI-ModelSelection-Univariate_", LC, "_dAIC.png")), height=6, width=10, units="in", res=320)
  # saving locally
  png(file.path(path.figs, paste0("NDVI-ModelSelection-Multivariate_", LC, "_dAIC.png")), height=6, width=10, units="in", res=320)
  print(plot.dAIC)
  dev.off()
  
  # Calculating dRMSE
  dRMSE <- modOutRMSE
  # Negative values indicate improvement
  dRMSE[,c("modLag", modNames)] <- modOutRMSE[,c("modLag", modNames)] - modOutRMSE$modLag
  summary(dRMSE)
  
  # Stacking so we can do a new daily corr figure
  # OG Model
  rmseStack <- stack(dRMSE[,c("modLag", modNames)])
  names(rmseStack) <- c("dRMSE", "model")
  rmseStack$yday <- dRMSE$yday
  summary(rmseStack)
  
  plot.dRMSE <- ggplot(data=rmseStack[,]) +
    ggtitle(LC) +
    geom_tile(aes(x=yday, y=model, fill=dRMSE)) +
    scale_fill_gradient2(low="green4", high="orange2", mid="gray80", midpoint=0) +
    scale_x_continuous(expand=c(0,0)) +
    theme_bw()
  
  # png(file.path(path.figs, paste0("NDVI-ModelSelection-Univariate_", LC, "_dRMSE.png")), height=6, width=10, units="in", res=320)
  png(file.path(path.figs, paste0("NDVI-ModelSelection-Multivariate_", LC, "_dRMSE.png")), height=6, width=10, units="in", res=320)
  print(plot.dRMSE)
  dev.off()
  
  # Calculating dRMSE
  dR2 <- modOutR2
  # POSITIVE values indicate improvement
  dR2[,c("modLag", modNames)] <- modOutR2[,c("modLag", modNames)] - modOutR2$modLag
  summary(dR2)
  
  # Stacking so we can do a new daily corr figure
  # OG Model
  r2Stack <- stack(dR2[,c("modLag", modNames)])
  names(r2Stack) <- c("dR2", "model")
  r2Stack$yday <- dR2$yday
  summary(r2Stack)
  
  plot.dR2 <- ggplot(data=r2Stack[,]) +
    ggtitle(LC) +
    geom_tile(aes(x=yday, y=model, fill=dR2)) +
    scale_fill_gradient2(low="orange2", high="green4", mid="gray80", midpoint=0) +
    scale_x_continuous(expand=c(0,0)) +
    theme_bw()
  
  png(file.path(path.figs, paste0("NDVI-ModelSelection-Multivariate_", LC, "_dR2c.png")), height=6, width=10, units="in", res=320)
  print(plot.dR2)
  dev.off()
  
  listAICd[[LC]] <- dAIC
  listRMSEd[[LC]] <- dRMSE
  listR2d[[LC]] <- dR2
  
  print("") # Just kicking the label to a new line to make things cleaner
} # End LC loop



# Plan will be to pick the model with the best dR2/dRMSE and then compare the AICs of the interactive vs. additive models --> that shouldn't require re-running anything.

#########################################
# Comparing across days LCs and days ----
#########################################
AICall <- dplyr::bind_rows(listAIC)
RMSEall <- dplyr::bind_rows(listRMSE)
R2all <- dplyr::bind_rows(listR2)
Errorall <- dplyr::bind_rows(listError)

dAICall <- dplyr::bind_rows(listAICd)
dRMSEall <- dplyr::bind_rows(listRMSEd)
dR2all <- dplyr::bind_rows(listR2d)


summary(R2all)
head(R2all[,grep("additive", names(R2all))])
head(R2all[,grep("interaction", names(R2all))])

dAICxn <- AICall[,c("landcover", "yday", "modLag")]
dAICxn[,names(AICall)[grep("interaction", names(AICall))]] <- AICall[,grep("interaction", names(AICall))] - AICall[,grep("additive", names(AICall))]
summary(dAICxn)

dRMSExn <- RMSEall[,c("landcover", "yday", "modLag")]
dRMSExn[,names(RMSEall)[grep("interaction", names(RMSEall))]] <- RMSEall[,grep("interaction", names(RMSEall))] - RMSEall[,grep("additive", names(RMSEall))]
summary(dRMSExn)

dR2xn <- R2all[,c("landcover", "yday", "modLag")]
dR2xn[,names(R2all)[grep("interaction", names(R2all))]] <- R2all[,grep("interaction", names(R2all))] - R2all[,grep("additive", names(R2all))]
summary(dR2xn)


dRMSEperc <- dRMSEall
dRMSEperc[,c("modLag", modNames)] <- dRMSEall[,c("modLag", modNames)]/RMSEall[,c("modLag", modNames)]
summary(dRMSEperc)

modStatsXn <- stack(dAICxn[,grep("interaction", names(dAICxn))])
names(modStatsXn) <- c("dAIC.xn", "model")
modStatsXn[,c("landcover", "yday")] <- dAICxn[,c("landcover", "yday")]
modStatsXn$dRMSE.xn <- stack(dRMSExn[,grep("interaction", names(dRMSExn))])[,"values"]
modStatsXn$dR2.xn <- stack(dR2xn[,grep("interaction", names(dR2xn))])[,"values"]
summary(modStatsXn)

modStatsAll <- stack(dAICall[,c("modLag", modNames)])
names(modStatsAll) <- c("dAIC", "model")
modStatsAll[,c("landcover", "yday")] <- AICall[,c("landcover", "yday")]
modStatsAll$RMSE <- stack(RMSEall[,c("modLag", modNames)])[,"values"]
modStatsAll$dRMSE <- stack(dRMSEall[,c("modLag", modNames)])[,"values"]
modStatsAll$dRMSEper <- stack(dRMSEperc[,c("modLag", modNames)])[,"values"]
modStatsAll$R2 <- stack(R2all[,c("modLag", modNames)])[,"values"]
modStatsAll$dR2 <- stack(dR2all[,c("modLag", modNames)])[,"values"]
modStatsAll$Error <- stack(Errorall[,c("modLag", modNames)])[,"values"]
modStatsAll$DroughtVar <- unlist(lapply(strsplit(as.character(modStatsAll$model), "-"), FUN=function(x){x[1]}))
modStatsAll$TempVar <- unlist(lapply(strsplit(as.character(modStatsAll$model), "-"), FUN=function(x){x[2]}))
modStatsAll$modelType <- unlist(lapply(strsplit(as.character(modStatsAll$model), "-"), FUN=function(x){x[3]}))

modStatsAll <- merge(modStatsAll, modStatsXn, all=T)

modStatsAll$DroughtVar <- as.factor(modStatsAll$DroughtVar)
modStatsAll$TempVar <- as.factor(modStatsAll$TempVar)
modStatsAll$modelType <- as.factor(modStatsAll$modelType)
modStatsAll$DroughtVar[is.na(modStatsAll$TempVar)] <- NA
summary(modStatsAll)

write.csv(modStatsAll, file.path(pathSave, paste0("DailyModel_VarSelection-Multivariate_ModelStats-ALL.csv")), row.names=F)


plot.dAIC <- ggplot(data=modStatsAll[!modStatsAll$model=="modLag",]) +
  ggtitle("Change AIC") +
  facet_grid(modelType~landcover, scales="free_y") +
  geom_tile(aes(x=yday, y=model, fill=dAIC)) +
  scale_fill_gradient2(low="green4", high="orange2", mid="gray80", midpoint=0) +
  scale_x_continuous(expand=c(0,0)) +
  theme_bw()

plot.dAICXn <- ggplot(data=modStatsAll[!is.na(modStatsAll$dAIC.xn),]) +
  ggtitle("Change AIC -- interaction vs. additive") +
  facet_wrap(~landcover) +
  geom_tile(aes(x=yday, y=model, fill=dAIC.xn)) +
  scale_fill_gradient2(low="green4", high="orange2", mid="gray80", midpoint=0) +
  scale_x_continuous(expand=c(0,0)) +
  theme_bw()

png(file.path(path.figs, paste0("NDVI-ModelSelection-Multivariate_AllLandcover_dAIC.png")), height=9, width=10, units="in", res=320)
plot.dAIC
dev.off()

png(file.path(path.figs, paste0("NDVI-ModelSelection-Multivariate_AllLandcover_dAIC-interactions.png")), height=9, width=10, units="in", res=320)
plot.dAICXn
dev.off()

plot.dRMSE <- ggplot(data=modStatsAll[!modStatsAll$model=="modLag",]) +
  ggtitle("Change RMSE") +
  facet_grid(modelType~landcover, scales="free_y") +
  geom_tile(aes(x=yday, y=model, fill=dRMSE)) +
  scale_fill_gradient2(low="green4", high="orange2", mid="gray80", midpoint=0) +
  scale_x_continuous(expand=c(0,0)) +
  theme_bw()

plot.dRMSEXn <- ggplot(data=modStatsAll[!is.na(modStatsAll$dRMSE.xn),]) +
  ggtitle("Change RMSE -- interaction vs.additive") +
  facet_wrap(.~landcover) +
  geom_tile(aes(x=yday, y=model, fill=dRMSE.xn)) +
  scale_fill_gradient2(low="green4", high="orange2", mid="gray80", midpoint=0) +
  scale_x_continuous(expand=c(0,0)) +
  theme_bw()

png(file.path(path.figs, paste0("NDVI-ModelSelection-Multivariate_AllLandcover_dRMSE.png")), height=9, width=10, units="in", res=320)
plot.dRMSE
dev.off()

png(file.path(path.figs, paste0("NDVI-ModelSelection-Multivariate_AllLandcover_dRMSE-interactions.png")), height=9, width=10, units="in", res=320)
plot.dRMSEXn
dev.off()


plot.RMSE <- ggplot(data=modStatsAll[modStatsAll$model!="modLag",]) +
  ggtitle("RMSE") +
  facet_grid(modelType~landcover, scales="free_y") +
  geom_tile(aes(x=yday, y=model, fill=RMSE)) +
  scale_fill_gradient2(low="green4", high="orange2", mid="gray80", midpoint=median(modStatsAll$RMSE)) +
  scale_x_continuous(expand=c(0,0)) +
  theme_bw()

# png(file.path(path.figs, paste0("NDVI-ModelSelection-Univariate_AllLandcover_RMSE.png")), height=9, width=10, units="in", res=320)
png(file.path("figures", paste0("NDVI-ModelSelection-Multivariate_AllLandcover_RMSE.png")), height=9, width=10, units="in", res=320)
plot.RMSE
dev.off()

plot.R2 <- ggplot(data=modStatsAll[modStatsAll$model!="modLag",]) +
  ggtitle("conditional R2 (fixed + random)") +
  facet_grid(modelType~landcover, scales="free_y") +
  geom_tile(aes(x=yday, y=model, fill=R2)) +
  scale_fill_gradient2(high="green4", low="orange2", mid="gray80", midpoint=median(modStatsAll$R2)) +
  scale_x_continuous(expand=c(0,0)) +
  theme_bw()

png(file.path("figures", paste0("NDVI-ModelSelection-Multivariate_AllLandcover_R2.png")), height=9, width=10, units="in", res=320)
plot.R2
dev.off()

plot.dR2 <- ggplot(data=modStatsAll[modStatsAll$model!="modLag",]) +
  ggtitle("Change in conditional R2 (fixed + random)") +
  facet_grid(modelType~landcover, scales="free_y") +
  geom_tile(aes(x=yday, y=model, fill=dR2)) +
  scale_fill_gradient2(high="green4", low="orange2", mid="gray80", midpoint=0) +
  scale_x_continuous(expand=c(0,0)) +
  theme_bw()


plot.dR2Xn <- ggplot(data=modStatsAll[!is.na(modStatsAll$dR2.xn),]) +
  ggtitle("Change in conditional R2 (fixed + random)") +
  facet_grid(modelType~landcover, scales="free_y") +
  geom_tile(aes(x=yday, y=model, fill=dR2.xn)) +
  scale_fill_gradient2(high="green4", low="orange2", mid="gray80", midpoint=0) +
  scale_x_continuous(expand=c(0,0)) +
  theme_bw()

png(file.path(path.figs, paste0("NDVI-ModelSelection-Multivariate_AllLandcover_dR2.png")), height=9, width=10, units="in", res=320)
plot.dR2
dev.off()

png(file.path(path.figs, paste0("NDVI-ModelSelection-Multivariate_AllLandcover_dR2-interactions.png")), height=9, width=10, units="in", res=320)
plot.dR2Xn
dev.off()
