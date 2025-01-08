# Creating a clean script to re-do the daily correlation modeling and do some prediction from it



# script to explore NDVI time series for the land cover classes of the Chicago region
library(ggplot2)
library(lubridate)
library(ggcorrplot)

# Setting the file paths. This may be different for your computer.
Sys.setenv(GOOGLE_DRIVE = "G:/Shared drives/Urban Ecological Drought/Manuscript - Urban Drought NDVI/")
# Sys.setenv(GOOGLE_DRIVE = "~/Google Drive/Shared drives/Urban Ecological Drought/Manuscript - Urban Drought NDVI/")
google.drive <- Sys.getenv("GOOGLE_DRIVE")

path.NDVI <- file.path(google.drive, "data", "data_raw")
path.figs <- file.path(google.drive, "exploratory figures/ModelSelection-Univariate")
pathSave <- file.path(google.drive, "data/processed_files/ModelSelection-Univariate")

if(!dir.exists(path.figs)) dir.create(path.figs, recursive = T)
if(!dir.exists(pathSave)) dir.create(pathSave, recursive = T)

# Reading in the raw NDVI data
fndvi <- dir(path.NDVI, "Landsat")

ndvi.all <- data.frame()
for(i in seq_along(fndvi)){
  fsplit <- strsplit(fndvi[i],"_")[[1]] # Split apart the file name
  
  fNOW <- read.csv(file.path(path.NDVI, fndvi[i]))
  fNOW$mission <- fsplit[1]
  fNOW$landcover <- strsplit(fsplit[2], "[.]")[[1]][1]
  
  ndvi.all <- rbind(ndvi.all, fNOW) # NOTE: This is NOT the best way to do this, but here we go anyways
}

# ndvi.all <- readRDS(file.path(google.drive, "data/r_files/processed_files/landsat_ndvi_all.RDS"))
head(ndvi.all)
ndvi.all$mission <- as.factor(ndvi.all$mission)
ndvi.all$landcover <- as.factor(ndvi.all$landcover)
ndvi.all$date <- as.Date(ndvi.all$date)
ndvi.all$year <- lubridate::year(ndvi.all$date)
ndvi.all$yday <- lubridate::yday(ndvi.all$date)
summary(ndvi.all)

# subset to just 2001-01-01 to 2023-12-31
ndvi.all <- ndvi.all[ndvi.all$date <= as.Date("2023-12-31"),]
summary(ndvi.all)


# reading in Trent's SPI
ChicagolandSPI <- read.csv(file.path(google.drive, "../data/data_sets/Daily Meteorological Data/Chicagoland_Daily_SPI.csv"))
ChicagolandSPEI <- read.csv(file.path(google.drive, "../data/data_sets/Daily Meteorological Data/Chicagoland_Daily_SPEI.csv"))
ChicagolandTemp <- read.csv(file.path(google.drive, "../data/data_sets/Daily Meteorological Data/Chicagoland_Daily_Temps.csv"))

# create column with date in ISO format; making it lowercase "date" so that it merges easier
ChicagolandSPI$date <- as.Date(ChicagolandSPI$Date, "%m/%d/%Y")
ChicagolandSPEI$date <- as.Date(ChicagolandSPEI$Date, "%m/%d/%Y")
ChicagolandTemp$date <- as.Date(ChicagolandTemp$Date, "%m/%d/%Y")
summary(ChicagolandSPI)
summary(ChicagolandSPEI)
summary(ChicagolandTemp)

dim(ChicagolandSPI); dim(ChicagolandSPEI); dim(ChicagolandTemp)

# Combining met data together in a single data frame
chiMet <- merge(ChicagolandTemp, ChicagolandSPI, all=T)
chiMet <- merge(chiMet, ChicagolandSPEI , all=T)
summary(chiMet)
dim(chiMet)

# merging met data and NDVI data together into a single data frame
ndviMet <- merge(ndvi.all, chiMet, all.x=T, all.y=F)
summary(ndviMet)

# saving ndviMet to the data drive so that predictors are paired together with the NDVI data
# saveRDS(ndviMet, file = file.path(google.drive, "data/processed_files/landsat_ndvi_metVars_combined.RDS"))
# write.csv(ndviMet, file = file.path(google.drive, "data/processed_files/landsat_ndvi_metVars_combined.csv"), row.names=F)

# saving some local files for Ross re-introduction runs
saveRDS(ndviMet, file = file.path("processed_data/landsat_ndvi_metVars_combined.RDS"))
write.csv(ndviMet, file = file.path("processed_data/landsat_ndvi_metVars_combined.csv"), row.names=F)

#########################################
# Daily model MEGAloop ----
# This is being modified from what we had before to do each var, with each timescale for each LC
# Starting with just doing them individually before doing any sort of selection
#########################################
LCtypes <- unique(ndviMet$landcover)
timescales <- c("14d", "30d", "60d", "90d")
varsMet <- c(paste0("TMIN", timescales), paste0("TMAX", timescales), paste0("X", timescales, ".SPI"), paste0("X", timescales, ".SPEI"))
names(ndviMet)

# quick correlation of the met variabels to see how tightly the different timescales are correlated
metCor <- cor(chiMet[,varsMet], use="pairwise.complete.obs")
summary(metCor)

# metCov <- cov(chiMet[,varsMet], use="pairwise.complete.obs")
# summary(metCov)

# creating corr plot of met variable analysis
# png(file.path(path.figs, paste0("MetVar_CorrPlot.png")), height=8, width=8, units="in", res=320)
png(file.path("figures", paste0("MetVar_CorrPlot.png")), height=8, width=8, units="in", res=320)
ggcorrplot(metCor, type="lower", lab=T)
dev.off()


listAIC <- listRMSE <- listR2 <- list()
listAICd <- listRMSEd <- listR2d <- list()

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
  modsOut$modIntOnly <- list()
  modsOut$modLag <- list()
  for(VAR in varsMet){
    modsOut[[VAR]] <- list()
  }

  # Right now we only care about the model sumary stats
  # For R2, we'll use the marginal R2 --> the part described by the fixed effects
  modOutAIC <- modOutR2 <- modOutRMSE <- data.frame(landcover=LC, yday=1:365)
  modOutAIC[, c("modIntOnly", "modLag", varsMet)] <- modOutR2[, c("modIntOnly", "modLag", varsMet)] <- modOutRMSE[, c("modIntOnly", "modLag", varsMet)] <- NA
  # mod.out <- data.frame(landcover=LC, yday=1:365, intercept=NA, coef.Lag=NA, coef.SPEI30=NA, coef
  
  
  # row.ind = 0 # Setting up an index that will tell us what row to save things in; we should start with 0 because we haven't done anything yet
  pb <- txtProgressBar(min=0, max=nrow(modOutAIC), style=3)
  for(i in 1:nrow(modOutAIC)){
    setTxtProgressBar(pb, i)
    # For testing using i=185 (which is July 4; yday(as.Date("2021-07-04"))) -- this is a period that should have a decent SPI relationship based off of the initial corr plots
    # dayNOW <- days.use[i] # This is almost exactly the same as above, but now i will go from 1 to 215 (the number of unique days.use we have)
    dayNOW = i # Repurposing old code, so doing a clunky approach here
    
    ## Using an even-sided window to train the model for now to understand the relationships
    # Here we're subsetting our big data frame to the SMALL temporal window we want --> this should help with temporal stationarity in the effects of our predictors
    rowNow <- which(datLC$yday>=dayNOW-7 & datLC$yday<=dayNOW+7 )
    dat.tmp <- datLC[rowNow,] # Subsets things to a particular window (not just a single day); otherwise we were working with just 5 years of data, which isn't helpful
    # summary(dat.tmp)
    
    # Doing some graphing that we're not saving for our own sanity
    # ggplot(data=dat.tmp) + geom_violin(aes(x=as.factor(year), y=NDVI, fill=mission), scale="width")
    # ggplot(data=dat.tmp) + geom_violin(aes(x=as.factor(year), y=X30d.SPI, fill=mission), scale="width")
    # ggplot(data=dat.tmp, aes(x=X30d.SPI, y=NDVI)) + geom_point(aes(color=mission)) + stat_smooth(method="lm")
     
    #Set up a normal (intercept-only) model
    modInt <- nlme::lme(NDVI ~ 1, random=list(mission=~1), data=dat.tmp[,], na.action=na.omit)
    modsOut$modIntOnly[[i]] <- modInt
    # summary(modInt)
    # modOutAIC <- modOutR2 <- modOutRMSE <- data.frame(landcover=LC, yday=1:365)
    modOutAIC$modIntOnly[i] <- AIC(modInt)
    modOutR2$modIntOnly[i] <- MuMIn::r.squaredGLMM(modInt)[2]
    modOutRMSE$modIntOnly[i] <- sqrt(mean(resid(modInt)^2))
    # AIC(modInt)
    
    #Set up a lag-only model
    modL <- nlme::lme(NDVI ~ NDVI.Lag14d, random=list(mission=~1), data=dat.tmp[,], na.action=na.omit)
    modsOut$modLag[[i]] <- modL
    modOutAIC$modLag[i] <- AIC(modL)
    modOutR2$modLag[i] <- MuMIn::r.squaredGLMM(modL)[2]
    modOutRMSE$modLag[i] <- sqrt(mean(resid(modL)^2))
    
    for(VAR in varsMet){
      # print(VAR)
      dat.tmp$VAR <- dat.tmp[,VAR]
      modMet <-  nlme::lme(NDVI ~ VAR + NDVI.Lag14d, random=list(mission=~1), data=dat.tmp[,], na.action=na.omit)
      summary(modMet)
      modOutAIC[i, VAR] <- AIC(modMet)
      modOutR2[i, VAR] <- MuMIn::r.squaredGLMM(modMet)[2]
      modOutRMSE[i, VAR] <- sqrt(mean(resid(modMet)^2))
    }
    
    # summary(modOutAIC)
    
  } # End day of year loop
  # summary(modOutAIC)
  # head(modOutAIC)
  # summary(modOutRMSE)
  # summary(modOutR2)

  # write.csv(modOutAIC, file.path(pathSave, paste0("DailyModel_VarSelection_AIC_", LC, ".csv")), row.names=F)
  # write.csv(modOutR2, file.path(pathSave, paste0("DailyModel_VarSelection_R2c_", LC, ".csv")), row.names=F)
  # write.csv(modOutRMSE, file.path(pathSave, paste0("DailyModel_VarSelection_RMSE_", LC, ".csv")), row.names=F)
  
  # saving local copies
  write.csv(modOutAIC, file.path("processed_data", paste0("DailyModel_VarSelection_AIC_", LC, ".csv")), row.names=F)
  write.csv(modOutR2, file.path("processed_data", paste0("DailyModel_VarSelection_R2c_", LC, ".csv")), row.names=F)
  write.csv(modOutRMSE, file.path("processed_data", paste0("DailyModel_VarSelection_RMSE_", LC, ".csv")), row.names=F)
  
  listAIC[[LC]] <- modOutAIC
  listRMSE[[LC]] <- modOutRMSE
  listR2[[LC]] <- modOutR2
  
  # Calculating dAIC
  dAIC <- modOutAIC
  # Negative values indicate improvement; greater than -2 is typically significant.
  dAIC[,c("modIntOnly", "modLag", varsMet)] <- modOutAIC[,c("modIntOnly", "modLag", varsMet)] - modOutAIC$modIntOnly 
  summary(dAIC)
  
  # Stacking so we can do a new daily corr figure
  # OG Model
  aicStack <- stack(dAIC[,c("modIntOnly", "modLag", varsMet)])
  names(aicStack) <- c("dAIC", "model")
  aicStack$yday <- dAIC$yday
  summary(aicStack)

  
  plot.dAIC <- ggplot(data=aicStack[aicStack$model!="modIntOnly",]) +
    ggtitle(LC) +
    geom_tile(aes(x=yday, y=model, fill=dAIC)) +
    scale_fill_gradient2(low="green4", high="orange2", mid="gray80", midpoint=0) +
    scale_x_continuous(expand=c(0,0)) +
    theme_bw()
  
  
  # png(file.path(path.figs, paste0("NDVI-ModelSelection-Univariate_", LC, "_dAIC.png")), height=6, width=10, units="in", res=320)
  # saving locally
  png(file.path("figures", paste0("NDVI-ModelSelection-Univariate_", LC, "_dAIC.png")), height=6, width=10, units="in", res=320)
  print(plot.dAIC)
  dev.off()
  
  # Calculating dRMSE
  dRMSE <- modOutRMSE
  # Negative values indicate improvement
  dRMSE[,c("modIntOnly", "modLag", varsMet)] <- modOutRMSE[,c("modIntOnly", "modLag", varsMet)] - modOutRMSE$modIntOnly
  summary(dRMSE)
  
  # Stacking so we can do a new daily corr figure
  # OG Model
  rmseStack <- stack(dRMSE[,c("modIntOnly", "modLag", varsMet)])
  names(rmseStack) <- c("dRMSE", "model")
  rmseStack$yday <- dRMSE$yday
  summary(rmseStack)
  
  plot.dRMSE <- ggplot(data=rmseStack[rmseStack$model!="modIntOnly",]) +
    ggtitle(LC) +
    geom_tile(aes(x=yday, y=model, fill=dRMSE)) +
    scale_fill_gradient2(low="green4", high="orange2", mid="gray80", midpoint=0) +
    scale_x_continuous(expand=c(0,0)) +
    theme_bw()
  
  # png(file.path(path.figs, paste0("NDVI-ModelSelection-Univariate_", LC, "_dRMSE.png")), height=6, width=10, units="in", res=320)
  png(file.path("figures", paste0("NDVI-ModelSelection-Univariate_", LC, "_dRMSE.png")), height=6, width=10, units="in", res=320)
  print(plot.dRMSE)
  dev.off()
  
  # Calculating dRMSE
  dR2 <- modOutR2
  # POSITIVE values indicate improvement
  dR2[,c("modIntOnly", "modLag", varsMet)] <- modOutR2[,c("modIntOnly", "modLag", varsMet)] - modOutR2$modIntOnly
  summary(dR2)
  
  # Stacking so we can do a new daily corr figure
  # OG Model
  r2Stack <- stack(dR2[,c("modIntOnly", "modLag", varsMet)])
  names(r2Stack) <- c("dR2", "model")
  r2Stack$yday <- dR2$yday
  summary(r2Stack)
  
  plot.dR2 <- ggplot(data=r2Stack[r2Stack$model!="modIntOnly",]) +
    ggtitle(LC) +
    geom_tile(aes(x=yday, y=model, fill=dR2)) +
    scale_fill_gradient2(low="orange2", high="green4", mid="gray80", midpoint=0) +
    scale_x_continuous(expand=c(0,0)) +
    theme_bw()
  
  png(file.path(path.figs, paste0("NDVI-ModelSelection-Univariate_", LC, "_dR2c.png")), height=6, width=10, units="in", res=320)
  print(plot.dR2)
  dev.off()
  
  listAICd[[LC]] <- dAIC
  listRMSEd[[LC]] <- dRMSE
  listR2d[[LC]] <- dR2
  
  print("") # Just kicking the label to a new line to make things cleaner
} # End LC loop
#########################################


#########################################
# Comparing across days LCs and days ----
#########################################
AICall <- dplyr::bind_rows(listAIC)
RMSEall <- dplyr::bind_rows(listRMSE)
R2all <- dplyr::bind_rows(listR2)

dAICall <- dplyr::bind_rows(listAICd)
dRMSEall <- dplyr::bind_rows(listRMSEd)
dR2all <- dplyr::bind_rows(listR2d)

summary(R2all)

dRMSEperc <- dRMSEall
dRMSEperc[,c("modIntOnly", "modLag", varsMet)] <- dRMSEall[,c("modIntOnly", "modLag", varsMet)]/RMSEall[,c("modIntOnly", "modLag", varsMet)]
summary(dRMSEperc)

modStatsAll <- stack(dAICall[,c("modIntOnly", "modLag", varsMet)])
names(modStatsAll) <- c("dAIC", "model")
modStatsAll[,c("landcover", "yday")] <- AICall[,c("landcover", "yday")]
modStatsAll$RMSE <- stack(RMSEall[,c("modIntOnly", "modLag", varsMet)])[,"values"]
modStatsAll$dRMSE <- stack(dRMSEall[,c("modIntOnly", "modLag", varsMet)])[,"values"]
modStatsAll$dRMSEper <- stack(dRMSEperc[,c("modIntOnly", "modLag", varsMet)])[,"values"]
modStatsAll$R2 <- stack(R2all[,c("modIntOnly", "modLag", varsMet)])[,"values"]
modStatsAll$dR2 <- stack(dR2all[,c("modIntOnly", "modLag", varsMet)])[,"values"]
summary(modStatsAll)

# write.csv(modStatsAll, file.path(pathSave, paste0("DailyModel_VarSelection-Univariate_ModelStats-ALL.csv")), row.names=F)
write.csv(modStatsAll, file.path("processed_data", paste0("DailyModel_VarSelection-Univariate_ModelStats-ALL.csv")), row.names=F)

plot.dAIC <- ggplot(data=modStatsAll[modStatsAll$model!="modIntOnly",]) +
  ggtitle("Change AIC") +
  facet_wrap(~landcover) +
  geom_tile(aes(x=yday, y=model, fill=dAIC)) +
  scale_fill_gradient2(low="green4", high="orange2", mid="gray80", midpoint=0) +
  scale_x_continuous(expand=c(0,0)) +
  theme_bw()

# png(file.path(path.figs, paste0("NDVI-ModelSelection-Univariate_AllLandcover_dAIC.png")), height=9, width=10, units="in", res=320)
png(file.path("figures", paste0("NDVI-ModelSelection-Univariate_AllLandcover_dAIC.png")), height=9, width=10, units="in", res=320)
plot.dAIC
dev.off()

plot.dRMSE <- ggplot(data=modStatsAll[modStatsAll$model!="modIntOnly",]) +
  ggtitle("Change RMSE") +
  facet_wrap(~landcover) +
  geom_tile(aes(x=yday, y=model, fill=dRMSE)) +
  scale_fill_gradient2(low="green4", high="orange2", mid="gray80", midpoint=0) +
  scale_x_continuous(expand=c(0,0)) +
  theme_bw()

# png(file.path(path.figs, paste0("NDVI-ModelSelection-Univariate_AllLandcover_dRMSE.png")), height=9, width=10, units="in", res=320)
png(file.path("figures", paste0("NDVI-ModelSelection-Univariate_AllLandcover_dRMSE.png")), height=9, width=10, units="in", res=320)
plot.dRMSE
dev.off()

plot.RMSE <- ggplot(data=modStatsAll[modStatsAll$model!="modIntOnly",]) +
  ggtitle("RMSE") +
  facet_wrap(~landcover) +
  geom_tile(aes(x=yday, y=model, fill=RMSE)) +
  scale_fill_gradient2(low="green4", high="orange2", mid="gray80", midpoint=median(modStatsAll$RMSE)) +
  scale_x_continuous(expand=c(0,0)) +
  theme_bw()

# png(file.path(path.figs, paste0("NDVI-ModelSelection-Univariate_AllLandcover_RMSE.png")), height=9, width=10, units="in", res=320)
png(file.path("figures", paste0("NDVI-ModelSelection-Univariate_AllLandcover_RMSE.png")), height=9, width=10, units="in", res=320)
plot.RMSE
dev.off()

plot.R2 <- ggplot(data=modStatsAll[modStatsAll$model!="modIntOnly",]) +
  ggtitle("conditional R2 (fixed + random)") +
  facet_wrap(~landcover) +
  geom_tile(aes(x=yday, y=model, fill=R2)) +
  scale_fill_gradient2(high="green4", low="orange2", mid="gray80", midpoint=median(modStatsAll$R2)) +
  scale_x_continuous(expand=c(0,0)) +
  theme_bw()

# png(file.path(path.figs, paste0("NDVI-ModelSelection-Univariate_AllLandcover_R2.png")), height=9, width=10, units="in", res=320)
png(file.path("figures", paste0("NDVI-ModelSelection-Univariate_AllLandcover_R2.png")), height=9, width=10, units="in", res=320)
plot.R2
dev.off()

plot.dR2 <- ggplot(data=modStatsAll[modStatsAll$model!="modIntOnly",]) +
  ggtitle("Change in conditional R2 (fixed + random)") +
  facet_wrap(~landcover) +
  geom_tile(aes(x=yday, y=model, fill=dR2)) +
  scale_fill_gradient2(high="green4", low="orange2", mid="gray80", midpoint=0) +
  scale_x_continuous(expand=c(0,0)) +
  theme_bw()

# png(file.path(path.figs, paste0("NDVI-ModelSelection-Univariate_AllLandcover_dR2.png")), height=9, width=10, units="in", res=320)
png(file.path("figures", paste0("NDVI-ModelSelection-Univariate_AllLandcover_dR2.png")), height=9, width=10, units="in", res=320)
plot.dR2
dev.off()

# ---------------------
# Aggregating across LC types & Days to get a mean value for each model --> focusing on CHANGE ----
# ---------------------
aggModelLC <- aggregate(cbind(dAIC, dRMSE, dR2) ~ model + landcover, data=modStatsAll[modStatsAll$model!="modIntOnly",], FUN=mean)
aggModelLC[,c("dAIC.sd", "dRMSE.sd", "dR2.sd")] <- aggregate(cbind(dAIC, dRMSE, dR2) ~ model + landcover, data=modStatsAll[modStatsAll$model!="modIntOnly",], FUN=sd)[,c("dAIC", "dRMSE", "dR2")]

# write.csv(aggModelLC, file.path(pathSave, paste0("DailyModel_VarSelection-Univariate_ModelStats-Summaries.csv")), row.names=F)
write.csv(aggModelLC, file.path("processed_data", paste0("DailyModel_VarSelection-Univariate_ModelStats-Summaries.csv")), row.names=F)

# Now calculating the average rank of a variable for each landcover class
summary(aggModelLC)
aggModelLC[,c("rank.dAIC", "rank.dRMSE", "rank.dR2")] <- NA
for(LC in LCtypes){
  rowsLC <- which(aggModelLC$landcover==LC)
  datLC <- aggModelLC[rowsLC,]
  # datLC[, c("model", "dRMSE")]
  
  datLC[order(datLC$dAIC), c("rank.dAIC")] <- 1:nrow(datLC)
  datLC[order(datLC$dRMSE), c("rank.dRMSE")] <- 1:nrow(datLC)
  datLC[order(datLC$dR2, decreasing = T), c("rank.dR2")] <- 1:nrow(datLC)
  # datLC[,c("model", "dR2", "rank.dR2")]
  
  aggModelLC[rowsLC,c("rank.dAIC", "rank.dRMSE", "rank.dR2")] <- datLC[,c("rank.dAIC", "rank.dRMSE", "rank.dR2")]
}
summary(aggModelLC)

# png(file.path(path.figs, paste0("NDVI-ModelSelection-Univariate_AverageYDAY_dAIC.png")), height=8, width=8, units="in", res=220)
png(file.path("figures", paste0("NDVI-ModelSelection-Univariate_AverageYDAY_dAIC.png")), height=8, width=8, units="in", res=220)
ggplot(data=aggModelLC) +
  coord_flip() +
  geom_boxplot(aes(x=model, y=dAIC), fill="gray50") +
  labs(title="Delta AIC") +
  theme_bw()
dev.off()

# png(file.path(path.figs, paste0("NDVI-ModelSelection-Univariate_Rank-Boxplot_dRMSE.png")), height=8, width=8, units="in", res=220)
png(file.path("figures", paste0("NDVI-ModelSelection-Univariate_Rank-Boxplot_dRMSE.png")), height=8, width=8, units="in", res=220)
ggplot(data=aggModelLC) +
  coord_flip() +
  geom_boxplot(aes(x=model, y=rank.dRMSE), fill="gray50") +
  labs(title = "RMSE Rank") +
  scale_y_continuous(breaks=c(1, 5, 10, 15, 17), labels=c("1\nBest", 5, 10, 15, "17\nLowest")) +
  theme_bw()
dev.off()

# png(file.path(path.figs, paste0("NDVI-ModelSelection-Univariate_Rank-Boxplot_dR2.png")), height=8, width=8, units="in", res=220)
png(file.path("figures", paste0("NDVI-ModelSelection-Univariate_Rank-Boxplot_dR2.png")), height=8, width=8, units="in", res=220)
ggplot(data=aggModelLC) +
  coord_flip() +
  geom_boxplot(aes(x=model, y=rank.dR2), fill="gray50") +
  labs(title="Delta R2 Rank") +
  scale_y_continuous(breaks=c(1, 5, 10, 15, 17), labels=c("1\nBest", 5, 10, 15, "17\nLowest")) +
  theme_bw()
dev.off()

# png(file.path(path.figs, paste0("NDVI-ModelSelection-Univariate_AverageYDAY_dRMSE.png")), height=8, width=8, units="in", res=220)
png(file.path("figures", paste0("NDVI-ModelSelection-Univariate_AverageYDAY_dRMSE.png")), height=8, width=8, units="in", res=220)
ggplot(data=aggModelLC) +
  coord_flip() +
  geom_boxplot(aes(x=model, y=dRMSE), fill="gray50") +
  labs(title="Delta RMSE") +
  theme_bw()
dev.off()


# png(file.path(path.figs, paste0("NDVI-ModelSelection-Univariate_AverageYDAY_dR2.png")), height=8, width=8, units="in", res=220)
png(file.path("figures", paste0("NDVI-ModelSelection-Univariate_AverageYDAY_dR2.png")), height=8, width=8, units="in", res=220)
ggplot(data=aggModelLC) +
  coord_flip() +
  geom_boxplot(aes(x=model, y=dR2), fill="gray50") +
  labs(title="Delta R2") +
  theme_bw()
dev.off()

png(file.path(path.figs, paste0("NDVI-ModelSelection-Univariate_AverageYDAY_Landcover_dRMSE.png")), height=10, width=8, units="in", res=220)
ggplot(data=aggModelLC, aes(x=model, y=dRMSE) ) +
  facet_wrap(~landcover, scales="free_x") +
  coord_flip() +
  geom_bar(stat="identity", fill="gray60") +
  # geom_errorbar(aes(ymin=dRMSE - dRMSE.sd, ymax=dRMSE+dRMSE.sd), linewidth=0.2) +
  theme_bw()
dev.off()

png(file.path(path.figs, paste0("NDVI-ModelSelection-Univariate_AverageYDAY_Landcover_dR2.png")), height=10, width=8, units="in", res=220)
ggplot(data=aggModelLC, aes(x=model, y=dR2) ) +
  facet_wrap(~landcover, scales="free_x") +
  coord_flip() +
  geom_bar(stat="identity", fill="gray60") +
  # geom_errorbar(aes(ymin=dR2 - dR2.sd, ymax=dR2+dR2.sd), linewidth=0.2) +
  theme_bw()
dev.off()

# ---------------------
# Aggregating to just the model/met var ----
# ---------------------
aggModel <- aggregate(cbind(dAIC, dRMSE, dR2, rank.dAIC, rank.dRMSE, rank.dR2) ~ model, data=aggModelLC, FUN=mean)
aggModel[,c("dAIC.sd", "dRMSE.sd", "dR2.sd", "rank.dAIC.sd", "rank.dRMSE.sd", "rank.dR2.sd")] <- aggregate(cbind(dAIC, dRMSE, dR2, rank.dAIC, rank.dRMSE, rank.dR2) ~ model, data=aggModelLC, FUN=sd)[,c("dAIC", "dRMSE", "dR2", "rank.dAIC", "rank.dRMSE", "rank.dR2")]
summary(aggModel)

png(file.path(path.figs, paste0("NDVI-ModelSelection-Univariate_Average_byModel_dRMSE.png")), height=10, width=8, units="in", res=220)
ggplot(data=aggModel, aes(x=model, y=dRMSE) ) +
  coord_flip() +
  geom_bar(stat="identity", fill="gray60") +
  geom_errorbar(aes(ymin=dRMSE - dRMSE.sd, ymax=dRMSE+dRMSE.sd), linewidth=0.2) +
  theme_bw()
dev.off()

png(file.path(path.figs, paste0("NDVI-ModelSelection-Univariate_Rank_dRMSE.png")), height=8, width=8, units="in", res=220)
ggplot(data=aggModel, aes(x=model, y=rank.dRMSE, fill=rank.dRMSE) ) +
  coord_flip() +
  geom_bar(stat="identity") +
  geom_errorbar(aes(ymin=rank.dRMSE - rank.dRMSE.sd, ymax=rank.dRMSE+rank.dRMSE.sd), linewidth=0.2) +
  scale_fill_gradient2(low="green4", high="orange2", mid="gray80", midpoint=median(aggModel$rank.dRMSE)) +
  theme_bw()
dev.off()

png(file.path(path.figs, paste0("NDVI-ModelSelection-Univariate_Rank_d2.png")), height=8, width=8, units="in", res=220)
ggplot(data=aggModel, aes(x=model, y=rank.dR2, fill=rank.dR2) ) +
  coord_flip() +
  geom_bar(stat="identity") +
  geom_errorbar(aes(ymin=rank.dR2 - rank.dR2.sd, ymax=rank.dR2+rank.dR2.sd), linewidth=0.2) +
  scale_fill_gradient2(low="green4", high="orange2", mid="gray80", midpoint=median(aggModel$rank.dR2)) +
  theme_bw()
dev.off()


png(file.path(path.figs, paste0("NDVI-ModelSelection-Univariate_Average_byModel_dR2.png")), height=10, width=8, units="in", res=220)
ggplot(data=aggModel, aes(x=model, y=dR2) ) +
  coord_flip() +
  geom_bar(stat="identity", fill="gray60") +
  geom_errorbar(aes(ymin=dR2 - dR2.sd, ymax=dR2+dR2.sd), linewidth=0.2) +
  theme_bw()
dev.off()

#########################################



