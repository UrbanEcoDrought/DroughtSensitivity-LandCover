# Creating a clean skip to re-do the daily correlation modeling and do some prediction from it



# script to explore NDVI time series for the land cover classes of the Chicago region
library(ggplot2)
library(lubridate)

# Setting the file paths. This may be different for your computer.
# Sys.setenv(GOOGLE_DRIVE = "G:/Shared drives/Urban Ecological Drought")
Sys.setenv(GOOGLE_DRIVE = "~/Google Drive/Shared drives/Urban Ecological Drought/Manuscript - Urban Drought NDVI/")
google.drive <- Sys.getenv("GOOGLE_DRIVE")

path.NDVI <- file.path(google.drive, "data", "data_raw")
path.figs <- file.path(google.drive, "exploratory figures/ModelSelection-Univariate")
pathSave <- file.path(google.drive, "data/processed_files/ModelSelection-Univariate")

if(!dir.exists(path.figs)) dir.create(path.figs, recursive = T)
if(!dir.exists(pathSave)) dir.create(pathSave, recursive = T)

# Reading in teh raw NDVI data
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


ndviMet <- merge(ndvi.all, ChicagolandTemp, all.x=T, all.y=F)
ndviMet <- merge(ndviMet, ChicagolandSPI, all.x=T, all.y=F)
ndviMet <- merge(ndviMet, ChicagolandSPEI, all.x=T, all.y=F)
summary(ndviMet)

# saving ndviMet to the data drive so that predictors are paired together with the NDVI data
saveRDS(ndviMet, file = file.path(google.drive, "data/processed_files/landsat_ndvi_metVars_combined.RDS"))
write.csv(ndviMet, file = file.path(google.drive, "data/processed_files/landsat_ndvi_metVars_combined.csv"), row.names=F)
#########################################
# Daily model MEGAloop ----
# This is being modified from what we had before to do each var, with each timescale for each LC
# Starting with just doing them individually before doing any sort of selection
#########################################
LCtypes <- unique(ndviMet$landcover)
timescales <- c("14d", "30d", "60d", "90d")
varsMet <- c(paste0("TMIN", timescales), paste0("TMAX", timescales), paste0("X", timescales, ".SPI"), paste0("X", timescales, ".SPEI"))
names(ndviMet)

for(LC in LCtypes){
  print(LC)
  # Subset the data 
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

  write.csv(modOutAIC, file.path(pathSave, paste0("DailyModel_VarSelection_AIC_", LC, ".csv")), row.names=F)
  write.csv(modOutR2, file.path(pathSave, paste0("DailyModel_VarSelection_R2c_", LC, ".csv")), row.names=F)
  write.csv(modOutRMSE, file.path(pathSave, paste0("DailyModel_VarSelection_RMSE_", LC, ".csv")), row.names=F)
  
  # Calculating dAIC
  dAIC <- modOutAIC
  # Negative values indicate improvement
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
  
  png(file.path(path.figs, paste0("NDVI-ModelSelection-Univariate_", LC, "_dAIC.png")), height=6, width=10, units="in", res=320)
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
  
  png(file.path(path.figs, paste0("NDVI-ModelSelection-Univariate_", LC, "_dRMSE.png")), height=6, width=10, units="in", res=320)
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
  
  
  print("") # Just kicking the label to a new line to make things cleaner
} # End LC loop
#########################################


