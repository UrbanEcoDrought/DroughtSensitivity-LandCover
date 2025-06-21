# Creating the figures that Christy had Sketched out
# Figure A: Significant Tstat plot for different land cover types
# Figure B: RMSE through time for different land cover types
# Figure C: Partial effects through time

library(ggplot2)
# library(dplyr)
# library(tidyr)
library(reshape2)
library(scales)
library(gridExtra) # For arranging multiple plots in one figure
library(viridis)
# Setting the file paths----
# This may be different for your computer.
# Sys.setenv(GOOGLE_DRIVE = "G:/Shared drives/Urban Ecological Drought/Manuscript - Urban Drought NDVI Daily Corrs/")
Sys.setenv(GOOGLE_DRIVE = "~/Google Drive/Shared drives/Urban Ecological Drought/Manuscript - Urban Drought NDVI Daily Corrs/")
google.drive <- Sys.getenv("GOOGLE_DRIVE")

path.NDVI <- file.path(google.drive, "data", "data_raw")
path.figs <- file.path(google.drive, "exploratory figures/prelim_pub_figs")
pathSave <- file.path(google.drive, "data/processed_files/FinalDailyModel")

# be sure we're loading modOutAdd3
daily.pe <- read.csv(file.path(pathSave, paste0("DailyModel_FinalModel_modOutAdd3_Stats_dailyPartialEffects_AllLandcovers.csv")))

# Figure C----
# Plot Partial Effects through Time

summary(daily.pe)

# need to reshape to stack daily partial effects
partial.dat <- daily.pe[,c("partial.Drought.date", "partial.Temp.date", "partial.Lag.date", "NDVI", "peDroughtStd.gsMean", "peTempStd.gsMean", "peLagStd.gsMean")]
partial.dat.stack <- stack(partial.dat)
names(partial.dat.stack) <- c("values", "var")

partial.dat.stack$yday <- daily.pe$yday
partial.dat.stack$landcover <- daily.pe$landcover
partial.dat.stack$date <- as.Date(daily.pe$date)
head(partial.dat.stack)
# adding in year and month from date
partial.dat.stack$year <- lubridate::year(partial.dat.stack$date)
partial.dat.stack$month<- lubridate::month(partial.dat.stack$date)
partial.dat.stack$month.name<- lubridate::month(partial.dat.stack$date, abb=T, label=T)


# Partial effects across multiple years
pe.vars <- c("partial.Temp.date", "partial.Drought.date", "partial.Lag.date")
partial.dat.stack$landcover <- factor(partial.dat.stack$landcover, c("crop", "forest", "grassland", "urban-open", "urban-low", "urban-medium", "urban-high"))

figC1 <- ggplot(partial.dat.stack[partial.dat.stack$var %in% pe.vars,]) + facet_grid(landcover~.)+
  geom_hline(yintercept=0, linetype="dashed") +
  geom_line(aes(x=date, y=values, col=var), linewidth=0.8) +
  scale_color_manual(values = c("partial.Temp.date" = "#E69F00", 
                                "partial.Drought.date" = "#0072B2", 
                                "partial.Lag.date" = "#009E73")) +
  scale_x_date(expand = c(0,0),breaks = seq(min(partial.dat.stack$date), max(partial.dat.stack$date), by = "24 months"), 
               labels = scales::date_format("%Y")) +
  labs(x = "Month", y="Partial Effect to NDVI", title = "All Years")+
  theme_bw()

figC2 <- ggplot(partial.dat.stack[partial.dat.stack$var %in% c("partial.Temp.date", "partial.Drought.date"),]) + facet_grid(landcover~.)+
  geom_hline(yintercept=0, linetype="dashed") +
  geom_line(aes(x=date, y=values, col=var), linewidth=0.8) +
  scale_color_manual(values = c("partial.Temp.date" = "#E69F00", 
                                "partial.Drought.date" = "#0072B2", 
                                "partial.Lag.date" = "#009E73")) +
  scale_x_date(expand = c(0,0),breaks = seq(min(partial.dat.stack$date), max(partial.dat.stack$date), by = "24 months"), 
               labels = scales::date_format("%Y")) +
  labs(x = "Month", y="Partial Effect to NDVI", title = "All Years")+
  theme_bw()

png(filename=file.path(path.figs,"figC1.png"), height=6, width=10, units="in", res=220)
print(figC1)
dev.off()
png(filename=file.path(path.figs,"figC2.png"), height=6, width=10, units="in", res=220)
print(figC2)
dev.off()

# drought year comparison plots

figC3 <- ggplot(partial.dat.stack[partial.dat.stack$year %in% c(2005,2012, 2021, 2023) & partial.dat.stack$var %in% pe.vars,]) + facet_grid(year~landcover)+
  geom_hline(yintercept=0, linetype="dashed") +
  geom_line(aes(x=yday, y=values, col=var), linewidth=0.8) +
  scale_color_manual(values = c("partial.Temp.date" = "#E69F00", 
                                "partial.Drought.date" = "#0072B2", 
                                "partial.Lag.date" = "#009E73")) +
  scale_x_continuous(
    breaks = c(1, 32, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335), 
    labels = month.abb
  ) +
  labs(x = "Month", y="Partial Effect to NDVI", title = "Raw Partial Effects 2005, 2011,2012")+
  theme_bw()+
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 45, hjust = 1))

figC3b <- ggplot(partial.dat.stack[partial.dat.stack$year %in% c(2005,2012, 2021, 2023) & partial.dat.stack$var %in% c("partial.Temp.date", "partial.Drought.date"),]) + facet_grid(year~landcover)+
  geom_hline(yintercept=0, linetype="dashed") +
  geom_line(aes(x=yday, y=values, col=var), linewidth=0.8) +
  scale_color_manual(values = c("partial.Temp.date" = "#E69F00", 
                                "partial.Drought.date" = "#0072B2", 
                                "partial.Lag.date" = "#009E73")) +
  scale_x_continuous(
    breaks = c(1, 32, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335), 
    labels = month.abb
  ) +
  labs(x = "Month", y="Partial Effect to NDVI", title = "Raw Partial Effects 2005, 2011,2012")+
  theme_bw()+
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 45, hjust = 1))

figC3c <- ggplot(partial.dat.stack[partial.dat.stack$month %in% c(4:10) & partial.dat.stack$year %in% c(2005,2012, 2021, 2023) & partial.dat.stack$var %in% c("partial.Temp.date", "partial.Drought.date"),]) + facet_grid(year~landcover)+
  geom_hline(yintercept=0, linetype="dashed") +
  geom_line(aes(x=yday, y=values, col=var), linewidth=0.8) +
  scale_color_manual(values = c("partial.Temp.date" = "#E69F00", 
                                "partial.Drought.date" = "#0072B2", 
                                "partial.Lag.date" = "#009E73")) +
  scale_x_continuous(
    breaks = c(1, 32, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335), 
    labels = month.abb
  ) +
  labs(x = "Month", y="Partial Effect to NDVI", title = "Raw Partial Effects 2005, 2011,2012")+
  theme_bw()+
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 45, hjust = 1))

# Scaled PE
figC4 <- ggplot(partial.dat.stack[partial.dat.stack$year %in% c(2005,2012, 2021, 2023) & partial.dat.stack$var %in% c("peTempStd.gsMean", "peDroughtStd.gsMean") & partial.dat.stack$month %in% c(4:10),]) + facet_grid(year~landcover)+
  geom_hline(yintercept=0, linetype="dashed") +
  geom_line(aes(x=yday, y=values, col=var), linewidth=0.8) +
  scale_color_manual(values = c("peTempStd.gsMean" = "#E69F00", 
                                "peDroughtStd.gsMean" = "#0072B2", 
                                "partial.Lag.date" = "#009E73")) +
  scale_x_continuous(
    breaks = c(1, 32, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335), 
    labels = month.abb
  ) +
  scale_y_continuous(labels = scales::label_percent()) +
  labs(x = "Month", y="PE as % Mean GS NDVI", title = "Stdandardized Partial Effects GS MEAN\nGROWING SEASON ONLY 2005, 2011,2012")+
  theme_bw()+
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 45, hjust = 1))



png(filename=file.path(path.figs,"figC3.png"), height=8, width=10, units="in", res=220)
print(figC3)
dev.off()

png(filename=file.path(path.figs,"figC3b.png"), height=8, width=10, units="in", res=220)
print(figC3b)
dev.off()

png(filename=file.path(path.figs,"figC3c.png"), height=8, width=10, units="in", res=220)
print(figC3c)
dev.off()

png(filename=file.path(path.figs,"figC4.png"), height=8, width=10, units="in", res=220)
print(figC4)
dev.off()


figC_ndvi <- ggplot(partial.dat.stack[partial.dat.stack$year %in% c(2005,2012, 2021, 2023) & partial.dat.stack$var %in% c("NDVI"),]) + facet_grid(year~landcover)+
  geom_hline(yintercept=0, linetype="dashed") +
  geom_tile(aes(x=yday, y=1, fill=values), linewidth=0.8) +
  scale_color_manual(values = c("partial.Temp.date" = "#E69F00", 
                                "partial.Drought.date" = "#0072B2", 
                                "partial.Lag.date" = "#009E73")) +
  scale_x_continuous(
    breaks = c(1, 32, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335), 
    labels = month.abb
  ) +
  labs(x = "Month", y="Partial Effect to NDVI", title = "Raw Partial Effects 2005, 2011,2012")+
  theme_bw()+
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 45, hjust = 1))

png(filename=file.path(path.figs,"figC_ndvi_check.png"), height=8, width=10, units="in", res=220)
print(figC_ndvi)
dev.off()


###############################
# Loading in SPEI model----
spei.pe <- read.csv(file.path(pathSave, paste0("DailyModel_FinalModel_modOutAdd1_Stats_dailyPartialEffects_AllLandcovers.csv")))
head(spei.pe)
# Figure C----
# Plot Partial Effects through Time

summary(spei.pe)

# need to reshape to stack daily partial effects
spei.partial.dat <- spei.pe[,c("partial.Drought.date", "partial.Temp.date", "partial.Lag.date", "NDVI", "peDroughtStd.gsMean", "peTempStd.gsMean", "peLagStd.gsMean")]
spei.partial.dat.stack <- stack(spei.partial.dat)
names(spei.partial.dat.stack) <- c("values", "var")

spei.partial.dat.stack$yday <- spei.pe$yday
spei.partial.dat.stack$landcover <- spei.pe$landcover
spei.partial.dat.stack$date <- as.Date(spei.pe$date)
head(spei.partial.dat.stack)
# adding in year and month from date
spei.partial.dat.stack$year <- lubridate::year(spei.partial.dat.stack$date)
spei.partial.dat.stack$month<- lubridate::month(spei.partial.dat.stack$date)
spei.partial.dat.stack$month.name<- lubridate::month(spei.partial.dat.stack$date, abb=T, label=T)


# Partial effects across multiple years
pe.vars <- c("partial.Temp.date", "partial.Drought.date", "partial.Lag.date")
partial.dat.stack$landcover <- factor(partial.dat.stack$landcover, c("crop", "forest", "forest-wet", "grassland", "urban-open", "urban-low", "urban-medium", "urban-high"))
spei.partial.dat.stack$landcover <- factor(spei.partial.dat.stack$landcover, c("crop", "forest", "forest-wet", "grassland", "urban-open", "urban-low", "urban-medium", "urban-high"))



# comparing the two partial effects
summary(spei.partial.dat.stack)
summary(partial.dat.stack)

partial.comp <- ggplot(partial.dat.stack[partial.dat.stack$year %in% c(2005,2012, 2021, 2023) & partial.dat.stack$var %in% c("peTempStd.gsMean", "peDroughtStd.gsMean") & partial.dat.stack$month %in% c(4:10),]) + facet_grid(year*var~landcover)+
  geom_hline(yintercept=0, linetype="dashed") +
  geom_line(aes(x=yday, y=values,col="SPEI_14day Model"), linewidth=0.8, ) +
  
  geom_line(data=spei.partial.dat.stack[spei.partial.dat.stack$year %in% c(2005,2012, 2021, 2023) & spei.partial.dat.stack$var %in% c("peTempStd.gsMean", "peDroughtStd.gsMean") & spei.partial.dat.stack$month %in% c(4:10),],
            aes(x=yday, y=values, col="SPI_30day Model"), linewidth=0.8) +
  scale_color_manual(values=c("SPEI_14day Model" = "orchid4", "SPI_30day Model"= "forestgreen")) +
  
  scale_x_continuous(
    breaks = c(1, 32, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335), 
    labels = month.abb
  ) +
  scale_y_continuous(labels = scales::label_percent()) +
  labs(x = "Month", y="PE as % Mean GS NDVI", title = "Comparison of SPI30 and SPEI14 model partial effects")+
  theme_bw()+
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 45, hjust = 1))


png(filename=file.path(path.figs,"ADD1_vs_Add3_partialEffects.png"), height=10, width=10, units="in", res=220)
print(partial.comp)
dev.off()

#############################
# RMSE Comparison----

modStatsAll <- read.csv(file.path(google.drive, "data/processed_files/ModelSelection-Multivariate", paste0("DailyModel_VarSelection-Multivariate_ModelStats-ALL.csv")))
head(modStatsAll)
unique(modStatsAll$model)

model.list <- c("SPEI30-Tmax_14day-additive", "SPI30day-Tmax_30day-additive")

mod13.stats <- modStatsAll[modStatsAll$model %in% model.list,]
mod13.stats$landcover <- factor(mod13.stats$landcover, c("crop", "forest","forest-wet", "grassland", "urban-open", "urban-low", "urban-medium", "urban-high"))
summary(mod13.stats)

model_stats <- read.csv(file.path(pathSave, paste0("DailyModel_FinalModel_modOutAdd1_Stats_climateNormPartialEffects_AllLandcovers.csv")))
summary(model_stats)
head(model_stats)
mod13.stats$rmse.std <- NA
for(i in unique(mod13.stats$model)){
  mod.temp <- mod13.stats[mod13.stats$model==i,]
  for(j in unique(mod13.stats$landcover)){
    mod.lc.temp <- mod.temp[mod.temp$landcover==j,]
    for(k in unique(mod13.stats$yday)){
      temp.df <- mod.lc.temp[mod.lc.temp$yday==k,]
      ndvi.temp <- model_stats[model_stats$landcover==j & model_stats$yday==k, "NDVI.norm"]
      
      mod13.stats[mod13.stats$model==i & mod13.stats$landcover==j & mod13.stats$yday== k, "rmse.std"] <- temp.df$RMSE/ndvi.temp
      
    }
  }
}

rmse.comp <- ggplot(data=mod13.stats) + facet_wrap(landcover~.) +
  geom_line(aes(x=yday, y=RMSE, col=model), linewidth=0.75) +
  geom_hline(yintercept=0, linetype="dashed") +
  theme_bw()+
  #scale_y_continuous(limits=c(0,0.135)) +
  # coord_cartesian(ylim=c(0,2)) +
  scale_x_continuous(expand=c(0,0),
    breaks = c(1, 32, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335), 
    labels = unique(partial.dat.stack$month.name)
  )


png(filename=file.path(path.figs,"ADD1_vs_Add3_RMSE Comparison.png"), height=10, width=10, units="in", res=220)
print(rmse.comp)
dev.off()

# # # # # # # # # # # # # #
# Directional Error Comparison----

SPEImodel_stats <- read.csv(file.path(pathSave, paste0("DailyModel_FinalModel_modOutAdd1_Stats_climateNormPartialEffects_AllLandcovers.csv")))
summary(SPEImodel_stats)

SPEImodel_stats$modelForm <- paste(SPEImodel_stats$TempVar, SPEImodel_stats$DroughtVar, sep="-")

SPImodel_stats <- read.csv(file.path(pathSave, paste0("DailyModel_FinalModel_modOutAdd3_Stats_climateNormPartialEffects_AllLandcovers.csv")))
summary(SPImodel_stats)

SPImodel_stats$modelForm <- paste(SPImodel_stats$TempVar, SPImodel_stats$DroughtVar, sep="-")


err.comp <- ggplot() + facet_grid(landcover~.) +
  geom_line(data = SPEImodel_stats, aes(x=yday, y=Error, col="SPEI_14day"), linewidth=0.75) +
  geom_line(data = SPImodel_stats, aes(x=yday, y=Error, col="SPI_30day"), linewidth=0.75) +
  geom_hline(yintercept=0, linetype="dashed") +
  scale_color_manual(values = c("SPEI_14day" = "orchid4", "SPI_30day" = "forestgreen" )) +
  theme_bw()+
  #scale_y_continuous(limits=c(0,0.135)) +
  # coord_cartesian(ylim=c(0,2)) +
  scale_x_continuous(expand=c(0,0),
                     breaks = c(1, 32, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335), 
                     labels = unique(partial.dat.stack$month.name))


model.diff <- SPEImodel_stats[,c("yday", "landcover")]

for(i in unique(SPEImodel_stats$landcover)){
  for(j in unique(SPEImodel_stats$yday)){
    temp.spei <- SPEImodel_stats[SPEImodel_stats$landcover==i & SPEImodel_stats$yday==j,]
    temp.spi <- SPImodel_stats[SPImodel_stats$landcover==i & SPImodel_stats$yday==j,]
    
    model.diff[model.diff$landcover==i & model.diff$yday==j, "errorDiff"] <- temp.spei$Error - temp.spi$Error
    model.diff[model.diff$landcover==i & model.diff$yday==j, "errorCol"] <- ifelse((temp.spei$Error - temp.spi$Error) > 0, "SPEI", "SPI")
  }
}

err.diff<- ggplot(data = model.diff) + 
  facet_grid(landcover ~ .) +
  
  # Use a rect to span entire y-axis with color cues for errorCol
  geom_rect(
    aes(xmin = yday - 0.5, xmax = yday + 0.5,
        ymin = -Inf, ymax = Inf, fill = errorCol),
    alpha = 0.75
  ) +
  
  # Now your errorDiff line will always show clearly
  geom_line(aes(x = yday, y = errorDiff), linewidth = 0.75, color = "black") +
  
  geom_hline(yintercept = 0, linetype = "dashed") +
  
  scale_fill_manual(values = c("SPEI" = "orchid4", "SPI" = "forestgreen"), name="Larger Error") +
  
  theme_bw() +
  
  scale_x_continuous(
    expand = c(0, 0),
    breaks = c(1, 32, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335),
    labels = unique(partial.dat.stack$month.name)
  )


png(filename=file.path(path.figs,"ADD1_vs_Add3_Error_Comparison.png"), height=10, width=10, units="in", res=220)
print(err.comp)
dev.off()

png(filename=file.path(path.figs,"ADD1_vs_Add3_errorDifferenceComparison.png"), height=10, width=10, units="in", res=220)
print(err.diff)
dev.off()