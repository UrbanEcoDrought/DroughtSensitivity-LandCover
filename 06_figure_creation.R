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
path.figs <- file.path(google.drive, "exploratory figures/FinalDailyModel")
pathSave <- file.path(google.drive, "data/processed_files/FinalDailyModel")

daily.pe <- read.csv(file.path(pathSave, paste0("DailyModel_FinalModel_modOutAdd1_Stats_dailyPartialEffects_AllLandcovers.csv")))

# reading in data----
# starting with the output file created in script 05_NDVI_MET_DilyModels-Final.R

# Step 1: Read the model statistics CSV
model_stats <- read.csv(file.path(pathSave, paste0("DailyModel_FinalModel_modOutAdd1_Stats_climateNormPartialEffects_AllLandcovers.csv")))
summary(model_stats)
head(model_stats)

# Step 2: Replace non-significant t-values with NA
model_stats2 <- model_stats

model_stats2$tVal.Lag[model_stats$pVal.Lag > 0.05] <- NA
model_stats2$tVal.Drought[model_stats$pVal.Drought > 0.05] <- NA
model_stats2$tVal.Temp[model_stats$pVal.Temp > 0.05] <- NA

# Select relevant columns
significant_stats <- model_stats2[, c("landcover", "yday", "tVal.Lag", "tVal.Drought", "tVal.Temp")]

# Step 3: Reshape the data into long format, including yday
significant_stats_long <- data.frame(
  LandCoverType = rep(significant_stats$landcover, 3),
  yday = rep(significant_stats$yday, 3),
  Variable = rep(c("tVal_Lag", "tVal_Drought", "tVal_Temp"), each = nrow(significant_stats)),
  t_stat_value = c(significant_stats$tVal.Lag, 
                   significant_stats$tVal.Drought, 
                   significant_stats$tVal.Temp)
)

summary(significant_stats_long)
# View the first few rows of the reshaped data with yday
head(significant_stats_long)

# Figure A----
# Filter significant t-stats (alpha = 0.05)
# Create categorical bins for t_stat_value using the cut function
significant_stats_long$t_stat_category <- cut(
  significant_stats_long$t_stat_value, 
  breaks = c(-Inf, -4, -2, 0, 2, 4, Inf),  # Define 6 bins, each with 5 intervals
  labels = c("Very Negative", "Negative", "mid.neg","mid.pos",  "Positive", "Very Positive"),  # Only 5 labels
  right = TRUE
)

# creating a filler date to get the months in there
significant_stats_long$Date <- as.Date(significant_stats_long$yday - 1, origin = "2000-01-01")

significant_stats_long$Variable <- factor(significant_stats_long$Variable, levels = c("tVal_Lag", "tVal_Drought", "tVal_Temp"))

figA <- ggplot(data = significant_stats_long) + 
  facet_grid(LandCoverType~.) +
  geom_tile(aes(x = Date, y = Variable, fill = t_stat_category)) +
  scale_fill_manual(values = c("Very Negative" = "#67001F", 
                               "Negative" = "#D7301F", 
                               "mid.neg" = "#F4A300", 
                               "mid.pos" = "#4E9F00", 
                               "Positive" = "#006F00", 
                               "Very Positive" = "#003300")) +
  scale_x_date(labels = scales::date_format("%b"), breaks = "1 month") + # Format x-axis with months
  labs(fill = "t-stat Value") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

png(filename="figures/figA.png", height=6, width=10, units="in", res=220)
print(figA)
dev.off()

# Figure B----
# Plot Figure 2: RMSE through DOY
# note: I think we will want to scale this by the mean daily NDVI data to help with comparisons.


# set color palette for the land cover types
LC_color_palette <- setNames(
  c("darkorange3", "darkgreen", "navajowhite1", "darkred", "red", "indianred", "lightpink3"),
  c("crop", "forest", "grassland", "urban-high", "urban-medium", "urban-low", "urban-open")
)

rmse_fig_dat <- model_stats
# creating a filler date to get the months in there
rmse_fig_dat$Date <- as.Date(rmse_fig_dat$yday - 1, origin = "2000-01-01")
summary(rmse_fig_dat)


figB<- ggplot(data=rmse_fig_dat) + facet_wrap(landcover~., scales="free") +
  geom_line(aes(x=Date, y=RMSE/NDVI.norm, col=landcover), linewidth=0.9) +
  # geom_smooth(aes(x=Date, y=RMSE/NDVI.mean, col=landcover), method="loess", se=T, span=0.65) +  # Adding the smooth line (using loess method)
  scale_color_manual(values=LC_color_palette) +
  scale_x_date(labels = scales::date_format("%b"), breaks = "1 month") + # Format x-axis with months
  theme_bw()

png(filename="figures/figB.png", height=6, width=10, units="in", res=220)
print(figB)
dev.off()
# Figure C----
# Plot Partial Effects through Time

summary(daily.pe)
mean.daily.pe <- aggregate(partial.Drought.date~landcover+yday, data=daily.pe, FUN=mean)
mean.daily.pe$partial.Temp.date <- aggregate(partial.Temp.date~landcover+yday, data=daily.pe, FUN=mean)$partial.Temp.date
mean.daily.pe$partial.Lag.date <- aggregate(partial.Lag.date~landcover+yday, data=daily.pe, FUN=mean)$partial.Lag.date
# names(mean.daily.pe) <- c("landcover", "yday", "mean.pe.Drought", "mean.pe.Temp", "mean.pe.Lag")
names(mean.daily.pe) <- c("landcover", "yday", "partial.Drought.date", "partial.Temp.date", "partial.Lag.date")

mean.daily.pe.stack <- stack(mean.daily.pe[,c("partial.Drought.date", "partial.Temp.date", "partial.Lag.date")])
mean.daily.pe.stack$landcover <- mean.daily.pe$landcover
mean.daily.pe.stack$yday <- mean.daily.pe$yday
names(mean.daily.pe.stack) <- c("peMean", "var", "landcover", "yday")
# calculating SE for the daily PE
se <- function(x) sd(x) / sqrt(length(x))
se.daily.pe <- aggregate(partial.Drought.date~landcover+yday, data=daily.pe, FUN=se)
se.daily.pe$partial.Temp.date <- aggregate(partial.Temp.date~landcover+yday, data=daily.pe, FUN=se)$partial.Temp.date
se.daily.pe$partial.Lag.date <- aggregate(partial.Lag.date~landcover+yday, data=daily.pe, FUN=se)$partial.Lag.date
# names(se.daily.pe) <- c("landcover", "yday", "se.pe.Drought", "se.pe.Temp", "se.pe.Lag")
names(se.daily.pe) <- c("landcover", "yday", "partial.Drought.date", "partial.Temp.date", "partial.Lag.date")

se.daily.pe.stack <- stack(se.daily.pe[,c("partial.Drought.date", "partial.Temp.date", "partial.Lag.date")])
se.daily.pe.stack$landcover <- se.daily.pe$landcover
se.daily.pe.stack$yday <- se.daily.pe$yday
head(se.daily.pe.stack)
names(se.daily.pe.stack) <- c("peStErr", "var", "landcover", "yday")

mean.daily.pe.plot <- merge(mean.daily.pe.stack, se.daily.pe.stack, by=c("landcover", "yday", "var"))
head(mean.daily.pe.plot)
# creating upper and lower bound for ribbons
mean.daily.pe.plot$peUB <- mean.daily.pe.plot$peMean + mean.daily.pe.plot$peStErr
mean.daily.pe.plot$peLB <- mean.daily.pe.plot$peMean - mean.daily.pe.plot$peStErr

head(mean.daily.pe.plot)

# need to reshape to stack partial effects
partial.dat <- daily.pe[,c("partial.Drought.date", "partial.Temp.date", "partial.Lag.date")]
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
# trying some plots

# mean effects overall yday
figC1 <- ggplot(mean.daily.pe.plot) + facet_wrap(landcover~., scales="free")+
  geom_hline(yintercept=0, linetype="dashed") +
  #geom_ribbon(aes(x=yday, ymin=peLB, ymax=peUB, col=var), alpha=0.55) +
  geom_line(aes(x=yday, y= peMean, col=var)) +
  scale_color_manual(values = c("partial.Temp.date" = "#E69F00", 
                                "partial.Drought.date" = "#0072B2", 
                                "partial.Lag.date" = "#009E73"))

figC2 <- ggplot(mean.daily.pe.plot[!mean.daily.pe.plot$var %in% "partial.Lag.date",]) + facet_wrap(landcover~., scales="free")+
  geom_hline(yintercept=0, linetype="dashed") +
  #geom_ribbon(aes(x=yday, ymin=peLB, ymax=peUB, col=var), alpha=0.55) +
  geom_line(aes(x=yday, y= peMean, col=var)) +
  scale_color_manual(values = c("partial.Temp.date" = "#E69F00", 
                                "partial.Drought.date" = "#0072B2", 
                                "partial.Lag.date" = "#009E73"))

png(filename="figures/figC1.png", height=6, width=10, units="in", res=220)
print(figC1)
dev.off()
png(filename="figures/figC2.png", height=6, width=10, units="in", res=220)
print(figC2)
dev.off()

# all years
ggplot(partial.dat.stack) + facet_grid(var~landcover, scales="free")+
  geom_hline(yintercept=0, linetype="dashed") +
  geom_line(aes(x=date, y=values, col=var)) +
  scale_color_manual(values = c("partial.Temp.date" = "#E69F00", 
                                "partial.Drought.date" = "#0072B2", 
                                "partial.Lag.date" = "#009E73")) +
  scale_x_date(expand = c(0,0),breaks = seq(min(partial.dat.stack$date), max(partial.dat.stack$date), by = "24 months"), 
               labels = scales::date_format("%Y")) +
  labs(x = "Month", y="Partial Effect to NDVI", title = "All Years")


# plotting 2005
ggplot(partial.dat.stack[partial.dat.stack$year %in% 2005,]) + facet_wrap(landcover~., scales="free")+
  geom_hline(yintercept=0, linetype="dashed") +
  geom_line(aes(x=yday, y=values, col=var)) +
  scale_color_manual(values = c("partial.Temp.date" = "#E69F00", 
                                "partial.Drought.date" = "#0072B2", 
                                "partial.Lag.date" = "#009E73")) +
  scale_x_continuous(
    breaks = c(1, 32, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335), 
    labels = month.abb
  ) +
  labs(x = "Month", y="Partial Effect to NDVI", title = 2005)

ggplot(partial.dat.stack[partial.dat.stack$year %in% 2005 & !partial.dat.stack$var %in% "partial.Lag.date",]) + facet_wrap(landcover~., scales="free")+
  geom_hline(yintercept=0, linetype="dashed") +
  geom_line(aes(x=yday, y=values, col=var)) +
  scale_color_manual(values = c("partial.Temp.date" = "#E69F00", 
                                "partial.Drought.date" = "#0072B2", 
                                "partial.Lag.date" = "#009E73")) +
  scale_x_continuous(
    breaks = c(1, 32, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335), 
    labels = month.abb
  ) +
  labs(x = "Month", y="Partial Effect to NDVI", title = 2005)


# Plotting 2012
  ggplot(partial.dat.stack[partial.dat.stack$year %in% 2012,]) + facet_wrap(landcover~., scales="free")+
    geom_hline(yintercept=0, linetype="dashed") +
    geom_line(aes(x=yday, y=values, col=var)) +
    scale_color_manual(values = c("partial.Temp.date" = "#E69F00", 
                                  "partial.Drought.date" = "#0072B2", 
                                  "partial.Lag.date" = "#009E73")) +
    scale_x_continuous(
      breaks = c(1, 32, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335), 
      labels = month.abb
    ) +
    labs(x = "Month", y="Partial Effect to NDVI", title=2012)

  # plotting 2023
  ggplot(partial.dat.stack[partial.dat.stack$year %in% 2023,]) + facet_wrap(landcover~., scales="free")+
    geom_hline(yintercept=0, linetype="dashed") +
    geom_line(aes(x=yday, y=values, col=var)) +
    scale_color_manual(values = c("partial.Temp.date" = "#E69F00", 
                                  "partial.Drought.date" = "#0072B2", 
                                  "partial.Lag.date" = "#009E73")) +
    scale_x_continuous(
      breaks = c(1, 32, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335), 
      labels = month.abb
    ) +
    labs(x = "Month", y="Partial Effect to NDVI", title=2023)
  
  ggplot(partial.dat.stack[partial.dat.stack$year %in% 2012,]) + facet_wrap(landcover~., scales="free")+
    geom_hline(yintercept=0, linetype="dashed") +
    geom_line(aes(x=yday, y=values, col=var)) +
    scale_color_manual(values = c("partial.Temp.date" = "#E69F00", 
                                  "partial.Drought.date" = "#0072B2", 
                                  "partial.Lag.date" = "#009E73")) +
    scale_x_continuous(
      breaks = c(1, 32, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335), 
      labels = month.abb
    ) +
    labs(x = "Month", y="Partial Effect to NDVI", title=2012)
  
  # plotting 2012
  ggplot(partial.dat.stack[partial.dat.stack$year %in% 2012 & !partial.dat.stack$var %in% "partial.Lag.date",]) + facet_wrap(landcover~.)+
    geom_hline(yintercept=0, linetype="dashed") +
    geom_line(aes(x=yday, y=values, col=var)) +
    scale_color_manual(values = c("partial.Temp.date" = "#E69F00", 
                                  "partial.Drought.date" = "#0072B2", 
                                  "partial.Lag.date" = "#009E73")) +
    scale_x_continuous(
      breaks = c(1, 32, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335), 
      labels = month.abb
    ) +
    labs(x = "Month", y="Partial Effect to NDVI", title=2012)
  
  # drought year comparison
  ggplot(partial.dat.stack[partial.dat.stack$year %in% c(2005, 2012),]) + facet_grid(var~landcover)+
    geom_hline(yintercept=0, linetype="dashed") +
    geom_line(aes(x=yday, y=values, col=as.factor(year))) +
    # scale_color_manual(values = c("partial.Temp.date" = "#E69F00", 
    #                               "partial.Drought.date" = "#0072B2", 
    #                               "partial.Lag.date" = "#009E73")) +
    scale_x_continuous(
      breaks = c(1, 32, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335), 
      labels = month.abb
    ) +
    labs(x = "Month", y="Partial Effect to NDVI", title="2005:2012 Comparison") +
    theme_bw()
  