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

daily.pe <- read.csv(file.path(pathSave, paste0("DailyModel_FinalModel_modOutAdd1_Stats_dailyPartialEffects_AllLandcovers.csv")))

# # # # # # # # # # # # # # # # # # # # 
# setting a theme
theme_pub <- function(base_size = 11, base_family = "sans") {
  theme_minimal(base_size = base_size, base_family = base_family) +
    theme(
      # Axis
      axis.title = element_text(size = base_size, face = "bold"),
      axis.text = element_text(size = base_size - 1),
      axis.line = element_line(color = "black"),
      axis.ticks = element_line(color = "black"),
      # Panel and Grid
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.border = element_blank(),
      # Plot
      plot.title = element_text(size = base_size + 2, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = base_size, hjust = 0.5),
      plot.caption = element_text(size = base_size - 2, hjust = 1),
      # Legend
      legend.title = element_text(size = base_size, face = "bold"),
      legend.text = element_text(size = base_size - 1),
      legend.key = element_blank(),
      legend.position = "bottom",
      legend.direction = "horizontal",
      legend.box = "horizontal",
      legend.box.just = "center",
      # Force one row
      legend.spacing.x = unit(0.4, "cm"),
      # Strip (facet labels)
      strip.text = element_text(size = base_size, face = "bold"),
      strip.background = element_rect(fill = "gray90", color = NA)
    )
}


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
significant_stats_long$in.gs <- ifelse(significant_stats_long$yday >=91 & significant_stats_long$yday <=304, "Yes", "No")

significant_stats_long$LandCoverType <- factor(significant_stats_long$LandCoverType, c("crop", "forest", "grassland", "urban-open", "urban-low", "urban-medium", "urban-high"))

figA <- ggplot(data = significant_stats_long) + 
  facet_grid(LandCoverType~.) +
  geom_tile(aes(x = Date, y = Variable, fill = t_stat_category, alpha = in.gs)) +
  scale_alpha_manual(values = c("No" = 0.75, "Yes" = 1), guide = "none") +  # Dim non-growing season
  scale_fill_manual(values = c("Very Negative" = "#762a83", 
                               "Negative" = "#af8dc3", 
                               "mid.neg" = "#e7d4e8", 
                               "mid.pos" = "#d9f0d3", 
                               "Positive" = "#7fbf7b", 
                               "Very Positive" = "#1b7837"), name="t-Stat Category") +
  scale_x_date(labels = scales::date_format("%b"), breaks = "1 month") + # Format x-axis with months
  guides(fill = guide_legend(nrow = 1), color = guide_legend(nrow = 1)) +
  labs(fill = "t-stat Value") +
  theme_pub()
  
  # theme(axis.text.x = element_text(angle = 90, hjust = 1))

png(filename=file.path(path.figs,"figA.png"), height=6, width=10, units="in", res=220)
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
rmse_fig_dat$landcover <- factor(rmse_fig_dat$landcover, c("crop", "forest", "grassland", "urban-open", "urban-low", "urban-medium", "urban-high"))

figB<- ggplot(data=rmse_fig_dat) + #facet_wrap(landcover~., scales="free") +
  geom_vline(xintercept = c(as.Date("2000-03-01"), as.Date("2000-10-31")), linetype="dashed") +
  geom_line(aes(x=Date, y=RMSE/NDVI.norm, col=landcover), linewidth=0.9) +
  # geom_smooth(aes(x=Date, y=RMSE/NDVI.mean, col=landcover), method="loess", se=T, span=0.65) +  # Adding the smooth line (using loess method)
  scale_color_manual(values=LC_color_palette, name="Land Cover") +
  scale_x_date(labels = scales::date_format("%b"), breaks = "1 month") + # Format x-axis with months
  theme_pub()


png(filename=file.path(path.figs,"figB.png"), height=6, width=10, units="in", res=220)
print(figB)
dev.off()

figB2 <- ggplot(data=rmse_fig_dat) + facet_grid(landcover~.) +
  geom_vline(xintercept = c(as.Date("2000-03-01"), as.Date("2000-10-31")), linetype="dashed") +
  geom_hline(yintercept=0, linetype="solid", col="black") +
  geom_line(aes(x=Date, y=Error, col=landcover), linewidth=0.9) +
  # geom_smooth(aes(x=Date, y=RMSE/NDVI.mean, col=landcover), method="loess", se=T, span=0.65) +  # Adding the smooth line (using loess method)
  scale_color_manual(values=LC_color_palette, name = "Land Cover") +
  scale_x_date(labels = scales::date_format("%b"), breaks = "1 month") + # Format x-axis with months
  theme_bw() +
  theme(legend.position = "bottom")

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
  theme_pub()

figC2 <- ggplot(partial.dat.stack[partial.dat.stack$var %in% c("partial.Temp.date", "partial.Drought.date"),]) + facet_grid(landcover~.)+
  geom_hline(yintercept=0, linetype="dashed") +
  geom_line(aes(x=date, y=values, col=var), linewidth=0.8) +
  scale_color_manual(values = c("partial.Temp.date" = "#E69F00", 
                                "partial.Drought.date" = "#0072B2", 
                                "partial.Lag.date" = "#009E73")) +
  scale_x_date(expand = c(0,0),breaks = seq(min(partial.dat.stack$date), max(partial.dat.stack$date), by = "24 months"), 
               labels = scales::date_format("%Y")) +
  labs(x = "Month", y="Partial Effect to NDVI", title = "All Years")+
  theme_pub()

png(filename=file.path(path.figs,"figC1.png"), height=6, width=10, units="in", res=220)
print(figC1)
dev.off()
png(filename=file.path(path.figs,"figC2.png"), height=6, width=10, units="in", res=220)
print(figC2)
dev.off()

# drought year comparison plots

figC3 <- ggplot(partial.dat.stack[partial.dat.stack$year %in% c(2005,2011,2012) & partial.dat.stack$var %in% pe.vars,]) + facet_grid(year~landcover)+
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
  theme_pub() +
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 45, hjust = 1))

figC3b <- ggplot(partial.dat.stack[partial.dat.stack$year %in% c(2005,2011,2012) & partial.dat.stack$var %in% c("partial.Temp.date", "partial.Drought.date"),]) + facet_grid(year~landcover)+
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
  theme_pub() +
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 45, hjust = 1))

figC3c <- ggplot(partial.dat.stack[partial.dat.stack$month %in% c(4:10) & partial.dat.stack$year %in% c(2005,2011,2012) & partial.dat.stack$var %in% c("partial.Temp.date", "partial.Drought.date"),]) + facet_grid(year~landcover)+
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
  theme_pub
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 45, hjust = 1))

# Scaled PE
  partial.dat.stack$in.gs <- ifelse(partial.dat.stack$yday >=91 & partial.dat.stack$yday <=304, "Yes", "No")
  

  figC4 <- ggplot(partial.dat.stack[partial.dat.stack$year %in% c(2005,2011, 2012) & partial.dat.stack$var %in% c("peTempStd.gsMean", "peDroughtStd.gsMean"),]) + facet_grid(year~landcover)+
  geom_vline(xintercept = c(91, 305), linetype="dotted") +
  geom_hline(yintercept=0, linetype="dashed") +
  geom_line(data = partial.dat.stack[partial.dat.stack$year %in% c(2005,2011, 2012) & partial.dat.stack$var %in% c("peTempStd.gsMean", "peDroughtStd.gsMean") & partial.dat.stack$yday %in% c(1:90),],aes(x=yday, y=values, col=var, alpha=in.gs), linewidth=0.8) +
  geom_line(data = partial.dat.stack[partial.dat.stack$year %in% c(2005,2011, 2012) & partial.dat.stack$var %in% c("peTempStd.gsMean", "peDroughtStd.gsMean") & partial.dat.stack$yday %in% c(61:304),], aes(x=yday, y=values, col=var, alpha=in.gs), linewidth=0.8) +
  geom_line(data = partial.dat.stack[partial.dat.stack$year %in% c(2005,2011, 2012) & partial.dat.stack$var %in% c("peTempStd.gsMean", "peDroughtStd.gsMean") & partial.dat.stack$yday %in% c(305:365),], aes(x=yday, y=values, col=var, alpha=in.gs), linewidth=0.8) +
  scale_alpha_manual(values = c("No" = 0.50, "Yes" = 1), guide = "none") +  # Dim non-growing season
  scale_color_manual(values = c("peTempStd.gsMean" = "#E69F00", 
                                "peDroughtStd.gsMean" = "#0072B2", 
                                "partial.Lag.date" = "#009E73")) +
  scale_x_continuous(
    breaks = c(1, 32, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335), 
    labels = month.abb
  ) +
  scale_y_continuous(labels = scales::label_percent()) +
  labs(x = "Month", y="PE as % Mean GS NDVI", title = "Stdandardized Partial Effects GS MEAN\nGROWING SEASON ONLY 2005, 2011,2012")+
  theme_pub()+
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

png(filename=file.path(path.figs,"figC4.png"), height=8, width=15, units="in", res=300)
print(figC4)
dev.off()


figC_ndvi <- ggplot(partial.dat.stack[partial.dat.stack$year %in% c(2005,2011,2012) & partial.dat.stack$var %in% c("NDVI"),]) + facet_grid(year~landcover)+
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

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# LandSat NDVI coverage figure

# want to make a plot with x = date, y = mission
# have different colored bars for the coverage of landSAT data
# have bars filled by the data coverage

# may also just show histogram with monthly breaks
ndvi.all <- readRDS(file=file.path(google.drive, "data/processed_files", "landcover_ndviAll.RDS"))


ggplot(data=ndvi.all) +
  geom_density(aes(x=date, y=..scaled..,col=mission))

##################
# Satellite density plots----
library(dplyr)
library(ggplot2)
library(lubridate)

ndvi.all <- readRDS(file.path(google.drive, "data/processed_files", "landcover_ndviAll.RDS"))

# Step 1: Bin dates (monthly here, but adjust as needed)
ndvi.binned <- ndvi.all %>%
  filter(!is.na(NDVI)) %>%
  mutate(
    date_bin = floor_date(date, unit = "month")
  ) %>%
  group_by(mission, date_bin) %>%
  summarize(n_obs = n(), .groups = "drop")

# Step 2: Normalize observation counts per mission
ndvi.binned <- ndvi.binned %>%
  group_by(mission) %>%
  mutate(density_scaled = n_obs / max(n_obs)) %>%
  ungroup()

# Step 3: Plot heat strips using geom_tile()
mapFigDens <- ggplot(ndvi.binned, aes(x = date_bin, y = stringr::str_to_title(mission), fill = density_scaled)) +
  geom_tile(color = NA, height = 0.8) +
  scale_fill_viridis_c(name = "Scaled Obs Density", option = "viridis") +
  labs(x = "Date", y = "Landsat Mission") +
  theme_pub()

png(filename=file.path(path.figs,"fig1_satellite_density.png"), height=8, width=10, units="in", res=220)
print(mapFigDens)
dev.off()


# ## # # # # # # # # # # # # # # # 
# Satellite count plots----
library(dplyr)
library(ggplot2)
library(lubridate)
library(stringr)

ndvi.all <- readRDS(file.path(google.drive, "data/processed_files", "landcover_ndviAll.RDS"))

# Step 1: Deduplicate based only on mission and date
ndvi.unique <- ndvi.all %>%
  filter(!is.na(NDVI)) %>%
  select(mission, date) %>%
  distinct()  # Now truly one row per unique image acquisition

ndvi.unique <- ndvi.unique[order(ndvi.unique$date, decreasing = F),]

# Step 2: Bin dates and count
ndvi.binned <- ndvi.unique %>%
  mutate(date_bin = floor_date(date, unit = "month")) %>%
  group_by(mission, date_bin) %>%
  summarize(n_obs = n(), .groups = "drop") %>%
  mutate(mission = str_to_title(mission))  # Optional: title case

# Step 3: Plot using counts
mapFigCount <- ggplot(ndvi.binned, aes(x = date_bin, y = stringr::str_to_title(mission), fill = n_obs)) +
  geom_tile(color = NA, height = 0.8) +
  scale_fill_viridis_c(name = "Observation Count", option = "viridis") +
  labs(x = "Date", y = "Landsat Mission") +
  theme_pub()



png(filename=file.path(path.figs,"fig1_satellite_count.png"), height=8, width=10, units="in", res=220)
print(mapFigCount)
dev.off()
