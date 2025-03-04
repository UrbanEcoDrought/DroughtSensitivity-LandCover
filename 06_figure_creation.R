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


# reading in data----
# starting with the output file created in script 05_NDVI_MET_DilyModels-Final.R

# Step 1: Read the model statistics CSV
model_stats <- read.csv(file.path(pathSave, paste0("DailyModel_FinalModel_Stats_PartialEffects_AllLandcovers.csv")))
summary(model_stats)
head(model_stats)

# Step 2: Filter rows where p-values for the variables are less than or equal to 0.05 (significant results)
significant_stats <- model_stats[model_stats$pVal.Lag <= 0.05 | model_stats$pVal.Drought <= 0.05 | model_stats$pVal.Temp <= 0.05, 
                                 c("landcover", "yday", "tVal.Lag", "tVal.Drought", "tVal.Temp", "pVal.Lag", "pVal.Drought", "pVal.Temp")]

# Step 3: Reshape the data into long format, including yday
significant_stats_long <- data.frame(
  LandCoverType = rep(significant_stats$landcover, 3),  # Repeat LandCoverType for each variable
  yday = rep(significant_stats$yday, 3),  # Repeat yday for each variable
  Variable = rep(c("tVal_Lag", "tVal_Drought", "tVal_Temp"), each = nrow(significant_stats)),  # Variable names
  t_stat_value = c(significant_stats$tVal.Lag, 
                   significant_stats$tVal.Drought, 
                   significant_stats$tVal.Temp),  # Combine the t-stat values into one vector
  p_value = c(significant_stats$pVal.Lag, 
              significant_stats$pVal.Drought, 
              significant_stats$pVal.Temp)  # Corresponding p-values for the t-statistics
)

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

ggplot(data = significant_stats_long) + 
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


ggplot(data=rmse_fig_dat) + facet_wrap(landcover~., scales="free") +
  geom_line(aes(x=Date, y=RMSE/NDVI.mean, col=landcover), linewidth=0.9) +
  # geom_smooth(aes(x=Date, y=RMSE/NDVI.mean, col=landcover), method="loess", se=T, span=0.65) +  # Adding the smooth line (using loess method)
  scale_color_manual(values=LC_color_palette) +
  scale_x_date(labels = scales::date_format("%b"), breaks = "1 month") + # Format x-axis with months
  theme_bw()


# Figure C----
# Plot Partial Effects through Time

summary(model_stats)

# need to reshape to stack partial effects
partial.dat <- model_stats[,c("partial.SPEI30", "partial.TMAX30", "partial.Lag")]
partial.dat.stack <- stack(partial.dat)
names(partial.dat.stack) <- c("values", "var")

partial.dat.stack$yday <- model_stats$yday
partial.dat.stack$landcover <- model_stats$landcover

# adding in date variable as a filler to get the month names
partial.dat.stack$date <- as.Date(partial.dat.stack$yday - 1, origin = "2000-01-01")

# trying some plots

forest.part <- partial.dat.stack[partial.dat.stack$landcover %in% "forest",]
ggplot(data=forest.part) +
  geom_line(aes(x=date, y=values, color=var)) +
  scale_color_manual(values=c("partial.SPEI30" = "blue", "partial.TMAX30" = "red3", "partial.Lag"="forestgreen")) +
  scale_x_date(labels = scales::date_format("%b"), breaks = "1 month") + # Format x-axis with months
  theme_bw()


ggplot(data=forest.part[!forest.part$var %in% "partial.Lag",]) +
  geom_line(aes(x=date, y=values, color=var)) +
  scale_color_manual(values=c("partial.SPEI30" = "blue", "partial.TMAX30" = "red3", "partial.Lag"="forestgreen")) +
  scale_x_date(labels = scales::date_format("%b"), breaks = "1 month") + # Format x-axis with months
  theme_bw()


ggplot(data=partial.dat.stack) + facet_wrap(landcover~.) +
  geom_line(aes(x=date, y=values, color=var)) +
  scale_color_manual(values=c("partial.SPEI30" = "blue", "partial.TMAX30" = "red3", "partial.Lag"="forestgreen")) +
  scale_x_date(labels = scales::date_format("%b"), breaks = "1 month") + # Format x-axis with months
  theme_bw()


ggplot(data=partial.dat.stack[!partial.dat.stack$var %in% "partial.Lag",]) +  facet_wrap(landcover~.) +
  geom_line(aes(x=date, y=values, color=var)) +
  scale_color_manual(values=c("partial.SPEI30" = "blue", "partial.TMAX30" = "red3", "partial.Lag"="forestgreen")) +
  scale_x_date(labels = scales::date_format("%b"), breaks = "1 month") + # Format x-axis with months
  theme_bw()

