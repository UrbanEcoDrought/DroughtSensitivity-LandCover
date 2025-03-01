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
model_stats <- read.csv(file.path(pathSave, paste0("DailyModel_FinalModel_Stats_AllLandcovers.csv")))
summary(model_stats)
head(model_stats)

# Step 2: Filter rows where p-values for the variables are less than or equal to 0.05 (significant results)
significant_stats <- model_stats[model_stats$pVal.Lag <= 0.05 | model_stats$pVal.SPEI14 <= 0.05 | model_stats$pVal.TMAX30 <= 0.05, 
                                 c("landcover", "yday", "tVal.Lag", "tVal.SPEI14", "tVal.TMAX30", "pVal.Lag", "pVal.SPEI14", "pVal.TMAX30")]

# Step 3: Reshape the data into long format, including yday
significant_stats_long <- data.frame(
  LandCoverType = rep(significant_stats$landcover, 3),  # Repeat LandCoverType for each variable
  yday = rep(significant_stats$yday, 3),  # Repeat yday for each variable
  Variable = rep(c("tVal_Lag", "tVal_SPEI14", "tVal_TMAX30"), each = nrow(significant_stats)),  # Variable names
  t_stat_value = c(significant_stats$tVal.Lag, 
                   significant_stats$tVal.SPEI14, 
                   significant_stats$tVal.TMAX30),  # Combine the t-stat values into one vector
  p_value = c(significant_stats$pVal.Lag, 
              significant_stats$pVal.SPEI14, 
              significant_stats$pVal.TMAX30)  # Corresponding p-values for the t-statistics
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


ggplot(data=rmse_fig_dat) +
  #geom_line(aes(x=Date, y=RMSE, col=landcover), linewidth=0.9) +
  geom_smooth(aes(x=Date, y=RMSE, col=landcover), method="loess", se=T, span=0.65) +  # Adding the smooth line (using loess method)
  scale_color_manual(values=LC_color_palette) +
  scale_x_date(labels = scales::date_format("%b"), breaks = "1 month") + # Format x-axis with months
  theme_bw()


# Figure C----
# Plot Partial Effects through Time
par(mfrow = c(2, 1), mar = c(4, 4, 2, 1)) # Two rows

# First panel: Partial Effects
plot(model_stats$date, model_stats$partial_effect_drought, type = "n", 
     xlab = "Day of Year (DOY)", ylab = "Partial Effect", 
     main = "Partial Effects through Time")

for (i in 1:length(land_cover_types)) {
  lc_data <- model_stats[model_stats$LandCoverType == land_cover_types[i], ]
  lines(lc_data$doy, lc_data$partial_effect_drought, col = colors[i], lwd = 2)
}

legend("topright", legend = land_cover_types, col = colors, lwd = 2)

# Second panel: Scaled Partial Effects
plot(model_stats$doy, model_stats$scaled_effect_drought, type = "n", 
     xlab = "Day of Year (DOY)", ylab = "Scaled Effect", 
     main = "Scaled Partial Effects through Time")

for (i in 1:length(land_cover_types)) {
  lc_data <- model_stats[model_stats$LandCoverType == land_cover_types[i], ]
  lines(lc_data$doy, lc_data$scaled_effect_drought, col = colors[i], lwd = 2)
}

legend("topright", legend = land_cover_types, col = colors, lwd = 2)

