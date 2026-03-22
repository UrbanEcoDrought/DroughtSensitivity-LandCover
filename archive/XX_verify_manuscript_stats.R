# XX_verify_manuscript_stats.R
# Verify specific manuscript statistics against pipeline outputs.

library(dplyr)

# Read daily partial effects
pe <- read.csv("G:/Shared drives/Urban Ecological Drought/Manuscript - Urban Drought NDVI Daily Corrs/data/processed_files/FinalDailyModel/DailyModel_FinalModel_modOutAdd1_Stats_dailyPartialEffects_AllLandcovers.csv")
pe$date <- as.Date(pe$date)
pe$year <- as.integer(format(pe$date, "%Y"))
pe$month <- as.integer(format(pe$date, "%m"))
pe$yday <- as.integer(format(pe$date, "%j"))

# Focus on 4 example years, growing season only
years_focus <- c(2005, 2012, 2021, 2023)
pe_gs <- pe[pe$year %in% years_focus & pe$yday >= 91 & pe$yday <= 304, ]

cat("================================================================\n")
cat("INVESTIGATION 1: Partial effect ranges per land cover per year\n")
cat("(growing season, standardized by GS mean NDVI)\n")
cat("================================================================\n\n")

for (lc in sort(unique(pe_gs$landcover))) {
  cat("---", lc, "---\n")
  for (yr in years_focus) {
    sub <- pe_gs[pe_gs$landcover == lc & pe_gs$year == yr, ]
    if (nrow(sub) == 0) next

    temp_min <- min(sub$peTempStd.gsMean, na.rm=TRUE)
    temp_max <- max(sub$peTempStd.gsMean, na.rm=TRUE)
    temp_range <- temp_max - temp_min

    drought_min <- min(sub$peDroughtStd.gsMean, na.rm=TRUE)
    drought_max <- max(sub$peDroughtStd.gsMean, na.rm=TRUE)
    drought_range <- drought_max - drought_min

    cat(sprintf("  %d: Temp range=%.1f%% (%.1f%% to %.1f%%)  Drought range=%.1f%% (%.1f%% to %.1f%%)\n",
                yr, temp_range*100, temp_min*100, temp_max*100,
                drought_range*100, drought_min*100, drought_max*100))
  }

  # Mean across 4 years
  temp_ranges <- drought_ranges <- c()
  for (yr in years_focus) {
    sub <- pe_gs[pe_gs$landcover == lc & pe_gs$year == yr, ]
    if (nrow(sub) == 0) next
    temp_ranges <- c(temp_ranges, max(sub$peTempStd.gsMean, na.rm=TRUE) - min(sub$peTempStd.gsMean, na.rm=TRUE))
    drought_ranges <- c(drought_ranges, max(sub$peDroughtStd.gsMean, na.rm=TRUE) - min(sub$peDroughtStd.gsMean, na.rm=TRUE))
  }
  cat(sprintf("  MEAN: Temp range=%.1f%%  Drought range=%.1f%%\n\n",
              mean(temp_ranges)*100, mean(drought_ranges)*100))
}

cat("================================================================\n")
cat("INVESTIGATION 2: July 2012 drought partial effects by land cover\n")
cat("(checking the −21% Urban-High vs '<−6%' for others claim)\n")
cat("================================================================\n\n")

pe_july2012 <- pe[pe$year == 2012 & pe$month == 7, ]
for (lc in sort(unique(pe_july2012$landcover))) {
  sub <- pe_july2012[pe_july2012$landcover == lc, ]
  cat(sprintf("  %s: drought PE min=%.1f%%, max=%.1f%%, on dates:\n",
              lc, min(sub$peDroughtStd.gsMean, na.rm=TRUE)*100,
              max(sub$peDroughtStd.gsMean, na.rm=TRUE)*100))
  # Show the most negative day
  worst <- sub[which.min(sub$peDroughtStd.gsMean), ]
  cat(sprintf("    Most negative: %.1f%% on %s (DOY %d)\n",
              worst$peDroughtStd.gsMean*100, as.character(worst$date), worst$yday))
}

cat("\n================================================================\n")
cat("INVESTIGATION 3: 2012 growing season drought min by land cover\n")
cat("================================================================\n\n")

pe_2012gs <- pe[pe$year == 2012 & pe$yday >= 91 & pe$yday <= 304, ]
for (lc in sort(unique(pe_2012gs$landcover))) {
  sub <- pe_2012gs[pe_2012gs$landcover == lc, ]
  worst <- sub[which.min(sub$peDroughtStd.gsMean), ]
  cat(sprintf("  %s: drought PE seasonal min = %.1f%% on %s\n",
              lc, worst$peDroughtStd.gsMean*100, as.character(worst$date)))
}

cat("\n================================================================\n")
cat("INVESTIGATION 4: Climate-norm partial effects ranges\n")
cat("(mean conditions, not year-specific)\n")
cat("================================================================\n\n")

cn <- read.csv("G:/Shared drives/Urban Ecological Drought/Manuscript - Urban Drought NDVI Daily Corrs/data/processed_files/FinalDailyModel/DailyModel_FinalModel_modOutAdd1_Stats_climateNormPartialEffects_AllLandcovers.csv")
cn_gs <- cn[cn$yday >= 91 & cn$yday <= 304, ]

# Need mean GS NDVI per LC for standardization
gs_ndvi <- aggregate(NDVI.norm ~ landcover, data=cn_gs, FUN=mean, na.rm=TRUE)

for (lc in sort(unique(cn_gs$landcover))) {
  sub <- cn_gs[cn_gs$landcover == lc, ]
  ndvi_mean <- gs_ndvi$NDVI.norm[gs_ndvi$landcover == lc]

  temp_min <- min(sub$partial.Temp.climateNorm, na.rm=TRUE) / ndvi_mean
  temp_max <- max(sub$partial.Temp.climateNorm, na.rm=TRUE) / ndvi_mean
  drought_min <- min(sub$partial.Drought.climateNorm, na.rm=TRUE) / ndvi_mean
  drought_max <- max(sub$partial.Drought.climateNorm, na.rm=TRUE) / ndvi_mean

  cat(sprintf("  %s (mean GS NDVI=%.3f):\n", lc, ndvi_mean))
  cat(sprintf("    Temp:    %.1f%% to %.1f%% (range=%.1f%%)\n", temp_min*100, temp_max*100, (temp_max-temp_min)*100))
  cat(sprintf("    Drought: %.1f%% to %.1f%% (range=%.1f%%)\n\n", drought_min*100, drought_max*100, (drought_max-drought_min)*100))
}
