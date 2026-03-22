# 06c_manuscript_statistics.R
# ============================================================================
# PURPOSE: Compute and verify ALL statistics cited in the manuscript
#
# This script reconstructs every quantitative claim in the manuscript text
# directly from the data, so discrepancies can be caught before submission.
# Can run standalone using local processed_data/ files (no Google Drive needed
# for the univariate verification, but needs Google Drive for final model data).
#
# SECTIONS:
#   1. Data date range (verify "2000-2023" claim)
#   2. Univariate model selection — reconstruct Table 1 values from raw data
#      and compare against manuscript-reported SPEI 30-day statistics
#   3. Summary means by variable category (SPEI, SPI, Tmax, Tmin)
#   4. Model error (RMSE) statistics for Figure 4 discussion
#   5. Partial effect ranges for Figure 5 discussion
#   6. Supplemental lag influence
#
# Manuscript: "The promise of intraannual sensitivity of heterogeneous landscapes
#              to inform ecological drought impacts"
# ============================================================================

library(lubridate)

# =============================================================================
# FILE PATHS
# =============================================================================

# Google Drive path: Urban Ecological Drought / Manuscript - Urban Drought NDVI Daily Corrs
Sys.setenv(GOOGLE_DRIVE = "G:/Shared drives/Urban Ecological Drought/Manuscript - Urban Drought NDVI Daily Corrs/")
# Sys.setenv(GOOGLE_DRIVE = "~/Google Drive/Shared drives/Urban Ecological Drought/Manuscript - Urban Drought NDVI Daily Corrs/")
google.drive <- Sys.getenv("GOOGLE_DRIVE")

# Final model output path (for figures data)
if (dir.exists(file.path(google.drive, "data/processed_files/FinalDailyModel"))) {
  pathFinal <- file.path(google.drive, "data/processed_files/FinalDailyModel")
  cat("Reading final model data from Google Drive\n")
} else {
  pathFinal <- "processed_data"
  cat("Google Drive not found, reading from local processed_data/\n")
}

# Growing season: DOY 91-304 (April 1 - October 31)
# Consistent across all scripts (04, 05, 06b, 06c)
gs_start <- 91
gs_end   <- 304

years_of_interest <- c(2005, 2012, 2021, 2023)

cat("\n")
cat("================================================================\n")
cat("MANUSCRIPT STATISTICS VERIFICATION\n")
cat("================================================================\n\n")

# =============================================================================
# 1. DATA DATE RANGE
# =============================================================================

cat("--- 1. Data date range ---\n")
ndvi_met <- read.csv("processed_data/landsat_ndvi_metVars_combined.csv")
ndvi_met$date <- as.Date(ndvi_met$date)
cat("Date range:", as.character(range(ndvi_met$date)), "\n")
cat("Number of observations:", nrow(ndvi_met), "\n")
cat("Land covers:", paste(sort(unique(ndvi_met$landcover)), collapse = ", "), "\n")
cat("Satellites:", paste(sort(unique(ndvi_met$mission)), collapse = ", "), "\n\n")

# =============================================================================
# 2. UNIVARIATE MODEL SELECTION — TABLE 1 VALUES
# =============================================================================
# Table 1 is produced by script 04 from the ALL file:
#   - Filters to growing season DOY 90-300
#   - Excludes modIntOnly and modLag
#   - Aggregates mean(dAIC, dR2, dRMSE, dRMSEper) per model+landcover
#   - Then averages across all 7 land covers
#
# The Summaries file (from script 03) uses ALL days of year — different numbers!

cat("--- 2. Univariate model selection (Table 1 reconstruction) ---\n")
cat("Manuscript claims: SPEI 30-day mean dR2 = 0.128, mean dRMSE = -0.09\n\n")

modStatsAll <- read.csv("processed_data/DailyModel_VarSelection-Univariate_ModelStats-ALL.csv")

# Replicate script 04 table construction: growing season DOY 90-300
gs <- modStatsAll[!modStatsAll$model %in% c("modIntOnly", "modLag") &
                    modStatsAll$yday >= gs_start &
                    modStatsAll$yday <= gs_end, ]

aggLC <- aggregate(cbind(dAIC, dR2, dRMSE, dRMSEper) ~ model + landcover,
                   data = gs, FUN = mean)
aggLC2 <- aggregate(cbind(dAIC, dR2, dRMSE, dRMSEper) ~ model,
                    data = aggLC, FUN = mean)

# Add ranks (matching script 04 logic)
aggLC2$dR2.rank <- rank(-aggLC2$dR2)
aggLC2$dRMSE.rank <- rank(aggLC2$dRMSE)
aggLC2$rank.avg <- (aggLC2$dR2.rank + aggLC2$dRMSE.rank) / 2
aggLC2$RankComb <- rank(aggLC2$rank.avg)

tbl <- aggLC2[order(aggLC2$RankComb), ]

cat("Table 1 values (growing season DOY 90-300, mean across 7 land covers):\n")
cat("Rounded to TABLE sig figs: dAIC=1dp, dR2=3dp, dRMSE=4dp, dRMSEper=3dp\n\n")
for (i in seq_len(nrow(tbl))) {
  r <- tbl[i, ]
  cat(sprintf("  Rank %4.1f  %-12s  dAIC=%6.1f  dR2=%6.3f  dRMSE=%8.4f  dRMSE%%=%7.3f\n",
              r$RankComb, r$model, r$dAIC, r$dR2, r$dRMSE, r$dRMSEper))
}

# Highlight SPEI 30-day
spei30 <- tbl[tbl$model == "X30d.SPEI", ]
cat(sprintf("\n>>> SPEI 30-day (Table 1):  dR2 = %.3f,  dRMSE = %.4f\n",
            spei30$dR2, spei30$dRMSE))
cat(sprintf("    Manuscript claims:      dR2 = 0.128,  dRMSE = -0.09\n"))
cat(sprintf("    Discrepancy:            dR2 off by %.3f, dRMSE off by %.4f\n\n",
            abs(spei30$dR2 - 0.128), abs(spei30$dRMSE - (-0.09))))

# Per-landcover breakdown for SPEI 30-day
cat("SPEI 30-day per land cover (GS DOY 90-300):\n")
spei30_lc <- aggLC[aggLC$model == "X30d.SPEI", ]
for (i in seq_len(nrow(spei30_lc))) {
  r <- spei30_lc[i, ]
  cat(sprintf("  %-15s  dR2 = %.3f,  dRMSE = %.4f\n",
              r$landcover, r$dR2, r$dRMSE))
}
cat(sprintf("  %-15s  dR2 = %.3f,  dRMSE = %.4f\n",
            "MEAN", mean(spei30_lc$dR2), mean(spei30_lc$dRMSE)))

# Also show the ALL-DOY summaries for comparison
cat("\n--- Comparison: ALL-DOY summaries (from script 03) ---\n")
summ <- read.csv("processed_data/DailyModel_VarSelection-Univariate_ModelStats-Summaries.csv")
spei30_summ <- summ[summ$model == "X30d.SPEI", ]
cat("SPEI 30-day per land cover (ALL days of year):\n")
for (i in seq_len(nrow(spei30_summ))) {
  r <- spei30_summ[i, ]
  cat(sprintf("  %-15s  dR2 = %.3f,  dRMSE = %.4f\n",
              r$landcover, r$dR2, r$dRMSE))
}
cat(sprintf("  %-15s  dR2 = %.3f,  dRMSE = %.4f\n",
            "MEAN", mean(spei30_summ$dR2), mean(spei30_summ$dRMSE)))

cat("\n  NOTE: Growing season means are HIGHER than all-DOY means because\n")
cat("  climate-NDVI relationships are stronger during active growth.\n")
cat("  Table 1 uses growing season values. Manuscript text should match.\n\n")

# =============================================================================
# 3. ALL VARIABLE MEANS (for manuscript text references)
# =============================================================================

cat("--- 3. Summary means by variable category ---\n")
cat("(Growing season DOY 90-300, mean across land covers)\n\n")

spei_models <- grep("SPEI", tbl$model, value = TRUE)
spi_models <- grep("SPI", tbl$model, value = TRUE)
tmax_models <- grep("TMAX", tbl$model, value = TRUE)
tmin_models <- grep("TMIN", tbl$model, value = TRUE)

cat(sprintf("  SPEI (all windows): mean dR2 = %.3f, mean dRMSE = %.4f, mean rank = %.1f\n",
            mean(tbl[tbl$model %in% spei_models, "dR2"]),
            mean(tbl[tbl$model %in% spei_models, "dRMSE"]),
            mean(tbl[tbl$model %in% spei_models, "RankComb"])))
cat(sprintf("  SPI  (all windows): mean dR2 = %.3f, mean dRMSE = %.4f, mean rank = %.1f\n",
            mean(tbl[tbl$model %in% spi_models, "dR2"]),
            mean(tbl[tbl$model %in% spi_models, "dRMSE"]),
            mean(tbl[tbl$model %in% spi_models, "RankComb"])))
cat(sprintf("  Tmax (all windows): mean dR2 = %.3f, mean dRMSE = %.4f, mean rank = %.1f\n",
            mean(tbl[tbl$model %in% tmax_models, "dR2"]),
            mean(tbl[tbl$model %in% tmax_models, "dRMSE"]),
            mean(tbl[tbl$model %in% tmax_models, "RankComb"])))
cat(sprintf("  Tmin (all windows): mean dR2 = %.3f, mean dRMSE = %.4f, mean rank = %.1f\n",
            mean(tbl[tbl$model %in% tmin_models, "dR2"]),
            mean(tbl[tbl$model %in% tmin_models, "dRMSE"]),
            mean(tbl[tbl$model %in% tmin_models, "RankComb"])))

cat("\n")

# =============================================================================
# 4. MODEL ERROR (RMSE) STATISTICS — FIGURE 4 VALUES
# =============================================================================

cat("--- 4. Model error (RMSE) statistics (Figure 4 values) ---\n")

model_stats <- read.csv(file.path(pathFinal,
  "DailyModel_FinalModel_modOutAdd1_Stats_climateNormPartialEffects_AllLandcovers.csv"))
model_stats$date_display <- as.Date(model_stats$yday - 1, origin = "2000-01-01")
model_stats$in_growing_season <- model_stats$yday >= gs_start &
                                  model_stats$yday <= gs_end
model_stats$rmse_normalized <- model_stats$RMSE / model_stats$NDVI.norm
model_stats$month <- lubridate::month(model_stats$date_display)

cat("Winter (Jan-Mar) normalized RMSE:\n")
cat(sprintf("  Mean = %.3f, SD = %.3f\n",
            mean(model_stats[model_stats$month %in% c(1, 2, 3), "rmse_normalized"]),
            sd(model_stats[model_stats$month %in% c(1, 2, 3), "rmse_normalized"])))

cat("Late fall (Nov-Dec) normalized RMSE:\n")
cat(sprintf("  Mean = %.3f, SD = %.3f\n",
            mean(model_stats[model_stats$month %in% c(11, 12), "rmse_normalized"]),
            sd(model_stats[model_stats$month %in% c(11, 12), "rmse_normalized"])))

cat("\nGrowing season normalized RMSE by land cover:\n")
rmseNorm_gs <- aggregate(
  rmse_normalized ~ landcover,
  data = model_stats[model_stats$in_growing_season == TRUE, ],
  FUN = function(x) c(mean = mean(x), min = min(x), max = max(x))
)
print(rmseNorm_gs)

cat("\n")

# =============================================================================
# 5. PARTIAL EFFECT RANGES — FIGURE 5 VALUES
# =============================================================================

cat("--- 5. Partial effect ranges (Figure 5 values) ---\n")

daily_pe <- read.csv(file.path(pathFinal,
  "DailyModel_FinalModel_modOutAdd1_Stats_dailyPartialEffects_AllLandcovers.csv"))
daily_pe$date <- as.Date(daily_pe$date)
daily_pe$year <- as.numeric(format(daily_pe$date, "%Y"))

pe_main <- daily_pe[daily_pe$year %in% years_of_interest, ]

pe_main_long <- reshape2::melt(
  pe_main,
  id.vars = c("yday", "landcover", "date", "year"),
  measure.vars = c("peDroughtStd.gsMean", "peTempStd.gsMean"),
  variable.name = "variable",
  value.name = "partial_effect_std"
)
pe_main_long$variable_clean <- c(
  "peTempStd.gsMean" = "Temperature",
  "peDroughtStd.gsMean" = "Drought"
)[as.character(pe_main_long$variable)]

pe_range <- aggregate(
  partial_effect_std ~ variable_clean + landcover + year,
  data = pe_main_long,
  FUN = function(x) c(min = min(x, na.rm = TRUE), max = max(x, na.rm = TRUE),
                      range = diff(range(x, na.rm = TRUE)))
)

for (lc in c("crop", "forest", "urban-high")) {
  for (var in c("Temperature", "Drought")) {
    sub <- pe_range[pe_range$landcover == lc & pe_range$variable_clean == var, ]
    cat(sprintf("  %s / %s:\n", lc, var))
    print(summary(sub))
    cat("\n")
  }
}

# =============================================================================
# 6. SUPPLEMENTAL LAG INFLUENCE
# =============================================================================

cat("--- 6. Supplemental lag influence ---\n")

pe_supp <- daily_pe[daily_pe$year %in% years_of_interest,
                     c("peDroughtStd.gsMean", "peTempStd.gsMean", "peLagStd.gsMean",
                       "yday", "landcover", "date", "year")]

pe_supp_long <- reshape2::melt(
  pe_supp,
  id.vars = c("yday", "landcover", "date", "year"),
  measure.vars = c("peDroughtStd.gsMean", "peTempStd.gsMean", "peLagStd.gsMean"),
  variable.name = "variable",
  value.name = "partial_effect_std"
)

variable_labels <- c("peTempStd.gsMean" = "Temperature",
                     "peDroughtStd.gsMean" = "Drought",
                     "peLagStd.gsMean" = "Lag")
pe_supp_long$variable_clean <- variable_labels[as.character(pe_supp_long$variable)]

pe_supp_summary <- aggregate(
  partial_effect_std ~ variable_clean + landcover + year,
  data = pe_supp_long,
  FUN = function(x) c(min = min(x, na.rm = TRUE), max = max(x, na.rm = TRUE),
                      range = diff(range(x, na.rm = TRUE)))
)

cat("Lag partial effect summary:\n")
print(summary(pe_supp_summary[pe_supp_summary$variable_clean == "Lag", ]))

cat("\n================================================================\n")
cat("VERIFICATION COMPLETE\n")
cat("================================================================\n")
cat("\nKEY ITEMS TO CHECK IN MANUSCRIPT:\n")
cat("  1. SPEI 30-day dR2 and dRMSE — see Section 2 above\n")
cat("  2. Date range — see Section 1 above for actual date range\n")
cat("  3. Growing season definition — all scripts now use DOY 91-304\n")
cat("     (April 1 - October 31). Manuscript should say April-October.\n")
cat("  4. Model description — code uses nlme::lme() with random=~1|mission,\n")
cat("     not simple linear models.\n")
cat("================================================================\n")
