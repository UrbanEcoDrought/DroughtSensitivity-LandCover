# 06b_publication_figs.R
# ============================================================================
# PURPOSE: Generate publication-quality figures for the manuscript
#
# Produces 3 of the 5 manuscript figures (Figures 1 & 2 are GIS-based maps):
#   Figure 3 (A): t-statistic significance heatmap — shows WHEN and WHERE each
#                 climate variable significantly predicts NDVI across land covers
#   Figure 4 (B): Normalized RMSE through time — shows model prediction error
#                 as a fraction of mean NDVI, revealing seasonal performance
#   Figure 5 (C): Standardized partial effects — shows the magnitude and
#                 direction of drought and temperature effects on NDVI,
#                 expressed as % of mean growing season NDVI
#
# Also generates supplemental figures (C4b by-variable view, lag term figure,
# and a filtered Forest vs Urban High comparison).
#
# INPUTS (from script 05 via Google Drive or local processed_data/):
#   - modOutAdd1 Stats: per-DOY model coefficients, t-values, p-values, RMSE
#   - modOutAdd1 dailyPartialEffects: per-date partial effects for each predictor
#
# All figures use the add1 model (SPEI 14-day + Tmax 30-day, additive).
#
# FIGURE DESIGN:
#   - Land cover types ordered rural → urban (Crop to Urban High)
#   - NLCD-inspired color palette for land cover classes
#   - Growing season (DOY 91-304) highlighted; non-growing season dimmed
#   - Drought years of interest: 2005, 2012, 2021, 2023
# ============================================================================

library(ggplot2)
library(reshape2)
library(scales)
library(gridExtra)
library(viridis)

# Setting the file paths (keep this section intact for data sharing)
# This may be different for your computer.
# Google Drive path: Urban Ecological Drought / Manuscript - Urban Drought NDVI Daily Corrs
Sys.setenv(GOOGLE_DRIVE = "G:/Shared drives/Urban Ecological Drought/Manuscript - Urban Drought NDVI Daily Corrs/")
# Sys.setenv(GOOGLE_DRIVE = "~/Google Drive/Shared drives/Urban Ecological Drought/Manuscript - Urban Drought NDVI Daily Corrs/")
google.drive <- Sys.getenv("GOOGLE_DRIVE")

# Model output path: reads from Google Drive if available, otherwise falls back to local processed_data/
if (dir.exists(file.path(google.drive, "data/processed_files/FinalDailyModel"))) {
  pathSave <- file.path(google.drive, "data/processed_files/FinalDailyModel")
  cat("Reading data from Google Drive:", pathSave, "\n")
} else {
  pathSave <- "processed_data"
  cat("Google Drive not found, reading from local processed_data/\n")
}

path.figs <- file.path(google.drive, "pub_figs")
dir.create(path.figs, recursive = FALSE, showWarnings = FALSE)
dir.create("figures", recursive = FALSE, showWarnings = FALSE)

# =============================================================================
# CONFIGURATION AND SETUP
# =============================================================================
# Central configuration for all figures. Land cover classes are ordered along
# a rural-to-urban gradient, matching the manuscript's conceptual framework of
# comparing natural vs. developed landscapes' drought sensitivity.
# Colors are drawn from the NLCD palette for consistency with mapped figures.

# Define land cover type mappings and standardized order (rural to urban)
landcover_mapping <- c(
  "crop" = "Crop",
  "forest" = "Forest", 
  "grassland" = "Grassland",
  "urban-open" = "Urban Open",
  "urban-low" = "Urban Low", 
  "urban-medium" = "Urban Medium",
  "urban-high" = "Urban High"
)

# Standard land cover order for all figures (rural to urban gradient)
landcover_order <- names(landcover_mapping)

# Color palette for land cover types (rural to urban gradient)
landcover_colors <- setNames(
  c("#DCD939", "#68AB5F", "#CCB879", 
    "#DEC5C5", "#D99282", "#EB0000", "#AB0000"),
  landcover_mapping[landcover_order]
)

# Variable color palette for climate variables
climate_colors <- c(
  "Temperature" = "#E69F00",
  "Drought" = "#0072B2", 
  "Lag" = "#009E73"
)

# Years of interest for detailed analysis
years_of_interest <- c(2005, 2012, 2021, 2023)

# Growing season definition (day of year)
growing_season_start <- 91   # April 1st
growing_season_end <- 304    # October 31st

# =============================================================================
# PUBLICATION THEME
# =============================================================================

theme_publication <- function(base_size = 11, base_family = "sans") {
  theme_minimal(base_size = base_size, base_family = base_family) +
    theme(
      # Ensure white backgrounds
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA),
      
      # Axis styling
      axis.title = element_text(size = base_size, face = "bold"),
      axis.text = element_text(size = base_size - 1),
      axis.line = element_line(color = "black", size = 0.5),
      axis.ticks = element_line(color = "black", size = 0.3),
      
      # Remove grid lines for cleaner look
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.border = element_blank(),
      
      # Title styling
      plot.title = element_text(size = base_size + 2, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = base_size, hjust = 0.5),
      plot.caption = element_text(size = base_size - 2, hjust = 1),
      
      # Legend styling
      legend.title = element_text(size = base_size, face = "bold"),
      legend.text = element_text(size = base_size - 1),
      legend.key = element_blank(),
      legend.background = element_rect(fill = "white", color = NA),
      legend.position = "bottom",
      legend.direction = "horizontal",
      legend.box = "horizontal",
      legend.box.just = "center",
      legend.spacing.x = unit(0.4, "cm"),
      
      # Facet label styling
      strip.text = element_text(size = base_size, face = "bold"),
      strip.background = element_rect(fill = "gray90", color = NA, size = 0.5)
    )
}

# =============================================================================
# DATA LOADING AND PREPARATION
# =============================================================================
# Two input files from the add1 final model (SPEI 14-day + Tmax 30-day):
#
# 1. climateNormPartialEffects: One row per DOY × land cover (365 × 7 = 2,555 rows).
#    Contains: model fit stats (R², RMSE, AIC), fixed-effect coefficients,
#    t-statistics and p-values for each predictor, and climate-normalized
#    partial effects averaged across all years.
#
# 2. dailyPartialEffects: One row per actual observation date × land cover.
#    Contains: year-specific partial effects (raw and standardized as % of
#    mean growing season NDVI). Used for Figure 5 to show how drought and
#    temperature effects vary across specific drought years.

# Load model statistics (per-DOY summary across all years)
cat("Loading model statistics...\n")
model_stats <- read.csv(file.path(pathSave, "DailyModel_FinalModel_modOutAdd1_Stats_climateNormPartialEffects_AllLandcovers.csv"))

# Load daily partial effects (per-date, year-specific)
cat("Loading daily partial effects...\n")
daily_pe <- read.csv(file.path(pathSave, "DailyModel_FinalModel_modOutAdd1_Stats_dailyPartialEffects_AllLandcovers.csv"))

# Convert date column from character to Date
daily_pe$date <- as.Date(daily_pe$date)

# Function to standardize land cover names
standardize_landcover <- function(data, landcover_col = "landcover") {
  data[[landcover_col]] <- landcover_mapping[data[[landcover_col]]]
  data[[landcover_col]] <- factor(data[[landcover_col]], levels = landcover_mapping[landcover_order])
  return(data)
}

# Function to add date information from day of year
add_date_info <- function(data, yday_col = "yday") {
  data$date_display <- as.Date(data[[yday_col]] - 1, origin = "2000-01-01")
  data$in_growing_season <- data[[yday_col]] >= growing_season_start & 
    data[[yday_col]] <= growing_season_end
  return(data)
}

# =============================================================================
# FIGURE A / FIGURE 3: T-STATISTIC SIGNIFICANCE HEATMAP
# =============================================================================
# Shows the t-statistic (effect size and direction) for each predictor variable
# across the full year, faceted by land cover type. Non-significant relationships
# (p > 0.05) are set to NA and rendered as grey, making it easy to see WHEN
# each climate driver significantly influences NDVI.
#
# Key ecological insights this figure reveals:
#   - Drought effects (SPEI) are strongest mid-summer across all land covers
#   - Temperature effects peak in spring/fall transition periods
#   - Lag (autocorrelation) is significant year-round, strongest in winter
#   - Urban-high shows weaker/fewer significant climate-NDVI relationships
#
# Purple = negative t-stat (variable increase → NDVI decrease)
# Green  = positive t-stat (variable increase → NDVI increase)

cat("Preparing Figure A data...\n")

# Create working copy and set non-significant t-statistics to NA
model_stats_fig_a <- model_stats
model_stats_fig_a$tVal.Lag[model_stats$pVal.Lag > 0.05] <- NA
model_stats_fig_a$tVal.Drought[model_stats$pVal.Drought > 0.05] <- NA
model_stats_fig_a$tVal.Temp[model_stats$pVal.Temp > 0.05] <- NA

# Reshape to long format for plotting
tstat_long <- data.frame(
  landcover = rep(model_stats_fig_a$landcover, 3),
  yday = rep(model_stats_fig_a$yday, 3),
  variable = rep(c("Lag", "Drought", "Temperature"), each = nrow(model_stats_fig_a)),
  t_statistic = c(model_stats_fig_a$tVal.Lag, 
                  model_stats_fig_a$tVal.Drought, 
                  model_stats_fig_a$tVal.Temp)
)

# Add date and growing season information
tstat_long <- add_date_info(tstat_long)
tstat_long <- standardize_landcover(tstat_long)

# Option 1: Create continuous color scale (recommended for publication)
# Use t_statistic directly for continuous color mapping

# Option 2: If you prefer categorical bins, uncomment below and adjust breaks as needed
# tstat_long$t_stat_category <- cut(
#   tstat_long$t_statistic,
#   breaks = c(-Inf, -4, -2, 0, 2, 4, Inf),
#   labels = c("Very Negative", "Negative", "Weakly Negative", 
#              "Weakly Positive", "Positive", "Very Positive"),
#   right = TRUE
# )

# Reorder variable factor for logical display
tstat_long$variable <- factor(tstat_long$variable, levels = c("Lag", "Drought", "Temperature"))

# Create Figure A with continuous color scale
figure_a <- ggplot(data = tstat_long) + 
  facet_grid(landcover ~ .) +
  geom_tile(aes(x = date_display, y = variable, fill = t_statistic, 
                alpha = in_growing_season)) +
  geom_vline(xintercept=as.Date("2000-04-01"), linetype="dotted") +
  geom_vline(xintercept=as.Date("2000-11-01"), linetype="dotted") +
  scale_alpha_manual(values = c("FALSE" = 0.6, "TRUE" = 1.0), guide = "none") +
  scale_fill_gradient2(
    low = "#762a83", mid = "white", high = "#1b7837",
    midpoint = 0,
    name = "t-Statistic",
    na.value = "grey90",
    limits = c(-6, 6)  # Adjust these limits based on your data range
  ) +
  scale_x_date(
    labels = date_format("%b"), 
    breaks = "1 month",
    expand = c(0, 0)
  ) +
  labs(
    # title = "Significance of Climate Variables on NDVI",
    # subtitle = "Growing season highlighted, non-significant relationships (p > 0.05) shown as grey",
    x = "Month", 
    y = "Land Cover Type",
    caption = "Only statistically significant relationships (p < 0.05) are colored"
  ) +
  guides(fill = guide_colorbar(title.position = "top", title.hjust = 0.5)) +
  theme_publication() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Save Figure A (Figure 3 in manuscript)
ggsave(file.path(path.figs, "figure_a_tstat_significance.png"),
       figure_a, height = 8, width = 12, units = "in", dpi = 300)
ggsave("figures/Figure_3_tstat_significance.png",
       figure_a, height = 8, width = 12, units = "in", dpi = 300)

cat("Figure A (manuscript Figure 3) saved successfully.\n")

# =============================================================================
# FIGURE B / FIGURE 4: MODEL ERROR THROUGH TIME
# =============================================================================
# Shows how well the model predicts NDVI across the year by plotting normalized
# RMSE (RMSE / mean NDVI). Normalizing by mean NDVI accounts for the fact that
# NDVI values and their variance are much lower in winter than summer.
#
# B1 (combined): All land covers on one plot — shows relative performance
# B2 (faceted): Raw prediction error per land cover — shows bias patterns
#
# Key pattern: Model error is highest in winter (low NDVI, high relative error)
# and lowest during the growing season when climate-NDVI relationships are strong.

cat("Preparing Figure B data...\n")

# Prepare RMSE data
rmse_data <- model_stats
rmse_data <- add_date_info(rmse_data)
rmse_data <- standardize_landcover(rmse_data)

# Normalized RMSE: divides prediction error by the mean NDVI for that DOY
# and land cover (NDVI.norm), putting error on a relative scale
rmse_data$rmse_normalized <- rmse_data$RMSE / rmse_data$NDVI.norm

# Create Figure B - All land covers on one plot
figure_b1 <- ggplot(data = rmse_data) +
  geom_vline(xintercept = c(growing_season_start, growing_season_end), 
             linetype = "dashed", color = "gray50", alpha = 0.7) +
  geom_line(aes(x = yday, y = rmse_normalized, color = landcover), 
            size = 1.0, alpha = 0.8) +
  scale_color_manual(values = landcover_colors, name = "Land Cover Type") +
  scale_x_continuous(
    breaks = c(1, 32, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335),
    labels = month.abb,
    expand = c(0.01, 0)
  ) +
  labs(
    # title = "Model Error Throughout the Year",
    # subtitle = "Normalized RMSE by land cover type",
    x = "Month",
    y = "RMSE / Mean NDVI",
    caption = "Dashed lines indicate growing season boundaries"
  ) +
  theme_publication() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Create Figure B2 - Faceted by land cover
figure_b2 <- ggplot(data = rmse_data) + 
  facet_grid(landcover ~ .) +
  geom_vline(xintercept = c(growing_season_start, growing_season_end), 
             linetype = "dashed", color = "gray50", alpha = 0.7) +
  geom_hline(yintercept = 0, linetype = "solid", color = "black", size = 0.3) +
  geom_line(aes(x = yday, y = Error, color = landcover), size = 1.0) +
  scale_color_manual(values = landcover_colors, guide = "none") +
  scale_x_continuous(
    breaks = c(1, 32, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335),
    labels = month.abb,
    expand = c(0.01, 0)
  ) +
  labs(
    # title = "Model Prediction Error by Land Cover Type",
    # subtitle = "Raw prediction error throughout the year",
    x = "Month",
    y = "Prediction Error",
    caption = "Dashed lines indicate growing season boundaries"
  ) +
  theme_publication() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Save Figure B variants (B1 = Figure 4 in manuscript)
ggsave(file.path(path.figs, "figure_b1_rmse_combined.png"),
       figure_b1, height = 6, width = 12, units = "in", dpi = 300)
ggsave("figures/Figure_4_rmse_combined.png",
       figure_b1, height = 6, width = 12, units = "in", dpi = 300)
ggsave(file.path(path.figs, "figure_b2_error_faceted.png"),
       figure_b2, height = 8, width = 10, units = "in", dpi = 300)

cat("Figure B variants saved successfully (B1 = manuscript Figure 4).\n")

# =============================================================================
# FIGURE C / FIGURE 5: STANDARDIZED PARTIAL EFFECTS
# =============================================================================
# Shows the contribution of each climate predictor to NDVI, standardized as a
# percentage of mean growing-season NDVI. This normalization allows meaningful
# comparison across land cover types that have very different absolute NDVI values.
#
# For example, a value of -0.10 means that predictor's effect reduces NDVI by
# 10% of its average growing-season value — a biologically meaningful metric.
#
# Plotted for 4 drought years of interest:
#   2005: moderate drought
#   2012: severe flash drought (Midwest)
#   2021: late-season drought
#   2023: summer drought
#
# Main publication figures show Temperature and Drought effects only.
# Supplemental figure includes the Lag (NDVI autocorrelation) term.
#
# The partial effect lines are split into 3 segments (DOY ≤90, 61-304, ≥305)
# to handle the plotting discontinuity at growing season boundaries while
# maintaining visual continuity through the overlap region (DOY 61-90).

cat("Preparing Figure C data...\n")

# Column naming convention from script 05:
#   peDroughtStd.gsMean = drought partial effect / mean growing season NDVI
#   peTempStd.gsMean    = temperature partial effect / mean growing season NDVI
#   peLagStd.gsMean     = lag partial effect / mean growing season NDVI
pe_columns_main <- c("peDroughtStd.gsMean", "peTempStd.gsMean")
pe_columns_supp <- c("peDroughtStd.gsMean", "peTempStd.gsMean", "peLagStd.gsMean")

partial_data <- daily_pe[, c(pe_columns_supp, "yday", "landcover", "date")]
partial_data$year <- as.numeric(format(partial_data$date, "%Y"))
partial_data$month <- as.numeric(format(partial_data$date, "%m"))

# Reshape to long format for MAIN publication figures (no lag)
partial_long_main <- reshape2::melt(
  partial_data, 
  id.vars = c("yday", "landcover", "date", "year", "month"),
  measure.vars = pe_columns_main,
  variable.name = "variable", 
  value.name = "partial_effect_std"
)

# Reshape to long format for SUPPLEMENTAL figure (includes lag)
partial_long_supp <- reshape2::melt(
  partial_data, 
  id.vars = c("yday", "landcover", "date", "year", "month"),
  measure.vars = pe_columns_supp,
  variable.name = "variable", 
  value.name = "partial_effect_std"
)

# Add date and growing season information for both datasets
partial_long_main <- add_date_info(partial_long_main)
partial_long_main <- standardize_landcover(partial_long_main)

partial_long_supp <- add_date_info(partial_long_supp)
partial_long_supp <- standardize_landcover(partial_long_supp)

# Clean up variable names for plotting
variable_labels_main <- c(
  "peTempStd.gsMean" = "Temperature",
  "peDroughtStd.gsMean" = "Drought"
)

variable_labels_supp <- c(
  "peTempStd.gsMean" = "Temperature",
  "peDroughtStd.gsMean" = "Drought",
  "peLagStd.gsMean" = "Lag"
)

partial_long_main$variable_clean <- variable_labels_main[partial_long_main$variable]
partial_long_main$variable_clean <- factor(partial_long_main$variable_clean, 
                                           levels = c("Temperature", "Drought"))

partial_long_supp$variable_clean <- variable_labels_supp[partial_long_supp$variable]
partial_long_supp$variable_clean <- factor(partial_long_supp$variable_clean, 
                                           levels = c("Temperature", "Drought", "Lag"))

# Filter for years of interest and remove any NA values
pe_main <- partial_long_main[partial_long_main$year %in% years_of_interest & 
                               !is.na(partial_long_main$partial_effect_std), ]

pe_supp <- partial_long_supp[partial_long_supp$year %in% years_of_interest & 
                               !is.na(partial_long_supp$partial_effect_std), ]

# =============================================================================
# MAIN PUBLICATION FIGURES (Temperature and Drought only)
# =============================================================================

# Create Figure C4 - Standardized partial effects by land cover and year
figure_c4 <- ggplot(pe_main) + 
  facet_grid(landcover~year, scales="free_y") +
  geom_vline(xintercept = c(growing_season_start, growing_season_end), 
             linetype = "dotted", color = "gray50", alpha = 0.7) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", size = 0.3) +
  geom_line(data = pe_main[pe_main$yday <= 90, ],
            aes(x = yday, y = partial_effect_std, color = variable_clean, alpha = in_growing_season), 
            size = 0.8) +
  geom_line(data = pe_main[pe_main$yday >= 61 & pe_main$yday <= 304, ],
            aes(x = yday, y = partial_effect_std, color = variable_clean, alpha = in_growing_season), 
            size = 0.8) +
  geom_line(data = pe_main[pe_main$yday >= 305, ],
            aes(x = yday, y = partial_effect_std, color = variable_clean, alpha = in_growing_season), 
            size = 0.8) +
  scale_alpha_manual(values = c("FALSE" = 0.5, "TRUE" = 1.0), guide = "none") +
  scale_color_manual(values = c("Temperature" = "#E69F00", "Drought" = "#0072B2"), 
                     name = "Climate Variable") +
  scale_x_continuous(
    breaks = c(1, 32, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335),
    labels = month.abb
  ) +
  scale_y_continuous(labels = scales::percent_format()) +
  #coord_cartesian(ylim=c(-.25,.15)) +
  labs(
    # title = "Standardized Partial Effects as % of Mean Growing Season NDVI",
    # subtitle = paste("Drought years:", paste(years_of_interest, collapse = ", ")),
    x = "Month",
    y = "PE as % Mean GS NDVI",
    caption = "Dotted lines indicate growing season boundaries; non-growing season dimmed"
  ) +
  theme_publication() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom"
  )

# Create Figure C4b - Same data but faceted by variable instead of land cover
figure_c4b <- ggplot(pe_main) + 
  facet_grid(year ~ variable_clean) +
  geom_vline(xintercept = c(growing_season_start, growing_season_end), 
             linetype = "dotted", color = "gray50", alpha = 0.7) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", size = 0.3) +
  geom_line(data = pe_main[pe_main$yday <= 90, ],
            aes(x = yday, y = partial_effect_std, color = landcover, alpha = in_growing_season), 
            size = 0.8) +
  geom_line(data = pe_main[pe_main$yday >= 61 & pe_main$yday <= 304, ],
            aes(x = yday, y = partial_effect_std, color = landcover, alpha = in_growing_season), 
            size = 0.8) +
  geom_line(data = pe_main[pe_main$yday >= 305, ],
            aes(x = yday, y = partial_effect_std, color = landcover, alpha = in_growing_season), 
            size = 0.8) +
  scale_alpha_manual(values = c("FALSE" = 0.25, "TRUE" = 1.0), guide = "none") +
  scale_color_manual(values = landcover_colors, name = "Land Cover Type") +
  scale_x_continuous(
    breaks = c(1, 32, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335),
    labels = month.abb
  ) +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(
    # title = "Standardized Partial Effects by Climate Variable",
    # subtitle = paste("Drought years:", paste(years_of_interest, collapse = ", ")),
    x = "Month",
    y = "PE as % Mean GS NDVI", 
    caption = "Dotted lines indicate growing season boundaries; non-growing season dimmed"
  ) +
  theme_publication() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom"
  )

# Filtered comparison of the two most contrasting land cover types:
# Forest (highest NDVI, most natural) vs Urban High (lowest NDVI, most impervious)
# This highlights how drought sensitivity differs at the extremes of the
# urban-rural gradient — a key finding discussed in the manuscript.
pe_filtered <- pe_main[pe_main$landcover %in% c("Forest", "Urban High"), ]

figure_c4_filtered <- ggplot(pe_filtered) + 
  facet_grid(landcover~year, scales="free_y") +
  geom_vline(xintercept = c(growing_season_start, growing_season_end), 
             linetype = "dotted", color = "gray50", alpha = 0.7) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", linewidth = 0.3) +
  geom_line(data = pe_filtered[pe_filtered$yday <= 90, ],
            aes(x = yday, y = partial_effect_std, color = variable_clean, alpha = in_growing_season), 
            linewidth = 1.2) +
  geom_line(data = pe_filtered[pe_filtered$yday >= 61 & pe_filtered$yday <= 304, ],
            aes(x = yday, y = partial_effect_std, color = variable_clean, alpha = in_growing_season), 
            linewidth = 1.2) +
  geom_line(data = pe_filtered[pe_filtered$yday >= 305, ],
            aes(x = yday, y = partial_effect_std, color = variable_clean, alpha = in_growing_season), 
            linewidth = 1.2) +
  scale_alpha_manual(values = c("FALSE" = 0.5, "TRUE" = 1.0), guide = "none") +
  scale_color_manual(values = c("Temperature" = "#E69F00", "Drought" = "#0072B2"), 
                     name = "Climate Variable") +
  scale_x_continuous(
    breaks = c(1, 60, 121, 182, 244, 305),
    labels = c("Jan", "Mar", "May", "Jul", "Sep", "Nov")
  ) +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(
    x = "Month",
    y = "PE as % Mean GS NDVI"
  ) +
  theme_minimal(base_size = 16) +
  theme(
    # Text elements - larger for readability
    axis.title = element_text(size = 18, face = "bold"),
    axis.text = element_text(size = 14),
    axis.text.x = element_text(angle = 0, hjust = 0.5),  # Horizontal labels now
    
    # Facet strips - larger and bold
    strip.text = element_text(size = 16, face = "bold"),
    strip.background = element_rect(fill = "grey90", color = NA),
    
    # Legend - larger
    legend.position = "bottom",
    legend.title = element_text(size = 18, face = "bold"),
    legend.text = element_text(size = 16),
    legend.key.size = unit(2, "lines"),
    
    # Panel
    panel.grid.major = element_line(color = "grey85", linewidth = 0.3),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
    
    # Overall margins
    plot.margin = margin(10, 10, 10, 10)
  )

ggsave(file.path(path.figs, "figure_c4_partial_effects_standardized_filtered.png"), 
       figure_c4_filtered, height = 10, width = 25, units = "in", dpi = 300, bg="white")
# =============================================================================
# SUPPLEMENTAL FIGURE (includes lag term)
# =============================================================================

# Create supplemental figure including lag term
figure_c_supplement <- ggplot(pe_supp) + 
  facet_grid(year ~ landcover) +
  geom_vline(xintercept = c(growing_season_start, growing_season_end), 
             linetype = "dotted", color = "gray50", alpha = 0.7) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", size = 0.3) +
  geom_line(data = pe_supp[pe_supp$yday <= 90, ],
            aes(x = yday, y = partial_effect_std, color = variable_clean, alpha = in_growing_season), 
            size = 0.8) +
  geom_line(data = pe_supp[pe_supp$yday >= 61 & pe_supp$yday <= 304, ],
            aes(x = yday, y = partial_effect_std, color = variable_clean, alpha = in_growing_season), 
            size = 0.8) +
  geom_line(data = pe_supp[pe_supp$yday >= 305, ],
            aes(x = yday, y = partial_effect_std, color = variable_clean, alpha = in_growing_season), 
            size = 0.8) +
  scale_alpha_manual(values = c("FALSE" = 0.5, "TRUE" = 1.0), guide = "none") +
  scale_color_manual(values = c("Temperature" = "#E69F00", "Drought" = "#0072B2", "Lag" = "#009E73"), 
                     name = "Climate Variable") +
  scale_x_continuous(
    breaks = c(1, 32, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335),
    labels = month.abb
  ) +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(
    # title = "Standardized Partial Effects Including Lag Term (Supplemental)",
    # subtitle = paste("All climate variables for drought years:", paste(years_of_interest, collapse = ", ")),
    x = "Month",
    y = "PE as % Mean GS NDVI",
    caption = "Dotted lines indicate growing season boundaries; lag term may be sparse due to discontinuous NDVI time series"
  ) +
  theme_publication() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom"
  )

# Save all Figure C variants (C4 = Figure 5 in manuscript)
ggsave(file.path(path.figs, "figure_c4_partial_effects_standardized.png"),
       figure_c4, height = 10, width = 15, units = "in", dpi = 300)
ggsave("figures/Figure_5_partial_effects.png",
       figure_c4, height = 10, width = 15, units = "in", dpi = 300)
ggsave(file.path(path.figs, "figure_c4b_partial_effects_by_variable.png"),
       figure_c4b, height = 10, width = 15, units = "in", dpi = 300)
ggsave(file.path(path.figs, "figure_c_supplement_with_lag.png"),
       figure_c_supplement, height = 10, width = 15, units = "in", dpi = 300)

cat("Figure C variants saved successfully:\n")
cat("- Main publication figures: C4 and C4b (Temperature and Drought only)\n")
cat("- Supplemental figure: C_supplement (includes Lag term)\n")

# =============================================================================
# SUMMARY STATISTICS AND VALIDATION
# =============================================================================
# Quick diagnostic summaries printed to console for sanity-checking the figures.
# More thorough verification is in 06c_manuscript_statistics.R.

cat("Generating summary statistics...\n")

# Calculate RMSE statistics by season
rmse_summary <- aggregate(
  rmse_normalized ~ landcover + in_growing_season, 
  data = rmse_data, 
  FUN = function(x) c(mean = mean(x), sd = sd(x), min = min(x), max = max(x))
)

# Print summary
cat("\nRMSE Summary by Land Cover and Season:\n")
print(rmse_summary)

# Calculate partial effect ranges for key years
pe_summary <- aggregate(
  partial_effect_std ~ variable_clean + landcover + year, 
  data = pe_main,
  FUN = function(x) c(min = min(x, na.rm = TRUE), max = max(x, na.rm = TRUE), 
                      range = diff(range(x, na.rm = TRUE)))
)

cat("\nPartial Effect Ranges Summary:\n")
print(pe_summary[1:10, ])  # Show first 10 rows

pe_summary[pe_summary$year %in% 2005,]
pe_summary[pe_summary$year %in% 2012,]
pe_summary[pe_summary$year %in% 2021,]
pe_summary[pe_summary$year %in% 2023,]

summary(pe_summary[pe_summary$landcover=="Crop" & pe_summary$variable_clean=="Temperature",])
summary(pe_summary[pe_summary$landcover=="Crop" & pe_summary$variable_clean=="Drought",])

summary(pe_summary[pe_summary$landcover=="Urban High" & pe_summary$variable_clean=="Temperature",])
summary(pe_summary[pe_summary$landcover=="Urban High" & pe_summary$variable_clean=="Drought",])

cat("\nAll figures generated successfully!\n")
cat("Files saved to:", path.figs, "\n")

# Ad-hoc statistics moved to 06c_manuscript_statistics.R
