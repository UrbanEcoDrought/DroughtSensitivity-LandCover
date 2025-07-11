# Creating publication-quality figures for Urban Ecological Drought manuscript
# Figure A: Significant t-statistic plot for different land cover types
# Figure B: RMSE through time for different land cover types  
# Figure C: Partial effects through time

library(ggplot2)
library(reshape2)
library(scales)
library(gridExtra)
library(viridis)

# Setting the file paths (keep this section intact for data sharing)
# This may be different for your computer.
# Sys.setenv(GOOGLE_DRIVE = "G:/Shared drives/Urban Ecological Drought/Manuscript - Urban Drought NDVI Daily Corrs/")
Sys.setenv(GOOGLE_DRIVE = "~/Google Drive/Shared drives/Urban Ecological Drought/Manuscript - Urban Drought NDVI Daily Corrs/")
google.drive <- Sys.getenv("GOOGLE_DRIVE")

path.NDVI <- file.path(google.drive, "data", "data_raw")
path.figs <- file.path(google.drive, "pub_figs")
pathSave <- file.path(google.drive, "data/processed_files/FinalDailyModel")

dir.create(path.figs, recursive = F)

# =============================================================================
# CONFIGURATION AND SETUP
# =============================================================================

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

# Load model statistics
cat("Loading model statistics...\n")
model_stats <- read.csv(file.path(pathSave, "DailyModel_FinalModel_modOutAdd1_Stats_climateNormPartialEffects_AllLandcovers.csv"))

# Load daily partial effects
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
# FIGURE A: T-STATISTIC SIGNIFICANCE HEATMAP----
# =============================================================================

cat("Preparing Figure A data...\n")

# Create working copy and filter for significance
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

# Save Figure A
ggsave(file.path(path.figs, "figure_a_tstat_significance.png"), 
       figure_a, height = 8, width = 12, units = "in", dpi = 300)

cat("Figure A saved successfully.\n")

# =============================================================================
# FIGURE B: MODEL ERROR THROUGH TIME----
# =============================================================================

cat("Preparing Figure B data...\n")

# Prepare RMSE data
rmse_data <- model_stats
rmse_data <- add_date_info(rmse_data)
rmse_data <- standardize_landcover(rmse_data)

# Calculate normalized RMSE (RMSE divided by mean NDVI for each day of year and land cover)
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

# Save Figure B variants
ggsave(file.path(path.figs, "figure_b1_rmse_combined.png"), 
       figure_b1, height = 6, width = 12, units = "in", dpi = 300)
ggsave(file.path(path.figs, "figure_b2_error_faceted.png"), 
       figure_b2, height = 8, width = 10, units = "in", dpi = 300)

cat("Figure B variants saved successfully.\n")

# =============================================================================
# FIGURE C: STANDARDIZED PARTIAL EFFECTS (C4 and C4b only)
# =============================================================================

cat("Preparing Figure C data...\n")

# Prepare partial effects data - focus on standardized versions
# Main publication figures: Temperature and Drought only
pe_columns_main <- c("peDroughtStd.gsMean", "peTempStd.gsMean")
# Supplemental figure: Include lag term as well
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
  facet_grid(landcover~year) +
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
  coord_cartesian(ylim=c(-.25,.15)) +
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

# Save all Figure C variants
ggsave(file.path(path.figs, "figure_c4_partial_effects_standardized.png"), 
       figure_c4, height = 8, width = 15, units = "in", dpi = 300)
ggsave(file.path(path.figs, "figure_c4b_partial_effects_by_variable.png"), 
       figure_c4b, height = 8, width = 15, units = "in", dpi = 300)
ggsave(file.path(path.figs, "figure_c_supplement_with_lag.png"), 
       figure_c_supplement, height = 8, width = 15, units = "in", dpi = 300)

cat("Figure C variants saved successfully:\n")
cat("- Main publication figures: C4 and C4b (Temperature and Drought only)\n")
cat("- Supplemental figure: C_supplement (includes Lag term)\n")

# =============================================================================
# SUMMARY STATISTICS AND VALIDATION
# =============================================================================

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

cat("\nAll figures generated successfully!\n")
cat("Files saved to:", path.figs, "\n")
