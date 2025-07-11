# creating tables for manuscript
# Creating a clean script to re-do the daily correlation modeling and do some prediction from it
library(ggplot2)
library(lubridate)
library(ggcorrplot)
library(flextable)
library(officer)
library(dplyr)

# Setting the file paths. This may be different for your computer.
# Sys.setenv(GOOGLE_DRIVE = "G:/Shared drives/Urban Ecological Drought/Manuscript - Urban Drought NDVI Daily Corrs/")
Sys.setenv(GOOGLE_DRIVE = "~/Google Drive/Shared drives/Urban Ecological Drought/Manuscript - Urban Drought NDVI/")
google.drive <- Sys.getenv("GOOGLE_DRIVE")

path.NDVI <- file.path("G:/Shared drives/Urban Ecological Drought/", "data", "UrbanEcoDrought_NDVI_LocalExtract")
path.figs <- file.path(google.drive, "exploratory figures/ModelSelection-Multivariate")
pathSave <- file.path(google.drive, "pub_figs")


# =============================================================================
# VARIABLE RECODING LOOKUP TABLES
# =============================================================================

# Univariate model name mappings (more publication-friendly)
univariate_model_mapping <- c(
  # Drought indices
  "SPEI14" = "SPEI 14-day",
  "SPEI30" = "SPEI 30-day", 
  "SPEI60" = "SPEI 60-day",
  "SPEI90" = "SPEI 90-day",
  "SPI14day" = "SPI 14-day",
  "SPI30day" = "SPI 30-day",
  "SPI60day" = "SPI 60-day", 
  "SPI90day" = "SPI 90-day",
  
  # Temperature indices
  "Tmax_14day" = "Tmax 14-day",
  "Tmax_30day" = "Tmax 30-day",
  "Tmax_60day" = "Tmax 60-day", 
  "Tmax_90day" = "Tmax 90-day",
  "Tmin_14day" = "Tmin 14-day",
  "Tmin_30day" = "Tmin 30-day",
  "Tmin_60day" = "Tmin 60-day",
  "Tmin_90day" = "Tmin 90-day"
)

# Drought variable mappings for multivariate table
drought_variable_mapping <- c(
  "SPEI14" = "SPEI 14-day",
  "SPEI30" = "SPEI 30-day",
  "SPI30day" = "SPI 30-day",
  "SPI60day" = "SPI 60-day"
)

# Temperature variable mappings for multivariate table  
temperature_variable_mapping <- c(
  "Tmax_30day" = "Tmax 30-day",
  "Tmax_14day" = "Tmax 14-day", 
  "Tmin_60day" = "Tmin 60-day",
  "Tmax_60day" = "Tmax 60-day"
)

# =============================================================================
# FUNCTION TO APPLY RECODING
# =============================================================================

recode_variables <- function(data, column_name, mapping_table) {
  # Recode variables using a lookup table
  # 
  # Args:
  #   data: dataframe containing the column to recode
  #   column_name: name of column to recode  
  #   mapping_table: named vector with old_name = new_name mappings
  #   
  # Returns:
  #   dataframe with recoded column
  
  # Store original values
  original_values <- data[[column_name]]
  
  # Check if all values in the column have mappings
  unmapped_values <- setdiff(unique(original_values), names(mapping_table))
  if (length(unmapped_values) > 0) {
    warning(paste("Unmapped values found:", paste(unmapped_values, collapse = ", ")))
  }
  
  # Apply the mapping
  data[[column_name]] <- mapping_table[original_values]
  
  # For any unmapped values (resulting in NA), keep the original value
  na_indices <- is.na(data[[column_name]])
  data[[column_name]][na_indices] <- original_values[na_indices]
  
  return(data)
}

# =============================================================================
# UNIVARIATE TABLE PROCESSING
# =============================================================================

cat("Processing univariate table...\n")

# Load univariate data
univar.tab <- read.csv(file.path(google.drive, "data/processed_files", "univariate_table.csv"), header = T)

# Apply model name recoding using correct column name "model" (lowercase)
univar.tab <- recode_variables(univar.tab, "model", univariate_model_mapping)

# Round decimals to 3 places
univar.tab[, c("dAIC", "dR2", "dRMSE", "dRMSEper")] <- 
  round(univar.tab[, c("dAIC", "dR2", "dRMSE", "dRMSEper")], digits = 3)

# Update column names with proper formatting
colnames(univar.tab) <- c("Model", "ΔAIC", "ΔR²", "ΔRMSE", "ΔRMSE (%)",
                          "R² Rank", "RMSE Rank", "Avg. Rank", "Combined Rank")

# Drop unnecessary columns
univar.tab <- univar.tab[, !names(univar.tab) %in% c("Avg. Rank", "ΔRMSE (%)")]

# Create publication-quality flextable
ft_univar <- flextable(univar.tab) |>
  colformat_num(j = c("ΔAIC", "ΔR²", "ΔRMSE"), digits = 3) |>
  width(j = 1, width = 1.5) |>
  width(j = 2:4, width = 1.1) |>
  width(j = 5:7, width = 0.8) |>
  theme_booktabs() |>
  # Add better formatting
  bold(part = "header") |>
  align(align = "center", part = "header") |>
  align(j = 1, align = "left", part = "body") |>
  align(j = 2:7, align = "center", part = "body")

# Save univariate table
doc_univar <- read_docx() |>
  body_add_par("Table 1. Univariate Model Comparison", style = "heading 1") |>
  body_add_flextable(ft_univar)

print(doc_univar, target = file.path(google.drive, "pub_figs", "univar_model_comparison_table.docx"))
cat("Univariate table saved successfully.\n")

# Update column names with proper formatting
colnames(univar.tab) <- c("Model", "ΔAIC", "ΔR²", "ΔRMSE", "ΔRMSE (%)",
                          "R² Rank", "RMSE Rank", "Avg. Rank", "Combined Rank")

# Drop unnecessary columns
univar.tab <- univar.tab[, !names(univar.tab) %in% c("Avg. Rank", "ΔRMSE (%)")]

# Create publication-quality flextable
ft_univar <- flextable(univar.tab) |>
  colformat_num(j = c("ΔAIC", "ΔR²", "ΔRMSE"), digits = 3) |>
  width(j = 1, width = 1.5) |>
  width(j = 2:4, width = 1.1) |>
  width(j = 5:7, width = 0.8) |>
  theme_booktabs() |>
  # Add better formatting
  bold(part = "header") |>
  align(align = "center", part = "header") |>
  align(j = 1, align = "left", part = "body") |>
  align(j = 2:7, align = "center", part = "body")

# Save univariate table
doc_univar <- read_docx() |>
  body_add_par("Table 1. Univariate Model Comparison", style = "heading 1") |>
  body_add_flextable(ft_univar)

print(doc_univar, target = file.path(google.drive, "pub_figs", "univar_model_comparison_table.docx"))
cat("Univariate table saved successfully.\n")

# =============================================================================
# MULTIVARIATE TABLE PROCESSING  
# =============================================================================

cat("Processing multivariate table...\n")

# Load multivariate data
multivar.tab <- read.csv(file.path(google.drive, "data/processed_files", "multivarAdditive_table.csv"), header = T)

# Apply variable recoding using the actual column names from your data
multivar.tab <- recode_variables(multivar.tab, "DroughtVar", drought_variable_mapping)
multivar.tab <- recode_variables(multivar.tab, "TempVar", temperature_variable_mapping)

# Update column names with proper formatting (using actual column structure)
colnames(multivar.tab) <- c("Model", "Drought Variable", "Temperature Variable", "Model Type", 
                            "ΔAIC", "ΔR²", "ΔRMSE", "AIC Rank", "R² Rank", "RMSE Rank", 
                            "Avg. Rank", "Combined Rank")

# Drop unnecessary columns  
multivar.tab <- multivar.tab[, !names(multivar.tab) %in% c("Model", "Model Type", "Avg. Rank")]

# Round decimals to 3 places
multivar.tab[, c("ΔAIC", "ΔR²", "ΔRMSE")] <- 
  round(multivar.tab[, c("ΔAIC", "ΔR²", "ΔRMSE")], digits = 3)

# Create publication-quality flextable
ft_multivar <- flextable(multivar.tab) |>
  colformat_num(j = c("ΔAIC", "ΔR²", "ΔRMSE"), digits = 3) |>
  width(j = 1:2, width = 1.5) |>
  width(j = 3:5, width = 1.1) |>
  width(j = 6:8, width = 0.8) |>
  theme_booktabs() |>
  # Add better formatting
  bold(part = "header") |>
  align(align = "center", part = "header") |>
  align(j = 1:2, align = "left", part = "body") |>
  align(j = 3:8, align = "center", part = "body")

# Save multivariate table
doc_multivar <- read_docx() |>
  body_add_par("Table 2. Multivariate Model Comparison", style = "heading 1") |>
  body_add_flextable(ft_multivar)

print(doc_multivar, target = file.path(google.drive, "pub_figs", "multivar_model_comparison_table.docx"))
cat("Multivariate table saved successfully.\n")

# =============================================================================
# VALIDATION AND SUMMARY
# =============================================================================

cat("\n=== RECODING SUMMARY ===\n")
cat("Univariate models recoded:\n")
for (i in 1:length(univariate_model_mapping)) {
  cat(paste0("  ", names(univariate_model_mapping)[i], " → ", univariate_model_mapping[i], "\n"))
}

cat("\nDrought variables recoded:\n")
for (i in 1:length(drought_variable_mapping)) {
  cat(paste0("  ", names(drought_variable_mapping)[i], " → ", drought_variable_mapping[i], "\n"))
}

cat("\nTemperature variables recoded:\n")
for (i in 1:length(temperature_variable_mapping)) {
  cat(paste0("  ", names(temperature_variable_mapping)[i], " → ", temperature_variable_mapping[i], "\n"))
}

cat("\nBoth tables have been saved with publication-ready formatting!\n")