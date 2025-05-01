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
pathSave <- file.path(google.drive, "data/processed_files")

# # # # # # # # # # # # # # # # # # # # # # # # 
# Univariate Table----
# loading in data
univar.tab <- read.csv(file.path(google.drive, "data/processed_files", "univariate_table.csv"), header=T)

# limiting decimals to 3 places
univar.tab[,c("dAIC", "dR2", "dRMSE", "dRMSEper")] <- round(univar.tab[,c("dAIC", "dR2", "dRMSE", "dRMSEper")], digits=3)

colnames(univar.tab) <- c("Model", "ΔAIC", "ΔR²", "ΔRMSE", "ΔRMSE (%)",
                  "R² Rank", "RMSE Rank", "Avg. Rank", "Combined Rank")
# Dropping Avg. Rank
univar.tab <- univar.tab[,!names(univar.tab)%in% c("Avg. Rank","ΔRMSE (%)")]

# Create flextable
ft <- flextable(univar.tab) |>
  colformat_num(j = c("ΔAIC", "ΔR²", "ΔRMSE"), digits = 3) |>
  width(j = 1, width = 1.5) |>
  width(j = 2:4, width = 1.1) |>
  width(j = 5:7, width = 0.8) |>
  theme_booktabs()

# Save as Word file
doc <- read_docx() |>
  body_add_par("Table 1. Univariate Model Comparison", style = "heading 1") |>
  body_add_flextable(ft)

print(doc, target = file.path(google.drive, "data/processed_files","univar_model_comparison_table.docx"))


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# multivariate table----

# loading in multivariate data
multivar.tab <- read.csv(file.path(google.drive, "data/processed_files", "multivarAdditive_table.csv"), header=T) 

colnames(multivar.tab) <- c("Model", "Drought Variable", "Temperature Variable","Model Type", "ΔAIC", "ΔR²", "ΔRMSE", "ΔRMSE (%)",
                          "R² Rank", "RMSE Rank", "Avg. Rank", "Combined Rank")
# Dropping Avg. Rank
multivar.tab <- multivar.tab[,!names(multivar.tab)%in% c("Model","Model Type","Avg. Rank")]

# rounding
multivar.tab[,c("ΔAIC", "ΔR²", "ΔRMSE")] <- round(multivar.tab[,c("ΔAIC", "ΔR²", "ΔRMSE")], digits=3)

# Create flextable
ft <- flextable(multivar.tab) |>
  colformat_num(j = c("ΔAIC", "ΔR²", "ΔRMSE"), digits = 3) |>
  width(j = 1:2, width = 1.5) |>
  width(j = 3:5, width = 1.1) |>
  width(j = 6:8, width = 0.8) |>
  theme_booktabs()

# Save as Word file
doc <- read_docx() |>
  body_add_par("Table 2. Multivariate Model Comparison", style = "heading 1") |>
  body_add_flextable(ft)

print(doc, target = file.path(google.drive, "data/processed_files","multivar_model_comparison_table.docx"))
      