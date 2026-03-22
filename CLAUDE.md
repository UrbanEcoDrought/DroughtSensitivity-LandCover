# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

This is an R-based ecological analysis project examining how different land cover types in the Chicago metropolitan region respond to drought conditions. It uses Landsat-derived NDVI (vegetation greenness) and GRIDMET meteorological variables (SPI, SPEI, temperature) over 2000–2024, fitting daily mixed-effects models to quantify drought sensitivity across 7 land cover classes.

## Pipeline Workflow

Scripts are numbered and must run in order. Script 00 is `source()`'d by 01 and 02, not run standalone.

```
00_EarthEngine_HelperFunctions.R  ← helper functions (sourced by 01, 02)
01_setup_NLCD.R                   ← creates NLCD land cover masks in Google Earth Engine
02_extract_Landsat_NDVI_L5-9_past.R ← extracts NDVI per land cover, saves CSVs to Google Drive
03_NDVI_Met_DailyModels-Univariate.R ← merges NDVI+met data, fits univariate daily models
04_NDVI_Met_DailyModels-Multivariate.R ← tests additive & interaction multi-variable models
05_NDVI_Met_DailyModels-Final.R   ← runs 5 selected model configurations, extracts coefficients
06b_publication_figs.R            ← publication-quality figures (main figure script)
07_table_creation.R               ← publication tables
```

`run_pipeline.R` is the master script that runs 03→04→05→06b→06c→07 in sequence with logging.

Sensitivity tests, exploratory utilities, and superseded scripts live in `archive/`.

## Key Architecture Decisions

- **Daily sliding-window models:** Each model is fit on a 14-day window (±7 days) around each day of year (DOY 1–365), separately per land cover type. This means scripts 03–05 loop over 365 days × 7 land covers.
- **Mixed-effects models:** All models use `nlme::lme()` with random intercept by satellite mission (`1|mission`) to account for inter-sensor differences across Landsat 5/7/8/9.
- **Google Earth Engine via `rgee`:** Scripts 00–02 require an authenticated `rgee` session. They push assets to GEE and export results to Google Drive.
- **Growing season:** April 1 (DOY 91) through October 31 (DOY 304). Figures and analysis are filtered to this range.

## Land Cover Classes

7 classes derived from NLCD, ordered from rural to urban in figures:
Crop, Forest, Grassland, Urban-Open, Urban-Low, Urban-Medium, Urban-High.

Note: The "forest" class uses NLCD codes 41 (Deciduous) + 42 (Evergreen) + 43 (Mixed) + 90 (Woody Wetlands). This composite was originally extracted as "forest-wet" in GEE to test whether including woody wetlands changed results — it did not (t-test p=0.60, annual r=0.997). The original upland-only "forest" (41/42/43) is dropped and "forest-wet" is renamed to "forest" in script 03. See commits `bbbdfea` and `0a9b48e` for history.

## File Paths

Production scripts use the `G:` mount for Google Drive:
- `G:/Shared drives/Urban Ecological Drought/Manuscript - Urban Drought NDVI Daily Corrs/`

The `GOOGLE_DRIVE` variable is set at the top of scripts that need it. Some archived scripts still reference the old `~/Google Drive/` paths.

## Key R Packages

- **rgee, terra, raster** — geospatial/GEE operations (scripts 00–02)
- **nlme** — mixed-effects model fitting (scripts 03–05)
- **MuMIn** — marginal/conditional R² extraction
- **ggplot2, gridExtra, scales** — figure generation
- **flextable, officer** — table creation (script 07)
- **dplyr, tidyr, lubridate, zoo** — data wrangling and rolling windows

## Processed Data

All intermediate and final data lives in `processed_data/`. The central merged dataset is `landsat_ndvi_metVars_combined.csv` (and `.RDS`). Model output CSVs follow the pattern `DailyModel_VarSelection-*.csv`.

## Figures

Output PNGs go to `figures/`. Publication figures from `06b_publication_figs.R` use a consistent NLCD color palette and land-cover ordering.
