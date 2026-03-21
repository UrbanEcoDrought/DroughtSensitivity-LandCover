# run_pipeline.R
# ============================================================================
# Master script to run the full analysis pipeline (scripts 03-07) in sequence.
#
# Usage: Rscript run_pipeline.R
#   or source("run_pipeline.R") from an R session
#
# Logs are written to logs/ directory with timestamps.
# Each script's output is captured so failures can be diagnosed.
#
# ESTIMATED RUNTIME:
#   Script 03: ~4-8 hours (365 days x 7 LCs x 18 univariate models)
#   Script 04: ~6-12 hours (365 days x 7 LCs x 32 multivariate models)
#   Script 05: ~6-12 hours (365 days x 7 LCs x 5 final models + partial effects)
#   Script 06b: ~2-5 minutes (figure generation)
#   Script 06c: ~1-2 minutes (statistics computation)
#   Script 07:  ~1 minute (table creation)
#   Total: roughly 16-32 hours depending on machine
# ============================================================================

# Setup
setwd("C:/Users/malexander/Documents/r_files/drought_grant/publication_code/DroughtSensitivity-LandCover")
dir.create("logs", showWarnings = FALSE)

timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
log_file <- file.path("logs", paste0("pipeline_", timestamp, ".log"))

# Logging helper
log_msg <- function(msg) {
  entry <- paste0("[", Sys.time(), "] ", msg)
  cat(entry, "\n")
  cat(entry, "\n", file = log_file, append = TRUE)
}

# Script runner
run_script <- function(script_name) {
  log_msg(paste("=== STARTING:", script_name, "==="))
  script_log <- file.path("logs", paste0(gsub("\\.R$", "", script_name), "_", timestamp, ".log"))

  start_time <- Sys.time()

  result <- tryCatch({
    # Capture all output to script-specific log
    sink(script_log, split = TRUE)
    source(script_name, local = new.env(parent = globalenv()))
    sink()
    "SUCCESS"
  }, error = function(e) {
    try(sink(), silent = TRUE)  # Make sure sink is closed
    log_msg(paste("ERROR in", script_name, ":", conditionMessage(e)))
    paste("FAILED:", conditionMessage(e))
  })

  elapsed <- round(difftime(Sys.time(), start_time, units = "mins"), 1)
  log_msg(paste("===", result, ":", script_name, "in", elapsed, "minutes ==="))
  log_msg("")

  if (grepl("^FAILED", result)) {
    log_msg("Pipeline halted due to error. Fix the issue and re-run from this script.")
    stop(paste("Pipeline stopped at", script_name))
  }

  invisible(result)
}

# ============================================================================
# RUN PIPELINE
# ============================================================================

log_msg("Pipeline started")
log_msg(paste("Working directory:", getwd()))
log_msg(paste("R version:", R.version.string))
log_msg("")

# Script 03: Univariate model selection
# Reads: NDVI data (G:/), GRIDMET met data (G:/)
# Writes: per-LC model stats, ALL file, Summaries → Google Drive
run_script("03_NDVI_Met_DailyModels-Univariate.R")

# Script 04: Multivariate model selection + Table 1
# Reads: ndviMet + univariate ALL file from Google Drive
# Writes: univariate_table.csv, multivariate model stats → Google Drive
run_script("04_NDVI_Met_DailyModels-Multivariate.R")

# Script 05: Final model fitting + Table 2 + partial effects
# Reads: ndviMet + multivariate stats from Google Drive
# Writes: final model stats, partial effects, multivarAdditive_table.csv → Google Drive
run_script("05_NDVI_Met_DailyModels-Final.R")

# Script 06b: Publication figures (Figures 3, 4, 5)
# Reads: final model outputs from Google Drive
# Writes: figures to Google Drive pub_figs/ and local figures/
run_script("06b_publication_figs.R")

# Script 06c: Manuscript statistics verification
# Reads: ALL file (local or Google Drive), final model outputs
# Writes: nothing (prints to console/log only)
run_script("06c_manuscript_statistics.R")

# Script 07: Publication tables (Tables 1, 2)
# Reads: univariate_table.csv, multivarAdditive_table.csv from Google Drive
# Writes: .docx table files to Google Drive pub_figs/
run_script("07_table_creation.R")

log_msg("============================================")
log_msg("PIPELINE COMPLETE")
log_msg(paste("Total time:", round(difftime(Sys.time(), as.POSIXct(paste0(
  substr(timestamp, 1, 4), "-", substr(timestamp, 5, 6), "-", substr(timestamp, 7, 8),
  " ", substr(timestamp, 10, 11), ":", substr(timestamp, 12, 13), ":", substr(timestamp, 14, 15)
)), units = "hours"), 1), "hours"))
log_msg("Check logs/ directory for per-script output.")
log_msg("Run 06c log to verify manuscript statistics.")
log_msg("============================================")
