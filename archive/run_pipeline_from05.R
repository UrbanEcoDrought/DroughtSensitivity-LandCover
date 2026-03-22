# run_pipeline_from05.R
# ============================================================================
# Partial pipeline rerun: scripts 05-07 only.
# Use when scripts 03-04 have already completed successfully and only 05+
# need to be rerun (e.g., after fixing the forest-wet filename bug).
#
# Usage: Rscript run_pipeline_from05.R
# ============================================================================

# Setup
setwd("C:/Users/malexander/Documents/r_files/drought_grant/publication_code/DroughtSensitivity-LandCover")
dir.create("logs", showWarnings = FALSE)

timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
log_file <- file.path("logs", "pipeline_console.log")

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
    sink(script_log, split = TRUE)
    source(script_name, local = new.env(parent = globalenv()))
    sink()
    "SUCCESS"
  }, error = function(e) {
    try(sink(), silent = TRUE)
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
# RUN PIPELINE (05-07 only)
# ============================================================================

log_msg("Pipeline started (from script 05)")
log_msg(paste("Working directory:", getwd()))
log_msg(paste("R version:", R.version.string))
log_msg("")

run_script("05_NDVI_Met_DailyModels-Final.R")
run_script("06b_publication_figs.R")
run_script("06c_manuscript_statistics.R")
run_script("07_table_creation.R")

log_msg("============================================")
log_msg("PIPELINE COMPLETE (05-07)")
log_msg("============================================")
