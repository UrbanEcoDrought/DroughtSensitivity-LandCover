---
description: Load project context and check workflow status for drought sensitivity analysis
---

# Session Start - Load Drought Sensitivity Context

Load project context and prepare for a new analysis session.

## Step 1: Read Prior Session Notes

Check for the most recent session log to pick up where we left off:

```bash
ls -t logs/session_*.md 2>/dev/null | head -1
```

If a session log exists, **read it first** — it contains:
- What was done last time
- Currently running processes
- Next session checklist items
- Pending manuscript updates

Then read:
- **CLAUDE.md**: Project overview, pipeline workflow, and key architecture decisions
- **Memory files**: Check `.claude/projects/*/memory/` for prior session context

## Step 2: Check Running Processes

A long-running pipeline may still be active from a prior session:

```bash
# Check for running R scripts
ps aux | grep "[R]script" | head -5

# Check pipeline log for completion or errors
grep -E "PIPELINE COMPLETE|FAILED|ERROR|=== STARTING" logs/pipeline_console.log 2>/dev/null | tail -10

# If pipeline is running, check which script is active
tail -3 logs/pipeline_console.log 2>/dev/null
```

## Step 3: Check Current Workflow Phase

Identify where we are in the drought sensitivity pipeline:

### GEE Data Extraction (scripts 00-02, requires rgee)
- [ ] 00_EarthEngine_HelperFunctions.R - Helper functions (sourced, not run)
- [ ] 01_setup_NLCD.R - Create NLCD land cover masks in GEE (run once)
- [ ] 02_extract_Landsat_NDVI_L5-9_past.R - Extract NDVI per LC from Landsat 5/7/8/9

### Model Selection (scripts 03-04)
- [ ] 03_NDVI_Met_DailyModels-Univariate.R - 16 univariate models × 365 days × 7 LCs
- [ ] 04_NDVI_Met_DailyModels-Multivariate.R - Drought+temperature combinations, Table 1

### Final Models & Publication Outputs (scripts 05-07)
- [ ] 05_NDVI_Met_DailyModels-Final.R - 5 selected models, coefficients, partial effects, Table 2
- [ ] 06b_publication_figs.R - Figures 3, 4, 5 (t-stats, RMSE, partial effects)
- [ ] 06c_manuscript_statistics.R - Verify all manuscript-cited statistics
- [ ] 07_table_creation.R - Tables 1 & 2 as Word documents

## Step 4: Review Git Status

```bash
git status                    # Uncommitted changes
git log --oneline -10         # Recent commits
git branch -v                 # Current branch
```

## Step 5: Check Data Availability

```bash
# Check local processed data
ls processed_data/

# Check if Google Drive is mounted (needed for scripts 03-07 data)
ls "G:/Shared drives/Urban Ecological Drought/" 2>/dev/null && echo "Google Drive accessible" || echo "Google Drive NOT mounted"

# Check figures directory
ls figures/
```

## Step 6: Summarize and Ready

Provide a brief summary:

1. **Prior Session**: Key takeaways from the last session log (if any)
2. **Running Processes**: Is the pipeline still running? Did it complete or fail?
3. **Current Phase**: Which part of the pipeline is active?
4. **Recent Work**: Last commit message or recent changes
5. **Data Status**: Is Google Drive mounted? Are processed_data/ files current?
6. **Pending**: Next steps from prior session checklist + pipeline sequence
7. **Issues**: Any failed runs, missing data, or manuscript discrepancies?

Then ask: **"What would you like to work on today?"**

---

## Quick Reference

| Topic | Location |
|-------|----------|
| **Project overview** | `CLAUDE.md` |
| **Session logs** | `logs/session_*.md` |
| **Pipeline log** | `logs/pipeline_console.log` |
| **Per-script logs** | `logs/03_NDVI_Met_*.log`, etc. |
| **GEE helpers** | `00_EarthEngine_HelperFunctions.R` |
| **NLCD mask setup** | `01_setup_NLCD.R` |
| **NDVI extraction** | `02_extract_Landsat_NDVI_L5-9_past.R` |
| **Univariate models** | `03_NDVI_Met_DailyModels-Univariate.R` |
| **Multivariate models** | `04_NDVI_Met_DailyModels-Multivariate.R` |
| **Final models** | `05_NDVI_Met_DailyModels-Final.R` |
| **Publication figures** | `06b_publication_figs.R` |
| **Manuscript statistics** | `06c_manuscript_statistics.R` |
| **Publication tables** | `07_table_creation.R` |
| **Local processed data** | `processed_data/` |
| **Figures** | `figures/` |
| **Google Drive (G:)** | `G:/Shared drives/Urban Ecological Drought/Manuscript - Urban Drought NDVI Daily Corrs/` |
| **Prior results backup** | `processed_data_backup_2023run/` |

## Key Reminders

- **Growing season**: DOY 91-304 (April 1 - October 31) across all scripts
- **7 land cover classes**: Crop, Forest, Grassland, Urban-Open, Urban-Low, Urban-Medium, Urban-High
- **Model**: nlme::lme() with random intercept by satellite mission (1|mission)
- **Primary model**: add1 = SPEI 14-day + Tmax 30-day (additive)
- **Google Drive**: All scripts now use `G:/Shared drives/Urban Ecological Drought/Manuscript - Urban Drought NDVI Daily Corrs/`
- **Sleep**: If pipeline is running, ensure sleep is disabled: `powercfg.exe //change standby-timeout-ac 0`
