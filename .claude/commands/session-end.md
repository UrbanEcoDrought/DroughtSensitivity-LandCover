---
description: Wrap up session with commits, documentation, and verification
---

# Session End Workflow

Guide through ending this drought sensitivity analysis session properly.

## Step 1: Review Changes

```bash
git status        # See all modified and untracked files
git diff          # Review actual changes
```

Identify files that shouldn't be committed:
- Large CSV data files (keep in `processed_data/` or Google Drive)
- RDS files > 50MB
- Temporary files (`*.tmp`, `*.swp`, `Rplots.pdf`)
- Google Drive sync artifacts
- Personal scratch or exploration scripts

## Step 2: Commit Workflow

Stage appropriate files and write descriptive commit message using tags:

| Tag | Use for |
|-----|---------|
| `[gee]` | Google Earth Engine scripts (00-02), NLCD masks, NDVI extraction |
| `[model]` | Model selection or fitting scripts (03-05) |
| `[figures]` | Publication figure scripts (06b), figure outputs |
| `[tables]` | Table creation scripts (07), table outputs |
| `[stats]` | Manuscript statistics verification (06c) |
| `[data]` | Data cleaning, merging, or processed data changes |
| `[docs]` | Documentation, CLAUDE.md, comments |
| `[fix]` | Bug fixes |
| `[cleanup]` | Code cleanup, archiving, refactoring |

Example: `[figures] Add manuscript-numbered figure saves (Figure_3, Figure_4, Figure_5)`

```bash
git add <files>
git commit -m "[tag] Descriptive message"
```

## Step 3: Verify Manuscript Consistency

If any model code or data processing changed this session:

1. **Run `06c_manuscript_statistics.R`** to recompute all cited values
2. **Flag any discrepancies** between computed values and manuscript text
3. **Check growing season definition** is DOY 91-304 everywhere
4. **Check sig figs**: ΔAIC (1dp), ΔR² (3dp), ΔRMSE (4dp)

## Step 4: Update Documentation

### CLAUDE.md (if workflow changed)
- Update pipeline descriptions
- Add new script documentation
- Note any data path changes

### Memory files (if persistent context learned)
- Save user preferences, project decisions, or reference information
- Update stale memories

## Step 5: Check Output Integrity

```bash
# Verify figures generated correctly
ls -lh figures/Figure_*.png

# Check processed data files exist and have reasonable sizes
ls -lh processed_data/*.csv

# If tables were regenerated, verify .docx outputs
ls -lh ~/Google\ Drive/Shared\ drives/Urban\ Ecological\ Drought/Manuscript\ -\ Urban\ Drought\ NDVI/pub_figs/*.docx 2>/dev/null
```

## Step 6: Final Summary

Provide session summary:

1. **Work Completed**: Scripts modified, analyses run, issues resolved
2. **Files Changed**: Which scripts were edited and why
3. **Commits**: What was committed
4. **Manuscript Impact**: Any statistics or figures that changed — flag for manuscript update
5. **Next Session**: Clear next steps

---

## Pre-Submission Checklist

For the final publication push, verify:

- [ ] All scripts run without errors (03 → 04 → 05 → 06b → 06c → 07)
- [ ] Growing season is DOY 91-304 in all scripts
- [ ] Table sig figs: ΔAIC=1dp, ΔR²=3dp, ΔRMSE=4dp
- [ ] `06c_manuscript_statistics.R` output matches manuscript text
- [ ] Figure 3 caption says "April-October" (not "March-October")
- [ ] Methods describe "linear mixed-effects models" (not "linear models")
- [ ] Data date range in manuscript matches actual data (2001-2023)
- [ ] Archived scripts have ARCHIVE header comments
- [ ] All changes committed and pushed
- [ ] Documentation updated

---

**Remember**:
- Never commit large data files to git
- Google Drive paths differ between scripts — check `CLAUDE.md` for details
- The `forest` class is actually `forest-wet` renamed — this is intentional
- Table CSVs live on Google Drive, not locally — rerun scripts 04/05 to regenerate
