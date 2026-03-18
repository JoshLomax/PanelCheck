# run_batch.R — Run the full PanelCheck analysis for multiple datasets
#
# Sources run_all.R once per dataset, saving plots and tables to a dedicated
# sub-folder for each dataset under BASE_DIR.
#
# QUICK START:
#   1. Set the working directory to the R/ folder:
#        setwd("path/to/PanelCheck/R")
#   2. Edit BASE_DIR and sensory_sets below.
#   3. Source this script:
#        source("run_batch.R")
#
# OUTPUT STRUCTURE:
#   BASE_DIR/
#   ├── <dataset1>/
#   │   ├── PanelCheck_results.xlsx
#   │   ├── fvalue_heatmap.png
#   │   └── ...
#   ├── <dataset2>/
#   │   └── ...
#   └── ...
#
# ─────────────────────────────────────────────────────────────────────────────

# ══ BATCH SETTINGS ════════════════════════════════════════════════════════════

# Directory containing input .xlsx files and where output sub-folders are created
BASE_DIR <- "../../panel_check_results"

# Dataset names — each must match an .xlsx file in BASE_DIR
sensory_sets <- c(
  "GP3by3_qda",
  "GP3by3_dfc",
  "2025_qda",
  "2025_dfc",
  "initial_qda",
  "initial_dfc"
)

SAVE_PLOTS  <- TRUE   # Write PNG plots for every dataset
SAVE_TABLES <- TRUE   # Write PanelCheck_results.xlsx for every dataset
FIG_WIDTH   <- 12
FIG_HEIGHT  <- 8

# ══ END BATCH SETTINGS ════════════════════════════════════════════════════════


# ── Batch loop ────────────────────────────────────────────────────────────────

batch_log <- list(completed = character(0), skipped = character(0))

for (set_name in sensory_sets) {

  data_file <- file.path(BASE_DIR, paste0(set_name, ".xlsx"))

  # Skip missing files without stopping the whole batch
  if (!file.exists(data_file)) {
    message(sprintf("\n[SKIPPING] %s — file not found:\n  %s", set_name, data_file))
    batch_log$skipped <- c(batch_log$skipped, set_name)
    next
  }

  cat(sprintf(
    "\n\n╔════════════════════════════════════════╗\n║  Analysing: %-28s║\n╚════════════════════════════════════════╝\n",
    paste0(set_name, "")
  ))

  # Set path globals — run_all.R reads these via if(!exists()) guards
  DATA_FILE  <- data_file
  OUTPUT_DIR <- file.path(BASE_DIR, set_name)

  # Create the output sub-folder now, before run_all.R starts, so that
  # results land in a dedicated folder even if SAVE_PLOTS/SAVE_TABLES
  # are set after the directory-creation check inside run_all.R.
  if (!dir.exists(OUTPUT_DIR)) {
    dir.create(OUTPUT_DIR, recursive = TRUE)
    message(sprintf("  Created results folder: %s", normalizePath(OUTPUT_DIR)))
  }

  ok <- tryCatch({
    source("run_all.R", echo = FALSE)
    TRUE
  }, error = function(e) {
    message(sprintf("  ! '%s' aborted: %s", set_name, conditionMessage(e)))
    FALSE
  })

  if (ok) batch_log$completed <- c(batch_log$completed, set_name)
  else    batch_log$skipped   <- c(batch_log$skipped,   set_name)

  # Remove dataset-specific globals so the next iteration sets them fresh
  rm(DATA_FILE, OUTPUT_DIR)
}


# ── Batch summary ─────────────────────────────────────────────────────────────

cat("\n\n════════════════════════════════════════\n")
cat(sprintf(" Batch complete: %d / %d datasets\n",
            length(batch_log$completed), length(sensory_sets)))
if (length(batch_log$completed) > 0)
  cat("  Completed:", paste(batch_log$completed, collapse = ", "), "\n")
if (length(batch_log$skipped) > 0)
  cat("  Skipped:  ", paste(batch_log$skipped,   collapse = ", "), "\n")
cat("════════════════════════════════════════\n\n")

invisible(batch_log)
