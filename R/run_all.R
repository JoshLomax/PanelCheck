# run_all.R — Master analysis script for PanelCheck R translation
#
# Run this file to perform a complete sensory panel analysis on your data.
#
# QUICK START:
#   1. Open RStudio and set the working directory to the R/ folder:
#        setwd("path/to/PanelCheck/R")
#   2. Edit the DATA_FILE path below to point to your data.
#   3. Source this script:
#        source("run_all.R")
#   4. Results appear in the console; plots open in the RStudio Plots pane.
#      Set SAVE_PLOTS = TRUE  to write PNG files to the output folder.
#      Set SAVE_TABLES = TRUE to write the results workbook (.xlsx).
#
# ERROR HANDLING:
#   Each step is wrapped in .try_step(). If a step errors (e.g. too many
#   samples for a particular plot, aliased model coefficients, etc.) it is
#   skipped gracefully and the reason is accumulated in a log. All skipped
#   steps are printed together at the very end of the run.
#
# ─────────────────────────────────────────────────────────────────────────────
# See README.md for data pre-processing guidance and folder setup instructions.

# ══ USER SETTINGS ════════════════════════════════════════════════════════════
# These defaults apply when sourcing run_all.R directly (single-dataset mode).
# When called from run_batch.R these variables are already set by the batch
# script and the if(!exists()) guards below leave them untouched.

if (!exists("DATA_FILE"))  DATA_FILE  <- "../Data_Bread.xlsx"
if (!exists("OUTPUT_DIR")) OUTPUT_DIR <- "../figs/R"
if (!exists("SAVE_PLOTS")) SAVE_PLOTS <- TRUE   # TRUE → save PNG plots
if (!exists("SAVE_TABLES")) SAVE_TABLES <- TRUE # TRUE → save xlsx workbook
if (!exists("FIG_WIDTH"))  FIG_WIDTH  <- 12
if (!exists("FIG_HEIGHT")) FIG_HEIGHT <- 8

# ══ END USER SETTINGS ════════════════════════════════════════════════════════


# ── 0. Setup ──────────────────────────────────────────────────────────────────

# Determine script directory so relative paths work whether sourced or run
script_dir <- tryCatch(
  dirname(sys.frame(1)$ofile),
  error = function(e) getwd()
)
setwd(script_dir)

# Source all modules
source("utils.R")
source("00_load_data.R")
source("01_panel_performance.R")
source("02_profile_plots.R")
source("03_mixed_model.R")
source("04_pca.R")
source("05_fvalue_overview.R")

# Install/load required packages
check_packages()

suppressPackageStartupMessages({
  library(ggplot2)

  # lme4/lmerTest/multcomp/FactoMineR all load MASS as a transitive dependency.
  # MASS exports select() which conflicts with dplyr. Because library() won't
  # reposition a package that is already attached, we detach-and-reattach dplyr
  # so it sits at position 2 (above MASS) on the search path.
  if ("package:dplyr" %in% search()) {
    suppressWarnings(detach("package:dplyr", character.only = TRUE, unload = FALSE))
  }
  library(dplyr)
})

# Create output directory if either save option is active
if ((SAVE_PLOTS || SAVE_TABLES) && !dir.exists(OUTPUT_DIR)) {
  dir.create(OUTPUT_DIR, recursive = TRUE)
  message("Created output directory: ", normalizePath(OUTPUT_DIR))
}

.save <- function(p, filename, w = FIG_WIDTH, h = FIG_HEIGHT) {
  if (SAVE_PLOTS && !is.null(p)) {
    path <- file.path(OUTPUT_DIR, filename)
    ggsave(filename = path, plot = p, width = w, height = h, dpi = 300)
    message("  Saved: ", path)
  }
}

# Initialise the skip log (functions defined in utils.R)
.reset_skip_log()


# ── 1. Load data ───────────────────────────────────────────────────────────────

cat("\n========================================\n")
cat(" PanelCheck R — Full Analysis\n")
cat("========================================\n\n")

df <- load_panel_data(DATA_FILE)
summarise_panel_data(df)

n_assessors <- length(unique(df$Assessor))
n_samples   <- length(unique(df$Sample))

# Accumulate all result tables here; written to a single xlsx at the end
tables <- list()


# ── 2. Panel performance (PRIORITY) ───────────────────────────────────────────

cat("\n--- Descriptive Statistics ---\n")
desc <- .try_step("Descriptive statistics",
                  descriptive_stats(df, by_sample = TRUE))
if (!is.null(desc)) {
  print(desc, n = 20)
  tables[["descriptive_stats"]]         <- desc
  tables[["descriptive_stats_overall"]] <- descriptive_stats(df, by_sample = FALSE)
}

cat("\n--- Panel Performance Statistics ---\n")
perf <- .try_step("Panel performance", panel_performance(df))
if (!is.null(perf)) {
  print_panel_performance(perf)
}

cat("\n--- Repeatability ANOVA (per assessor) ---\n")
rep_anova <- .try_step("Repeatability ANOVA", repeatability_anova(df))
if (!is.null(rep_anova)) {
  rep_anova_wide <- rep_anova %>%
    filter(Effect %in% c("Sample", "Replicate")) %>%
    select(Assessor, Attribute, Effect, F_value, p_value, sig)
  print(tibble::as_tibble(rep_anova_wide), n = 30)
}

cat("\n--- Table 5: Panellist Performance Counts ---\n")
perf_counts <- if (!is.null(perf) && !is.null(rep_anova)) {
  .try_step("Table 5: performance counts",
            panel_performance_counts(perf$discrimination, rep_anova, perf$agreement))
} else NULL
if (!is.null(perf_counts)) print(perf_counts)

cat("\n--- Table 6: Panel ANOVA F-ratios ---\n")
anova_wide <- if (!is.null(perf)) {
  .try_step("Table 6: ANOVA F-ratios", format_anova_table(perf$anova))
} else NULL
if (!is.null(anova_wide)) print(anova_wide)

# Store tables
if (!is.null(perf_counts))  tables[["Table5_perf_counts"]]      <- perf_counts
if (!is.null(anova_wide))   tables[["Table6_anova_Fratios"]]    <- anova_wide
if (!is.null(perf)) {
  tables[["panel_performance"]]       <- perf$summary
  tables[["assessor_discrimination"]] <- perf$discrimination
  tables[["assessor_agreement"]]      <- perf$agreement
  tables[["panel_anova"]]             <- perf$anova
}
if (!is.null(rep_anova)) tables[["repeatability_anova"]] <- rep_anova

# Assessor-level flag summary (QC table — separate from Table 5 / Table 6)
assessor_flags <- if (!is.null(perf)) {
  .try_step("Assessor flag summary", assessor_flag_summary(perf))
} else NULL
if (!is.null(assessor_flags)) {
  print(assessor_flags)
  tables[["assessor_flag_summary"]] <- assessor_flags
}


# ── 3. F-value overview plots ─────────────────────────────────────────────────

cat("\n--- F-value Overview Plots ---\n")

if (!is.null(perf)) {
  p_fheat <- .try_step("Plot: F-value heatmap",
                        plot_fvalue_heatmap(perf$discrimination))
  if (!is.null(p_fheat)) { print(p_fheat); .save(p_fheat, "fvalue_heatmap.png", w = 10, h = 6) }

  p_pval <- .try_step("Plot: p-value heatmap",
                       plot_pvalue_heatmap(perf$discrimination))
  if (!is.null(p_pval)) { print(p_pval); .save(p_pval, "pvalue_heatmap.png", w = 10, h = 6) }

  p_disc_bars <- .try_step("Plot: discrimination bars",
                             plot_discrimination_bars(perf$discrimination))
  if (!is.null(p_disc_bars)) { print(p_disc_bars); .save(p_disc_bars, "discrimination_bars.png", w = 8, h = 5) }

  p_fdot <- .try_step("Plot: F-value dotplot",
                       plot_fvalue_dotplot(perf$discrimination, n_samples = n_samples, ncol = 4))
  if (!is.null(p_fdot)) { print(p_fdot); .save(p_fdot, "fvalue_dotplot.png") }

  p_panel_anova <- .try_step("Plot: panel ANOVA F-values",
                               plot_panel_anova(perf$anova))
  if (!is.null(p_panel_anova)) { print(p_panel_anova); .save(p_panel_anova, "panel_anova_fvalues.png", w = 10, h = 9) }
}


# ── 4. Profile plots ───────────────────────────────────────────────────────────

cat("\n--- Profile Plots ---\n")

p_all_profiles <- .try_step("Plot: all-assessor profile",
                              plot_profiles(df, ncol = 4))
if (!is.null(p_all_profiles)) { print(p_all_profiles); .save(p_all_profiles, "profile_all_assessors.png", w = FIG_WIDTH, h = 10) }

p_means <- .try_step("Plot: panel mean scores",
                      plot_mean_scores(df))
if (!is.null(p_means)) { print(p_means); .save(p_means, "panel_mean_scores.png") }

p_spider <- .try_step("Plot: spider chart",
                       plot_spider(df))
if (!is.null(p_spider)) { print(p_spider); .save(p_spider, "spider_plot.png", w = 8, h = 7) }

# Individual assessor profiles — each wrapped independently
for (ass in sort(unique(df$Assessor))) {
  p_ass <- .try_step(
    paste0("Plot: assessor profile — ", ass),
    plot_assessor_profile(df, ass)
  )
  if (!is.null(p_ass)) {
    print(p_ass)
    safe_name <- gsub("[^A-Za-z0-9_-]", "_", ass)
    .save(p_ass, paste0("profile_", safe_name, ".png"))
  }
}


# ── 5. Mixed model ANOVA + Tukey HSD ──────────────────────────────────────────

cat("\n--- Mixed Model ANOVA + Tukey HSD ---\n")
message("Fitting mixed models (this may take a moment)...")

mm <- .try_step("Mixed model ANOVA", mixed_model_anova(df, verbose = TRUE))

if (!is.null(mm)) {
  cat("\nFixed effects (Sample F-tests):\n")
  print(mm$fixed_effects)

  thsd <- .try_step("Tukey HSD", tukey_hsd(mm))

  if (!is.null(thsd)) {
    cat("\nTukey HSD compact letter display:\n")
    print(tibble::as_tibble(thsd$cld), n = 50)

    p_tukey_means <- .try_step("Plot: Tukey means + CLD",
                                plot_tukey_means(thsd))
    if (!is.null(p_tukey_means)) { print(p_tukey_means); .save(p_tukey_means, "tukey_means_cld.png") }

    p_tukey_heat <- .try_step("Plot: Tukey p-value heatmap",
                               plot_tukey_heatmap(thsd))
    if (!is.null(p_tukey_heat)) { print(p_tukey_heat); .save(p_tukey_heat, "tukey_heatmap.png", w = 10, h = 7) }

    tables[["tukey_contrasts"]] <- thsd$contrasts
    tables[["tukey_cld"]]       <- thsd$cld
    tables[["tukey_emmeans"]]   <- thsd$emmeans
  } else {
    # thsd was skipped (e.g. too many samples) — already logged by .try_step / tukey_hsd()
  }

  p_vc <- .try_step("Plot: variance components",
                     plot_variance_components(mm))
  if (!is.null(p_vc)) { print(p_vc); .save(p_vc, "variance_components.png", w = FIG_WIDTH, h = 6) }

  tables[["mixed_model_fixed"]]  <- mm$fixed_effects
  tables[["mixed_model_random"]] <- mm$random_effects

  # Variance component flags are embedded in mm$random_effects (via flag_variance_components())
  # Expose the flagged VC table as its own sheet for visibility
  tables[["vc_flags"]] <- mm$random_effects
}


# ── 6. PCA ────────────────────────────────────────────────────────────────────

cat("\n--- Principal Component Analysis ---\n")

pca <- .try_step("PCA", run_pca(df))

if (!is.null(pca)) {
  cat("Variance explained:\n")
  cat(sprintf("  C1: %.1f%%   C2: %.1f%%   C1+C2: %.1f%%\n",
              pca$pct_var[1], pca$pct_var[2],
              pca$pct_var[1] + pca$pct_var[2]))

  p_biplot <- .try_step("Plot: PCA biplot",
                         plot_pca_biplot(pca))
  if (!is.null(p_biplot)) { print(p_biplot); .save(p_biplot, "pca_biplot.png", w = 9, h = 7) }

  p_scree <- .try_step("Plot: PCA scree",
                        plot_pca_scree(pca))
  if (!is.null(p_scree)) { print(p_scree); .save(p_scree, "pca_scree.png", w = 7, h = 5) }

  p_loadings <- .try_step("Plot: PCA loadings",
                            plot_pca_loadings(pca))
  if (!is.null(p_loadings)) { print(p_loadings); .save(p_loadings, "pca_loadings.png", w = 9, h = 5) }

  tables[["pca_sample_coords"]]   <- pca$ind_coords
  tables[["pca_variable_coords"]] <- pca$var_coords
}


# ── 7. Write Excel output ──────────────────────────────────────────────────────
#
# Strategy:
#   1. Sanitise every table (coerce list-columns → character, factors → character).
#   2. Try a single bulk write with writexl::write_xlsx().
#   3. On failure, test each sheet individually in a temp file; collect the ones
#      that pass and write those, logging any that still fail.

.sanitise_df <- function(df) {
  if (is.null(df) || !is.data.frame(df)) return(df)
  df[] <- lapply(df, function(col) {
    if      (is.list(col))   vapply(col, function(x) paste(x, collapse = "; "), character(1))
    else if (is.factor(col)) as.character(col)
    else col
  })
  df
}

if (SAVE_TABLES && length(tables) > 0) {
  xlsx_path <- file.path(OUTPUT_DIR, "PanelCheck_results.xlsx")

  # Sanitise all tables; truncate sheet names to Excel's 31-character limit.
  safe_tables        <- lapply(tables, .sanitise_df)
  names(safe_tables) <- substr(names(safe_tables), 1, 31)

  # Attempt bulk write first.
  bulk_ok <- tryCatch({
    writexl::write_xlsx(safe_tables, path = xlsx_path)
    TRUE
  }, error = function(e) {
    message("  ! Bulk xlsx write failed: ", conditionMessage(e))
    message("    Retrying sheet by sheet...")
    FALSE
  })

  # Fallback: test each sheet individually, keep those that succeed.
  if (!bulk_ok) {
    working <- list()
    for (nm in names(safe_tables)) {
      tmp <- tempfile(fileext = ".xlsx")
      sheet_ok <- tryCatch({
        writexl::write_xlsx(list(x = safe_tables[[nm]]), path = tmp)
        TRUE
      }, error = function(e) {
        .log_skip(
          paste0("Excel sheet '", nm, "'"),
          conditionMessage(e)
        )
        FALSE
      })
      if (sheet_ok) working[[nm]] <- safe_tables[[nm]]
    }

    if (length(working) > 0) {
      tryCatch(
        writexl::write_xlsx(working, path = xlsx_path),
        error = function(e) {
          .log_skip("Excel workbook final write", conditionMessage(e))
          message("  ! Could not write workbook: ", conditionMessage(e))
        }
      )
    } else {
      .log_skip("Excel workbook write", "No sheets could be serialised successfully")
    }
  }

  if (file.exists(xlsx_path)) {
    message("Saved results workbook: ", xlsx_path,
            "\n  Sheets: ", paste(names(safe_tables), collapse = ", "))
  }
}


# ── 8. Done ────────────────────────────────────────────────────────────────────

cat("\n========================================\n")
cat(" Analysis complete!\n")
if (SAVE_PLOTS || SAVE_TABLES) {
  cat(" Outputs saved to:", normalizePath(OUTPUT_DIR), "\n")
  if (SAVE_PLOTS)  cat("   Plots:  PNG files\n")
  if (SAVE_TABLES) cat("   Tables: PanelCheck_results.xlsx\n")
}
if (!SAVE_PLOTS)  cat(" Set SAVE_PLOTS  = TRUE to export PNG plots.\n")
if (!SAVE_TABLES) cat(" Set SAVE_TABLES = TRUE to export the results workbook.\n")
cat("========================================\n")

# Print cumulative skip log — all steps that were bypassed this run
.print_skip_summary()

# Return the key objects invisibly so they are accessible after sourcing
invisible(list(df = df, perf = perf, mm = mm, pca = pca))
