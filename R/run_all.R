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
#      Set SAVE_PLOTS = TRUE to write PNG files to the output folder.
#
# ─────────────────────────────────────────────────────────────────────────────

# ══ USER SETTINGS ════════════════════════════════════════════════════════════

# Path to your data file (relative to this script, or use an absolute path)
DATA_FILE   <- "../Data_Bread.xlsx"

# Where to save plots and CSV output (relative to this script)
OUTPUT_DIR  <- "../figs/R"

# Set TRUE to save plots as PNG files
SAVE_PLOTS  <- TRUE

# Figure dimensions (inches)
FIG_WIDTH   <- 12
FIG_HEIGHT  <- 8

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
  # so it sits at position 2 (above MASS) on the search path. Any MASS-specific
  # function needed directly should be called as MASS::function().
  if ("package:dplyr" %in% search()) {
    suppressWarnings(detach("package:dplyr", character.only = TRUE, unload = FALSE))
  }
  library(dplyr)
})

# Create output directory
if (SAVE_PLOTS && !dir.exists(OUTPUT_DIR)) {
  dir.create(OUTPUT_DIR, recursive = TRUE)
  message("Created output directory: ", normalizePath(OUTPUT_DIR))
}

.save <- function(p, filename, w = FIG_WIDTH, h = FIG_HEIGHT) {
  if (SAVE_PLOTS) {
    path <- file.path(OUTPUT_DIR, filename)
    ggsave(filename = path, plot = p, width = w, height = h, dpi = 300)
    message("  Saved: ", path)
  }
}


# ── 1. Load data ──────────────────────────────────────────────────────────────

cat("\n========================================\n")
cat(" PanelCheck R — Full Analysis\n")
cat("========================================\n\n")

df <- load_panel_data(DATA_FILE)
summarise_panel_data(df)

n_assessors <- length(unique(df$Assessor))
n_samples   <- length(unique(df$Sample))

# Accumulate all result tables here; written to a single xlsx at the end
tables <- list()


# ── 2. Panel performance (PRIORITY) ──────────────────────────────────────────

cat("\n--- Descriptive Statistics ---\n")
desc <- descriptive_stats(df, by_sample = TRUE)
print(desc, n = 20)
tables[["descriptive_stats"]]         <- desc
tables[["descriptive_stats_overall"]] <- descriptive_stats(df, by_sample = FALSE)

cat("\n--- Panel Performance Statistics ---\n")
perf <- panel_performance(df)
print_panel_performance(perf)

cat("\n--- Repeatability ANOVA (per assessor) ---\n")
rep_anova <- repeatability_anova(df)
rep_anova_wide <- rep_anova %>%
  filter(Effect %in% c("Sample", "Replicate")) %>%
  select(Assessor, Attribute, Effect, F_value, p_value, sig)
print(tibble::as_tibble(rep_anova_wide), n = 30)

cat("\n--- Table 5: Panellist Performance Counts ---\n")
perf_counts <- panel_performance_counts(perf$discrimination, rep_anova, perf$agreement)
print(perf_counts)

cat("\n--- Table 6: Panel ANOVA F-ratios ---\n")
anova_wide <- format_anova_table(perf$anova)
print(anova_wide)

tables[["Table5_perf_counts"]]     <- perf_counts
tables[["Table6_anova_Fratios"]]   <- anova_wide
tables[["panel_performance"]]      <- perf$summary
tables[["assessor_discrimination"]] <- perf$discrimination
tables[["assessor_agreement"]]      <- perf$agreement
tables[["panel_anova"]]             <- perf$anova
tables[["repeatability_anova"]]     <- rep_anova


# ── 3. F-value overview plots ─────────────────────────────────────────────────

cat("\n--- F-value Overview Plots ---\n")

p_fheat <- plot_fvalue_heatmap(perf$discrimination)
print(p_fheat)
.save(p_fheat, "fvalue_heatmap.png", w = 10, h = 6)

p_pval <- plot_pvalue_heatmap(perf$discrimination)
print(p_pval)
.save(p_pval, "pvalue_heatmap.png", w = 10, h = 6)

p_disc_bars <- plot_discrimination_bars(perf$discrimination)
print(p_disc_bars)
.save(p_disc_bars, "discrimination_bars.png", w = 8, h = 5)

p_fdot <- plot_fvalue_dotplot(perf$discrimination, n_samples = n_samples, ncol = 4)
print(p_fdot)
.save(p_fdot, "fvalue_dotplot.png", w = FIG_WIDTH, h = FIG_HEIGHT)

p_panel_anova <- plot_panel_anova(perf$anova)
print(p_panel_anova)
.save(p_panel_anova, "panel_anova_fvalues.png", w = 10, h = 9)


# ── 4. Profile plots ──────────────────────────────────────────────────────────

cat("\n--- Profile Plots ---\n")

p_all_profiles <- plot_profiles(df, ncol = 4)
print(p_all_profiles)
.save(p_all_profiles, "profile_all_assessors.png", w = FIG_WIDTH, h = 10)

p_means <- plot_mean_scores(df)
print(p_means)
.save(p_means, "panel_mean_scores.png", w = FIG_WIDTH, h = FIG_HEIGHT)

p_spider <- plot_spider(df)
print(p_spider)
.save(p_spider, "spider_plot.png", w = 8, h = 7)

# Individual assessor profiles
for (ass in sort(unique(df$Assessor))) {
  p_ass <- plot_assessor_profile(df, ass)
  print(p_ass)
  .save(p_ass, paste0("profile_", ass, ".png"), w = FIG_WIDTH, h = FIG_HEIGHT)
}


# ── 5. Mixed model ANOVA + Tukey HSD ─────────────────────────────────────────

cat("\n--- Mixed Model ANOVA + Tukey HSD ---\n")
message("Fitting mixed models (this may take a moment)...")
mm   <- mixed_model_anova(df, verbose = TRUE)
thsd <- tukey_hsd(mm)

cat("\nFixed effects (Sample F-tests):\n")
print(mm$fixed_effects)
cat("\nTukey HSD compact letter display:\n")
print(tibble::as_tibble(thsd$cld), n = 50)

p_tukey_means <- plot_tukey_means(thsd)
print(p_tukey_means)
.save(p_tukey_means, "tukey_means_cld.png", w = FIG_WIDTH, h = FIG_HEIGHT)

p_tukey_heat <- plot_tukey_heatmap(thsd)
print(p_tukey_heat)
.save(p_tukey_heat, "tukey_heatmap.png", w = 10, h = 7)

p_vc <- plot_variance_components(mm)
print(p_vc)
.save(p_vc, "variance_components.png", w = FIG_WIDTH, h = 6)

tables[["mixed_model_fixed"]]  <- mm$fixed_effects
tables[["mixed_model_random"]] <- mm$random_effects
tables[["tukey_contrasts"]]    <- thsd$contrasts
tables[["tukey_cld"]]          <- thsd$cld
tables[["tukey_emmeans"]]      <- thsd$emmeans


# ── 6. PCA ────────────────────────────────────────────────────────────────────

cat("\n--- Principal Component Analysis ---\n")
pca <- run_pca(df)
cat("Variance explained:\n")
cat(sprintf("  C1: %.1f%%   C2: %.1f%%   C1+C2: %.1f%%\n",
            pca$pct_var[1], pca$pct_var[2],
            pca$pct_var[1] + pca$pct_var[2]))

p_biplot <- plot_pca_biplot(pca)
print(p_biplot)
.save(p_biplot, "pca_biplot.png", w = 9, h = 7)

p_scree <- plot_pca_scree(pca)
print(p_scree)
.save(p_scree, "pca_scree.png", w = 7, h = 5)

p_loadings <- plot_pca_loadings(pca)
print(p_loadings)
.save(p_loadings, "pca_loadings.png", w = 9, h = 5)

tables[["pca_sample_coords"]]   <- pca$ind_coords
tables[["pca_variable_coords"]] <- pca$var_coords


# ── 7. Write Excel output ─────────────────────────────────────────────────────

if (SAVE_PLOTS) {
  xlsx_path <- file.path(OUTPUT_DIR, "PanelCheck_results.xlsx")
  writexl::write_xlsx(tables, path = xlsx_path)
  message("Saved results workbook: ", xlsx_path,
          "\n  Sheets: ", paste(names(tables), collapse = ", "))
}


# ── 8. Done ───────────────────────────────────────────────────────────────────

cat("\n========================================\n")
cat(" Analysis complete!\n")
if (SAVE_PLOTS) {
  cat(" Outputs saved to:", normalizePath(OUTPUT_DIR), "\n")
  cat(" Tables:  PanelCheck_results.xlsx\n")
} else {
  cat(" Set SAVE_PLOTS = TRUE to export plots and tables.\n")
}
cat("========================================\n\n")

# Return the key objects invisibly so they are accessible after sourcing
invisible(list(df = df, perf = perf))
