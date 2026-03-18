# utils.R — Shared helper functions for PanelCheck R translation
#
# Source this file at the top of any analysis script:
#   source("utils.R")
#
# FONT NOTE: theme_clean() uses Barlow Semi Condensed. Install via:
#   install.packages("sysfonts")
#   sysfonts::font_add_google("Barlow Semi Condensed", "Barlow Semi Condensed")
#   showtext::showtext_auto()
# Or install the font system-wide from https://fonts.google.com/specimen/Barlow+Semi+Condensed


# ── Package check ─────────────────────────────────────────────────────────────

check_packages <- function() {
  required <- c("readxl", "dplyr", "tidyr", "purrr", "ggplot2", "car",
                "RColorBrewer", "lme4", "lmerTest", "emmeans", "multcomp",
                "FactoMineR", "factoextra", "ggrepel", "glue", "tibble",
                "writexl")
  optional <- c("sysfonts", "showtext")

  missing_req <- required[!sapply(required, requireNamespace, quietly = TRUE)]
  missing_opt <- optional[!sapply(optional, requireNamespace, quietly = TRUE)]

  if (length(missing_req) > 0) {
    stop(
      "Required packages missing. Install with:\n",
      "install.packages(c(", paste0('"', missing_req, '"', collapse = ", "), "))"
    )
  }
  if (length(missing_opt) > 0) {
    message(
      "Optional packages not installed:\n",
      "  install.packages(c(", paste0('"', missing_opt, '"', collapse = ", "), "))\n",
      "  (sysfonts + showtext needed for Barlow Semi Condensed font)"
    )
  }
  invisible(TRUE)
}


# ── Attribute helpers ──────────────────────────────────────────────────────────

#' Return the names of sensory attribute columns (all cols after the first 3)
get_attributes <- function(df) names(df)[-(1:3)]

#' Return vector of unique assessor IDs
get_assessors <- function(df) unique(df[[1]])

#' Return vector of unique sample IDs
get_samples <- function(df) unique(df[[2]])


# ── Theme ──────────────────────────────────────────────────────────────────────

#' Clean, minimal ggplot theme using Barlow Semi Condensed
#'
#' Applied to all PanelCheck plots for consistent styling.
#' Falls back gracefully if the font is not installed.
theme_clean <- function(base_size = 11) {
  theme_minimal(base_size = base_size, base_family = "Barlow Semi Condensed") +
    theme(
      # Grid
      panel.grid.minor   = element_blank(),
      panel.grid.major   = element_line(colour = "grey92", linewidth = 0.4),
      # Text hierarchy
      plot.title         = element_text(
                             family = "BarlowSemiCondensed-Bold",
                             size   = rel(1.15),
                             hjust  = 0,
                             margin = margin(b = 4)
                           ),
      plot.subtitle      = element_text(
                             colour = "grey40",
                             size   = rel(0.88),
                             hjust  = 0,
                             margin = margin(b = 8)
                           ),
      plot.caption       = element_text(colour = "grey55", size = rel(0.75), hjust = 1),
      axis.title         = element_text(family = "BarlowSemiCondensed-Medium", size = rel(0.9)),
      axis.text          = element_text(colour = "grey30", size = rel(0.85)),
      # Facet strips
      strip.text         = element_text(
                             family = "BarlowSemiCondensed-Bold",
                             size   = rel(0.9),
                             hjust  = 0
                           ),
      strip.background   = element_rect(fill = "grey88", colour = NA),
      # Legend
      legend.position    = "bottom",
      legend.title       = element_text(family = "BarlowSemiCondensed-Medium", size = rel(0.85)),
      legend.text        = element_text(size = rel(0.82)),
      legend.key.size    = unit(0.85, "lines"),
      # Margins
      plot.margin        = margin(10, 12, 8, 10)
    )
}


# ── RColorBrewer palettes ──────────────────────────────────────────────────────

#' Significance colour scale — RdYlBu (red = highly significant, blue = n.s.)
#'
#' Used for the 4-level significance band across all plots.
sig_colours <- function() {
  pal <- RColorBrewer::brewer.pal(4, "RdYlBu")
  # pal order: red, orange-yellow, light-blue, dark-blue
  # Assign: most sig -> least sig
  c(
    "p <= 0.001" = pal[1],   # red
    "p <= 0.01"  = pal[2],   # orange-yellow
    "p <= 0.05"  = pal[3],   # light blue
    "p > 0.05"   = pal[4]    # dark blue
  )
}

#' Attribute colour scale — Spectral (up to 11 distinguishable colours)
#'
#' Used when lines/points are coloured by sensory attribute.
#' @param n Number of colours needed
palette_attributes <- function(n) {
  if (n > 11) stop("Spectral supports up to 11 colours; reduce the number of attributes.")
  RColorBrewer::brewer.pal(max(n, 3), "Spectral")[seq_len(n)]
}

#' Sample colour scale — scales gracefully to any number of samples.
#'
#' Uses Set2 for ≤8 samples; interpolates via colorRampPalette for larger sets
#' so that plots with many products still render (colours will be less distinct).
#' @param n Number of colours needed
palette_samples <- function(n) {
  base_pal <- RColorBrewer::brewer.pal(8, "Set2")
  if (n <= 8) base_pal[seq_len(max(n, 3))][seq_len(n)]
  else        colorRampPalette(base_pal)(n)
}

#' Performance colour scale — RdYlGn traffic light
#'
#' Good = green, Moderate = yellow, Poor = red.
palette_performance <- function() {
  pal <- RColorBrewer::brewer.pal(3, "RdYlGn")
  c("Good" = pal[3], "Moderate" = pal[2], "Poor" = pal[1])
}


# ── Performance flagging ───────────────────────────────────────────────────────
#
# FLAG_THRESHOLDS: single config object for all domain cutoffs.
# apply_flag(domain, value): returns list(emoji, tier, descriptor).
# .add_flags(domain, values): vectorised wrapper returning two named vectors.
# .flag_footer(): standard disclaimer string for printed outputs.
#
# Confidence annotations:
#   ✅ Commonly cited / analytically grounded
#   ⚠️  General practice — configurable defaults, adjust to panel context
#   🔍 Needs verification before citing in publications

FLAG_THRESHOLDS <- list(

  agreement = list(
    # ⚠️ General practice — no single paper defines these cutoffs
    pass    = 0.80,
    monitor = 0.60
  ),

  discrimination = list(
    # ✅ Standard alpha; borderline zone is ⚠️ general practice
    pass    = 0.05,
    monitor = 0.10
  ),

  replicate_sig = list(
    # ⚠️ General practice — significant Replicate effect signals session-to-session
    # inconsistency; thresholds mirror the discrimination domain but interpretation
    # is inverted (low p = bad, high p = good).
    concern = 0.05,
    monitor = 0.10
  ),

  repeatability_cv = list(
    # ⚠️ General practice — CV% thresholds from variance component
    # interpretation norms, not a single definitive source
    pass    = 20,
    monitor = 40
  ),

  vc_as_ratio = list(
    # Assessor:Sample / Residual ratio
    # ✅ < 1.0 is analytically grounded (residual exceeds signal)
    # ⚠️ 1.5 and 2.0 cutoffs are general practice
    concern_low = 1.0,
    pass        = 1.5,
    monitor     = 2.0
  ),

  vc_assessor_pct = list(
    # ⚠️ General practice — reflects industry SOP norms
    pass    = 25,
    monitor = 40
  ),

  vc_replicate_pct = list(
    # ⚠️ General practice; near-zero tolerance is ✅ analytically grounded
    pass     = 5,
    monitor  = 10,
    zero_tol = 1e-6
  )
)

#' Apply a performance flag to a single numeric value.
#'
#' @param domain  One of: "agreement", "discrimination", "replicate_sig",
#'                "repeatability_cv", "vc_as_ratio", "vc_assessor_pct",
#'                "vc_replicate_pct".
#' @param value   Numeric value to evaluate.
#' @return Named list: emoji (chr), tier (chr — PASS/MONITOR/CONCERN/UNKNOWN),
#'         descriptor (chr).
apply_flag <- function(domain, value) {
  th <- FLAG_THRESHOLDS[[domain]]
  if (is.null(th)) stop("Unknown flagging domain: ", domain)

  if (is.na(value) || is.null(value)) {
    return(list(emoji = "\u2B1C", tier = "UNKNOWN", descriptor = "Insufficient data"))
  }

  switch(domain,

    agreement = {
      if (value < 0)
        list(emoji = "\U1F534", tier = "CONCERN",
             descriptor = "Inverse scoring \u2014 check scale direction")
      else if (value >= th$pass)
        list(emoji = "\U1F7E2", tier = "PASS",
             descriptor = "Strong agreement")
      else if (value >= th$monitor)
        list(emoji = "\U1F7E1", tier = "MONITOR",
             descriptor = "Moderate agreement \u2014 monitor")
      else
        list(emoji = "\U1F534", tier = "CONCERN",
             descriptor = "Poor agreement \u2014 recommend exclusion")
    },

    discrimination = {
      if (value < th$pass)
        list(emoji = "\U1F7E2", tier = "PASS",
             descriptor = "Discriminates samples reliably")
      else if (value < th$monitor)
        list(emoji = "\U1F7E1", tier = "MONITOR",
             descriptor = "Borderline discrimination")
      else
        list(emoji = "\U1F534", tier = "CONCERN",
             descriptor = "Poor discrimination \u2014 cannot distinguish samples")
    },

    replicate_sig = {
      # Inverted: a NON-significant Replicate effect is desirable (good repeatability).
      if (value > th$monitor)
        list(emoji = "\U1F7E2", tier = "PASS",
             descriptor = "No significant session effect \u2014 good repeatability")
      else if (value > th$concern)
        list(emoji = "\U1F7E1", tier = "MONITOR",
             descriptor = "Borderline session effect \u2014 monitor")
      else
        list(emoji = "\U1F534", tier = "CONCERN",
             descriptor = "Significant session inconsistency \u2014 investigate")
    },

    repeatability_cv = {
      if (value < th$pass)
        list(emoji = "\U1F7E2", tier = "PASS",
             descriptor = "Good repeatability")
      else if (value < th$monitor)
        list(emoji = "\U1F7E1", tier = "MONITOR",
             descriptor = "Marginal repeatability \u2014 monitor")
      else
        list(emoji = "\U1F534", tier = "CONCERN",
             descriptor = "Poor repeatability \u2014 investigate")
    },

    vc_as_ratio = {
      if (value < th$concern_low)
        list(emoji = "\U1F534", tier = "CONCERN",
             descriptor = "Residual exceeds disagreement \u2014 unreliable panel signal")
      else if (value <= th$pass)
        list(emoji = "\U1F7E2", tier = "PASS",
             descriptor = "Acceptable consensus on sample ordering")
      else if (value <= th$monitor)
        list(emoji = "\U1F7E1", tier = "MONITOR",
             descriptor = "Moderate disagreement on sample ordering")
      else
        list(emoji = "\U1F534", tier = "CONCERN",
             descriptor = "High disagreement \u2014 review attribute")
    },

    vc_assessor_pct = {
      if (value < th$pass)
        list(emoji = "\U1F7E2", tier = "PASS",
             descriptor = "Low scale bias")
      else if (value < th$monitor)
        list(emoji = "\U1F7E1", tier = "MONITOR",
             descriptor = "Moderate scale bias \u2014 anchor retraining suggested")
      else
        list(emoji = "\U1F534", tier = "CONCERN",
             descriptor = "Scale bias dominates \u2014 retraining required")
    },

    vc_replicate_pct = {
      if (value < th$zero_tol)
        list(emoji = "\U1F7E1", tier = "MONITOR",
             descriptor = "Near-zero variance \u2014 check replicate blinding")
      else if (value < th$pass)
        list(emoji = "\U1F7E2", tier = "PASS",
             descriptor = "Good session consistency")
      else if (value < th$monitor)
        list(emoji = "\U1F7E1", tier = "MONITOR",
             descriptor = "Marginal session consistency")
      else
        list(emoji = "\U1F534", tier = "CONCERN",
             descriptor = "Poor session consistency")
    }
  )
}

# Vectorised wrapper — apply_flag() over a vector; returns two named character
# vectors suitable for use inside dplyr::mutate().
.add_flags <- function(domain, values) {
  flags <- lapply(values, apply_flag, domain = domain)
  list(
    flag       = vapply(flags, `[[`, "", "emoji"),
    descriptor = vapply(flags, `[[`, "", "descriptor")
  )
}

#' Standard disclaimer footer for all flagged table outputs.
.flag_footer <- function() {
  paste0(
    "\u26a0\ufe0f Performance flags are based on widely used guidelines in ",
    "sensory science. Thresholds may be adjusted to reflect panel context, ",
    "product category, and institutional SOPs. See CLAUDE.md for reference ",
    "details and confidence levels."
  )
}


# ── Analysis skip log ──────────────────────────────────────────────────────────
#
# A lightweight mutable log that accumulates the names and reasons of any steps
# that error out during run_all.R, then prints them all at the very end.
#
# Usage in run_all.R:
#   .reset_skip_log()                          # once, at the top
#   x <- .try_step("My step", some_fn(data))  # wraps any expression
#   .print_skip_summary()                      # once, at the bottom
#
# .try_step() returns the expression result on success, or NULL (invisibly) on
# error, so downstream code can guard with:  if (!is.null(x)) { ... }

.log_env <- new.env(parent = emptyenv())
.log_env$skips <- list()

#' Reset the skip log — call once at the start of each run_all.R run.
.reset_skip_log <- function() {
  .log_env$skips <- list()
  invisible(NULL)
}

#' Wrap an expression: evaluate it; on error log a skip and return NULL.
#'
#' @param step_name Short label for this step (shown in the end-of-run summary).
#' @param expr      Expression to evaluate (passed as a lazy promise — use bare
#'                  code, not a quoted string).
#' @param reason    Optional override message for the skip reason. Defaults to
#'                  the error message from the condition.
#' @return Result of \code{expr} on success; \code{NULL} invisibly on error.
.try_step <- function(step_name, expr, reason = NULL) {
  tryCatch(
    expr,
    error = function(e) {
      msg <- if (!is.null(reason)) reason else conditionMessage(e)
      .log_env$skips <- c(.log_env$skips, list(list(step = step_name, reason = msg)))
      message(sprintf("  ! Skipped '%s': %s", step_name, msg))
      invisible(NULL)
    }
  )
}

#' Print a summary of all steps skipped during the current run.
.print_skip_summary <- function() {
  n <- length(.log_env$skips)
  cat("\n--- Run summary ---\n")
  if (n == 0) {
    cat("  All steps completed successfully.\n")
  } else {
    cat(sprintf("  %d step(s) were skipped:\n\n", n))
    for (s in .log_env$skips) {
      cat(sprintf("    [SKIPPED] %s\n", s$step))
      cat(sprintf("      Reason : %s\n\n", s$reason))
    }
  }
}


# ── Significance helper ────────────────────────────────────────────────────────

sig_label <- function(p) {
  case_when(
    is.na(p)   ~ "p > 0.05",
    p <= 0.001 ~ "p <= 0.001",
    p <= 0.01  ~ "p <= 0.01",
    p <= 0.05  ~ "p <= 0.05",
    TRUE       ~ "p > 0.05"
  )
}
