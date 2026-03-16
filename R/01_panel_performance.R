# 01_panel_performance.R - Sensory Panel Performance Statistics  [PRIORITY]
#
# Mirrors the ANOVA and assessor-performance analyses from PanelCheck's
# Math_Tools.py, F_Plot.py, MSE_Plot.py, and Eggshell_Plot.py.
#
# Main functions:
#   descriptive_stats()      - min, max, mean, SD, SEM, CV per attribute (± sample)
#   panel_anova()            - 2-way ANOVA table (Sample + Assessor effects)
#   assessor_discrimination()- per-assessor F-values (ability to distinguish samples)
#   repeatability_anova()    - formal 2-way ANOVA per assessor partitioning
#                              Sample vs Replicate vs Residual variance
#   assessor_repeatability() - within-assessor CV across replicates
#   assessor_agreement()     - correlation of each assessor with panel mean
#   panel_performance()      - combined summary table (all three metrics)
#
# Usage:
#   source("00_load_data.R")
#   source("01_panel_performance.R")
#   df   <- load_panel_data("../Data_Bread.xlsx")
#   perf <- panel_performance(df)
#   print(perf$summary)

suppressPackageStartupMessages({
  library(car)      # Type III SS for unbalanced designs
  library(dplyr)
  library(tidyr)
  library(purrr)
})


# ── 0. Descriptive statistics ─────────────────────────────────────────────────

#' Compute descriptive statistics for all sensory attributes
#'
#' Returns n, min, max, mean, SD, SEM, and CV per attribute.
#' When by_sample = TRUE, statistics are broken down by sample as well.
#'
#' @param df        Data frame from load_panel_data()
#' @param by_sample Logical — compute stats per Sample × Attribute? (default TRUE)
#' @return A tibble with columns: Attribute, [Sample], n, min, max, mean, sd, sem, cv_pct
#'
#' @examples
#' desc <- descriptive_stats(df)
#' desc_overall <- descriptive_stats(df, by_sample = FALSE)
descriptive_stats <- function(df, by_sample = TRUE) {
  attrs <- names(df)[-(1:3)]

  long <- df %>%
    pivot_longer(cols = all_of(attrs), names_to = "Attribute", values_to = "value") %>%
    filter(!is.na(value))

  group_vars <- if (by_sample) c("Attribute", "Sample") else "Attribute"

  long %>%
    group_by(across(all_of(group_vars))) %>%
    summarise(
      n      = n(),
      min    = min(value),
      max    = max(value),
      mean   = mean(value),
      sd     = sd(value),
      sem    = sd / sqrt(n),
      cv_pct = ifelse(mean != 0, (sd / mean) * 100, NA_real_),
      .groups = "drop"
    ) %>%
    mutate(across(where(is.numeric), ~ round(.x, 3)))
}


# ── 1. Panel ANOVA ────────────────────────────────────────────────────────────

#' 3-factor ANOVA per attribute: Sample + Assessor + Replicate (Type III SS)
#'
#' Uses Type III (marginal) sums of squares via car::Anova() so that F-ratios
#' are correct for unbalanced designs (unequal cell counts across assessors or
#' samples). Model fitted with sum-to-zero contrasts as required for Type III SS.
#' Includes the Sample:Assessor interaction to assess panel agreement.
#'
#' @param df Data frame from load_panel_data()
#' @return A tibble: Attribute | Effect | SS | df | MS | F_value | p_value | sig
#'
#' @examples
#' anova_tbl <- panel_anova(df)
#' tbl6 <- format_anova_table(anova_tbl)
panel_anova <- function(df) {
  attrs <- names(df)[-(1:3)]

  map(attrs, function(attr) {
    d <- df %>%
      select(Assessor, Sample, Replicate, value = all_of(attr)) %>%
      filter(!is.na(value)) %>%
      mutate(
        Assessor  = factor(Assessor),
        Sample    = factor(Sample),
        Replicate = factor(Replicate)
      )

    has_reps <- length(unique(d$Replicate)) > 1

    tryCatch({
      # Fit with sum-to-zero contrasts — required for unambiguous Type III SS.
      # car::Anova(type = "III") gives correct F-ratios for unbalanced designs.
      ctr <- list(Sample = "contr.sum", Assessor = "contr.sum",
                  Replicate = "contr.sum")

      # Include Sample:Assessor interaction only when every assessor has at
      # least one observation for every sample.  Missing cells (common in large
      # unbalanced panels) cause aliased coefficients and car::Anova() errors.
      cell_counts       <- with(d, table(Sample, Assessor))
      can_fit_interact  <- !any(cell_counts == 0)

      if (has_reps) {
        formula <- if (can_fit_interact)
          value ~ Sample + Assessor + Replicate + Sample:Assessor
        else
          value ~ Sample + Assessor + Replicate
        fit <- lm(formula, data = d, contrasts = ctr)
      } else {
        formula <- if (can_fit_interact)
          value ~ Sample + Assessor + Sample:Assessor
        else
          value ~ Sample + Assessor
        fit <- lm(formula, data = d, contrasts = ctr[c("Sample", "Assessor")])
      }
      sm   <- car::Anova(fit, type = "III")
      rows <- .parse_car_anova(sm, attr)
      rows
    }, error = function(e) {
      tibble(Attribute = attr, Effect = "ERROR", SS = NA_real_, df = NA_integer_,
             MS = NA_real_, F_value = NA_real_, p_value = NA_real_, sig = NA_character_)
    })
  }) |> list_rbind()
}

# Internal: parse car::Anova() Type III output into a tidy tibble.
# car::Anova returns: Sum Sq | Df | F value | Pr(>F)  (no Mean Sq column).
# Intercept and Residuals rows are dropped; MS is computed as SS/df.
.parse_car_anova <- function(car_out, attr) {
  tbl <- as.data.frame(car_out)
  tbl$Effect <- trimws(rownames(tbl))
  tbl <- tbl[!tbl$Effect %in% c("(Intercept)", "Residuals"), , drop = FALSE]

  tibble(
    Attribute = attr,
    Effect    = tbl$Effect,
    SS        = tbl[["Sum Sq"]],
    df        = as.integer(tbl[["Df"]]),
    MS        = tbl[["Sum Sq"]] / tbl[["Df"]],
    F_value   = tbl[["F value"]],
    p_value   = tbl[["Pr(>F)"]],
    sig       = .sig_stars(tbl[["Pr(>F)"]])
  )
}

.sig_stars <- function(p) {
  case_when(
    is.na(p)  ~ "",
    p <= 0.001 ~ "***",
    p <= 0.01  ~ "**",
    p <= 0.05  ~ "*",
    p <= 0.1   ~ ".",
    TRUE       ~ "ns"
  )
}


# ── 2. Assessor discrimination (1-way ANOVA per assessor per attribute) ────────

#' F-values and p-values per assessor: can each assessor distinguish samples?
#'
#' For each (assessor, attribute) pair a one-way ANOVA is run with Sample as
#' the factor. A significant F-value means the assessor reliably differentiates
#' products on that attribute.
#'
#' @param df Data frame from load_panel_data()
#' @return A tibble: Assessor | Attribute | F_value | p_value | sig | mean_score
#'
#' @examples
#' disc <- assessor_discrimination(df)
assessor_discrimination <- function(df) {
  attrs     <- names(df)[-(1:3)]
  assessors <- unique(df$Assessor)

  map(assessors, function(ass) {
    d_ass <- filter(df, Assessor == ass)

    map(attrs, function(attr) {
      d <- d_ass %>%
        select(Sample, value = all_of(attr)) %>%
        filter(!is.na(value)) %>%
        mutate(Sample = factor(Sample))

      if (length(unique(d$Sample)) < 2 || nrow(d) < 3) {
        return(tibble(Assessor = ass, Attribute = attr,
                      F_value = NA_real_, p_value = NA_real_,
                      sig = NA_character_, mean_score = NA_real_))
      }

      # Zero-variance: all scores identical — F = 0, p = 1 (non-significant)
      if (sd(d$value, na.rm = TRUE) == 0) {
        return(tibble(Assessor = ass, Attribute = attr,
                      F_value = 0, p_value = 1,
                      sig = "ns", mean_score = mean(d$value, na.rm = TRUE)))
      }

      tryCatch({
        fit <- aov(value ~ Sample, data = d)
        sm  <- summary(fit)[[1]]
        tibble(
          Assessor   = ass,
          Attribute  = attr,
          F_value    = sm["Sample", "F value"],
          p_value    = sm["Sample", "Pr(>F)"],
          sig        = .sig_stars(sm["Sample", "Pr(>F)"]),
          mean_score = mean(d$value, na.rm = TRUE)
        )
      }, error = function(e) {
        tibble(Assessor = ass, Attribute = attr,
               F_value = NA_real_, p_value = NA_real_,
               sig = NA_character_, mean_score = NA_real_)
      })
    }) |> list_rbind()
  }) |> list_rbind()
}


# ── 3. Repeatability ANOVA (formal 2-way per assessor) ───────────────────────

#' Formal repeatability ANOVA per assessor per attribute
#'
#' For each (assessor, attribute) pair fits: value ~ Sample + Replicate
#' This partitions total variance into:
#'   - Sample effect: discrimination ability (is significant = good)
#'   - Replicate effect: session-to-session inconsistency (is significant = poor)
#'   - Residual: unexplained noise
#'
#' Returns F-values and p-values for both effects, plus MS_residual (error variance).
#'
#' @param df Data frame from load_panel_data()
#' @return A tibble: Assessor | Attribute | Effect | SS | df | MS | F_value | p_value | sig
#'
#' @examples
#' rep_anova <- repeatability_anova(df)
repeatability_anova <- function(df) {
  attrs     <- names(df)[-(1:3)]
  assessors <- unique(df$Assessor)

  map(assessors, function(ass) {
    d_ass <- filter(df, Assessor == ass)

    map(attrs, function(attr) {
      d <- d_ass %>%
        select(Sample, Replicate, value = all_of(attr)) %>%
        filter(!is.na(value)) %>%
        mutate(Sample    = factor(Sample),
               Replicate = factor(Replicate))

      if (length(unique(d$Replicate)) < 2) {
        return(tibble(Assessor = ass, Attribute = attr, Effect = "Replicate",
                      SS = NA_real_, df = NA_integer_, MS = NA_real_,
                      F_value = NA_real_, p_value = NA_real_, sig = NA_character_))
      }

      # Zero-variance case: all scores identical (e.g. all 0s).
      # ANOVA produces NaN (0/0). F = 0, p = 1 correctly conveys non-significance.
      if (sd(d$value, na.rm = TRUE) == 0) {
        n_s <- length(unique(d$Sample))
        n_r <- length(unique(d$Replicate))
        df_resid <- max(nrow(d) - n_s - n_r + 1L, 0L)
        return(bind_rows(
          tibble(Assessor = ass, Attribute = attr,
                 Effect = c("Sample", "Replicate"),
                 SS = 0, df = as.integer(c(n_s - 1L, n_r - 1L)),
                 MS = 0, F_value = 0, p_value = 1, sig = "ns"),
          tibble(Assessor = ass, Attribute = attr,
                 Effect = "Residual",
                 SS = 0, df = as.integer(df_resid),
                 MS = 0, F_value = NA_real_, p_value = NA_real_, sig = NA_character_)
        ))
      }

      tryCatch({
        # Sum-to-zero contrasts + Type III SS for correct results when
        # an assessor has missing sample×replicate combinations.
        fit <- lm(value ~ Sample + Replicate, data = d,
                  contrasts = list(Sample = "contr.sum", Replicate = "contr.sum"))
        sm  <- as.data.frame(car::Anova(fit, type = "III"))
        sm$Effect <- trimws(rownames(sm))

        effects_only <- sm[!sm$Effect %in% c("(Intercept)", "Residuals"), ]
        rows <- tibble(
          Assessor  = ass,
          Attribute = attr,
          Effect    = effects_only$Effect,
          SS        = effects_only[["Sum Sq"]],
          df        = as.integer(effects_only[["Df"]]),
          MS        = effects_only[["Sum Sq"]] / effects_only[["Df"]],
          F_value   = effects_only[["F value"]],
          p_value   = effects_only[["Pr(>F)"]],
          sig       = .sig_stars(effects_only[["Pr(>F)"]])
        )

        # Append residual row (MS_residual = repeatability error variance)
        res_row <- sm[sm$Effect == "Residuals", ]
        res_ss  <- res_row[["Sum Sq"]]
        res_df  <- as.integer(res_row[["Df"]])
        bind_rows(rows,
          tibble(Assessor = ass, Attribute = attr, Effect = "Residual",
                 SS = res_ss, df = res_df,
                 MS = res_ss / res_df, F_value = NA_real_,
                 p_value = NA_real_, sig = NA_character_)
        )
      }, error = function(e) {
        tibble(Assessor = ass, Attribute = attr, Effect = "ERROR",
               SS = NA_real_, df = NA_integer_, MS = NA_real_,
               F_value = NA_real_, p_value = NA_real_, sig = NA_character_)
      })
    }) |> list_rbind()
  }) |> list_rbind()
}


# ── 4. Assessor repeatability (within-assessor CV across replicates) ──────────

#' Coefficient of Variation (CV) across replicates per assessor per attribute
#'
#' Low CV = high repeatability (assessor scores consistently across replications).
#' This mirrors PanelCheck's MSE / repeatability component.
#'
#' @param df Data frame from load_panel_data()
#' @return A tibble: Assessor | Attribute | Sample | mean | sd | cv_pct | n_reps
#'
#' @examples
#' rep_tbl <- assessor_repeatability(df)
assessor_repeatability <- function(df) {
  attrs <- names(df)[-(1:3)]

  df %>%
    pivot_longer(cols = all_of(attrs), names_to = "Attribute", values_to = "value") %>%
    filter(!is.na(value)) %>%
    group_by(Assessor, Sample, Attribute) %>%
    summarise(
      mean   = mean(value),
      sd     = sd(value),
      cv_pct = ifelse(mean != 0, (sd / mean) * 100, NA_real_),
      n_reps = n(),
      .groups = "drop"
    )
}

#' Summary of repeatability per assessor (averaged over samples and attributes)
#'
#' @param rep_tbl Output of assessor_repeatability()
#' @return A tibble: Assessor | mean_cv | median_cv | mean_sd
assessor_repeatability_summary <- function(rep_tbl) {
  rep_tbl %>%
    filter(!is.na(cv_pct)) %>%
    group_by(Assessor) %>%
    summarise(
      mean_cv   = mean(cv_pct, na.rm = TRUE),
      median_cv = median(cv_pct, na.rm = TRUE),
      mean_sd   = mean(sd, na.rm = TRUE),
      .groups   = "drop"
    )
}


# ── 4. Assessor agreement (correlation with panel mean) ───────────────────────

#' Pearson correlation of each assessor's scores with the panel consensus
#'
#' The panel mean (consensus) is the average across all assessors (excluding the
#' focal assessor to avoid circularity). A high correlation indicates the
#' assessor's perception aligns with the group.
#'
#' @param df Data frame from load_panel_data()
#' @return A tibble: Assessor | Attribute | r | p_value | sig
#'
#' @examples
#' agree <- assessor_agreement(df)
assessor_agreement <- function(df) {
  attrs     <- names(df)[-(1:3)]
  assessors <- unique(df$Assessor)

  # Panel mean per Sample×Attribute (all assessors, average over replicates)
  panel_mean <- df %>%
    pivot_longer(cols = all_of(attrs), names_to = "Attribute", values_to = "value") %>%
    group_by(Sample, Attribute) %>%
    summarise(panel_mean = mean(value, na.rm = TRUE), .groups = "drop")

  map(assessors, function(ass) {
    # Assessor mean per Sample×Attribute (average over replicates)
    ass_mean <- df %>%
      filter(Assessor == ass) %>%
      pivot_longer(cols = all_of(attrs), names_to = "Attribute", values_to = "value") %>%
      group_by(Sample, Attribute) %>%
      summarise(ass_mean = mean(value, na.rm = TRUE), .groups = "drop")

    joined <- inner_join(ass_mean, panel_mean, by = c("Sample", "Attribute"))

    map(attrs, function(attr) {
      d <- filter(joined, Attribute == attr)

      if (nrow(d) < 3 || sd(d$ass_mean, na.rm = TRUE) == 0 ||
          sd(d$panel_mean, na.rm = TRUE) == 0) {
        return(tibble(Assessor = ass, Attribute = attr,
                      r = NA_real_, p_value = NA_real_, sig = NA_character_))
      }

      ct <- tryCatch(
        cor.test(d$ass_mean, d$panel_mean, method = "pearson"),
        error = function(e) NULL
      )

      if (is.null(ct)) {
        tibble(Assessor = ass, Attribute = attr,
               r = NA_real_, p_value = NA_real_, sig = NA_character_)
      } else {
        tibble(
          Assessor  = ass,
          Attribute = attr,
          r         = ct$estimate,
          p_value   = ct$p.value,
          sig       = .sig_stars(ct$p.value)
        )
      }
    }) |> list_rbind()
  }) |> list_rbind()
}


# ── 5. Combined panel performance summary ─────────────────────────────────────

#' Compute all panel performance metrics and return a comprehensive summary
#'
#' Runs discrimination, repeatability, and agreement analyses then combines them
#' into a single per-assessor summary table and detailed per-attribute tables.
#'
#' @param df Data frame from load_panel_data()
#' @return A named list:
#'   $discrimination  - per-assessor × attribute F-values (ability to discriminate)
#'   $repeatability   - per-assessor × attribute × sample CV (consistency)
#'   $agreement       - per-assessor × attribute correlation with panel mean
#'   $summary         - one row per assessor with overall performance scores
#'   $anova           - full 2-way ANOVA table across all attributes
#'
#' @examples
#' perf <- panel_performance(df)
#' print(perf$summary)
#' print(perf$discrimination)
panel_performance <- function(df) {
  message("Running panel performance analysis...")

  message("  1/4  2-way ANOVA...")
  anova_tbl <- panel_anova(df)

  message("  2/4  Assessor discrimination (1-way ANOVA per assessor)...")
  disc <- assessor_discrimination(df)

  message("  3/4  Assessor repeatability (CV across replicates)...")
  rep_detail <- assessor_repeatability(df)
  rep_summ   <- assessor_repeatability_summary(rep_detail)

  message("  4/4  Assessor agreement (correlation with panel mean)...")
  agree <- assessor_agreement(df)

  # ── Build summary table ───────────────────────────────────────────────────
  disc_summ <- disc %>%
    group_by(Assessor) %>%
    summarise(
      # Cap extreme F-values before averaging (arise when within-group variance ≈ 0)
      # F > 1000 signals "perfect discrimination" - capped at 999 for display
      mean_F_disc     = mean(pmin(F_value, 999), na.rm = TRUE),
      pct_sig_disc    = mean(p_value <= 0.05, na.rm = TRUE) * 100,
      .groups = "drop"
    )

  agree_summ <- agree %>%
    group_by(Assessor) %>%
    summarise(
      mean_r_agree    = mean(r, na.rm = TRUE),
      pct_sig_agree   = mean(p_value <= 0.05, na.rm = TRUE) * 100,
      .groups = "drop"
    )

  summary_tbl <- disc_summ %>%
    left_join(rep_summ,   by = "Assessor") %>%
    left_join(agree_summ, by = "Assessor") %>%
    arrange(Assessor) %>%
    mutate(across(where(is.numeric), ~ round(.x, 3)))

  # Add interpretation flags
  summary_tbl <- summary_tbl %>%
    mutate(
      discrimination_flag = case_when(
        pct_sig_disc >= 50 ~ "Good",
        pct_sig_disc >= 25 ~ "Moderate",
        TRUE               ~ "Poor"
      ),
      repeatability_flag = case_when(
        mean_cv <= 20 ~ "Good",
        mean_cv <= 40 ~ "Moderate",
        TRUE          ~ "Poor"
      ),
      agreement_flag = case_when(
        mean_r_agree >= 0.7 ~ "Good",
        mean_r_agree >= 0.4 ~ "Moderate",
        TRUE                ~ "Poor"
      )
    )

  message("Done.")

  list(
    discrimination = disc,
    repeatability  = rep_detail,
    agreement      = agree,
    summary        = summary_tbl,
    anova          = anova_tbl
  )
}


# ── 6. Print helper ────────────────────────────────────────────────────────────

#' Pretty-print the panel performance summary
#'
#' @param perf Output of panel_performance()
print_panel_performance <- function(perf) {
  cat("=== Panel Performance Summary ===\n\n")

  sm <- perf$summary
  cat(sprintf(
    "%-10s  %-8s  %-10s  %-10s  %-10s  %-10s  %-10s  %-10s\n",
    "Assessor", "Mean F", "% Sig Disc", "Mean CV%", "Mean r",
    "% Sig Agr", "Discrim.", "Agreement"
  ))
  cat(strrep("-", 95), "\n")

  for (i in seq_len(nrow(sm))) {
    r <- sm[i, ]
    cat(sprintf(
      "%-10s  %-8.2f  %-10.1f  %-10.1f  %-10.3f  %-10.1f  %-10s  %-10s\n",
      r$Assessor,
      r$mean_F_disc %||% NA,
      r$pct_sig_disc %||% NA,
      r$mean_cv %||% NA,
      r$mean_r_agree %||% NA,
      r$pct_sig_agree %||% NA,
      r$discrimination_flag,
      r$agreement_flag
    ))
  }

  cat("\nInterpretation:\n")
  cat("  Discrimination (% attributes with significant F): Good ≥50%, Moderate ≥25%\n")
  cat("  Repeatability (mean CV% across replicates):       Good ≤20%, Moderate ≤40%\n")
  cat("  Agreement (mean r with panel mean):               Good ≥0.7, Moderate ≥0.4\n")

  invisible(perf)
}

`%||%` <- function(a, b) if (!is.null(a) && !all(is.na(a))) a else b


# ── 7. Table 5: Panellist performance counts (wide format) ────────────────────

#' Per-assessor counts for discrimination, repeatability, and no-interaction,
#' pivoted wide with assessors as columns — matching Table 5 in published reports.
#'
#' Definitions:
#'   Discrimination  : # attributes with significant Sample F (p <= alpha)
#'   Repeatability   : # attributes with non-significant Replicate F (p > alpha)
#'   No interaction  : # attributes where assessor positively and significantly
#'                     agrees with panel mean (r > 0, p <= alpha)
#'   Total           : sum of all three (maximum = n_attributes * 3)
#'
#' @param disc      perf$discrimination  (output of assessor_discrimination())
#' @param rep_anova output of repeatability_anova()
#' @param agree     perf$agreement       (output of assessor_agreement())
#' @param alpha     significance threshold (default 0.05)
#' @return A tibble: rows = Metric, columns = each Assessor ID
#'
#' @examples
#' counts <- panel_performance_counts(perf$discrimination, rep_anova, perf$agreement)
#' print(counts)
panel_performance_counts <- function(disc, rep_anova, agree, alpha = 0.05) {

  discrim <- disc %>%
    filter(!is.na(p_value)) %>%
    group_by(Assessor) %>%
    summarise(Discrimination = sum(p_value <= alpha, na.rm = TRUE), .groups = "drop")

  repeatab <- rep_anova %>%
    filter(Effect == "Replicate", !is.na(p_value)) %>%
    group_by(Assessor) %>%
    summarise(Repeatability = sum(p_value > alpha, na.rm = TRUE), .groups = "drop")

  no_int <- agree %>%
    filter(!is.na(p_value)) %>%
    group_by(Assessor) %>%
    summarise(`No interaction` = sum(r > 0 & p_value <= alpha, na.rm = TRUE),
              .groups = "drop")

  counts <- discrim %>%
    left_join(repeatab, by = "Assessor") %>%
    left_join(no_int,   by = "Assessor") %>%
    mutate(Total = Discrimination + Repeatability + `No interaction`)

  # Pivot: metrics as rows, assessors as columns (Table 5 layout)
  counts %>%
    pivot_longer(cols = -Assessor, names_to = "Metric", values_to = "n") %>%
    pivot_wider(names_from = Assessor, values_from = n) %>%
    mutate(Metric = factor(Metric,
                           levels = c("Discrimination", "Repeatability",
                                      "No interaction", "Total"))) %>%
    arrange(Metric)
}


# ── 8. Table 6: Panel ANOVA results in wide format ────────────────────────────

#' Panel ANOVA F-ratios with significance stars, pivoted wide — matching Table 6.
#'
#' Rows = sensory attributes; columns = ANOVA effects (Sample, Assessor, Replicate).
#' Each cell contains the rounded F-value and significance stars, e.g. "25 ***".
#'
#' @param anova_tbl perf$anova  (output of panel_anova())
#' @param effects   Effects to include as columns
#' @return A tibble: Attribute | Sample | Assessor | Replicate
#'
#' @examples
#' tbl6 <- format_anova_table(perf$anova)
#' print(tbl6)
format_anova_table <- function(anova_tbl,
                                effects = c("Sample", "Assessor", "Replicate")) {
  anova_tbl %>%
    filter(Effect %in% effects) %>%
    mutate(
      cell   = case_when(
        is.na(F_value) ~ "",
        TRUE           ~ paste0(round(F_value), " ", sig)
      ),
      # Rename to match published table header
      Effect = recode(Effect, "Assessor" = "Panellist")
    ) %>%
    select(Attribute, Effect, cell) %>%
    pivot_wider(names_from  = Effect,
                values_from = cell,
                values_fill = "") %>%
    select(Attribute, any_of(c("Sample", "Panellist", "Replicate")))
}
