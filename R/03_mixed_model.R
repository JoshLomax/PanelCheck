# 03_mixed_model.R - Mixed Model ANOVA + Tukey HSD post-hoc test
#
# Implements:
#   - Mixed model ANOVA per attribute:
#       Fixed:  Sample (product/genotype)
#       Random: Assessor (panellist)
#               Assessor:Replicate (session within panellist)
#               Assessor:Sample (panellist x product interaction)
#   - Tukey HSD pairwise comparison of samples (emmeans, p < 0.05)
#   - Compact letter display (CLD) for publication-ready result tables
#   - Plot: sample means +/- SE with Tukey letter groupings
#
# Requires: lme4, lmerTest, emmeans, multcomp
#
# Usage:
#   source("00_load_data.R")
#   source("03_mixed_model.R")
#   df  <- load_panel_data("../Data_Bread.xlsx")
#   mm  <- mixed_model_anova(df)
#   print(mm$fixed_effects)   # F-tests for Sample effect per attribute
#   thsd <- tukey_hsd(mm)
#   print(thsd$cld)            # Compact letter display

suppressPackageStartupMessages({
  # Load MASS-dependent packages first so tidyverse wins the namespace conflict
  library(lme4)
  library(lmerTest)
  library(multcomp)
  library(emmeans)
  # Tidyverse loaded last — dplyr::select, filter, mutate etc. take precedence over MASS
  library(dplyr)
  library(tidyr)
  library(purrr)
  library(ggplot2)
  library(tibble)
})


# ── 1. Mixed model ANOVA ──────────────────────────────────────────────────────

#' Fit a mixed model ANOVA for each sensory attribute
#'
#' Model:  value ~ Sample + (1|Assessor) + (1|Assessor:Replicate) + (1|Assessor:Sample)
#'
#' - Sample            : fixed effect  (product differentiation)
#' - Assessor          : random effect (panellist main effect / scale use)
#' - Assessor:Replicate: random effect (session reproducibility within panellist)
#' - Assessor:Sample   : random effect (panellist x product interaction)
#'
#' F-tests use Satterthwaite denominator df approximation (lmerTest).
#'
#' @param df       Data frame from load_panel_data()
#' @param verbose  Print progress messages? (default TRUE)
#' @return A named list:
#'   $fixed_effects  - F-test table for Sample effect: Attribute | F | df_num | df_den | p_value | sig
#'   $random_effects - Variance components per attribute
#'   $models         - Named list of fitted lmer objects (one per attribute)
#'
#' @examples
#' mm <- mixed_model_anova(df)
#' print(mm$fixed_effects)
mixed_model_anova <- function(df, verbose = TRUE) {
  attrs <- names(df)[-(1:3)]

  models        <- list()
  fixed_rows    <- list()
  random_rows   <- list()

  for (attr in attrs) {
    if (verbose) message("  Mixed model: ", attr)

    d <- df %>%
      select(Assessor, Sample, Replicate, value = all_of(attr)) %>%
      filter(!is.na(value)) %>%
      mutate(
        Assessor  = factor(Assessor),
        Sample    = factor(Sample),
        Replicate = factor(Replicate)
      )

    fit <- tryCatch(
      suppressMessages(
        lmerTest::lmer(
          value ~ Sample + (1 | Assessor) + (1 | Assessor:Replicate) + (1 | Assessor:Sample),
          data    = d,
          REML    = TRUE,
          control = lme4::lmerControl(optimizer = "bobyqa",
                                      optCtrl   = list(maxfun = 2e5))
        )
      ),
      error = function(e) {
        message("    WARNING: model failed for ", attr, " — ", conditionMessage(e))
        NULL
      }
    )

    if (is.null(fit)) {
      fixed_rows[[attr]]  <- tibble(Attribute = attr, F_value = NA_real_,
                                    df_num = NA_real_, df_den = NA_real_,
                                    p_value = NA_real_, sig = NA_character_,
                                    note = "Model failed")
      next
    }

    models[[attr]] <- fit

    # Fixed effect F-test (Satterthwaite df)
    an <- anova(fit, type = "III")
    if ("Sample" %in% rownames(an)) {
      fixed_rows[[attr]] <- tibble(
        Attribute = attr,
        F_value   = an["Sample", "F value"],
        df_num    = an["Sample", "NumDF"],
        df_den    = an["Sample", "DenDF"],
        p_value   = an["Sample", "Pr(>F)"],
        sig       = .sig_stars(an["Sample", "Pr(>F)"])
      )
    }

    # Random effects variance components
    vc <- as.data.frame(lme4::VarCorr(fit))
    random_rows[[attr]] <- tibble(
      Attribute  = attr,
      Component  = vc$grp,
      Variance   = vc$vcov,
      Std_Dev    = vc$sdcor
    )
  }

  list(
    fixed_effects  = bind_rows(fixed_rows),
    random_effects = bind_rows(random_rows),
    models         = models
  )
}


# ── 2. Tukey HSD post-hoc ─────────────────────────────────────────────────────

#' Tukey HSD pairwise comparison of samples from mixed model
#'
#' Uses emmeans to estimate marginal means and applies Tukey adjustment.
#' Returns pairwise contrasts AND a compact letter display (CLD) for each attribute.
#'
#' Large designs (>50 samples) produce n*(n-1)/2 pairwise tests — this scales
#' quadratically and can take hours or exhaust memory. The max_samples guard
#' catches this early and returns empty results with a clear warning.
#'
#' @param mm          Output of mixed_model_anova()
#' @param alpha       Significance threshold for CLD groupings (default 0.05)
#' @param max_samples Maximum number of sample levels before aborting (default 50).
#'                    Set to Inf to override the guard and run regardless.
#' @return A named list:
#'   $contrasts  - All pairwise comparisons: Attribute | contrast | estimate | SE | df | t | p_adj | sig
#'   $cld        - Compact letter display:   Attribute | Sample | emmean | SE | .group
#'   $emmeans    - Marginal means:           Attribute | Sample | emmean | SE | lower.CL | upper.CL
#'
#' @examples
#' thsd <- tukey_hsd(mm)
#' print(thsd$cld)
tukey_hsd <- function(mm, alpha = 0.05, max_samples = 50) {
  if (length(mm$models) == 0) stop("No fitted models found in mm$models.")

  # ── Sample-count guard ────────────────────────────────────────────────────
  # Tukey HSD pairwise comparisons scale as n*(n-1)/2.  With >50 samples this
  # creates >1,225 tests; with 210 samples it creates 21,945.  multcomp::cld()
  # can hang or fail at that scale.  Bail out early with a clear message.
  n_samples <- tryCatch(
    nlevels(mm$models[[1]]@frame[["Sample"]]),
    error = function(e) 0L
  )
  if (n_samples > max_samples) {
    n_pairs <- choose(n_samples, 2)
    # Use stop() so .try_step() in run_all.R catches this as a skip and
    # returns NULL, preventing downstream plot functions from being called
    # with empty data. The reason is logged in the skip summary.
    stop(
      "tukey_hsd() skipped: ", n_samples, " sample levels detected ",
      "(", n_pairs, " pairwise comparisons). ",
      "max_samples = ", max_samples, ". Set max_samples = Inf to override. ",
      "Consider filtering to a subset of samples before calling tukey_hsd()."
    )
    empty <- tibble(Attribute = character(), contrast = character(),
                    estimate = double(), SE = double(), df = double(),
                    t_ratio = double(), p_adj = double(), sig = character())
    return(list(
      contrasts = empty,
      emmeans   = tibble(Attribute = character(), Sample = character(),
                         emmean = double(), SE = double(),
                         lower_CL = double(), upper_CL = double()),
      cld       = tibble(Attribute = character(), Sample = character(),
                         emmean = double(), SE = double(), letter = character())
    ))
  }

  contrast_rows <- list()
  cld_rows      <- list()
  emmean_rows   <- list()

  for (attr in names(mm$models)) {
    fit <- mm$models[[attr]]

    em <- tryCatch(
      emmeans::emmeans(fit, ~ Sample),
      error = function(e) {
        message("  emmeans failed for ", attr, ": ", conditionMessage(e))
        NULL
      }
    )
    if (is.null(em)) next

    # Pairwise contrasts with Tukey adjustment
    pw <- as.data.frame(pairs(em, adjust = "tukey"))
    contrast_rows[[attr]] <- pw %>%
      transmute(
        Attribute = attr,
        contrast  = as.character(contrast),
        estimate  = estimate,
        SE        = SE,
        df        = df,
        t_ratio   = t.ratio,
        p_adj     = p.value,
        sig       = .sig_stars(p.value)
      )

    # Emmeans (marginal means with 95% CI)
    em_df <- as.data.frame(em)
    emmean_rows[[attr]] <- em_df %>%
      transmute(
        Attribute = attr,
        Sample    = as.character(Sample),
        emmean    = emmean,
        SE        = SE,
        lower_CL  = lower.CL,
        upper_CL  = upper.CL
      )

    # Compact letter display (suppress tukey->sidak note for mixed model df)
    cld_df <- tryCatch(
      suppressMessages(
        multcomp::cld(em, alpha = alpha, Letters = letters, adjust = "tukey")
      ) %>% as.data.frame(),
      error = function(e) NULL
    )
    if (!is.null(cld_df)) {
      cld_rows[[attr]] <- tibble::tibble(
        Attribute = attr,
        Sample    = as.character(cld_df$Sample),
        emmean    = cld_df$emmean,
        SE        = cld_df$SE,
        letter    = trimws(cld_df$.group)
      )
    }
  }

  list(
    contrasts = bind_rows(contrast_rows),
    emmeans   = bind_rows(emmean_rows),
    cld       = bind_rows(cld_rows)
  )
}


# ── 3. Plot: sample means with Tukey letter groupings ────────────────────────

#' Bar plot of estimated marginal means with Tukey HSD letter groupings
#'
#' One facet per attribute. Bars = emmean, error bars = SE.
#' Tukey letters above bars indicate non-significant groups (shared letter = p > 0.05).
#'
#' @param thsd      Output of tukey_hsd()
#' @param attributes Character vector to subset attributes (NULL = all)
#' @return A ggplot object
#'
#' @examples
#' p <- plot_tukey_means(thsd)
#' print(p)
plot_tukey_means <- function(thsd, attributes = NULL) {
  d <- thsd$cld
  if (!is.null(attributes)) d <- filter(d, Attribute %in% attributes)
  if (nrow(d) == 0) stop("No CLD data to plot. Check tukey_hsd() output.")

  em <- thsd$emmeans
  if (!is.null(attributes)) em <- filter(em, Attribute %in% attributes)

  # Join emmeans CIs into cld for error bars
  d <- d %>%
    left_join(select(em, Attribute, Sample, lower_CL, upper_CL),
              by = c("Attribute", "Sample"))

  n_samps  <- length(unique(d$Sample))
  samp_pal <- palette_samples(n_samps)

  # Letter y-position: just above upper CI
  d <- d %>%
    group_by(Attribute) %>%
    mutate(letter_y = max(upper_CL, na.rm = TRUE) * 1.08) %>%
    ungroup()

  ggplot(d, aes(x = Sample, y = emmean, fill = Sample)) +
    geom_col(width = 0.72, colour = NA) +
    geom_errorbar(
      aes(ymin = lower_CL, ymax = upper_CL),
      width     = 0.22,
      linewidth = 0.55,
      colour    = "grey25"
    ) +
    geom_text(
      aes(y = letter_y, label = letter),
      size   = 3.8,
      family = "Barlow Semi Condensed",
      colour = "grey20"
    ) +
    facet_wrap(~Attribute, scales = "free_y") +
    scale_fill_manual(values = samp_pal, guide = "none") +
    scale_y_continuous(expand = expansion(mult = c(0, 0.18))) +
    labs(
      title    = "Sample Means with Tukey HSD Groupings",
      subtitle = "Bars = estimated marginal mean; error bars = 95% CI; shared letter = p > 0.05",
      x        = "Sample",
      y        = "Estimated Marginal Mean"
    ) +
    theme_clean() +
    theme(axis.text.x = element_text(angle = 40, hjust = 1))
}


#' Heatmap of Tukey HSD pairwise p-values per attribute
#'
#' Rows and columns = sample pairs. Fill = adjusted p-value.
#' Red = significantly different (p < 0.05), blue = not.
#'
#' @param thsd Output of tukey_hsd()
#' @return A ggplot object
plot_tukey_heatmap <- function(thsd) {
  d <- thsd$contrasts %>%
    filter(!is.na(p_adj)) %>%
    mutate(
      pair     = contrast,
      sig_band = sig_label(p_adj),
      sig_band = factor(sig_band, levels = names(sig_colours()))
    )

  ggplot(d, aes(x = Attribute, y = pair, fill = sig_band)) +
    geom_tile(colour = "white", linewidth = 0.6) +
    geom_text(aes(label = sig), size = 3.2, colour = "white") +
    scale_fill_manual(values = sig_colours(), name = "Significance", drop = FALSE) +
    scale_x_discrete(expand = expansion(0)) +
    scale_y_discrete(expand = expansion(0)) +
    labs(
      title    = "Tukey HSD Pairwise Comparisons",
      subtitle = "Cell colour = significance band | * p<0.05  ** p<0.01  *** p<0.001",
      x        = "Attribute",
      y        = "Sample Pair"
    ) +
    theme_clean() +
    theme(
      axis.text.x     = element_text(angle = 40, hjust = 1),
      panel.grid      = element_blank(),
      legend.position = "bottom"
    ) +
    guides(fill = guide_legend(title.position = "top", nrow = 1))
}


#' Variance components bar chart: proportion of total variance explained
#'
#' Shows how much variance is explained by Assessor, Session, interaction, and residual.
#'
#' @param mm Output of mixed_model_anova()
#' @return A ggplot object
plot_variance_components <- function(mm) {
  d <- mm$random_effects %>%
    filter(Component != "Residual") %>%
    bind_rows(
      mm$random_effects %>%
        filter(Component == "Residual") %>%
        rename()
    ) %>%
    group_by(Attribute) %>%
    mutate(pct = Variance / sum(Variance) * 100) %>%
    ungroup() %>%
    mutate(
      Component = case_when(
        Component == "Assessor"          ~ "Panellist",
        Component == "Assessor:Replicate" ~ "Session (within panellist)",
        Component == "Assessor:Sample"   ~ "Panellist x Sample",
        Component == "Residual"          ~ "Residual",
        TRUE                             ~ Component
      ),
      Component = factor(Component, levels = c(
        "Panellist", "Session (within panellist)",
        "Panellist x Sample", "Residual"
      ))
    )

  vc_pal <- RColorBrewer::brewer.pal(4, "Set2")

  ggplot(d, aes(x = Attribute, y = pct, fill = Component)) +
    geom_col(width = 0.75) +
    scale_fill_manual(values = vc_pal, name = "Variance Source") +
    scale_y_continuous(labels = function(x) paste0(x, "%"),
                       expand = expansion(mult = c(0, 0.02))) +
    labs(
      title    = "Random Effects Variance Components",
      subtitle = "Proportion of total variance explained by each random effect",
      x        = "Attribute",
      y        = "% Variance"
    ) +
    theme_clean() +
    theme(
      axis.text.x     = element_text(angle = 40, hjust = 1),
      legend.position = "bottom"
    ) +
    guides(fill = guide_legend(title.position = "top", nrow = 2))
}


# ── Internal ──────────────────────────────────────────────────────────────────

.sig_stars <- function(p) {
  case_when(
    is.na(p)   ~ "",
    p <= 0.001 ~ "***",
    p <= 0.01  ~ "**",
    p <= 0.05  ~ "*",
    p <= 0.1   ~ ".",
    TRUE       ~ "ns"
  )
}
