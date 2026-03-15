# 02_profile_plots.R - Profile plots per assessor vs. panel consensus
#
# Mirrors profile_Plot.py from the original PanelCheck application.
#
# Main functions:
#   plot_profiles()          - faceted profile plot: all assessors, all attributes
#   plot_assessor_profile()  - profile for one specific assessor
#   plot_mean_scores()       - bar chart of panel mean scores per attribute
#   plot_spider()            - polar/spider chart of panel mean scores
#
# Usage:
#   source("00_load_data.R")
#   source("02_profile_plots.R")
#   df <- load_panel_data("../Data_Bread.xlsx")
#   p  <- plot_profiles(df)
#   print(p)

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(purrr)
  library(RColorBrewer)
})


# ── Helper: build tidy long-format data ───────────────────────────────────────

.profile_long <- function(df) {
  attrs <- names(df)[-(1:3)]
  df %>%
    pivot_longer(cols = all_of(attrs), names_to = "Attribute", values_to = "Score") %>%
    filter(!is.na(Score)) %>%
    group_by(Assessor, Sample, Attribute) %>%
    summarise(Score = mean(Score), .groups = "drop")
}

.panel_mean_long <- function(long_df) {
  long_df %>%
    group_by(Sample, Attribute) %>%
    summarise(Panel_mean = mean(Score, na.rm = TRUE), .groups = "drop")
}


# ── 1. All-assessor profile plot (faceted by assessor) ────────────────────────

#' Profile plot: individual assessor lines with panel mean overlay
#'
#' Each facet = one assessor. Coloured lines = attributes.
#' Dashed grey line = panel consensus mean.
#'
#' @param df         Data frame from load_panel_data()
#' @param attributes Character vector of attributes to include (NULL = all)
#' @param ncol       Number of facet columns
#' @return A ggplot object
#'
#' @examples
#' p <- plot_profiles(df)
#' print(p)
#' ggsave("profile_all_assessors.png", p, width = 14, height = 10)
plot_profiles <- function(df, attributes = NULL, ncol = 4) {
  attrs <- names(df)[-(1:3)]
  if (!is.null(attributes)) attrs <- intersect(attributes, attrs)

  long       <- .profile_long(df %>% select(1:3, all_of(attrs)))
  panel_mean <- .panel_mean_long(long)

  # Single grand-mean line: average across all attributes per sample
  grand_mean <- long %>%
    group_by(Sample) %>%
    summarise(grand_mean = mean(Score, na.rm = TRUE), .groups = "drop")

  n_attrs <- length(attrs)
  attr_pal <- palette_attributes(n_attrs)
  names(attr_pal) <- sort(unique(long$Attribute))

  ggplot(long, aes(x = Sample, y = Score, colour = Attribute, group = Attribute)) +
    geom_line(linewidth = 0.85, alpha = 0.9) +
    geom_point(size = 2.2) +
    geom_line(
      data        = grand_mean,
      aes(x = Sample, y = grand_mean, group = 1),
      colour      = "grey25",
      linetype    = "dashed",
      linewidth   = 0.6,
      inherit.aes = FALSE
    ) +
    facet_wrap(~Assessor, ncol = ncol) +
    scale_colour_manual(values = attr_pal, name = "Attribute") +
    scale_x_discrete(expand = expansion(add = 0.4)) +
    labs(
      title    = "Assessor Profile Plots",
      subtitle = "Solid lines = individual assessor mean; dashed = panel grand mean (all attributes)",
      x        = "Sample",
      y        = "Mean Score"
    ) +
    theme_clean(base_size = 10) +
    theme(
      axis.text.x     = element_text(angle = 40, hjust = 1),
      legend.position = "bottom"
    ) +
    guides(colour = guide_legend(nrow = 2, byrow = TRUE))
}


# ── 2. Single-assessor profile (one facet per attribute) ──────────────────────

#' Profile plot for a single assessor
#'
#' Each facet = one attribute. Blue solid line = assessor; dashed grey = panel mean.
#'
#' @param df         Data frame from load_panel_data()
#' @param assessor   Assessor ID string (e.g., "AS1")
#' @param attributes Character vector of attributes to include (NULL = all)
#' @return A ggplot object
#'
#' @examples
#' p <- plot_assessor_profile(df, "AS1")
#' print(p)
plot_assessor_profile <- function(df, assessor, attributes = NULL) {
  if (!assessor %in% df$Assessor)
    stop("Assessor '", assessor, "' not found in data.")

  attrs <- names(df)[-(1:3)]
  if (!is.null(attributes)) attrs <- intersect(attributes, attrs)

  long       <- .profile_long(df %>% select(1:3, all_of(attrs)))
  panel_mean <- .panel_mean_long(long)
  ass_long   <- filter(long, Assessor == assessor)

  # Accent colour: first colour from Set1 (strong blue)
  acc_col <- RColorBrewer::brewer.pal(9, "Set1")[2]

  ggplot(ass_long, aes(x = Sample, y = Score, group = 1)) +
    geom_ribbon(
      data        = filter(panel_mean, Attribute %in% attrs),
      aes(x = Sample, y = Panel_mean,
          ymin = Panel_mean * 0.85, ymax = Panel_mean * 1.15, group = 1),
      fill        = "grey88",
      alpha       = 0.6,
      inherit.aes = FALSE
    ) +
    geom_line(
      data        = filter(panel_mean, Attribute %in% attrs),
      aes(x = Sample, y = Panel_mean, group = 1),
      colour      = "grey50",
      linetype    = "dashed",
      linewidth   = 0.7,
      inherit.aes = FALSE
    ) +
    geom_line(colour = acc_col, linewidth = 1.1) +
    geom_point(colour = acc_col, size = 3, shape = 16) +
    facet_wrap(~Attribute, scales = "free_y") +
    scale_x_discrete(expand = expansion(add = 0.4)) +
    labs(
      title    = paste("Profile Plot -", assessor),
      subtitle = "Solid blue = assessor mean; dashed = panel mean (+/-15% band)",
      x        = "Sample",
      y        = "Mean Score"
    ) +
    theme_clean()
}


# ── 3. Panel mean score chart ──────────────────────────────────────────────────

#' Bar chart of panel mean scores per sample, faceted by attribute
#'
#' @param df         Data frame from load_panel_data()
#' @param attributes Character vector of attributes to include (NULL = all)
#' @return A ggplot object
#'
#' @examples
#' p <- plot_mean_scores(df)
#' print(p)
plot_mean_scores <- function(df, attributes = NULL) {
  attrs <- names(df)[-(1:3)]
  if (!is.null(attributes)) attrs <- intersect(attributes, attrs)

  summ <- df %>%
    pivot_longer(cols = all_of(attrs), names_to = "Attribute", values_to = "Score") %>%
    filter(!is.na(Score)) %>%
    group_by(Sample, Attribute) %>%
    summarise(
      mean = mean(Score),
      sd   = sd(Score),
      n    = n(),
      se   = sd / sqrt(n),
      .groups = "drop"
    )

  n_samps  <- length(unique(summ$Sample))
  samp_pal <- palette_samples(n_samps)

  ggplot(summ, aes(x = Sample, y = mean, fill = Sample)) +
    geom_col(width = 0.72, colour = NA) +
    geom_errorbar(
      aes(ymin = mean - se, ymax = mean + se),
      width    = 0.22,
      linewidth = 0.55,
      colour   = "grey30"
    ) +
    facet_wrap(~Attribute, scales = "free_y") +
    scale_fill_manual(values = samp_pal, guide = "none") +
    scale_y_continuous(expand = expansion(mult = c(0, 0.12))) +
    labs(
      title    = "Panel Mean Scores by Sample",
      subtitle = "Error bars = +/-1 SE across all assessors and replicates",
      x        = "Sample",
      y        = "Mean Score"
    ) +
    theme_clean() +
    theme(axis.text.x = element_text(angle = 40, hjust = 1))
}


# ── 4. Spider / radar chart of panel means ────────────────────────────────────

#' Spider plot: panel mean scores across all attributes per sample
#'
#' Uses polar coordinates. Coloured lines/fills = samples.
#'
#' @param df      Data frame from load_panel_data()
#' @param samples Character vector of samples to include (NULL = all)
#' @return A ggplot object
#'
#' @examples
#' p <- plot_spider(df)
#' print(p)
plot_spider <- function(df, samples = NULL) {
  attrs <- names(df)[-(1:3)]
  samps <- if (is.null(samples)) unique(df$Sample) else samples

  attrs_ordered <- sort(attrs)
  n_attrs       <- length(attrs_ordered)

  summ <- df %>%
    filter(Sample %in% samps) %>%
    pivot_longer(cols = all_of(attrs_ordered), names_to = "Attribute", values_to = "Score") %>%
    filter(!is.na(Score)) %>%
    group_by(Sample, Attribute) %>%
    summarise(mean = mean(Score), .groups = "drop") %>%
    mutate(x_pos = match(Attribute, attrs_ordered)) %>%
    arrange(Sample, x_pos)

  # Close the path explicitly: repeat the first attribute at position n+1.
  # coord_polar maps position n+1 to the same angle as position 1, so geom_path
  # draws the closing arc as a normal segment rather than cutting through the plot.
  closing <- summ %>%
    filter(x_pos == 1L) %>%
    mutate(x_pos = n_attrs + 1L)

  summ_closed <- bind_rows(summ, closing) %>%
    arrange(Sample, x_pos)

  n_samps  <- length(samps)
  samp_pal <- palette_samples(n_samps)

  ggplot(summ_closed,
         aes(x = x_pos, y = mean, colour = Sample, fill = Sample, group = Sample)) +
    geom_polygon(alpha = 0.08, linewidth = 0) +
    geom_path(linewidth = 1) +
    # Points on the original positions only (not the duplicated closing row)
    geom_point(data = filter(summ_closed, x_pos <= n_attrs), size = 2.5) +
    coord_polar() +
    scale_x_continuous(
      breaks = seq_len(n_attrs),
      labels = attrs_ordered,
      limits = c(1, n_attrs + 1),
      expand = expansion(0)
    ) +
    scale_colour_manual(values = samp_pal) +
    scale_fill_manual(values = samp_pal) +
    labs(
      title  = "Sensory Profile - Spider Plot",
      x      = NULL,
      y      = "Mean Score",
      colour = "Sample",
      fill   = "Sample"
    ) +
    theme_clean() +
    theme(
      axis.text.x      = element_text(size = rel(0.85)),
      legend.position  = "right",
      panel.grid.major = element_line(colour = "grey88", linewidth = 0.4)
    )
}
