# 05_fvalue_overview.R - F-value overview plots
#
# Mirrors F_Plot.py from the original PanelCheck application.
# Visualises which assessors and attributes show significant product discrimination.
#
# Main functions:
#   plot_fvalue_heatmap()      - heatmap of F-values: assessors x attributes
#   plot_fvalue_dotplot()      - dot plot with significance colouring
#   plot_pvalue_heatmap()      - continuous -log10(p) heatmap
#   plot_discrimination_bars() - % significant attributes per assessor
#   plot_panel_anova()         - panel-level 2-way ANOVA F-values
#
# Usage:
#   source("00_load_data.R")
#   source("01_panel_performance.R")
#   source("05_fvalue_overview.R")
#   df   <- load_panel_data("../Data_Bread.xlsx")
#   disc <- assessor_discrimination(df)
#   p    <- plot_fvalue_heatmap(disc)
#   print(p)

suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
  library(RColorBrewer)
})


# ── Significance palette (sourced from utils.R via sig_colours()) ─────────────
# RdYlBu: red = p<=0.001, orange-yellow = p<=0.01, light-blue = p<=0.05,
#         dark-blue = p>0.05
.sig_band <- function(p) {
  case_when(
    is.na(p)   ~ "p > 0.05",
    p <= 0.001 ~ "p <= 0.001",
    p <= 0.01  ~ "p <= 0.01",
    p <= 0.05  ~ "p <= 0.05",
    TRUE       ~ "p > 0.05"
  )
}


# ── 1. F-value heatmap ────────────────────────────────────────────────────────

#' Heatmap of F-values: assessors (rows) x attributes (cols)
#'
#' Cell fill = p-value significance band (RdYlBu).
#' Cell text  = F-value (capped at 999 for display).
#'
#' @param disc     Output of assessor_discrimination() from 01_panel_performance.R
#' @param show_f   Annotate cells with F-value? (default TRUE)
#' @param title    Plot title
#' @return A ggplot object
#'
#' @examples
#' disc <- assessor_discrimination(df)
#' p    <- plot_fvalue_heatmap(disc)
#' print(p)
plot_fvalue_heatmap <- function(disc,
                                show_f = TRUE,
                                title  = "F-value Overview: Assessor Discrimination") {
  SIG_COLS <- sig_colours()

  d <- disc %>%
    mutate(
      sig_band = .sig_band(p_value),
      sig_band = factor(sig_band, levels = names(SIG_COLS)),
      F_disp   = pmin(F_value, 999),
      F_label  = ifelse(is.na(F_value), "", sprintf("%.1f", F_disp)),
      # White text on dark tiles, dark text on light tiles
      txt_col  = ifelse(sig_band %in% c("p <= 0.001"), "white", "grey20")
    )

  p <- ggplot(d, aes(x = Attribute, y = Assessor, fill = sig_band)) +
    geom_tile(colour = "white", linewidth = 0.7) +
    scale_fill_manual(
      values = SIG_COLS,
      name   = "Significance",
      drop   = FALSE
    ) +
    scale_x_discrete(expand = expansion(0)) +
    scale_y_discrete(expand = expansion(0)) +
    labs(
      title = title,
      x     = "Attribute",
      y     = "Assessor"
    ) +
    theme_clean() +
    theme(
      axis.text.x     = element_text(angle = 40, hjust = 1),
      panel.grid      = element_blank(),
      legend.position = "bottom"
    ) +
    guides(fill = guide_legend(title.position = "top", nrow = 1))

  if (show_f) {
    p <- p + geom_text(
      aes(label = F_label, colour = txt_col),
      size = 3,
      show.legend = FALSE
    ) +
    scale_colour_identity()
  }
  p
}


# ── 2. F-value dot plot ───────────────────────────────────────────────────────

#' Dot / lollipop plot of F-values, coloured by significance
#'
#' X-axis = attribute, Y-axis = F-value, panels = assessor.
#' Dashed horizontal line = critical F at alpha = 0.05.
#' F-values > 100 are capped for display (marked with "+").
#'
#' @param disc      Output of assessor_discrimination()
#' @param n_samples Number of samples (used to compute critical F)
#' @param ncol      Number of facet columns
#' @return A ggplot object
#'
#' @examples
#' p <- plot_fvalue_dotplot(disc, n_samples = 5)
#' print(p)
plot_fvalue_dotplot <- function(disc, n_samples = NULL, ncol = 4) {
  SIG_COLS <- sig_colours()

  # Cap extreme F-values for display readability
  F_CAP <- 50

  d <- disc %>%
    filter(!is.na(F_value)) %>%
    mutate(
      sig_band = .sig_band(p_value),
      sig_band = factor(sig_band, levels = names(SIG_COLS)),
      F_plot   = pmin(F_value, F_CAP),
      capped   = F_value > F_CAP
    )

  p <- ggplot(d, aes(x = Attribute, y = F_plot, colour = sig_band)) +
    geom_segment(
      aes(xend = Attribute, yend = 0),
      linewidth = 0.55,
      alpha     = 0.7
    ) +
    geom_point(
      aes(shape = capped),
      size = 3.2
    ) +
    scale_colour_manual(values = SIG_COLS, name = "Significance", drop = FALSE) +
    scale_shape_manual(
      values = c("FALSE" = 16, "TRUE" = 17),
      labels = c("FALSE" = "Normal", "TRUE" = paste0("Capped at ", F_CAP, "+")),
      name   = "F-value"
    ) +
    facet_wrap(~Assessor, ncol = ncol, scales = "free_y") +
    scale_y_continuous(expand = expansion(mult = c(0, 0.12))) +
    labs(
      title    = "F-value Dot Plot by Assessor",
      subtitle = paste0("Triangles = F-value capped at ", F_CAP,
                        "+ (zero within-group variance)"),
      x        = "Attribute",
      y        = "F-value"
    ) +
    theme_clean(base_size = 10) +
    theme(axis.text.x = element_text(angle = 40, hjust = 1))

  if (!is.null(n_samples) && n_samples > 1) {
    crit_f <- qf(0.95, df1 = n_samples - 1, df2 = 999)
    p <- p + geom_hline(
      yintercept = crit_f,
      linetype   = "dashed",
      colour     = "grey45",
      linewidth  = 0.45
    )
  }
  p
}


# ── 3. Continuous p-value heatmap ─────────────────────────────────────────────

#' Heatmap of -log10(p-value) with continuous Blues fill
#'
#' Darker = more significant. White asterisk marks p <= 0.05.
#'
#' @param disc Output of assessor_discrimination()
#' @return A ggplot object
plot_pvalue_heatmap <- function(disc) {
  d <- disc %>%
    filter(!is.na(p_value)) %>%
    mutate(neg_log_p = -log10(pmax(p_value, 1e-10)))

  ggplot(d, aes(x = Attribute, y = Assessor, fill = neg_log_p)) +
    geom_tile(colour = "white", linewidth = 0.7) +
    geom_text(
      aes(label = ifelse(p_value <= 0.05, "*", "")),
      size   = 5.5,
      colour = "white",
      fontface = "bold"
    ) +
    scale_fill_distiller(
      palette   = "Blues",
      direction = 1,
      name      = "-log10(p)",
      limits    = c(0, NA)
    ) +
    scale_x_discrete(expand = expansion(0)) +
    scale_y_discrete(expand = expansion(0)) +
    labs(
      title    = "p-value Heatmap - Assessor Discrimination",
      subtitle = "Darker = more significant | * = p <= 0.05",
      x        = "Attribute",
      y        = "Assessor"
    ) +
    theme_clean() +
    theme(
      axis.text.x     = element_text(angle = 40, hjust = 1),
      panel.grid      = element_blank(),
      legend.position = "right"
    )
}


# ── 4. Per-assessor discrimination bar chart ──────────────────────────────────

#' Bar chart: % of attributes significantly discriminated per assessor
#'
#' Bars coloured by performance tier (RdYlGn: Good/Moderate/Poor).
#'
#' @param disc  Output of assessor_discrimination()
#' @param alpha Significance threshold (default 0.05)
#' @return A ggplot object
#'
#' @examples
#' p <- plot_discrimination_bars(disc)
#' print(p)
plot_discrimination_bars <- function(disc, alpha = 0.05) {
  PERF_COLS <- palette_performance()

  d <- disc %>%
    filter(!is.na(p_value)) %>%
    group_by(Assessor) %>%
    summarise(
      pct_sig = mean(p_value <= alpha, na.rm = TRUE) * 100,
      n_attrs = n(),
      n_sig   = sum(p_value <= alpha, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(performance = case_when(
      pct_sig >= 50 ~ "Good",
      pct_sig >= 25 ~ "Moderate",
      TRUE          ~ "Poor"
    ),
    performance = factor(performance, levels = c("Good", "Moderate", "Poor")))

  ggplot(d, aes(x = reorder(Assessor, -pct_sig), y = pct_sig, fill = performance)) +
    geom_col(width = 0.7, colour = NA) +
    geom_text(
      aes(label = paste0(n_sig, "/", n_attrs)),
      vjust    = -0.4,
      size     = 3.4,
      colour   = "grey30",
      family   = "Barlow Semi Condensed"
    ) +
    geom_hline(yintercept = 50, linetype = "dashed", colour = "grey45", linewidth = 0.5) +
    scale_fill_manual(values = PERF_COLS, name = "Performance", drop = FALSE) +
    scale_y_continuous(
      limits = c(0, 115),
      breaks = seq(0, 100, 25),
      expand = expansion(mult = c(0, 0))
    ) +
    labs(
      title    = "Assessor Discrimination Ability",
      subtitle = paste0("% of attributes with significant sample F-value (alpha = ", alpha, ")"),
      x        = "Assessor",
      y        = "% Attributes Significant",
      caption  = "Dashed line = 50% threshold | Labels = n significant / n total attributes"
    ) +
    theme_clean() +
    theme(legend.position = "bottom")
}


# ── 5. Panel-level 2-way ANOVA ────────────────────────────────────────────────

#' Lollipop plot of panel-level 2-way ANOVA F-values
#'
#' One facet per effect (Sample, Assessor, interaction).
#' Coloured by significance band (RdYlBu).
#'
#' @param anova_tbl Output of panel_anova() from 01_panel_performance.R
#' @param effects   Effects to display
#' @return A ggplot object
#'
#' @examples
#' anova_tbl <- panel_anova(df)
#' p <- plot_panel_anova(anova_tbl)
#' print(p)
plot_panel_anova <- function(anova_tbl,
                             effects = c("Sample", "Assessor", "Sample:Assessor")) {
  SIG_COLS <- sig_colours()

  d <- anova_tbl %>%
    filter(Effect %in% effects, !is.na(F_value)) %>%
    mutate(
      sig_band = .sig_band(p_value),
      sig_band = factor(sig_band, levels = names(SIG_COLS)),
      Effect   = factor(Effect, levels = effects),
      F_plot   = pmin(F_value, 999)
    )

  ggplot(d, aes(x = Attribute, y = F_plot, colour = sig_band)) +
    geom_segment(
      aes(xend = Attribute, yend = 0),
      linewidth = 0.6,
      alpha     = 0.75
    ) +
    geom_point(size = 3.2) +
    scale_colour_manual(values = SIG_COLS, name = "Significance", drop = FALSE) +
    facet_wrap(~Effect, ncol = 1, scales = "free_y") +
    scale_y_continuous(expand = expansion(mult = c(0, 0.12))) +
    labs(
      title = "Panel-level 2-way ANOVA F-values",
      x     = "Attribute",
      y     = "F-value"
    ) +
    theme_clean() +
    theme(
      axis.text.x     = element_text(angle = 40, hjust = 1),
      legend.position = "bottom"
    ) +
    guides(colour = guide_legend(title.position = "top", nrow = 1))
}
