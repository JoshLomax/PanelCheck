# 04_pca.R - Principal Component Analysis for sample differentiation
#
# Uses FactoMineR::PCA() on the consensus matrix (attributes averaged across
# all assessors and replicates). Produces a publication-ready custom biplot
# following the structure of the reference example.
#
# Main functions:
#   build_consensus_matrix() - average scores to a samples x attributes matrix
#   run_pca()                - run FactoMineR::PCA and return structured results
#   plot_pca_biplot()        - custom ggplot2 biplot with arrows + ggrepel labels
#   plot_pca_scree()         - scree plot of explained variance
#   plot_pca_loadings()      - bar chart of variable contributions per component
#
# Requires: FactoMineR, factoextra, ggrepel, glue, tibble, dplyr, ggplot2
#
# Usage:
#   source("00_load_data.R")
#   source("04_pca.R")
#   df  <- load_panel_data("../Data_Bread.xlsx")
#   pca <- run_pca(df)
#   p   <- plot_pca_biplot(pca)
#   print(p)

suppressPackageStartupMessages({
  # Load MASS-dependent packages first so tidyverse wins the namespace conflict
  library(FactoMineR)
  library(factoextra)
  # Tidyverse loaded last — dplyr::select, filter, mutate etc. take precedence over MASS
  library(dplyr)
  library(tidyr)
  library(tibble)
  library(ggplot2)
  library(ggrepel)
  library(glue)
  library(RColorBrewer)
})


# ── 1. Build consensus matrix ─────────────────────────────────────────────────

#' Average sensory scores across all assessors and replicates
#'
#' Returns a samples x attributes data frame suitable for PCA.
#' Row names = sample IDs; columns = sensory attributes.
#'
#' @param df Data frame from load_panel_data()
#' @return A data frame (samples x attributes)
#'
#' @examples
#' consensus <- build_consensus_matrix(df)
build_consensus_matrix <- function(df) {
  attrs <- names(df)[-(1:3)]

  df %>%
    pivot_longer(cols = all_of(attrs), names_to = "Attribute", values_to = "value") %>%
    filter(!is.na(value)) %>%
    group_by(Sample, Attribute) %>%
    summarise(mean = mean(value), .groups = "drop") %>%
    pivot_wider(names_from = "Attribute", values_from = "mean") %>%
    column_to_rownames("Sample")
}


# ── 2. Run PCA ────────────────────────────────────────────────────────────────

#' Run FactoMineR PCA on the sensory consensus matrix
#'
#' @param df          Data frame from load_panel_data()
#' @param scale.unit  Standardise variables to unit variance? (default TRUE)
#' @param ncp         Number of components to retain (default 5)
#' @param n_top_vars  Number of top-contributing variables to highlight (default all)
#' @param sample_meta Optional data frame with a 'Sample' column and any grouping
#'                    variables (e.g., treatment, colour, shape).
#'                    If NULL, samples are labelled by name only.
#' @return A named list:
#'   $pca          - FactoMineR PCA object
#'   $consensus    - consensus matrix used as input
#'   $var_coords   - variable coordinates (loadings) for biplot arrows
#'   $ind_coords   - individual (sample) coordinates
#'   $top_vars     - top contributing variables
#'   $pct_var      - % variance explained per component
#'   $sample_meta  - sample metadata (passed through or auto-generated)
#'
#' @examples
#' pca <- run_pca(df)
run_pca <- function(df,
                    scale.unit   = TRUE,
                    ncp          = 5,
                    n_top_vars   = NULL,
                    sample_meta  = NULL) {

  consensus <- build_consensus_matrix(df)
  n_attrs   <- ncol(consensus)

  # Run PCA
  pca_res <- FactoMineR::PCA(
    consensus,
    scale.unit = scale.unit,
    ncp        = ncp,
    graph      = FALSE
  )

  # % variance per component
  pct_var <- pca_res$eig[, 2]

  # ── Top variables by combined contribution to Dim1 + Dim2 ────────────────
  n_keep <- if (is.null(n_top_vars)) n_attrs else min(n_top_vars, n_attrs)

  top_vars <- pca_res$var$contrib[, 1:2] %>%
    as.data.frame() %>%
    rownames_to_column("variable") %>%
    mutate(contribution = Dim.1 + Dim.2) %>%
    slice_max(contribution, n = n_keep)

  # ── Variable coordinates for biplot arrows ────────────────────────────────
  var_sum <- factoextra::facto_summarize(
    pca_res, element = "var",
    result  = c("coord", "contrib", "cos2")
  )
  colnames(var_sum)[2:3] <- c("x", "y")

  # Individual (sample) coordinates
  ind_coords <- pca_res$ind$coord %>%
    as.data.frame() %>%
    rownames_to_column("Sample") %>%
    rename(x = Dim.1, y = Dim.2)

  # Arrow scaling factor (fixes parenthesis bug in original example)
  r <- min(
    (max(ind_coords$x) - min(ind_coords$x)) /
      (max(var_sum$x)   - min(var_sum$x)),
    (max(ind_coords$y) - min(ind_coords$y)) /
      (max(var_sum$y)   - min(var_sum$y))
  )

  # Scale arrows to 60% of the scatter plot range (matches example: r * 0.6)
  var_coords <- pca_res$var$coord[top_vars$variable, , drop = FALSE] %>%
    as.data.frame() %>%
    rownames_to_column("variable") %>%
    select(variable, Dim.1, Dim.2) %>%
    mutate(
      x = r * 0.6 * Dim.1,
      y = r * 0.6 * Dim.2
    )

  # ── Sample metadata ───────────────────────────────────────────────────────
  if (is.null(sample_meta)) {
    sample_meta <- tibble(Sample = rownames(consensus))
  } else {
    if (!"Sample" %in% names(sample_meta))
      stop("sample_meta must contain a 'Sample' column.")
    sample_meta <- sample_meta %>%
      filter(Sample %in% rownames(consensus))
  }

  # Merge individual coordinates with sample metadata
  ind_coords <- ind_coords %>%
    left_join(sample_meta, by = "Sample")

  list(
    pca         = pca_res,
    consensus   = consensus,
    var_coords  = var_coords,
    ind_coords  = ind_coords,
    top_vars    = top_vars,
    pct_var     = pct_var,
    sample_meta = sample_meta
  )
}


# ── 3. Custom biplot ──────────────────────────────────────────────────────────

#' Custom PCA biplot with ggplot2
#'
#' Follows the structure of the reference example:
#' - Sample points (optionally coloured/shaped by metadata columns)
#' - Attribute arrows scaled to the scatter range
#' - ggrepel labels for both samples and attributes
#' - Axis labels with % variance explained (via glue)
#'
#' @param pca         Output of run_pca()
#' @param fill_var    Column in sample_meta to map to point fill (NULL = none)
#' @param shape_var   Column in sample_meta to map to point shape (NULL = none)
#' @param label_var   Column in sample_meta to use as sample label (NULL = Sample name)
#' @param arrow_col   Colour for attribute arrows and labels (default: RdBu palette)
#' @param point_size  Size of sample points (default 3)
#' @param label_size  Text size for labels (default 3.2)
#' @param title       Plot title
#' @return A ggplot object
#'
#' @examples
#' # Basic biplot
#' p <- plot_pca_biplot(pca)
#'
#' # With sample metadata grouping
#' meta <- data.frame(Sample = c("Bread1","Bread2","Bread3","Bread4","Bread5"),
#'                    Type   = c("Wheat","Rye","Wheat","Rye","Wheat"))
#' pca2 <- run_pca(df, sample_meta = meta)
#' p2   <- plot_pca_biplot(pca2, fill_var = "Type")
plot_pca_biplot <- function(pca,
                            fill_var   = NULL,
                            shape_var  = NULL,
                            label_var  = NULL,
                            arrow_col  = NULL,
                            point_size = 3,
                            label_size = 3.2,
                            title      = "PCA Biplot - Sensory Panel Consensus") {

  ind   <- pca$ind_coords
  vars  <- pca$var_coords
  pct   <- pca$pct_var

  # Axis labels with % variance (mirrors example glue usage)
  x_lab <- glue::glue("C1: {round(pct[1], 1)}% variance")
  y_lab <- glue::glue("C2: {round(pct[2], 1)}% variance")

  # Sample label column
  label_col <- if (!is.null(label_var) && label_var %in% names(ind)) label_var else "Sample"

  # Arrow colour: single colour if no grouping, or can be extended to group-based
  if (is.null(arrow_col)) {
    arrow_col <- RColorBrewer::brewer.pal(3, "Dark2")[1]
  }

  # Base plot
  p <- ggplot(ind, aes(x = x, y = y)) +
    # Reference lines
    geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.45, alpha = 0.6,
               colour = "grey50") +
    geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.45, alpha = 0.6,
               colour = "grey50")

  # ── Attribute arrows ────────────────────────────────────────────────────────
  p <- p +
    geom_segment(
      data        = vars,
      mapping     = aes(x = 0, y = 0, xend = x, yend = y),
      inherit.aes = FALSE,
      colour      = arrow_col,
      linewidth   = 0.35,
      arrow       = arrow(angle = 28, length = unit(0.18, "cm"), type = "closed")
    ) +
    geom_text_repel(
      data        = vars,
      mapping     = aes(x = x, y = y, label = variable),
      inherit.aes = FALSE,
      colour      = arrow_col,
      fontface    = "italic",
      size        = label_size * 0.88,
      hjust       = -0.1,
      box.padding = 0.15,
      segment.size = 0.2,
      family      = "Barlow Semi Condensed"
    )

  # ── Sample points ───────────────────────────────────────────────────────────
  # Build aes dynamically based on what metadata columns are available
  point_aes <- aes(x = x, y = y)
  if (!is.null(fill_var)  && fill_var  %in% names(ind))
    point_aes <- modifyList(point_aes, aes(fill  = .data[[fill_var]]))
  if (!is.null(shape_var) && shape_var %in% names(ind))
    point_aes <- modifyList(point_aes, aes(shape = .data[[shape_var]]))

  n_samps  <- nrow(ind)
  samp_pal <- palette_samples(min(n_samps, 8))

  p <- p +
    geom_point(
      mapping = point_aes,
      size    = point_size,
      alpha   = 0.95,
      colour  = "grey20",
      shape   = if (is.null(shape_var)) 21 else NULL
    )

  # Fill scale
  if (!is.null(fill_var) && fill_var %in% names(ind)) {
    n_lvls <- length(unique(ind[[fill_var]]))
    p <- p + scale_fill_manual(
      values = palette_samples(min(n_lvls, 8)),
      name   = fill_var
    )
  } else {
    # Colour points by sample name
    p <- p + aes(fill = Sample) +
      scale_fill_manual(values = samp_pal, guide = "none")
  }

  # Shape scale
  if (!is.null(shape_var) && shape_var %in% names(ind)) {
    p <- p + scale_shape_manual(
      values = c(21, 22, 24, 23, 25)[seq_len(length(unique(ind[[shape_var]])))],
      name   = shape_var
    )
  }

  # ── Sample labels ──────────────────────────────────────────────────────────
  p <- p +
    geom_text_repel(
      mapping     = aes(label = .data[[label_col]]),
      size        = label_size,
      hjust       = -0.2,
      vjust       = 0.5,
      box.padding = 0.2,
      colour      = "grey15",
      family      = "Barlow Semi Condensed"
    )

  # ── Scales and theme ────────────────────────────────────────────────────────
  p <- p +
    labs(
      title  = title,
      x      = x_lab,
      y      = y_lab,
      fill   = if (!is.null(fill_var))  fill_var  else "Sample"
    ) +
    theme_clean() +
    theme(
      panel.grid.major = element_blank(),
      panel.border     = element_rect(colour = "grey70", fill = NA, linewidth = 0.5),
      legend.position  = if (!is.null(fill_var) || !is.null(shape_var)) "right" else "none"
    )

  p
}


# ── 4. Scree plot ─────────────────────────────────────────────────────────────

#' Scree plot: % variance explained per principal component
#'
#' @param pca    Output of run_pca()
#' @param n_comp Number of components to display (default: all)
#' @return A ggplot object
#'
#' @examples
#' p <- plot_pca_scree(pca)
plot_pca_scree <- function(pca, n_comp = NULL) {
  pct <- pca$pct_var
  if (!is.null(n_comp)) pct <- pct[seq_len(min(n_comp, length(pct)))]

  d <- tibble(
    Component  = paste0("C", seq_along(pct)),
    Pct_var    = pct,
    Cumulative = cumsum(pct)
  ) %>%
    mutate(Component = factor(Component, levels = Component))

  ggplot(d, aes(x = Component)) +
    geom_col(aes(y = Pct_var),
             fill  = RColorBrewer::brewer.pal(3, "Set2")[1],
             width = 0.65) +
    geom_line(aes(y = Cumulative, group = 1),
              colour    = RColorBrewer::brewer.pal(3, "Dark2")[2],
              linewidth = 0.9) +
    geom_point(aes(y = Cumulative),
               colour = RColorBrewer::brewer.pal(3, "Dark2")[2],
               size   = 2.5) +
    geom_hline(yintercept = 80, linetype = "dashed", colour = "grey50",
               linewidth  = 0.4) +
    scale_y_continuous(
      labels = function(x) paste0(x, "%"),
      expand = expansion(mult = c(0, 0.05)),
      limits = c(0, 105)
    ) +
    labs(
      title    = "PCA Scree Plot",
      subtitle = "Bars = variance per component; line = cumulative; dashed = 80% threshold",
      x        = "Principal Component",
      y        = "% Variance Explained"
    ) +
    theme_clean()
}


# ── 5. Variable contributions bar chart ───────────────────────────────────────

#' Bar chart of variable contributions to PC1 and PC2
#'
#' @param pca  Output of run_pca()
#' @return A ggplot object
#'
#' @examples
#' p <- plot_pca_loadings(pca)
plot_pca_loadings <- function(pca) {
  contrib <- pca$pca$var$contrib[, 1:2] %>%
    as.data.frame() %>%
    rownames_to_column("variable") %>%
    pivot_longer(cols = c(Dim.1, Dim.2),
                 names_to  = "Component",
                 values_to = "Contribution") %>%
    mutate(Component = recode(Component,
                              "Dim.1" = "Component 1",
                              "Dim.2" = "Component 2"))

  ggplot(contrib, aes(x = reorder(variable, Contribution), y = Contribution,
                      fill = Component)) +
    geom_col(width = 0.7) +
    coord_flip() +
    facet_wrap(~Component, scales = "free_x") +
    scale_fill_manual(
      values = RColorBrewer::brewer.pal(3, "Set2")[1:2],
      guide  = "none"
    ) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
    geom_hline(
      yintercept = 100 / nrow(pca$pca$var$contrib),
      linetype   = "dashed", colour = "grey50", linewidth = 0.4
    ) +
    labs(
      title    = "Variable Contributions to PCA",
      subtitle = "Dashed line = expected contribution if all variables equal",
      x        = "Attribute",
      y        = "% Contribution"
    ) +
    theme_clean()
}
