# PanelCheck

> Sensory panel performance analysis — original macOS app extended with a full R translation.

![R](https://img.shields.io/badge/R-%3E%3D4.1-276DC3?logo=r&logoColor=white)
![Platform](https://img.shields.io/badge/platform-RStudio-75AADB?logo=rstudio&logoColor=white)
![License](https://img.shields.io/badge/license-GPL--2-lightgrey)
![Status](https://img.shields.io/badge/status-active-brightgreen)

---

## Overview

**PanelCheck** is a sensory panel analysis tool originally developed as a macOS desktop application (Python 2.7 + wxPython). This repository extends the original with a full **R translation** of all key statistical analyses, enabling reproducible, scriptable panel performance evaluation in RStudio — no GUI required.

The example dataset `Data_Bread.xlsx` (8 assessors × 5 breads × 2 replicates × 10 attributes) is included and works with both the app and the R scripts.

---

## Repository Structure

```
PanelCheck/
├── README.md                   # This file
├── Data_Bread.xlsx             # Example sensory panel data
│
├── PanelCheck.app/             # Original macOS Python application (do not modify)
│   └── ...                     # Python 2.7 + wxPython GUI source
│
├── css/                        # Styling assets for the HTML report
│   ├── style.css
│   ├── header.html
│   └── external-links-js.html
│
└── R/                          # R translation of PanelCheck analyses
    ├── 00_load_data.R          # Data loading and validation
    ├── 01_panel_performance.R  # ANOVA-based panel performance stats
    ├── 02_profile_plots.R      # Profile plots per assessor per attribute
    ├── 03_mixed_model.R        # Mixed model ANOVA + Tukey HSD + variance components
    ├── 04_pca.R                # PCA consensus analysis (biplot, scree, loadings)
    ├── 05_fvalue_overview.R    # F-value overview plots across all attributes
    ├── 06_eggshell_plot.R      # Eggshell assessor performance plots
    ├── run_all.R               # Master script — runs all analyses on a dataset
    └── utils.R                 # Shared helper functions
```

---

## R Translation

The `R/` folder contains a clean, modular reimplementation of all PanelCheck analyses. It is designed to be run in RStudio and prioritises **reproducibility and readability** over GUI interaction.

### Quick Start

Install required packages (first time only):

```r
install.packages(c("readxl", "dplyr", "tidyr", "purrr", "ggplot2",
                   "car", "lme4", "lmerTest", "emmeans", "multcomp",
                   "FactoMineR", "ggrepel"))
```

Run the full pipeline:

```r
source("R/run_all.R")
```

Or run individual modules:

```r
source("R/00_load_data.R")
data <- load_panel_data("Data_Bread.xlsx")

source("R/01_panel_performance.R")
perf <- panel_performance(data)
```

### Input Data Format

| Column | Type | Description |
|--------|------|-------------|
| `Assessor` | character | Panelist ID (e.g., AS1, AS2) |
| `Sample` | character | Product/sample ID (e.g., Bread1) |
| `Replicate` | numeric | Replicate number (1, 2, …) |
| `Attr1…N` | numeric | Sensory attribute scores |

### Analyses

| Script | Analysis |
|--------|----------|
| `01_panel_performance.R` | Descriptive stats · 2-way ANOVA · discrimination · repeatability · agreement |
| `02_profile_plots.R` | Per-assessor profiles vs. panel mean · spider chart |
| `03_mixed_model.R` | Mixed model ANOVA · Tukey HSD · variance components |
| `04_pca.R` | Consensus PCA biplot · scree plot · loadings |
| `05_fvalue_overview.R` | F-value heatmap/dotplot · p-value heatmap |

---

## Original macOS App

> **Note:** The original app requires macOS High Sierra or later and is not actively maintained.
> Windows users: download from [panelcheck.com](http://www.panelcheck.com/Home/panelcheck_downloads).

The app is archived in `PanelCheck.app/`. It is built on Python 2.7 and wxPython. All data-analysis features work as intended; miscellaneous features such as the *About* and *Help* sections may not function as expected.

### Installation

1. Download this repository
2. Unzip and place the folder somewhere meaningful (*not* in Downloads)
3. First launch — use **Finder** to navigate to the folder, then `cmd+click` the icon and follow the prompt

Double-clicking will launch the app normally on subsequent runs.

### A Minimal Example

Open `Data_Bread.xlsx` via **File › Import › Excel…** and match the Assessor, Sample, and Replicate columns. Then explore:

| View | What it shows |
|------|---------------|
| **Univariate › Profile plots** | Individual vs. consensus scoring per attribute |
| **Multivariate › Tucker-1** | Assessor agreement across attributes |
| **Consensus › PCA scores** | Product similarity map |
| **Overall › Overview Plot (F values)** | F-value summary across all attributes |

Export any plot using the **disk icon** at the bottom left of the plot panel.

---

## Attribution

Original PanelCheck application developed by [CPHFOOD](https://github.com/CPHFOOD/PanelCheck).

The R translation in this fork was developed with assistance from **Claude Code** (Anthropic), an AI-powered coding assistant.
