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
│   └── ...
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
    ├── run_all.R               # Master script — runs all analyses on a dataset
    └── utils.R                 # Shared helper functions
```

---

## R Translation

The `R/` folder contains a clean, modular reimplementation of all PanelCheck analyses. It is designed to be run in RStudio and prioritises **reproducibility and readability** over GUI interaction.

### Recommended Folder Setup

If you are new to R, the easiest way to keep your project organised is to create a dedicated folder on your computer with the following structure. The `PanelCheck/` folder from this repository sits alongside your own data and output folders:

```
my_project/
├── PanelCheck/          ← this repository (cloned or downloaded)
│   ├── R/
│   │   └── run_all.R    ← edit DATA_FILE and OUTPUT_DIR here
│   └── ...
│
├── data/                ← put your prepared input .xlsx file here
│   └── my_panel_data.xlsx
│
└── output/              ← plots (PNG) and results table (.xlsx) are saved here
    ├── fvalue_heatmap.png
    ├── pca_biplot.png
    └── PanelCheck_results.xlsx
```

In `run_all.R`, set the paths to match:

```r
DATA_FILE  <- "../../data/my_panel_data.xlsx"   # relative to R/
OUTPUT_DIR <- "../../output"                     # relative to R/
```

> **Tip:** Paths in `run_all.R` are relative to the `R/` folder. Two `../` steps move up to `my_project/`. You can also use an absolute path (e.g., `"C:/Users/name/my_project/data/my_panel_data.xlsx"` on Windows or `"/Users/name/my_project/data/my_panel_data.xlsx"` on macOS).

---

### Quick Start

**Step 1 — Install required packages** *(first time only)*

```r
install.packages(c(
  "readxl", "writexl",
  "dplyr", "tidyr", "purrr",
  "ggplot2", "ggrepel",
  "car", "lme4", "lmerTest", "emmeans", "multcomp",
  "FactoMineR", "RColorBrewer"
))
```

**Step 2 — Prepare your data** (see [Data Preparation](#data-preparation) below)

**Step 3 — Edit the settings at the top of `run_all.R`**

```r
DATA_FILE   <- "../../data/my_panel_data.xlsx"  # path to your data
OUTPUT_DIR  <- "../../output"                   # where to save results
SAVE_PLOTS  <- TRUE   # TRUE = save PNG plots to OUTPUT_DIR
SAVE_TABLES <- TRUE   # TRUE = save Excel results workbook to OUTPUT_DIR
```

**Step 4 — Run the full pipeline in RStudio**

```r
source("R/run_all.R")
```

Or run individual modules:

```r
source("R/00_load_data.R")
df <- load_panel_data("data/my_panel_data.xlsx")

source("R/01_panel_performance.R")
perf <- panel_performance(df)
```

---

### Data Preparation

#### Required format

Your input file must be an `.xlsx` spreadsheet with the first three columns named **exactly** as shown:

| Column | Type | Description |
|---|---|---|
| `Assessor` | text | Panelist ID (e.g., `AS1`, `John Smith`) |
| `Sample` | text | Product/sample ID (e.g., `Bread1`, `Treatment A`) |
| `Replicate` | number | Replicate number (`1`, `2`, …) |
| `Attr1` … `AttrN` | number | Sensory attribute scores — one column per attribute |

Each row is one rating: a single assessor scoring one sample in one replicate session. A balanced panel with 8 assessors, 5 samples, and 2 replicates would have 8 × 5 × 2 = **80 rows**.

#### Preparing your data from an existing file

If your data uses different column names (e.g., exported from survey software or another sensory tool), use the following template as a starting point. Copy this into a separate R script, adjust the column names and attribute range to match your file, then save the result as a new `.xlsx` ready for `run_all.R`.

```r
library(dplyr)
library(stringr)
library(readxl)
library(writexl)

# 1. Load your raw data file
raw <- read_xlsx("path/to/your_raw_data.xlsx")

# 2. Select and rename the key columns to match PanelCheck requirements.
#    Replace judge_name, sample_description, rep with your actual column names.
#    Replace first_attribute:last_attribute with the range of your sensory columns.
panel_data <- raw |>
  select(judge_name, sample_description, rep, first_attribute:last_attribute) |>
  rename(
    Assessor  = judge_name,
    Sample    = sample_description,
    Replicate = rep
  ) |>

  # 3. Clean up sample names — removes leading/trailing spaces and
  #    collapses any internal double-spaces (a common data entry issue).
  mutate(Sample = Sample |> str_trim() |> str_squish())

# 4. Save the prepared data ready for run_all.R
write_xlsx(panel_data, "data/my_panel_data.xlsx")
```

> **Common issue:** Sample names that look identical but differ by a hidden space (e.g., `"Treatment A"` vs `"Treatment A "`) will be treated as separate samples. The `str_trim()` + `str_squish()` step above prevents this.

---

### Analyses

| Script | Analysis |
|---|---|
| `01_panel_performance.R` | Descriptive stats · 2-way ANOVA (Type III) · assessor discrimination · repeatability · agreement |
| `02_profile_plots.R` | Per-assessor profiles vs. panel mean · spider chart |
| `03_mixed_model.R` | Mixed model ANOVA · Tukey HSD · variance components |
| `04_pca.R` | Consensus PCA biplot · scree plot · variable loadings |
| `05_fvalue_overview.R` | F-value heatmap · dotplot · p-value heatmap · discrimination bars |

### Error Handling

Each step in `run_all.R` is wrapped in a graceful error handler. If a step fails (e.g., too many samples for a Tukey HSD comparison, or a plot that cannot render at the current scale), it is skipped and the reason is logged. A summary of all skipped steps is printed at the end of every run:

```
--- Run summary ---
  1 step(s) were skipped:

    [SKIPPED] Tukey HSD
      Reason : 192 sample levels detected (18336 pairwise comparisons).
               Set max_samples = Inf to override.
```

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
|---|---|
| **Univariate › Profile plots** | Individual vs. consensus scoring per attribute |
| **Multivariate › Tucker-1** | Assessor agreement across attributes |
| **Consensus › PCA scores** | Product similarity map |
| **Overall › Overview Plot (F values)** | F-value summary across all attributes |

Export any plot using the **disk icon** at the bottom left of the plot panel.

---

## Attribution

Original PanelCheck application developed by [CPHFOOD](https://github.com/CPHFOOD/PanelCheck).

The R translation in this fork was developed with assistance from **Claude Code** (Anthropic), an AI-powered coding assistant.
