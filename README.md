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
    ├── 01_panel_performance.R  # ANOVA-based panel performance stats + QC flags
    ├── 02_profile_plots.R      # Profile plots per assessor per attribute
    ├── 03_mixed_model.R        # Mixed model ANOVA + Tukey HSD + variance components
    ├── 04_pca.R                # PCA consensus analysis (biplot, scree, loadings)
    ├── 05_fvalue_overview.R    # F-value overview plots across all attributes
    ├── run_all.R               # Master script — runs all analyses on a single dataset
    ├── run_batch.R             # Batch script — loops run_all.R over multiple datasets
    └── utils.R                 # Shared helpers · QC flag thresholds · skip-log system
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
| `01_panel_performance.R` | Descriptive stats · 3-way ANOVA (Type III SS) · assessor discrimination · repeatability · agreement · QC flags |
| `02_profile_plots.R` | Per-assessor profiles vs. panel mean · spider chart |
| `03_mixed_model.R` | Mixed model ANOVA · Tukey HSD · variance components · variance component flags |
| `04_pca.R` | Consensus PCA biplot · scree plot · variable loadings |
| `05_fvalue_overview.R` | F-value heatmap · dotplot · p-value heatmap · discrimination bars |

---

### Batch Processing

`run_batch.R` loops `run_all.R` over a list of datasets, saving plots and results to a dedicated sub-folder per dataset. Use this when you have multiple panels or conditions stored as separate `.xlsx` files in the same directory.

**Configure the batch settings at the top of `run_batch.R`:**

```r
# Directory containing all input .xlsx files; output sub-folders are created here
BASE_DIR <- "../../panel_check_results"

# Dataset names — each must match a file named <name>.xlsx in BASE_DIR
sensory_sets <- c(
  "study1_qda",
  "study1_dfc",
  "study2_qda"
)

SAVE_PLOTS  <- TRUE   # Write PNG plots for every dataset
SAVE_TABLES <- TRUE   # Write PanelCheck_results.xlsx for every dataset
```

**Output structure:**

```
panel_check_results/
├── study1_qda.xlsx          ← input
├── study1_qda/              ← results (auto-created)
│   ├── PanelCheck_results.xlsx
│   ├── fvalue_heatmap.png
│   └── ...
├── study1_dfc.xlsx
├── study1_dfc/
│   └── ...
└── ...
```

**Run the batch:**

```r
setwd("path/to/PanelCheck/R")
source("run_batch.R")
```

A summary is printed on completion showing which datasets completed and which were skipped:

```
════════════════════════════════════════
 Batch complete: 2 / 3 datasets
  Completed: study1_qda, study1_dfc
  Skipped:   study2_qda
════════════════════════════════════════
```

Datasets are skipped — without stopping the batch — if the input file is missing or if `run_all.R` encounters a fatal error. Per-step errors within each dataset are handled by the skip-log system (see Error Handling below).

> **Note:** `run_all.R` can still be run directly on a single dataset. The `if(!exists())` guards at the top of `run_all.R` ensure that settings set by `run_batch.R` take priority, while standalone runs fall back to the default values defined in `run_all.R`.

---

### Performance Flags

All assessor-level performance tables include automated QC flags that highlight results requiring attention. Flags are computed independently for each domain — no composite score is produced.

#### Flag tiers

| Flag | Tier | Meaning |
|---|---|---|
| 🟢 | PASS | Meets accepted thresholds — no action required |
| 🟡 | MONITOR | Marginal — flag for review or retraining |
| 🔴 | CONCERN | Outside accepted thresholds — consider exclusion or retraining |
| ⬜ | Insufficient data | Cannot be evaluated |

#### Flagged domains

| Domain | Statistic | Table |
|---|---|---|
| **Agreement** | Pearson r with panel mean | Assessor Agreement |
| **Discrimination** | p-value from one-way ANOVA (Sample effect) | Discrimination, Repeatability ANOVA |
| **Repeatability** | CV% across replicates | — |
| **Session consistency** | p-value for Replicate effect in repeatability ANOVA | Repeatability ANOVA |
| **Assessor scale bias** | Assessor variance as % of total (mixed model) | Random Effects |
| **Panel consensus** | Assessor:Sample / Residual variance ratio | Random Effects |

#### Updating the thresholds

All thresholds are stored in a single `FLAG_THRESHOLDS` list at the top of the flagging section in `R/utils.R`. Edit the values there to reflect your panel context, product category, or institutional SOPs — no other files need to change.

```r
FLAG_THRESHOLDS <- list(

  agreement = list(
    pass    = 0.80,   # r ≥ 0.80  → 🟢 PASS
    monitor = 0.60    # r ≥ 0.60  → 🟡 MONITOR, else 🔴 CONCERN
  ),

  discrimination = list(
    pass    = 0.05,   # p < 0.05  → 🟢 PASS
    monitor = 0.10    # p < 0.10  → 🟡 MONITOR, else 🔴 CONCERN
  ),

  repeatability_cv = list(
    pass    = 20,     # CV < 20%  → 🟢 PASS
    monitor = 40      # CV < 40%  → 🟡 MONITOR, else 🔴 CONCERN
  ),

  vc_as_ratio = list(
    concern_low = 1.0,  # ratio < 1.0 → 🔴 (residual exceeds signal)
    pass        = 1.5,  # ratio ≤ 1.5 → 🟢 PASS
    monitor     = 2.0   # ratio ≤ 2.0 → 🟡 MONITOR, else 🔴 CONCERN
  ),

  vc_assessor_pct = list(
    pass    = 25,     # Assessor% < 25% → 🟢 PASS (low scale bias)
    monitor = 40      # Assessor% < 40% → 🟡 MONITOR, else 🔴 CONCERN
  ),

  vc_replicate_pct = list(
    pass     = 5,     # Assessor:Replicate% < 5%  → 🟢 PASS
    monitor  = 10,    # Assessor:Replicate% < 10% → 🟡 MONITOR, else 🔴 CONCERN
    zero_tol = 1e-6   # Near-zero variance         → 🟡 (possible memory effect)
  )
)
```

Flag logic is applied by `apply_flag(domain, value)` in `utils.R`. The function handles edge cases including negative correlation values (agreement), inverted p-value interpretation (Replicate effect), and near-zero variance.

> **Note on thresholds:** Cutoffs are based on general guidelines in sensory science. Treat them as starting points.

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
