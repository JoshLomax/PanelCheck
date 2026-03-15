# 00_load_data.R — Data loading and validation
#
# Main function: load_panel_data()
#
# Usage:
#   source("00_load_data.R")
#   df <- load_panel_data("../Data_Bread.xlsx")

source(file.path(dirname(sys.frame(1)$ofile %||% "."), "utils.R"), local = TRUE)

# Fallback for `%||%` if not in environment
`%||%` <- function(a, b) if (!is.null(a)) a else b


#' Load and validate sensory panel data
#'
#' Reads an Excel or CSV file and returns a validated data frame with columns:
#'   Assessor | Sample | Replicate | Attr1 | Attr2 | ...
#'
#' @param path    Path to the data file (.xlsx, .xls, .csv, .txt/.tsv)
#' @param sheet   Sheet name or number for Excel files (default: 1)
#' @param sep     Separator for text files (default: auto-detect)
#' @param assessor_col  Name or index of the assessor column (default: 1)
#' @param sample_col    Name or index of the sample column   (default: 2)
#' @param rep_col       Name or index of the replicate column (default: 3)
#'
#' @return A tibble with standardised column names and numeric attributes.
#'         Attribute column names are preserved from the source file.
#'
#' @examples
#' df <- load_panel_data("../Data_Bread.xlsx")
load_panel_data <- function(path,
                            sheet        = 1,
                            sep          = NULL,
                            assessor_col = 1,
                            sample_col   = 2,
                            rep_col      = 3) {

  if (!file.exists(path)) stop("File not found: ", path)

  ext <- tolower(tools::file_ext(path))

  # ── Read file ────────────────────────────────────────────────────────────────
  if (ext %in% c("xlsx", "xls")) {
    if (!requireNamespace("readxl", quietly = TRUE))
      stop("Package 'readxl' required. Install with: install.packages('readxl')")
    raw <- readxl::read_excel(path, sheet = sheet)
  } else if (ext %in% c("csv", "txt", "tsv")) {
    if (is.null(sep)) {
      sep <- if (ext == "csv") "," else "\t"
    }
    raw <- utils::read.delim(path, sep = sep, stringsAsFactors = FALSE,
                             check.names = FALSE)
    raw <- tibble::as_tibble(raw)
  } else {
    stop("Unsupported file type: .", ext,
         ". Use .xlsx, .xls, .csv, .txt, or .tsv")
  }

  # ── Rename first three columns to standard names ─────────────────────────────
  colnames(raw)[assessor_col] <- "Assessor"
  colnames(raw)[sample_col]   <- "Sample"
  colnames(raw)[rep_col]      <- "Replicate"

  # Reorder so the three key columns are first
  key_cols  <- c("Assessor", "Sample", "Replicate")
  attr_cols <- setdiff(names(raw), key_cols)
  df <- raw[, c(key_cols, attr_cols)]

  # ── Type coercion ────────────────────────────────────────────────────────────
  df$Assessor   <- as.character(df$Assessor)
  df$Sample     <- as.character(df$Sample)
  df$Replicate  <- as.integer(df$Replicate)

  for (a in attr_cols) {
    df[[a]] <- suppressWarnings(as.numeric(df[[a]]))
  }

  # ── Validation ───────────────────────────────────────────────────────────────
  n_assessors  <- length(unique(df$Assessor))
  n_samples    <- length(unique(df$Sample))
  n_replicates <- length(unique(df$Replicate))
  n_attrs      <- length(attr_cols)

  # Check for missing values
  n_missing <- sum(is.na(df[, attr_cols]))
  if (n_missing > 0) {
    warning(n_missing, " missing value(s) found in attribute columns. ",
            "Rows with NAs will be excluded from individual analyses.")
  }

  # Check for balanced design
  expected_rows <- n_assessors * n_samples * n_replicates
  if (nrow(df) != expected_rows) {
    message(
      "Note: Data appears unbalanced.\n",
      "  Rows in file:  ", nrow(df), "\n",
      "  Expected (", n_assessors, " assessors × ",
      n_samples, " samples × ", n_replicates, " reps): ", expected_rows
    )
  }

  message(
    "Loaded: ", nrow(df), " rows | ",
    n_assessors, " assessors | ",
    n_samples, " samples | ",
    n_replicates, " replicate(s) | ",
    n_attrs, " attributes"
  )

  df
}


#' Summarise the structure of a loaded panel data frame
#'
#' @param df Data frame returned by load_panel_data()
#' @return Invisibly returns a list of summary elements; prints to console.
summarise_panel_data <- function(df) {
  attrs <- get_attributes(df)

  cat("=== Panel Data Summary ===\n")
  cat("Assessors  :", paste(sort(unique(df$Assessor)), collapse = ", "), "\n")
  cat("Samples    :", paste(sort(unique(df$Sample)),   collapse = ", "), "\n")
  cat("Replicates :", paste(sort(unique(df$Replicate)),collapse = ", "), "\n")
  cat("Attributes :", paste(attrs, collapse = ", "), "\n")
  cat("Rows       :", nrow(df), "\n")

  cat("\nAttribute score ranges:\n")
  for (a in attrs) {
    vals <- df[[a]]
    cat(sprintf("  %-14s  min=%.1f  max=%.1f  mean=%.2f  NAs=%d\n",
                a, min(vals, na.rm = TRUE), max(vals, na.rm = TRUE),
                mean(vals, na.rm = TRUE), sum(is.na(vals))))
  }

  invisible(list(
    assessors  = unique(df$Assessor),
    samples    = unique(df$Sample),
    replicates = unique(df$Replicate),
    attributes = attrs
  ))
}
