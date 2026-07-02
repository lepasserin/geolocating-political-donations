# Purpose: Compare Source Data with the IJF Cleaned Version.
# Author: Benedict Cummins-Mburu
# Last Updated: 27 May 2026
# Contact: b.cumminsmburu@utoronto.ca

# ------- Setup --------
library(data.table)
library(tidyverse)

source_data <- read_csv(
  "data/raw_data/OpenData-ElectionsCanada-asSubmitted.csv"
)
source_data_audit <- read_csv(
  "data/raw_data/od_cntrbtn_audt_e.csv"
)

IJF_data_old <- data.table::fread("data/raw_data/raw_data_IJF.csv")
IJF_data_fix <- data.table::fread(
  "data/raw_data/raw_data_IJF_fix2026-06-17.csv"
)

SOURCE_COLUMNS <- c(
  "Political Entity",
  "Recipient ID",
  "Recipient",
  "Recipient last name",
  "Recipient first name",
  "Recipient middle initial",
  "Political Party of Recipient",
  "Electoral District",
  "Electoral event",
  "Fiscal/Election date",
  "Form ID",
  "Financial Report",
  "Part Number of Return",
  "Financial Report part",
  "Contributor type",
  "Contributor name",
  "Contributor last name",
  "Contributor first name",
  "Contributor middle initial",
  "Contributor City",
  "Contributor Province",
  "Contributor Postal code",
  "Contribution Received date",
  "Monetary amount",
  "Non-Monetary amount",
  "Contribution given through",
  "Leadership contestant"
)

IJF_COLUMNS <- c(
  "rid",
  "year",
  "s",
  "added",
  "electoral_event",
  "donor_location",
  "electoral_district",
  "donation_date",
  "donation_year",
  "donor_full_name",
  "donor_type",
  "amount",
  "amount_monetary",
  "amount_non_monetary",
  "political_entity",
  "political_party",
  "recipient"
)


# ------- Summary-stats comparison: IJF old vs IJF fix --------

# Per-column summary for a single CSV. Reads with fread, summarises,
# then drops the table and frees memory before returning.
summarise_csv <- function(path) {
  dt <- data.table::fread(path, showProgress = FALSE)

  out <- data.table::rbindlist(lapply(names(dt), function(col) {
    x <- dt[[col]]
    is_num <- is.numeric(x)
    data.table(
      column = col,
      type = class(x)[1],
      n = length(x),
      n_missing = sum(is.na(x)),
      n_distinct = data.table::uniqueN(x),
      min = if (is_num) suppressWarnings(min(x, na.rm = TRUE)) else NA_real_,
      max = if (is_num) suppressWarnings(max(x, na.rm = TRUE)) else NA_real_,
      mean = if (is_num) mean(x, na.rm = TRUE) else NA_real_,
      sum = if (is_num) sum(as.numeric(x), na.rm = TRUE) else NA_real_
    )
  }))

  attr(out, "nrow") <- nrow(dt)
  attr(out, "ncol") <- ncol(dt)
  rm(dt)
  gc()
  out
}

sum_old <- summarise_csv("data/raw_data/raw_data_IJF.csv")
sum_fix <- summarise_csv("data/raw_data/raw_data_IJF_fix2026-06-17.csv")

# ---- 1. Overall dimensions ----
dims <- data.table(
  metric = c("rows", "columns"),
  old = c(attr(sum_old, "nrow"), attr(sum_old, "ncol")),
  fix = c(attr(sum_fix, "nrow"), attr(sum_fix, "ncol"))
)[, delta := fix - old][]
print(dims)

# ---- 2. Column-set differences ----
cols_old <- sum_old$column
cols_fix <- sum_fix$column
cat("\nColumns only in OLD:\n")
print(setdiff(cols_old, cols_fix))
cat("\nColumns only in FIX:\n")
print(setdiff(cols_fix, cols_old))

# ---- 3. Per-column stat comparison (overlapping columns) ----
comparison <- merge(
  sum_old,
  sum_fix,
  by = "column",
  suffixes = c("_old", "_fix"),
  all = FALSE
)

# Deltas (fix - old) for the numeric-style metrics
for (m in c("n", "n_missing", "n_distinct", "min", "max", "mean", "sum")) {
  comparison[[paste0(m, "_delta")]] <-
    comparison[[paste0(m, "_fix")]] - comparison[[paste0(m, "_old")]]
}

# Flag whether the column type changed between versions
comparison[, type_changed := type_old != type_fix]

# Reorder so each metric's old/fix/delta sit together
metric_cols <- c(
  "type_old",
  "type_fix",
  "type_changed",
  unlist(lapply(
    c("n", "n_missing", "n_distinct", "min", "max", "mean", "sum"),
    function(m) paste0(m, c("_old", "_fix", "_delta"))
  ))
)
comparison <- comparison[, c("column", metric_cols), with = FALSE]

print(comparison)

# ---- 4. Quick view: only columns that actually changed ----
changed <- comparison[
  type_changed |
    n_delta != 0 |
    n_missing_delta != 0 |
    n_distinct_delta != 0 |
    (!is.na(sum_delta) & sum_delta != 0) |
    (!is.na(mean_delta) & mean_delta != 0)
]
cat("\n==== Columns with differences ====\n")
print(changed)


names(source_data)
