

options(width = 120)


PROJECT_DIR <- "C:/Users/User/Downloads/tuition-exchange-rate-forecasting-main (1)/tuition-exchange-rate-forecasting-main"


# ---- 01 - Data Preparation and Preprocessing ----

# Find the project folder by walking up from the working directory
find_root <- function(marker) {
  p <- normalizePath(getwd(), winslash = "/")
  start <- p
  repeat {                                    # 1) look in this folder and its parents
    if (file.exists(file.path(p, marker))) return(p)
    parent <- dirname(p)
    if (parent == p) break
    p <- parent
  }
  subs <- list.dirs(start, recursive = FALSE, full.names = TRUE)   # 2) look one level down
  hit <- subs[file.exists(file.path(subs, marker))]
  if (length(hit) > 0) return(normalizePath(hit[1], winslash = "/"))
  NULL
}

project_root <- if (nzchar(PROJECT_DIR)) normalizePath(PROJECT_DIR, winslash = "/", mustWork = FALSE) else find_root("data/raw")
if (is.null(project_root) || !dir.exists(file.path(project_root, "data", "raw"))) {
  stop("Cannot find the data/raw folder. Set PROJECT_DIR at the top of the script ",
       "to the folder that contains the 'data' folder.")
}

raw_dir <- file.path(project_root, "data", "raw")

cat("Raw data folder:", raw_dir, "\n")
cat("\nFiles:\n")
cat(sort(list.files(raw_dir), method = "radix"), sep = "\n")


# ---- 2. Loading and Inspecting the Raw Data ----

raw_files <- sort(list.files(raw_dir, pattern = "\\.csv$"), method = "radix")
raw_tables <- list()

for (f in raw_files) {
  df <- read.csv(file.path(raw_dir, f), check.names = FALSE,
                 stringsAsFactors = FALSE)
  raw_tables[[f]] <- df

  cat("\nFile:", f, "\n")
  cat("Rows and columns:", nrow(df), ncol(df), "\n")
  cat("Column names:", paste(names(df), collapse = ", "), "\n")
  print(head(df))
}

# Show the exact keys available
for (nm in names(raw_tables)) print(nm)


# ---- 3. Data Cleaning and Standardisation ----

# Specify each file's series and date format
files <- list(
  "Exchange rate Euro.csv"  = list(series = "DEXUSEU", fmt = "%Y-%m-%d"),
  "Exchange rate pound.csv" = list(series = "DEXUSUK", fmt = "%Y-%m-%d"),
  "Exchange rate_usd.csv"   = list(series = "DEXSLUS", fmt = "%m/%d/%Y")
)

START_DATE <- as.Date("2000-01-01")
END_DATE   <- as.Date("2025-12-31")

# Store prepared tables and check results
clean_tables <- list()
checks <- list()

for (filename in names(files)) {
  series <- files[[filename]]$series
  fmt    <- files[[filename]]$fmt

  # Copy the original table
  df <- raw_tables[[filename]]

  # Convert dates and rates; invalid values become missing
  df$date <- as.Date(df$observation_date, format = fmt)
  df[[series]] <- suppressWarnings(as.numeric(df[[series]]))

  # Count invalid dates before filtering
  invalid_dates <- sum(is.na(df$date))

  # Select 2000-2025 and sort chronologically
  keep <- !is.na(df$date) & df$date >= START_DATE & df$date <= END_DATE
  df <- df[keep, c("date", series)]
  df <- df[order(df$date), ]

  stopifnot("Duplicate dates found" = !anyDuplicated(df$date))

  # Record data-quality checks
  checks[[series]] <- data.frame(
    Series = series,
    Rows = nrow(df),
    Start = min(df$date),
    End = max(df$date),
    Invalid_dates = invalid_dates,
    Duplicate_dates = sum(duplicated(df$date)),
    Missing_rates = sum(is.na(df[[series]]))
  )

  # Keep the prepared table for merging later
  clean_tables[[series]] <- df
}

# Display all checks together
print(do.call(rbind, checks), row.names = FALSE)


# ---- 4. Date Alignment and Missing-Value Assessment ----

# Align all three series by date, retaining missing values
combined <- merge(clean_tables$DEXSLUS, clean_tables$DEXUSUK,
                  by = "date", all = TRUE)
combined <- merge(combined, clean_tables$DEXUSEU, by = "date", all = TRUE)
combined <- combined[order(combined$date), ]
rownames(combined) <- NULL

rate_cols <- c("DEXSLUS", "DEXUSUK", "DEXUSEU")

# Identify dates with all rates missing versus only some missing
missing <- is.na(combined[, rate_cols])
n_missing <- rowSums(missing)
cat("All three missing:", sum(n_missing == 3), "\n")
cat("Only some missing:", sum(n_missing > 0 & n_missing < 3), "\n")

# Preview dates containing missing rates
print(head(combined[n_missing > 0, ], 10))


# ---- 5. Missing-Value Treatment and Exchange-Rate Conversion ----

# Keep rows with all three exchange rates available
rates <- combined[complete.cases(combined[, rate_cols]), ]

# Check that rates are positive
stopifnot(all(rates[, rate_cols] > 0))

# Express each foreign currency in Sri Lankan rupees
rates$USD <- rates$DEXSLUS
rates$GBP <- rates$DEXSLUS * rates$DEXUSUK
rates$EUR <- rates$DEXSLUS * rates$DEXUSEU

# Keep the final columns in chronological order
rates <- rates[, c("date", "USD", "GBP", "EUR")]
rownames(rates) <- NULL

# Check the resulting dataset
cat("Reporting days:", nrow(rates), "\n")
print(head(rates))

cat("\nMissing values:\n")
print(colSums(is.na(rates)))

cat("\nDuplicate dates:", sum(duplicated(rates$date)), "\n")

cat("\nData types:\n")
print(sapply(rates, function(x) class(x)[1]))

stopifnot(!anyNA(rates[, c("USD", "GBP", "EUR")]))
stopifnot(all(rates[, c("USD", "GBP", "EUR")] > 0))
stopifnot(!anyDuplicated(rates$date))


# ---- 6. Exporting the Processed Dataset ----

# Create the processed-data folder
processed_dir <- file.path(project_root, "data", "processed")
dir.create(processed_dir, showWarnings = FALSE, recursive = TRUE)

# Save all cleaned reporting-day observations
write.csv(rates, file.path(processed_dir, "exchange_rates_lkr.csv"),
          row.names = FALSE)

# Confirm the sample size and date range
cat("Rows:", nrow(rates), "\n")
cat("Period:", format(min(rates$date)), "to", format(max(rates$date)), "\n")
print(head(rates))
