# =============================================================================
# 02 - EXPLORATORY DATA ANALYSIS (EDA)
# CM3023 - Financial Time Series Analysis
# Forecasting USD/LKR, GBP/LKR and EUR/LKR exchange rates
# =============================================================================
#
# PURPOSE
#   Study the structure and statistical behaviour of the three exchange-rate
#   series BEFORE any forecasting model is built, so that modelling decisions
#   (log returns, random-walk benchmark, GARCH volatility, non-normal intervals,
#   treatment of the 2022 break) are based on evidence.
#
# INPUT
#   data/processed/exchange_rates_lkr.csv   (produced by 01_data_preparation.R)
#   helpers.R                               (small helper functions, see below)
#
# OUTPUT
#   Tables are printed in the R Console and plots appear in the RStudio Plots
#   pane. This script does not save any files; use the Export button in the
#   Plots pane if a figure must be saved.
#
# HELPER FUNCTIONS USED (all defined in helpers.R)
#   describe_cols()          summary table (count, mean, std, min, quartiles, max)
#   pd_skew(), pd_kurt()     skewness and kurtosis of a series
#   rolling_sd()             rolling standard deviation
#   adf_test(), adf_crit5()  Augmented Dickey-Fuller test and its 5% critical value
#   kpss_p()                 p-value of the KPSS stationarity test
#   arch_lm()                ARCH-LM test for volatility clustering
#   jb_test()                Jarque-Bera test of normality (also skewness/kurtosis)
#   plot_acf_r(), plot_pacf_r()  autocorrelation and partial-autocorrelation plots
#
# SECTIONS
#   2.  Import libraries and load the processed data
#   3.  Dataset structure and data quality
#   4.  Descriptive statistics of exchange-rate levels
#   5.  Exchange-rate trends over time
#   6.  Daily percentage changes (returns)
#   7.  Distribution of daily percentage changes
#   8.  Correlation between currency series
#   9.  Rolling volatility
#   10. Stationarity check: ADF test
#   11. Autocorrelation of daily percentage changes
#   12. Reporting-date gaps
#   13. Additional diagnostics: log returns
#   14. Stationarity tests: ADF and KPSS
#   15. Volatility clustering and normality tests
#   16. ACF and PACF of returns and squared returns
#   17. Normality check: QQ plots
#   18. Structural break: the 2022 rupee float
#   19. Unchanged rates by year
# =============================================================================

# Print wider tables in the Console so that columns are not wrapped.
options(width = 120)

# Folder that contains the project (the folder with data/processed/...).
# Change this path to match your computer, or set it to "" so that the script
# searches for the project folder automatically.
PROJECT_DIR <- "C:/Users/User/Downloads/tuition-exchange-rate-forecasting-main (1)/tuition-exchange-rate-forecasting-main"

# Location of helpers.R. Leave "" to search for it automatically inside the
# project folder.
HELPERS_PATH <- ""


# ---- 02 - Exploratory Data Analysis (EDA) ----


# ---- 2. Import Libraries and Load the Processed Data ----

# Locate the project folder by walking up from the working directory
# tseries provides the ADF and KPSS stationarity tests used in this script.
library(tseries)

# find_root(): searches for the project folder that contains the file "marker".
# It checks the working directory and its parent folders, and then the folders
# one level below the working directory. It returns NULL if nothing is found.
find_root <- function(marker) {
  p <- normalizePath(getwd(), winslash = "/")   # current working directory
  start <- p
  repeat {                                    # 1) look in this folder and its parents
    if (file.exists(file.path(p, marker))) return(p)
    parent <- dirname(p)
    if (parent == p) break                    # reached the top of the drive
    p <- parent
  }
  subs <- list.dirs(start, recursive = FALSE, full.names = TRUE)   # 2) look one level down
  hit <- subs[file.exists(file.path(subs, marker))]
  if (length(hit) > 0) return(normalizePath(hit[1], winslash = "/"))
  NULL
}

# Use PROJECT_DIR if it is set; otherwise search for the folder automatically.
project_root <- if (nzchar(PROJECT_DIR)) normalizePath(PROJECT_DIR, winslash = "/", mustWork = FALSE) else find_root("data/processed/exchange_rates_lkr.csv")

# Stop with a clear message if the processed data file cannot be found.
if (is.null(project_root) || !file.exists(file.path(project_root, "data", "processed", "exchange_rates_lkr.csv"))) {
  stop("Could not find data/processed/exchange_rates_lkr.csv. ",
       "Run 01_data_preparation.R first and check the project folder.")
}

# Read the processed data (columns: date, USD, GBP, EUR in LKR per unit).
data_path <- file.path(project_root, "data", "processed", "exchange_rates_lkr.csv")
df <- read.csv(data_path, stringsAsFactors = FALSE)
df$date <- as.Date(df$date)              # convert the text dates to Date objects

# Study period of the project.
START_DATE <- as.Date("2000-01-01")
END_DATE   <- as.Date("2025-12-31")

# Keep only the study period, sort by date and reset the row numbers.
df <- df[df$date >= START_DATE & df$date <= END_DATE, ]
df <- df[order(df$date), ]
rownames(df) <- NULL

# Names of the exchange-rate columns and of all required columns.
currency_cols <- c("USD", "GBP", "EUR")
required_cols <- c("date", currency_cols)

# Stop if any required column is missing from the data.
missing_cols <- setdiff(required_cols, names(df))
if (length(missing_cols) > 0) {
  stop("Required columns missing from processed data: ",
       paste(missing_cols, collapse = ", "))
}

# Print a short summary of what was loaded.
cat("Project folder:", project_root, "\n")
cat("Data file:", data_path, "\n")
cat("Dataset shape:", nrow(df), "x", ncol(df), "\n")
cat("Study period:", format(min(df$date)), "to", format(max(df$date)), "\n")
print(head(df))


# Load the helper functions (pandas/statsmodels-style calculations)
# If HELPERS_PATH was left empty, search for helpers.R in the project folder
# and in the working directory.
helper_path <- HELPERS_PATH
if (!nzchar(helper_path)) {
  found <- c(list.files(project_root, pattern = "^helpers\\.R$", recursive = TRUE, full.names = TRUE),
             list.files(getwd(), pattern = "^helpers\\.R$", full.names = TRUE))
  helper_path <- if (length(found)) found[1] else ""
}
if (!nzchar(helper_path) || !file.exists(helper_path)) {
  stop("helpers.R not found. Save it in the project folder or set HELPERS_PATH at the top of this script.")
}
# Run helpers.R so that its functions become available in this session.
source(helper_path)


# ---- 3. Dataset Structure and Data Quality ----
# Confirm that the data are complete and valid before analysing them.

# Data type of each column (expected: one Date column and three numeric columns).
cat("Data types:\n")
print(data.frame(dtype = sapply(df, function(x) class(x)[1])))

# Number of missing values in each required column.
quality_summary <- data.frame(missing_values = colSums(is.na(df[required_cols])))
cat("\nMissing values:\n")
print(quality_summary)

# Duplicate dates and zero/negative rates (neither should occur).
cat("\nDuplicate dates:", sum(duplicated(df$date)), "\n")
cat("Non-positive exchange-rate values:\n")
print(data.frame(count = colSums(df[currency_cols] <= 0)))

# Number of rows and unique dates (they should be equal) and the date range.
cat("\nRows:", nrow(df), "\n")
cat("Unique dates:", length(unique(df$date)), "\n")
cat("Date range:", format(min(df$date)), "to", format(max(df$date)), "\n")

# Hard checks: the script stops with the given message if any check fails.
stopifnot("Missing dates found." = !anyNA(df$date))
stopifnot("Duplicate dates found." = !anyDuplicated(df$date))
stopifnot("Missing exchange rates found." = !anyNA(df[currency_cols]))
stopifnot("Non-positive exchange rates found." = all(df[currency_cols] > 0))


# ---- 4. Descriptive Statistics of Exchange-Rate Levels ----
# Count, mean, standard deviation, minimum, quartiles and maximum of each rate.
level_summary <- describe_cols(df[currency_cols])
# Add the range (maximum minus minimum) as an extra column.
level_summary <- cbind(level_summary,
                       range = level_summary[, "max"] - level_summary[, "min"])
print(round(level_summary, 4))


# ---- 5. Exchange-Rate Trends Over Time ----
# One line chart per currency. A steady upward trend and the sharp jump in 2022
# indicate that the levels are not stationary.

for (currency in currency_cols) {
  plot(df$date, df[[currency]], type = "l", lwd = 1,
       main = paste0(currency, "/LKR Exchange Rate Over Time"),
       xlab = "Date", ylab = "LKR per unit of foreign currency")
  grid()
}


# ---- 6. Daily Percentage Changes (Returns) ----

# pct_change(): percentage change between consecutive observations,
# 100 * (P_t / P_{t-1} - 1). The first value is NA (no previous observation).
pct_change <- function(x) c(NA, diff(x) / head(x, -1) * 100)

# Apply it to each currency and build a returns table.
returns <- data.frame(date = df$date, lapply(df[currency_cols], pct_change))
returns <- returns[-1, ]            # first day has no previous observation
rownames(returns) <- NULL

# Summary statistics of the returns, plus skewness and kurtosis.
return_summary <- cbind(
  describe_cols(returns[currency_cols]),
  skewness = sapply(returns[currency_cols], pd_skew),
  kurtosis = sapply(returns[currency_cols], pd_kurt)
)
print(round(return_summary, 4))

# Plot the daily changes. Large spikes (especially March 2022) and periods of
# calm and turbulence are visible.
for (currency in currency_cols) {
  plot(returns$date, returns[[currency]], type = "l", lwd = 0.7,
       main = paste0(currency, "/LKR Daily Percentage Changes"),
       xlab = "Date", ylab = "Change (%)")
  abline(h = 0)                       # reference line at zero change
  grid()
}


# ---- 7. Distribution of Daily Percentage Changes ----
# Histograms show whether the changes are close to a normal (bell-shaped)
# distribution. Tall, narrow centres with a few extreme values indicate heavy tails.

for (currency in currency_cols) {
  values <- returns[[currency]]
  values <- values[!is.na(values)]
  # 50 equal-width bins between the smallest and largest daily change.
  hist(values, breaks = seq(min(values), max(values), length.out = 51),
       col = "blue", border = "black",
       main = paste0("Distribution of ", currency, "/LKR Daily Changes"),
       xlab = "Daily percentage change (%)", ylab = "Frequency")
}


# ---- 8. Correlation Between Currency Series ----

# plot_corr(): draws a correlation matrix as a colour map with the correlation
# value written in each cell (red = positive, blue = negative, white = zero).
plot_corr <- function(m, title) {
  n <- nrow(m)
  image(1:n, 1:n, t(m[n:1, ]), zlim = c(-1, 1),
        col = colorRampPalette(c("#3B4CC0", "white", "#B40426"))(100),
        axes = FALSE, xlab = "", ylab = "", main = title)
  axis(1, at = 1:n, labels = colnames(m))
  axis(2, at = 1:n, labels = rev(rownames(m)), las = 1)
  for (i in 1:n) for (j in 1:n) text(j, n - i + 1, sprintf("%.2f", m[i, j]))
  box()
}

# Correlation of the levels and of the daily changes.
level_corr  <- cor(df[currency_cols])
return_corr <- cor(returns[currency_cols])

# Draw the two correlation maps side by side, then reset the layout.
par(mfrow = c(1, 2))
plot_corr(level_corr,  "Correlation: Exchange-Rate Levels")
plot_corr(return_corr, "Correlation: Daily Percentage Changes")
par(mfrow = c(1, 1))

# Print the same correlations as numbers.
cat("Level correlations:\n")
print(round(level_corr, 3))
cat("\nReturn correlations:\n")
print(round(return_corr, 3))


# ---- 9. Rolling Volatility ----
# Standard deviation of the daily changes over a moving window of 20
# observations (about one month). A value is reported once at least 15
# observations are available. Peaks show periods of high volatility.

rolling_volatility <- data.frame(
  date = returns$date,
  lapply(returns[currency_cols], rolling_sd, window = 20, min_periods = 15)
)

for (currency in currency_cols) {
  plot(rolling_volatility$date, rolling_volatility[[currency]], type = "l", lwd = 1,
       main = paste0(currency, "/LKR 20-Observation Rolling Volatility"),
       xlab = "Date", ylab = "Rolling standard deviation of daily change (%)")
  grid()
}


# ---- 10. Stationarity Check: Augmented Dickey-Fuller (ADF) Test ----
# ADF null hypothesis: the series has a unit root (is non-stationary).
# A large p-value means the series is non-stationary; a p-value near 0 means
# it is stationary. The test is applied to the levels and to the daily changes.

# List of the six series to test, named "<currency>_level" and
# "<currency>_daily_change_pct".
adf_inputs <- c(
  setNames(lapply(currency_cols, function(cur) df[[cur]]),
           paste0(currency_cols, "_level")),
  setNames(lapply(currency_cols, function(cur) returns[[cur]]),
           paste0(currency_cols, "_daily_change_pct"))
)

# Run the ADF test on each series and collect statistic, p-value, lags used,
# number of observations and the 5% critical value in one table.
adf_results <- do.call(rbind, lapply(names(adf_inputs), function(nm) {
  r <- adf_test(adf_inputs[[nm]])
  data.frame(series = nm,
             `ADF statistic` = r$stat,
             `p-value` = r$p,
             `used lags` = r$lags,
             observations = r$nobs,
             `5% critical value` = adf_crit5(r$nobs),
             check.names = FALSE)
}))

# Round all numeric columns for display.
adf_results[-1] <- round(adf_results[-1], 4)
print(adf_results, row.names = FALSE)


# ---- 11. Autocorrelation of Daily Percentage Changes ----
# The autocorrelation function (ACF) shows how strongly each day's change is
# related to the changes 1, 2, ..., 30 days earlier. Bars outside the dashed
# band are statistically significant.

for (currency in currency_cols) {
  plot_acf_r(returns[[currency]], lags = 30,
             main = paste0("Autocorrelation of ", currency, "/LKR Daily Changes"))
}


# ---- 12. Reporting-Date Gaps ----
# The data contain reporting days only (no weekends or holidays), so the gap
# between consecutive observations is sometimes more than one calendar day.
# This section measures those gaps.

# Number of calendar days between consecutive observations.
date_gaps <- as.numeric(diff(sort(df$date)))
cat("Gap in days between consecutive available observations:\n")
print(round(describe_cols(data.frame(days = date_gaps)), 2))

# How often each gap length occurs (1 = weekdays, 3 = weekends, etc.).
cat("\nMost common gaps (days):\n")
gap_counts <- sort(table(date_gaps), decreasing = TRUE)
gap_table <- data.frame(gap_days = names(gap_counts), count = as.integer(gap_counts))
print(head(gap_table, 10), row.names = FALSE)


# ---- 13. Additional Diagnostics: Log Returns ----
# From here the analysis uses LOG returns, 100 * (ln P_t - ln P_{t-1}), which
# are the standard input for ARIMA and GARCH models.

BREAK <- as.Date("2022-03-08")   # LKR float

# Log returns in percent (computed from the df loaded earlier)
log_ret <- data.frame(date = df$date[-1],
                      lapply(df[currency_cols], function(x) diff(log(x)) * 100))

# Check the size of the log-return table (rows, number of currency columns).
cat(nrow(log_ret), ncol(log_ret[currency_cols]), "\n")
print(head(log_ret))

# Does the lag-3 spike in squared returns survive without 2022?
# acf3(): autocorrelation at lag 3 (element 4 of the ACF output, because the
# first element is lag 0).
acf3 <- function(x) as.numeric(acf(x, lag.max = 3, plot = FALSE)$acf)[4]

# The same data with every observation from 2022 removed.
ex2022 <- log_ret[format(log_ret$date, "%Y") != "2022", ]

# Compare the lag-3 autocorrelation of squared returns and of returns for the
# full sample and for the sample without 2022. A large drop means the spike
# was mostly caused by the 2022 shock.
lag3_check <- do.call(rbind, lapply(currency_cols, function(cur) {
  data.frame(
    series = cur,
    `lag-3 ACF sq. returns (full)`        = acf3(log_ret[[cur]]^2),
    `lag-3 ACF sq. returns (excl. 2022)`  = acf3(ex2022[[cur]]^2),
    `lag-3 ACF returns (full)`            = acf3(log_ret[[cur]]),
    `lag-3 ACF returns (excl. 2022)`      = acf3(ex2022[[cur]]),
    check.names = FALSE)
}))
lag3_check[-1] <- round(lag3_check[-1], 3)
print(lag3_check, row.names = FALSE)


# ---- 14. Stationarity Tests: ADF and KPSS ----
# Two tests with opposite null hypotheses give stronger evidence:
#   ADF  : H0 = non-stationary (unit root)   -> small p-value = stationary
#   KPSS : H0 = stationary                   -> small p-value = non-stationary
# Both are applied to the log levels and to the log returns of each currency.
# (KPSS p-values are limited to the range 0.01 - 0.10 by the test's tables.)

rows <- list()
for (cur in currency_cols) {
  series_list <- list(`log level` = log(df[[cur]]), `log return` = log_ret[[cur]])
  for (nm in names(series_list)) {
    s <- series_list[[nm]]
    rows[[length(rows) + 1]] <- data.frame(
      series = cur, transform = nm,
      `ADF p` = adf_test(s)$p, `KPSS p` = kpss_p(s),
      check.names = FALSE)
  }
}
stationarity <- do.call(rbind, rows)
stationarity[-(1:2)] <- round(stationarity[-(1:2)], 4)
print(stationarity, row.names = FALSE)


# ---- 15. Volatility Clustering and Normality Tests ----
# ARCH-LM test (10 lags): H0 = no ARCH effects. A p-value near 0 means that
#   volatility clustering is present, which supports GARCH-type models.
# Jarque-Bera test: H0 = the data are normally distributed. A p-value near 0
#   means normality is rejected. Skewness and kurtosis are also reported.

rows <- list()
for (cur in currency_cols) {
  s  <- log_ret[[cur]]
  al <- arch_lm(s, nlags = 10)
  jb <- jb_test(s)
  rows[[length(rows) + 1]] <- data.frame(
    series = cur, `ARCH-LM p` = unname(al["p"]), `JB p` = jb$p,
    skew = jb$skew, kurtosis = jb$kurt, check.names = FALSE)
}
vol_norm <- do.call(rbind, rows)
vol_norm[-1] <- round(vol_norm[-1], 4)
print(vol_norm, row.names = FALSE)


# ---- 16. ACF and PACF of Returns and Squared Returns ----
# For each currency three plots are drawn side by side:
#   ACF of returns          : linear dependence on past returns
#   PACF of returns         : direct dependence at each lag
#   ACF of squared returns  : dependence in volatility (volatility clustering)

for (cur in currency_cols) {
  par(mfrow = c(1, 3))
  plot_acf_r(log_ret[[cur]],   lags = 30, main = paste(cur, "ACF of returns"))
  plot_pacf_r(log_ret[[cur]],  lags = 30, main = paste(cur, "PACF of returns"))
  plot_acf_r(log_ret[[cur]]^2, lags = 30, main = paste(cur, "ACF of squared returns"))
  par(mfrow = c(1, 1))
}


# ---- 17. Normality Check: QQ Plots ----
# A QQ plot compares the data with a normal distribution. Points on the red
# line mean normality; points curving away at both ends mean heavy tails.

par(mfrow = c(1, 3))
for (cur in currency_cols) {
  qqnorm(log_ret[[cur]], main = paste(cur, "QQ plot"))
  qqline(log_ret[[cur]], col = "red")
}
par(mfrow = c(1, 1))


# ---- 18. Structural Break: The 2022 Rupee Float ----
# The rupee was floated in March 2022. The log returns are split into three
# periods (before 2022, during 2022 and 2023-2025) and compared.

periods <- list(
  "2000-2021"     = log_ret[log_ret$date <= as.Date("2021-12-31"), ],
  "2022 (crisis)" = log_ret[log_ret$date >= as.Date("2022-01-01") &
                            log_ret$date <= as.Date("2022-12-31"), ],
  "2023-2025"     = log_ret[log_ret$date >= as.Date("2023-01-01"), ]
)

# For each period and currency: mean, standard deviation, minimum and maximum
# of the daily log return.
period_tables <- lapply(names(periods), function(p) {
  m <- t(sapply(periods[[p]][currency_cols], function(x)
    c(mean = mean(x), std = sd(x), min = min(x), max = max(x))))
  colnames(m) <- paste(p, colnames(m), sep = " | ")
  m
})
summary_tbl <- do.call(cbind, period_tables)
print(round(summary_tbl, 3))

# Plot each exchange rate with the float date marked by a red dashed line.
for (cur in currency_cols) {
  plot(df$date, df[[cur]], type = "l",
       main = paste0(cur, "/LKR with 2022 break"),
       xlab = "Date", ylab = cur)
  abline(v = BREAK, col = "red", lty = 2)
  legend("topleft", legend = "LKR float (Mar 2022)", col = "red", lty = 2, bty = "n")
}


# ---- 19. Unchanged Rates by Year ----
# Share of days in each year on which the exchange rate did not change at all
# (log return exactly 0). High shares for USD/LKR reflect a closely managed
# rate and repeated reporting of the same value.

# Year of each observation.
yr <- as.integer(format(log_ret$date, "%Y"))
# Share of zero returns for each currency and year.
zero_share <- sapply(currency_cols, function(cur)
  tapply(log_ret[[cur]] == 0, yr, mean))
print(round(zero_share, 3))
