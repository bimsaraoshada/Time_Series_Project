
options(width = 120)

PROJECT_DIR <- "C:/Users/User/Downloads/tuition-exchange-rate-forecasting-main (1)/tuition-exchange-rate-forecasting-main"


HELPERS_PATH <- ""


# ---- 02 - Exploratory Data Analysis (EDA) ----


# ---- 2. Import Libraries and Load the Processed Data ----

# Locate the project folder by walking up from the working directory
library(tseries)
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

project_root <- if (nzchar(PROJECT_DIR)) normalizePath(PROJECT_DIR, winslash = "/", mustWork = FALSE) else find_root
("data/processed/exchange_rates_lkr.csv")

if (is.null(project_root) || !file.exists(file.path(project_root, "data", "processed", "exchange_rates_lkr.csv"))) {
  stop("Could not find data/processed/exchange_rates_lkr.csv. ",
       "Run 01_data_preparation.R first and check the project folder.")
}

data_path <- file.path(project_root, "data", "processed", "exchange_rates_lkr.csv")
df <- read.csv(data_path, stringsAsFactors = FALSE)
df$date <- as.Date(df$date)

START_DATE <- as.Date("2000-01-01")
END_DATE   <- as.Date("2025-12-31")

df <- df[df$date >= START_DATE & df$date <= END_DATE, ]
df <- df[order(df$date), ]
rownames(df) <- NULL

currency_cols <- c("USD", "GBP", "EUR")
required_cols <- c("date", currency_cols)

missing_cols <- setdiff(required_cols, names(df))
if (length(missing_cols) > 0) {
  stop("Required columns missing from processed data: ",
       paste(missing_cols, collapse = ", "))
}

cat("Project folder:", project_root, "\n")
cat("Data file:", data_path, "\n")
cat("Dataset shape:", nrow(df), "x", ncol(df), "\n")
cat("Study period:", format(min(df$date)), "to", format(max(df$date)), "\n")
print(head(df))


# Load the helper functions (pandas/statsmodels-style calculations)
helper_path <- HELPERS_PATH
if (!nzchar(helper_path)) {
  found <- c(list.files(project_root, pattern = "^helpers\\.R$", recursive = TRUE, full.names = TRUE),
             list.files(getwd(), pattern = "^helpers\\.R$", full.names = TRUE))
  helper_path <- if (length(found)) found[1] else ""
}
if (!nzchar(helper_path) || !file.exists(helper_path)) {
  stop("helpers.R not found. Save it in the project folder or set HELPERS_PATH at the top of this script.")
}
source(helper_path)


# ---- 3. Dataset Structure and Data Quality ----

cat("Data types:\n")
print(data.frame(dtype = sapply(df, function(x) class(x)[1])))

quality_summary <- data.frame(missing_values = colSums(is.na(df[required_cols])))
cat("\nMissing values:\n")
print(quality_summary)

cat("\nDuplicate dates:", sum(duplicated(df$date)), "\n")
cat("Non-positive exchange-rate values:\n")
print(data.frame(count = colSums(df[currency_cols] <= 0)))

cat("\nRows:", nrow(df), "\n")
cat("Unique dates:", length(unique(df$date)), "\n")
cat("Date range:", format(min(df$date)), "to", format(max(df$date)), "\n")

stopifnot("Missing dates found." = !anyNA(df$date))
stopifnot("Duplicate dates found." = !anyDuplicated(df$date))
stopifnot("Missing exchange rates found." = !anyNA(df[currency_cols]))
stopifnot("Non-positive exchange rates found." = all(df[currency_cols] > 0))


# ---- 4. Descriptive Statistics of Exchange-Rate Levels ----

level_summary <- describe_cols(df[currency_cols])
level_summary <- cbind(level_summary,
                       range = level_summary[, "max"] - level_summary[, "min"])
print(round(level_summary, 4))


# ---- 5. Exchange-Rate Trends Over Time ----

for (currency in currency_cols) {
  plot(df$date, df[[currency]], type = "l", lwd = 1,
       main = paste0(currency, "/LKR Exchange Rate Over Time"),
       xlab = "Date", ylab = "LKR per unit of foreign currency")
  grid()
}


# ---- 6. Daily Percentage Changes (Returns) ----

pct_change <- function(x) c(NA, diff(x) / head(x, -1) * 100)

returns <- data.frame(date = df$date, lapply(df[currency_cols], pct_change))
returns <- returns[-1, ]            # first day has no previous observation
rownames(returns) <- NULL

return_summary <- cbind(
  describe_cols(returns[currency_cols]),
  skewness = sapply(returns[currency_cols], pd_skew),
  kurtosis = sapply(returns[currency_cols], pd_kurt)
)
print(round(return_summary, 4))

for (currency in currency_cols) {
  plot(returns$date, returns[[currency]], type = "l", lwd = 0.7,
       main = paste0(currency, "/LKR Daily Percentage Changes"),
       xlab = "Date", ylab = "Change (%)")
  abline(h = 0)
  grid()
}


# ---- 7. Distribution of Daily Percentage Changes ----

for (currency in currency_cols) {
  values <- returns[[currency]]
  values <- values[!is.na(values)]
  hist(values, breaks = seq(min(values), max(values), length.out = 51),
       col = "blue", border = "black",
       main = paste0("Distribution of ", currency, "/LKR Daily Changes"),
       xlab = "Daily percentage change (%)", ylab = "Frequency")
}


# ---- 8. Correlation Between Currency Series ----

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

level_corr  <- cor(df[currency_cols])
return_corr <- cor(returns[currency_cols])

par(mfrow = c(1, 2))
plot_corr(level_corr,  "Correlation: Exchange-Rate Levels")
plot_corr(return_corr, "Correlation: Daily Percentage Changes")
par(mfrow = c(1, 1))

cat("Level correlations:\n")
print(round(level_corr, 3))
cat("\nReturn correlations:\n")
print(round(return_corr, 3))


# ---- 9. Rolling Volatility ----

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

adf_inputs <- c(
  setNames(lapply(currency_cols, function(cur) df[[cur]]),
           paste0(currency_cols, "_level")),
  setNames(lapply(currency_cols, function(cur) returns[[cur]]),
           paste0(currency_cols, "_daily_change_pct"))
)

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

adf_results[-1] <- round(adf_results[-1], 4)
print(adf_results, row.names = FALSE)


# ---- 11. Autocorrelation of Daily Percentage Changes ----

for (currency in currency_cols) {
  plot_acf_r(returns[[currency]], lags = 30,
             main = paste0("Autocorrelation of ", currency, "/LKR Daily Changes"))
}


# ---- 12. Reporting-Date Gaps ----

date_gaps <- as.numeric(diff(sort(df$date)))
cat("Gap in days between consecutive available observations:\n")
print(round(describe_cols(data.frame(days = date_gaps)), 2))

cat("\nMost common gaps (days):\n")
gap_counts <- sort(table(date_gaps), decreasing = TRUE)
gap_table <- data.frame(gap_days = names(gap_counts), count = as.integer(gap_counts))
print(head(gap_table, 10), row.names = FALSE)


# ---- 13. Additional Diagnostics: Log Returns ----

BREAK <- as.Date("2022-03-08")   # LKR float

# Log returns in percent (computed from the df loaded earlier)
log_ret <- data.frame(date = df$date[-1],
                      lapply(df[currency_cols], function(x) diff(log(x)) * 100))

cat(nrow(log_ret), ncol(log_ret[currency_cols]), "\n")
print(head(log_ret))

# Does the lag-3 spike in squared returns survive without 2022?
acf3 <- function(x) as.numeric(acf(x, lag.max = 3, plot = FALSE)$acf)[4]

ex2022 <- log_ret[format(log_ret$date, "%Y") != "2022", ]

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

for (cur in currency_cols) {
  par(mfrow = c(1, 3))
  plot_acf_r(log_ret[[cur]],   lags = 30, main = paste(cur, "ACF of returns"))
  plot_pacf_r(log_ret[[cur]],  lags = 30, main = paste(cur, "PACF of returns"))
  plot_acf_r(log_ret[[cur]]^2, lags = 30, main = paste(cur, "ACF of squared returns"))
  par(mfrow = c(1, 1))
}


# ---- 17. Normality Check: QQ Plots ----

par(mfrow = c(1, 3))
for (cur in currency_cols) {
  qqnorm(log_ret[[cur]], main = paste(cur, "QQ plot"))
  qqline(log_ret[[cur]], col = "red")
}
par(mfrow = c(1, 1))


# ---- 18. Structural Break: The 2022 Rupee Float ----

periods <- list(
  "2000-2021"     = log_ret[log_ret$date <= as.Date("2021-12-31"), ],
  "2022 (crisis)" = log_ret[log_ret$date >= as.Date("2022-01-01") &
                            log_ret$date <= as.Date("2022-12-31"), ],
  "2023-2025"     = log_ret[log_ret$date >= as.Date("2023-01-01"), ]
)

period_tables <- lapply(names(periods), function(p) {
  m <- t(sapply(periods[[p]][currency_cols], function(x)
    c(mean = mean(x), std = sd(x), min = min(x), max = max(x))))
  colnames(m) <- paste(p, colnames(m), sep = " | ")
  m
})
summary_tbl <- do.call(cbind, period_tables)
print(round(summary_tbl, 3))

for (cur in currency_cols) {
  plot(df$date, df[[cur]], type = "l",
       main = paste0(cur, "/LKR with 2022 break"),
       xlab = "Date", ylab = cur)
  abline(v = BREAK, col = "red", lty = 2)
  legend("topleft", legend = "LKR float (Mar 2022)", col = "red", lty = 2, bty = "n")
}


# ---- 19. Unchanged Rates by Year ----

yr <- as.integer(format(log_ret$date, "%Y"))
zero_share <- sapply(currency_cols, function(cur)
  tapply(log_ret[[cur]] == 0, yr, mean))
print(round(zero_share, 3))
