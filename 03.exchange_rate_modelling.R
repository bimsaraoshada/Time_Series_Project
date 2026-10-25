# =============================================================================
# EXCHANGE-RATE FORECASTING FOR TUITION PLANNING
# CM3023 - Financial Time Series Analysis
# =============================================================================
#
# PURPOSE
#   Forecast the USD/LKR, GBP/LKR and EUR/LKR exchange rates 5 and 20 reporting
#   days ahead, compare statistical and machine-learning models against a naive
#   benchmark, model volatility, build 95% prediction intervals and convert the
#   results into illustrative tuition costs in Sri Lankan rupees (LKR).
#
# INPUT
#   exchange_rates_lkr.csv  (columns: date, USD, GBP, EUR; rupees per unit of
#   foreign currency; produced in the data-preparation stage from FRED series).
#
# OUTPUT
#   A new folder results/run_<date_time>/ containing Excel tables (.xlsx),
#   PNG figures and saved model objects. Every table is also printed in the
#   R Console, and every plot is also drawn in the RStudio Plots pane.
#
# MAIN STEPS (section numbers match the report chapters)
#   1.  Install and load packages
#   2.  Settings (periods, horizons, fees, output folder, helper functions)
#   3.  Import and check the data
#   4.  Training-period diagnostics (stationarity, ACF/PACF)
#   4B. ARIMA candidate comparison (AIC, BIC, Ljung-Box) and residual checks
#   4C. Seasonality check (STL decomposition)
#   4D. One-shot forecast demonstration (why walk-forward is needed)
#   5.  Candidate models, feature construction and forecasting functions
#   6.  Walk-forward validation (2022-2023) and model selection
#   7.  Final test (2024-2025) of the frozen, validation-selected models
#   8.  Volatility models (EWMA and GARCH) and their evaluation
#   9.  GARCH prediction intervals and their coverage
#   10. Tuition-cost forecasts and scenarios as of 31 December 2025
#   11. Final residual diagnostics and reproducibility information
#
# HOW TO RUN
#   Put this script and exchange_rates_lkr.csv in one RStudio project folder,
#   then run the sections in order (select a section and press Ctrl+Enter).
#   Section 6 is slow because every model is refitted at every forecast origin.
#
# KEY DESIGN RULES
#   * Data are split chronologically (training / validation / test); there is
#     no random shuffling.
#   * Every forecast uses only information available at its forecast origin.
#   * Model settings are chosen on the validation period only and are then
#     frozen before the test period is used.
#   * Genuine extreme observations (the 2022 rupee float) are never removed.
# =============================================================================

# 1. INSTALL AND LOAD PACKAGES -----------------------------------------------
# Packages used:
#   readr, dplyr, ggplot2 : reading data, data manipulation, plotting
#   forecast              : ARIMA, ETS (exponential smoothing), auto.arima
#   tseries               : ADF and KPSS stationarity tests
#   zoo                   : rolling-window calculations for the features
#   ranger, xgboost       : Random Forest and XGBoost models
#   rugarch               : GARCH volatility models
#   vars                  : vector autoregression (VAR)
#   writexl               : saving tables as Excel files
packages <- c("readr", "dplyr", "ggplot2", "forecast", "tseries",
              "zoo", "ranger", "xgboost", "rugarch", "vars", "writexl")

# Find the packages that are not yet installed on this computer.
missing_packages <- packages[!vapply(packages, requireNamespace,
                                   logical(1), quietly = TRUE)]

# Install only the missing packages (needs an internet connection).
if (length(missing_packages)) {
  install.packages(missing_packages, repos = "https://cloud.r-project.org")
}

# Load every package into the current R session.
invisible(lapply(packages, function(p) library(p, character.only = TRUE)))

# Fix the random seed so that random steps (Random Forest, XGBoost, simulation)
# give the same results each time the script is run.
set.seed(2026)
options(width = 200)   # wider Console so tables are not wrapped

# 2. SETTINGS: CHANGE THESE BEFORE STARTING ---------------------------------
# Name of the processed data file (must be in the project folder).
DATA_FILE <- "exchange_rates_lkr.csv"
# If the file is elsewhere, use forward slashes, for example:
# DATA_FILE <- "C:/Users/ASUS/Downloads/exchange_rates_lkr.csv"

# QUICK_RUN = TRUE : USD only, forecast every 20 observations (fast rehearsal,
#                    the VAR and pooled extensions are skipped automatically).
# QUICK_RUN = FALSE: all three currencies, every 5 observations (final results).
QUICK_RUN <- FALSE
CURRENCIES <- if (QUICK_RUN) "USD" else c("USD", "GBP", "EUR")   # currencies analysed
ORIGIN_STEP <- if (QUICK_RUN) 20L else 5L   # distance (in reporting days) between forecast origins

RUN_VAR <- TRUE      # multivariate VAR on the three return series (extension)
RUN_POOLED <- TRUE   # pooled RF / XGBoost with one-hot currency columns (extension)

# Forecast horizons in reporting days (about one week and one month).
HORIZONS <- c(5L, 20L)

# Chronological split of the data:
#   training   : start of data to TRAIN_END   (initial model fitting, diagnostics)
#   validation : VAL_START to VAL_END         (choosing model settings; includes the 2022 float)
#   test       : TEST_START to TEST_END       (final evaluation, never used for choices)
TRAIN_END <- as.Date("2021-12-31")
VAL_START <- as.Date("2022-01-01")
VAL_END <- as.Date("2023-12-31")
TEST_START <- as.Date("2024-01-01")
TEST_END <- as.Date("2025-12-31")

RECENT_N <- 1250L  # approximately five reporting years
                   # (length of the "recent" estimation window)
SIMULATIONS <- 2000L   # number of simulated GARCH return paths per interval

# Clearly hypothetical fees; replace with your specified fees if needed.
# Each value is the tuition fee in the foreign currency (USD, GBP or EUR).
TUITION <- c(USD = 5000, GBP = 5000, EUR = 5000)

# Safety checks on the settings: origin step must be at least 1 and every
# fee must be positive.
stopifnot(ORIGIN_STEP >= 1, all(TUITION > 0))

# A new output folder each run avoids overwriting previous results.
OUT <- file.path("results", paste0("run_", format(Sys.time(), "%Y%m%d_%H%M%S")))
dir.create(OUT, recursive = TRUE, showWarnings = FALSE)

##Saving tables
# save_table(): saves a table as an Excel file in the output folder and also
# prints it in the Console. Long tables (more than max_print rows) show only
# the first 10 rows in the Console; the full table is always in the Excel file.
save_table <- function(x, filename, max_print = 60) {
  x <- as.data.frame(x)
  # An empty table (for example when no model failed) is replaced by a note.
  if (ncol(x) == 0L) x <- data.frame(note = "no rows")
  # Make sure the file extension is .xlsx.
  file <- sub("\\.csv$", ".xlsx", filename)
  writexl::write_xlsx(x, file.path(OUT, file))
  cat("\n===== ", file, " (", nrow(x), " rows) =====\n", sep = "")
  # Round numbers to 4 decimals for display only (the Excel file is unrounded).
  num <- vapply(x, is.numeric, logical(1))
  xr <- x
  xr[num] <- lapply(xr[num], function(v) round(v, 4))
  if (nrow(xr) <= max_print) {
    print(xr, row.names = FALSE)
  } else {
    print(utils::head(xr, 10), row.names = FALSE)
    cat("... ", nrow(xr) - 10, " more rows are in the Excel file\n", sep = "")
  }
  invisible(x)
}
# show_and_save: draws a base-R plot in the Plots pane, then saves a PNG copy.
show_and_save <- function(filename, plot_fun, width = 1400, height = 1000, res = 140) {
  plot_fun()                                   # draw the plot on screen
  # Copy the plot on screen into a PNG file in the output folder.
  grDevices::dev.copy(grDevices::png, filename = file.path(OUT, filename),
                      width = width, height = height, res = res)
  invisible(grDevices::dev.off())              # close the PNG file
  par(mfrow = c(1, 1))                         # reset the plotting layout
}
# show_gg: prints a ggplot in the Plots pane, then saves a PNG copy.
show_gg <- function(p, filename, width = 13, height = 7) {
  print(p)                                     # draw the ggplot on screen
  ggplot2::ggsave(file.path(OUT, filename), p, width = width, height = height)
}

# Save the main settings of this run so that the results can be reproduced.
saveRDS(list(currencies = CURRENCIES, horizons = HORIZONS, step = ORIGIN_STEP,
             recent_n = RECENT_N, seed = 2026, tuition = TUITION,
             run_var = RUN_VAR, run_pooled = RUN_POOLED),
        file.path(OUT, "settings.rds"))

# 3. IMPORT AND CHECK THE DATA ----------------------------------------------
# Read the processed exchange-rate file (rupees per unit of foreign currency).
rates <- readr::read_csv(DATA_FILE, show_col_types = FALSE)

# Data-quality checks: if any check fails, the script stops with a message.
stopifnot(all(c("date", "USD", "GBP", "EUR") %in% names(rates)))   # required columns exist
rates$date <- as.Date(rates$date)                                  # convert text to dates
rates <- rates[order(rates$date), c("date", "USD", "GBP", "EUR")]  # sort by date, keep needed columns
stopifnot(!anyNA(rates), !anyDuplicated(rates$date))               # no missing values or duplicate dates
stopifnot(all(vapply(rates[c("USD", "GBP", "EUR")], is.numeric, logical(1))))   # rates are numeric
stopifnot(all(is.finite(as.matrix(rates[-1]))), all(as.matrix(rates[-1]) > 0))  # finite and positive
stopifnot(min(rates$date) < TRAIN_END, max(rates$date) >= TEST_END)             # data cover all periods
# Deliberately keep genuine 2022 observations. Never cap large returns here.

# Look at the first and last rows of the data.
print(head(rates))
print(tail(rates))

# Count the observations in the training, validation and test periods.
split_counts <- data.frame(
  period = c("Initial training", "Validation", "Final testing"),
  n = c(sum(rates$date <= TRAIN_END),
        sum(rates$date >= VAL_START & rates$date <= VAL_END),
        sum(rates$date >= TEST_START & rates$date <= TEST_END)))
save_table(split_counts, "01_split_counts.xlsx")

# 4. TRAINING-ONLY DIAGNOSTICS: ACF / PACF -----------------------------------
# All diagnostics in this section use the TRAINING period only, so that no
# information from the validation or test period influences model design.

# sig_lags(): lists the lags whose autocorrelation lies outside the 95%
# confidence band (+/- 1.96 / sqrt(n)). These lags suggest MA (ACF) or
# AR (PACF) terms and are used only as guidance.
sig_lags <- function(v, n) {
  idx <- which(abs(v) > 1.96 / sqrt(n))
  if (length(idx)) paste(idx, collapse = ",") else "none"
}
diagnostics <- list()
for (cc in CURRENCIES) {
  x <- rates[[cc]][rates$date <= TRAIN_END]   # training-period rates
  z <- log(x)                                 # log exchange rate
  r <- 100 * diff(z)                          # daily percentage log returns
  # Autocorrelation (ACF) and partial autocorrelation (PACF) of the returns.
  acf_vals <- as.numeric(acf(r, lag.max = 10, plot = FALSE)$acf)[-1]
  pacf_vals <- as.numeric(pacf(r, lag.max = 10, plot = FALSE)$acf)
  diagnostics[[cc]] <- data.frame(
    currency = cc,
    # ADF test: H0 = unit root (non-stationary). Large p-value = not stationary.
    adf_log_level_p = suppressWarnings(tseries::adf.test(z)$p.value),
    # KPSS test: H0 = stationary. Small p-value = not stationary.
    kpss_log_level_p = suppressWarnings(tseries::kpss.test(z, null = "Level")$p.value),
    # The same two tests on returns (expected to indicate stationarity).
    adf_return_p = suppressWarnings(tseries::adf.test(r)$p.value),
    kpss_return_p = suppressWarnings(tseries::kpss.test(r, null = "Level")$p.value),
    # Number of differences recommended by the KPSS test (d in ARIMA).
    recommended_d_kpss = forecast::ndiffs(z, test = "kpss", max.d = 2),
    return_acf_sig_lags_1to10 = sig_lags(acf_vals, length(r)),
    return_pacf_sig_lags_1to10 = sig_lags(pacf_vals, length(r)))
  # Diagnostic plots: log rate, returns, ACF/PACF and ACF of squared returns
  # (slowly decaying ACF = non-stationary; ACF of squared returns shows
  # volatility clustering).
  show_and_save(paste0("02_", cc, "_training_diagnostics.png"), function() {
    par(mfrow = c(2, 3))
    plot(z, type = "l", main = paste(cc, "training log rate"),
         xlab = "Observation", ylab = "Log rate")
    plot(r, type = "l", main = "Training log returns (%)",
         xlab = "Observation", ylab = "%")
    acf(z, lag.max = 60, main = "ACF of log rate (slow decay = non-stationary)")
    acf(r, main = "ACF of log returns")
    pacf(r, main = "PACF of log returns")
    acf(r^2, main = "ACF of squared returns (volatility clustering)")
  }, width = 1800, height = 1000)
}
diagnostics <- dplyr::bind_rows(diagnostics)
save_table(diagnostics, "02_training_stationarity.xlsx")

# The candidate ARIMA family below uses d=1 (log rates differenced once).
# Review this before modelling. Stop if the training-only check disagrees.
if (any(diagnostics$recommended_d_kpss != 1)) {
  stop("Training KPSS does not recommend d=1 for every currency. Review 02_training_stationarity.xlsx and the plots before changing the ARIMA family.")
}

# 4B. ARIMA ORDER COMPARISON ON TRAINING DATA (AIC, BIC, LJUNG-BOX) -----------
# Same idea as the lecture code: fit several (p,d,q), compare AIC/BIC (smaller is
# better), then check whether the residuals look like white noise.
# AIC/BIC are only comparable between models with the same d on the same data.
# Ljung-Box H0: residuals are white noise. p > 0.05 supports white noise. With
# thousands of heavy-tailed observations a rejection is common; interpret with care.
# R2_returns_insample = share of the variance of daily log returns explained by the
# model (in-sample). Values near 0 mean returns are almost unpredictable.
# This table is DESCRIPTIVE. The model actually used is chosen on validation RMSE.

# Candidate ARIMA(p,1,q) models: p = autoregressive order, q = moving-average
# order, d = 1 (one difference), drift = include a constant trend or not.
arima_specs <- data.frame(
  model = c("ARIMA_010", "ARIMA_010_drift", "ARIMA_110", "ARIMA_011", "ARIMA_111"),
  p = c(0, 0, 1, 0, 1),
  q = c(0, 0, 0, 1, 1),
  drift = c(FALSE, TRUE, FALSE, FALSE, FALSE))
arima_ic <- list()     # will store the AIC/BIC/Ljung-Box results
fits_keep <- list()    # will store the fitted models for the residual checks
for (cc in CURRENCIES) {
  z <- log(rates[[cc]][rates$date <= TRAIN_END])   # training log rates
  r_ret <- diff(z)                                 # training log returns
  for (k in seq_len(nrow(arima_specs))) {
    sp <- arima_specs[k, ]
    # Fit the ARIMA model by maximum likelihood; skip it if the fit fails.
    fit <- tryCatch(
      forecast::Arima(z, order = c(sp$p, 1L, sp$q),
                      include.drift = sp$drift, method = "ML"),
      error = function(e) NULL)
    if (is.null(fit)) next
    e <- as.numeric(residuals(fit))                # model residuals
    fits_keep[[paste(cc, sp$model)]] <- fit
    arima_ic[[length(arima_ic) + 1L]] <- data.frame(
      currency = cc, model = sp$model, p = sp$p, d = 1L, q = sp$q,
      drift = sp$drift, AIC = AIC(fit), BIC = BIC(fit),
      # Ljung-Box test at lag 10 (degrees of freedom reduced by p + q).
      ljung_box_p = Box.test(e, lag = 10, type = "Ljung-Box",
                             fitdf = sp$p + sp$q)$p.value,
      # In-sample share of return variance explained by the model.
      R2_returns_insample = 1 - sum(e[-1]^2) / sum((r_ret - mean(r_ret))^2))
  }
}
# Combine the results and flag the lowest AIC and lowest BIC for each currency.
arima_ic <- dplyr::bind_rows(arima_ic) %>%
  dplyr::group_by(currency) %>%
  dplyr::mutate(best_AIC = AIC == min(AIC), best_BIC = BIC == min(BIC)) %>%
  dplyr::ungroup()
save_table(arima_ic, "02b_arima_aic_bic_ljungbox.xlsx")

# plot(res), acf(res), checkresiduals() and the Ljung-Box test.
# Residual checks for the lowest-AIC candidate of each currency.
for (cc in CURRENCIES) {
  # Pick the candidate with the lowest AIC for this currency.
  best <- arima_ic %>% dplyr::filter(currency == cc) %>%
    dplyr::arrange(AIC) %>% dplyr::slice(1)
  fit <- fits_keep[[paste(cc, best$model)]]
  res <- residuals(fit)
  cat("\nResidual checks for ", cc, " using ", best$model,
      " (lowest training AIC)\n", sep = "")
  # Residual plot and residual ACF.
  show_and_save(paste0("02b_", cc, "_", best$model, "_residuals.png"), function() {
    par(mfrow = c(1, 2))
    plot(res, type = "l", main = paste(cc, best$model, "residuals"), ylab = "Residual")
    acf(res, main = paste(cc, "ACF of residuals"))
  }, width = 1600, height = 700)
  # checkresiduals(): residual plot, ACF, histogram and Ljung-Box test together.
  show_and_save(paste0("02b_", cc, "_", best$model, "_checkresiduals.png"),
                function() forecast::checkresiduals(fit, lag = 10),
                width = 1400, height = 1000)
}

# auto.arima cross-check (with trace = TRUE). It searches
# p and q with d = 1 on the log training series and prints every model it tries.
# If in_candidate_list is FALSE, consider adding that order to arima_specs above.
auto_rows <- list()
for (cc in CURRENCIES) {
  z <- log(rates[[cc]][rates$date <= TRAIN_END])
  cat("\nauto.arima trace for ", cc, " (log rate, training period)\n", sep = "")
  # Automatic search over (p,d,q) with d fixed at 1 and no seasonal terms.
  auto_fit <- forecast::auto.arima(z, d = 1, seasonal = FALSE, trace = TRUE)
  ao <- forecast::arimaorder(auto_fit)                 # selected (p, d, q)
  has_drift <- "drift" %in% names(coef(auto_fit))      # was a drift term chosen?
  auto_rows[[cc]] <- data.frame(
    currency = cc, auto_p = ao[["p"]], auto_d = ao[["d"]], auto_q = ao[["q"]],
    auto_drift = has_drift, AIC = AIC(auto_fit),
    # TRUE if the automatic choice is one of our five candidate models.
    in_candidate_list = any(arima_specs$p == ao[["p"]] & arima_specs$q == ao[["q"]] &
                              arima_specs$drift == has_drift))
}
save_table(dplyr::bind_rows(auto_rows), "02b_auto_arima_crosscheck.xlsx")

# 4C. SEASONAL DECOMPOSITION CHECK -------------------------------------------
# Monthly averages of the log rate (training period only) are decomposed into
# trend + seasonal + remainder. Seasonal strength F_s is between 0 and 1:
#   F_s = max(0, 1 - Var(remainder) / Var(seasonal + remainder))
# A value near 0 means no meaningful seasonality, so SARIMA / Holt-Winters /
# SARIMAX are not needed. STL is robust; decompose() is the classical method.
seasonality_rows <- list()
for (cc in CURRENCIES) {
  d <- rates[rates$date <= TRAIN_END, c("date", cc)]
  ym <- format(d$date, "%Y-%m")                  # year-month label of each day
  m <- tapply(log(d[[cc]]), ym, mean)            # monthly mean of the log rate
  first_month <- as.Date(paste0(names(m)[1], "-01"))
  last_month <- as.Date(paste0(names(m)[length(m)], "-01"))
  # The monthly series must have no missing months for a ts object.
  if (length(seq(first_month, last_month, by = "month")) != length(m)) {
    warning(cc, ": monthly series has gaps; seasonality check skipped.")
    next
  }
  # Convert to a monthly time series (frequency = 12).
  tsm <- ts(as.numeric(m),
            start = c(as.integer(substr(names(m)[1], 1, 4)),
                      as.integer(substr(names(m)[1], 6, 7))),
            frequency = 12)
  dec <- stl(tsm, s.window = "periodic")         # STL decomposition
  comp <- dec$time.series                        # trend, seasonal and remainder parts
  # Seasonal strength measure defined above.
  fs <- max(0, 1 - var(comp[, "remainder"]) /
              var(comp[, "seasonal"] + comp[, "remainder"]))
  seasonality_rows[[cc]] <- data.frame(
    currency = cc, months = length(tsm), seasonal_strength = fs)
  # STL and classical decomposition plots.
  show_and_save(paste0("02c_", cc, "_stl_decomposition.png"), function() {
    plot(dec, main = paste(cc, "STL decomposition, monthly mean log rate"))
  }, width = 1200, height = 900, res = 130)
  show_and_save(paste0("02c_", cc, "_classical_decomposition.png"), function() {
    plot(decompose(tsm, type = "additive"))
  }, width = 1200, height = 900, res = 130)
}
save_table(dplyr::bind_rows(seasonality_rows), "02c_seasonality_check.xlsx")
# Report wording if values are near 0: "Seasonal strength was close to zero, so
# no seasonal model was fitted."

# 4D. LECTURE-STYLE DEMONSTRATION: auto.arima + forecast vs actual + accuracy() --
# Exactly the lecture workflow: fit on the training data, forecast the whole
# validation period in one go (h = length(test)), plot forecast against actual
# with a legend, and print accuracy(). A one-shot long forecast is a poor way to
# forecast exchange rates; the walk-forward evaluation in sections 6-7 is the
# proper test. This is for learning and for the report's method description.
lecture_rows <- list()
for (cc in CURRENCIES) {
  tr <- rates[[cc]][rates$date <= TRAIN_END]                             # training data
  te <- rates[[cc]][rates$date >= VAL_START & rates$date <= VAL_END]     # validation period
  cat("\nLecture-style auto.arima for ", cc, " (training data, lambda = 0)\n", sep = "")
  # lambda = 0 applies a log transformation inside the model.
  auto_model1 <- forecast::auto.arima(tr, lambda = 0, trace = TRUE)
  # Forecast the whole validation period at once (one long horizon).
  forecast_values <- forecast::forecast(auto_model1, h = length(te))
  cat("\nFirst forecast values for ", cc, ":\n", sep = "")
  print(utils::head(as.data.frame(forecast_values)))
  # Plot the forecast and its intervals against the actual values.
  show_and_save(paste0("02d_", cc, "_lecture_forecast_vs_actual.png"), function() {
    plot(forecast_values, include = 300,
         main = paste(cc, "forecasts vs actual values (lecture style)"),
         xlab = "Observation", ylab = "LKR per unit")
    lines(length(tr) + seq_along(te), te, lwd = 3)
    legend("topleft", legend = c("Forecast", "Actual"),
           lty = 1, lwd = c(2, 3), col = c("blue", "black"))
  }, width = 1400, height = 800)
  # accuracy(): error measures on the training set and on the validation actuals.
  acc <- forecast::accuracy(forecast_values, te)
  lecture_rows[[cc]] <- cbind(currency = cc, set = rownames(acc), as.data.frame(acc))
}
save_table(dplyr::bind_rows(lecture_rows), "02d_lecture_style_accuracy.xlsx")

# 5. HELPER FUNCTIONS AND CANDIDATES ----------------------------------------
# One-step and 20-step targets must both remain within each evaluation period.
# All models use the same origins. Last origins without a 20-step target are omitted.

# origins_for(): returns the row numbers of the forecast origins whose forecast
# targets fall inside the period [start, end], taking every ORIGIN_STEP-th one.
origins_for <- function(start, end) {
  n <- nrow(rates)
  ids <- seq_len(n - max(HORIZONS))
  ids <- ids[rates$date[ids + 1L] >= start &
               rates$date[ids + max(HORIZONS)] <= end]
  if (!length(ids)) stop("No forecast origins in requested period")
  ids[seq.int(1L, length(ids), by = ORIGIN_STEP)]
}
val_origins <- origins_for(VAL_START, VAL_END)     # origins used for validation
test_origins <- origins_for(TEST_START, TEST_END)  # origins used for the final test

# window_start(): first row used to fit a model at origin i.
#   "expanding" = all observations from the start of the sample;
#   "recent"    = only the latest RECENT_N observations.
window_start <- function(i, window) {
  if (window == "expanding") 1L else max(1L, i - RECENT_N + 1L)
}

# Candidate models. Families:
#   Naive, ARIMA, ETS (exponential smoothing), LM (linear regression), RF, XGB
#   VAR, RF_pooled, XGB_pooled  (extensions; need at least 2 currencies)
candidate_base <- dplyr::bind_rows(
  data.frame(arima_specs, family = "ARIMA"),
  # Random Forest: minimum node size 5 or 20.
  data.frame(model = c("RF_leaf5", "RF_leaf20"), family = "RF",
             leaf = c(5, 20), drift = FALSE),
  # XGBoost: maximum tree depth 2 or 3.
  data.frame(model = c("XGB_depth2", "XGB_depth3"), family = "XGB",
             depth = c(2, 3), drift = FALSE),
  # ETS is fitted to the log rate. "ANN" = simple exponential smoothing;
  # "ZZN" lets ETS choose error and trend (damped or not), with NO seasonality.
  data.frame(model = c("SES", "ETS_auto"), family = "ETS",
             ets_model = c("ANN", "ZZN"), drift = FALSE),
  # Linear regression on the same lag features as RF/XGB.
  # "lags" uses only ret_0 and lags 1-5; "all" uses every feature.
  data.frame(model = c("LM_all", "LM_lags"), family = "LM",
             feat = c("all", "lags"), drift = FALSE))

# Optional extensions: VAR (lag 1 and 2) and pooled Random Forest / XGBoost.
extensions <- list()
if (RUN_VAR && length(CURRENCIES) >= 2L) {
  extensions$var <- data.frame(model = c("VAR_p1", "VAR_p2"), family = "VAR",
                               p = c(1, 2), drift = FALSE)
}
if (RUN_POOLED && length(CURRENCIES) >= 2L) {
  extensions$pooled <- dplyr::bind_rows(
    data.frame(model = "POOL_RF_leaf20", family = "RF_pooled",
               leaf = 20, drift = FALSE),
    data.frame(model = "POOL_XGB_depth3", family = "XGB_pooled",
               depth = 3, drift = FALSE))
}
candidate_base <- dplyr::bind_rows(c(list(candidate_base), unname(extensions)))

# Every candidate is run with both estimation windows (expanding and recent),
# and each configuration gets a unique identifier such as ARIMA_111__recent.
configs <- merge(candidate_base, data.frame(window = c("expanding", "recent")))
configs$id <- paste(configs$model, configs$window, sep = "__")

# The naive benchmark (forecast = latest observed rate) has no window choice.
naive_config <- data.frame(model = "Naive", family = "Naive", drift = FALSE,
                           window = "expanding", id = "Naive")
configs <- dplyr::bind_rows(naive_config, configs)
save_table(configs, "03_candidate_settings.xlsx", max_print = Inf)
N_FAMILIES <- length(unique(configs$family))
message("Candidate configurations: ", nrow(configs), " in ", N_FAMILIES, " families.")

# Features use rates observed through origin t, with no future observations.
# Return-based inputs avoid dependence on absolute currency price scale.
# make_features(): builds the machine-learning predictors for one currency:
#   ret_0            current log return
#   ret_lag1..       lagged returns (lags 1-5, 10 and 20)
#   mean / sd        rolling mean and standard deviation of returns (5, 20, 60)
#   distance         log rate minus its rolling mean (5, 20, 60)
# Rolling windows end at the current observation (trailing, never centred).
make_features <- function(s) {
  z <- log(s)
  r <- c(NA_real_, diff(z))
  f <- data.frame(ret_0 = r)
  for (k in c(1L, 2L, 3L, 4L, 5L, 10L, 20L)) {
    f[[paste0("ret_lag", k)]] <- dplyr::lag(r, k)
  }
  for (w in c(5L, 20L, 60L)) {
    f[[paste0("mean", w)]] <- zoo::rollapplyr(r, w, mean, fill = NA)
    f[[paste0("sd", w)]] <- zoo::rollapplyr(r, w, sd, fill = NA)
    f[[paste0("distance", w)]] <- z - zoo::rollapplyr(z, w, mean, fill = NA)
  }
  f
}
# Build the feature table separately for each currency.
features <- lapply(CURRENCIES, function(cc) make_features(rates[[cc]]))
names(features) <- CURRENCIES

# One-hot currency columns (Currency_USD, Currency_GBP, Currency_EUR) for pooled models.
# onehot(): creates n rows of 0/1 indicator columns that identify the currency.
onehot <- function(cur, n) {
  m <- as.data.frame(matrix(0, nrow = n, ncol = length(CURRENCIES)))
  names(m) <- paste0("Currency_", CURRENCIES)
  m[[paste0("Currency_", cur)]] <- 1
  m
}

# Cache so VAR and pooled models (one fit serves every currency) are fitted once
# per origin and horizon, not once per currency.
pred_cache <- new.env(parent = emptyenv())

# fit_arima(): fits ARIMA(p,1,q) to the log of the rates (lambda = 0) and stops
# with an error if the optimiser did not converge.
fit_arima <- function(s, cfg) {
  fit <- forecast::Arima(s, order = c(cfg$p, 1L, cfg$q), lambda = 0,
                         include.drift = cfg$drift, method = "ML", biasadj = TRUE)
  if (!is.null(fit$code) && fit$code != 0) stop("ARIMA convergence failure")
  fit
}

# Fit one supervised model and predict the h-step log return for the rows in newx.
#   kind = "RF"  : Random Forest (300 trees, mtry = square root of predictors)
#   kind = "XGB" : XGBoost (learning rate 0.03, 200 rounds)
#   kind = "LM"  : linear regression
fit_predict_ml <- function(kind, x, y, newx, cfg, seed) {
  if (kind == "RF") {
    fit <- ranger::ranger(x = x, y = y, num.trees = 300,
                          mtry = max(1L, floor(sqrt(ncol(x)))),
                          min.node.size = cfg$leaf, num.threads = 1, seed = seed)
    as.numeric(predict(fit, data = newx)$predictions)
  } else if (kind == "XGB") {
    dm <- xgboost::xgb.DMatrix(as.matrix(x), label = y)
    fit <- xgboost::xgb.train(
      params = list(objective = "reg:squarederror", max_depth = as.integer(cfg$depth),
                    eta = 0.03, subsample = 0.8, colsample_bytree = 0.8,
                    min_child_weight = 10, nthread = 1, seed = seed),
      data = dm, nrounds = 200, verbose = 0)
    as.numeric(predict(fit, xgboost::xgb.DMatrix(as.matrix(newx))))
  } else if (kind == "LM") {
    # Some features are exact linear combinations (e.g. mean5 of ret_0..ret_lag4).
    # lm() drops the aliased terms, so predictions stay valid.
    fit <- stats::lm(y ~ ., data = data.frame(y = y, x))
    as.numeric(suppressWarnings(predict(fit, newdata = newx)))
  } else stop("Unknown model kind: ", kind)
}

# point_predict(): returns the forecast exchange rate h reporting days after the
# forecast origin (row i) for currency cc, using the model described by cfg.
# Only data up to row i are used (no information from the future).
point_predict <- function(cc, i, h, cfg) {
  s <- rates[[cc]]                                  # rate series of this currency
  if (cfg$family == "Naive") return(s[i])           # naive: latest observed rate
  first <- window_start(i, cfg$window)              # first row of the estimation window

  # ARIMA: fit on the window and forecast h steps ahead.
  if (cfg$family == "ARIMA") {
    fit <- fit_arima(s[first:i], cfg)
    # lambda=0 logs internally; biasadj=TRUE gives approximate mean rate forecasts.
    return(as.numeric(forecast::forecast(fit, h = h, biasadj = TRUE)$mean[h]))
  }

  # ETS (exponential smoothing) without a seasonal component.
  if (cfg$family == "ETS") {
    # Fitted to the log rate; exp() of the forecast is a median-type rate forecast.
    fit <- forecast::ets(log(s[first:i]), model = cfg$ets_model)
    return(as.numeric(exp(forecast::forecast(fit, h = h)$mean[h])))
  }

  # VAR: joint model of the log returns of all currencies. The forecast returns
  # are summed over h steps and converted back to a rate.
  if (cfg$family == "VAR") {
    key <- paste(cfg$id, i, h)
    if (!exists(key, envir = pred_cache, inherits = FALSE)) {
      m <- sapply(CURRENCIES, function(c2) diff(log(rates[[c2]][first:i])))
      fit <- vars::VAR(m, p = as.integer(cfg$p), type = "const")
      fc <- predict(fit, n.ahead = h)$fcst
      assign(key, vapply(CURRENCIES, function(c2) {
        rates[[c2]][i] * exp(sum(fc[[c2]][, "fcst"]))
      }, numeric(1)), envir = pred_cache)
    }
    return(unname(get(key, envir = pred_cache)[cc]))
  }

  # Pooled Random Forest / XGBoost: one model trained on all currencies together,
  # with one-hot columns identifying the currency.
  if (cfg$family %in% c("RF_pooled", "XGB_pooled")) {
    key <- paste(cfg$id, i, h)
    if (!exists(key, envir = pred_cache, inherits = FALSE)) {
      X <- list(); Y <- list(); NEWX <- list()
      for (c2 in CURRENCIES) {
        f2 <- features[[c2]]
        s2 <- rates[[c2]]
        # Training rows: the target (h steps ahead) must be observed by origin i.
        js2 <- seq.int(first, i - h)
        js2 <- js2[complete.cases(f2[js2, , drop = FALSE])]
        stopifnot(length(js2) >= 100, max(js2 + h) <= i,
                  complete.cases(f2[i, , drop = FALSE]))
        X[[c2]] <- cbind(f2[js2, , drop = FALSE], onehot(c2, length(js2)))
        Y[[c2]] <- log(s2[js2 + h] / s2[js2])     # h-step log return (the target)
        NEWX[[c2]] <- cbind(f2[i, , drop = FALSE], onehot(c2, 1L))
      }
      x <- dplyr::bind_rows(X)
      newx <- dplyr::bind_rows(NEWX)
      y <- unlist(Y, use.names = FALSE)
      set.seed(2026 + i + h)
      kind <- sub("_pooled", "", cfg$family)
      yhat <- fit_predict_ml(kind, x, y, newx, cfg, seed = 2026 + i + h)
      assign(key, stats::setNames(vapply(seq_along(CURRENCIES), function(k) {
        rates[[CURRENCIES[k]]][i] * exp(yhat[k])
      }, numeric(1)), CURRENCIES), envir = pred_cache)
    }
    return(unname(get(key, envir = pred_cache)[cc]))
  }

  # Single-currency supervised models: RF, XGB, LM
  f <- features[[cc]]
  # Training target for origin j: log(S[j+h]/S[j]). Require j+h <= i.
  # Even with precomputed features, no training label may extend past origin i.
  js <- seq.int(first, i - h)
  js <- js[complete.cases(f[js, , drop = FALSE])]
  stopifnot(length(js) >= 100, max(js + h) <= i,
            complete.cases(f[i, , drop = FALSE]))
  y <- log(s[js + h] / s[js])             # target: h-step log return
  x <- f[js, , drop = FALSE]              # predictors for the training rows
  newx <- f[i, , drop = FALSE]            # predictors at the forecast origin
  # The "lags" linear model uses only the current return and lags 1-5.
  if (cfg$family == "LM" && cfg$feat == "lags") {
    keep <- c("ret_0", paste0("ret_lag", 1:5))
    x <- x[, keep, drop = FALSE]
    newx <- newx[, keep, drop = FALSE]
  }
  set.seed(2026 + i + h)
  yhat <- fit_predict_ml(cfg$family, x, y, newx, cfg, seed = 2026 + i + h)
  # A transformed point prediction; not automatically an expected exchange rate.
  as.numeric(s[i] * exp(yhat))            # convert predicted log return to a rate
}

# Explicitly record failed fits. Do not rank partially successful candidates.
# safe_point(): runs point_predict() but, if a model fails, records the failure
# and returns NA instead of stopping the whole script.
failure_log <- list()
safe_point <- function(cc, i, h, cfg, phase) {
  tryCatch({
    p <- point_predict(cc, i, h, cfg)
    if (length(p) != 1 || !is.finite(p) || p <= 0) stop("Invalid point forecast")
    p
  }, error = function(e) {
    failure_log[[length(failure_log) + 1L]] <<- data.frame(
      phase = phase, currency = cc, origin = rates$date[i], h = h,
      id = cfg$id, message = conditionMessage(e))
    NA_real_
  })
}

# evaluate_points(): walk-forward evaluation. For every candidate model, horizon
# and forecast origin, it stores the forecast and the rate actually observed at
# the target date.
evaluate_points <- function(cc, ids, candidate_rows, phase) {
  rows <- list()
  for (j in seq_len(nrow(candidate_rows))) {
    cfg <- candidate_rows[j, , drop = FALSE]
    message(phase, ": ", cc, " / ", cfg$id)        # progress message
    for (h in HORIZONS) for (i in ids) {
      rows[[length(rows) + 1L]] <- data.frame(
        phase = phase, currency = cc, id = cfg$id, family = cfg$family,
        h = h, origin = rates$date[i], target = rates$date[i + h],
        actual = rates[[cc]][i + h], pred = safe_point(cc, i, h, cfg, phase))
    }
  }
  dplyr::bind_rows(rows)
}

# R-squared measures (out of sample):
#   R2_vs_naive = 1 - SSE(model) / SSE(naive). Positive = better than "no change".
#                 USE THIS to compare models.
#   R2_level    = 1 - SSE(model) / SST(actual level). Plain R-squared on the rate.
#                 Close to 1 for EVERY model (even naive) because rates trend, so
#                 it cannot show whether a model is any good.
add_r2 <- function(pred_df) {
  # Naive forecasts for the same currency, horizon and origin.
  naive <- pred_df %>% dplyr::filter(family == "Naive") %>%
    dplyr::select(currency, h, origin, naive_pred = pred)
  pred_df %>% dplyr::left_join(naive, by = c("currency", "h", "origin")) %>%
    dplyr::group_by(currency, h, family, id) %>%
    dplyr::summarise(
      R2_vs_naive = if (all(is.finite(pred))) {
        1 - sum((actual - pred)^2) / sum((actual - naive_pred)^2) } else NA_real_,
      R2_level = if (all(is.finite(pred))) {
        1 - sum((actual - pred)^2) / sum((actual - mean(actual))^2) } else NA_real_,
      .groups = "drop")
}

# point_scores(): summarises forecast accuracy for each model configuration:
# number of forecasts, number of failures, MAE, RMSE and the R-squared measures.
point_scores <- function(x) {
  x %>% dplyr::group_by(currency, h, family, id) %>%
    dplyr::summarise(n = dplyr::n(), failed = sum(!is.finite(pred)),
                     MAE = if (all(is.finite(pred))) mean(abs(actual - pred)) else NA_real_,
                     RMSE = if (all(is.finite(pred))) sqrt(mean((actual - pred)^2)) else NA_real_,
                     .groups = "drop") %>%
    dplyr::left_join(add_r2(x), by = c("currency", "h", "family", "id"))
}

# 6. WALK-FORWARD VALIDATION: 2022-2023 -------------------------------------
# This is the slow section. Each fit is refitted at each selected origin.
# Earlier validation observations become training data only after observed.
# Hyperparameters and window choice are selected here, NEVER on test outcomes.

# Evaluate every candidate on the validation origins, for each currency.
validation <- dplyr::bind_rows(lapply(CURRENCIES, function(cc) {
  ans <- evaluate_points(cc, val_origins, configs, "validation")
  save_table(ans, paste0("04_validation_predictions_", cc, ".xlsx"))
  ans
}))

# Accuracy of each candidate on the validation period.
validation_scores <- point_scores(validation)
save_table(validation_scores, "04_validation_scores.xlsx", max_print = Inf)

# Validation RMSE separately for 2022 (float year) and 2023.
validation_by_year <- validation %>%
  dplyr::mutate(year = as.integer(format(target, "%Y"))) %>%
  dplyr::group_by(currency, h, id, year) %>%
  dplyr::summarise(n = dplyr::n(), failed = sum(!is.finite(pred)),
                   MAE = mean(abs(actual - pred)), RMSE = sqrt(mean((actual - pred)^2)),
                   .groups = "drop")
save_table(validation_by_year, "04_validation_2022_vs_2023.xlsx")

# Choose one configuration per model family / currency / horizon by RMSE.
# (Ties are broken by MAE and then by the configuration name.)
selected <- validation_scores %>% dplyr::filter(failed == 0, is.finite(RMSE)) %>%
  dplyr::group_by(currency, h, family) %>%
  dplyr::arrange(RMSE, MAE, id, .by_group = TRUE) %>%
  dplyr::slice_head(n = 1) %>% dplyr::ungroup()

# Stop if any family has no fully successful candidate.
if (nrow(selected) != length(CURRENCIES) * length(HORIZONS) * N_FAMILIES) {
  save_table(dplyr::bind_rows(failure_log), "fit_failures.xlsx")
  stop("At least one model family has no fully successful validation candidate. Inspect fit_failures.xlsx.")
}

# The overall chosen point-forecast model for each currency and horizon is the
# family winner with the lowest validation RMSE.
chosen <- selected %>% dplyr::group_by(currency, h) %>%
  dplyr::arrange(RMSE, MAE, id, .by_group = TRUE) %>%
  dplyr::slice_head(n = 1) %>% dplyr::ungroup()
save_table(selected, "05_selected_per_family.xlsx", max_print = Inf)
save_table(chosen, "05_chosen_point_models.xlsx", max_print = Inf)

# Save a checkpoint before testing. This fixes the selected settings.
saveRDS(list(selected = selected, chosen = chosen, configs = configs),
        file.path(OUT, "05_frozen_point_choices.rds"))

# 6B. PLOT: HOW DID THE MODELS BEHAVE AROUND THE MARCH 2022 FLOAT? ------------
# Best configuration per family, 5-day forecasts, validation predictions.

FLOAT_DATE <- as.Date("2022-03-08")   # date of the rupee float (dashed line in the plot)
for (cc in CURRENCIES) {
  # Models selected for this currency at the 5-day horizon.
  ids <- selected$id[selected$currency == cc & selected$h == 5L]
  d <- validation %>%
    dplyr::filter(currency == cc, h == 5L, id %in% ids,
                  target >= as.Date("2022-01-15"), target <= as.Date("2022-08-31"))
  act <- dplyr::distinct(d, target, actual)    # actual rates (one line, in black)
  pp <- ggplot2::ggplot(d, ggplot2::aes(x = target)) +
    ggplot2::geom_line(data = act, ggplot2::aes(y = actual), colour = "black", linewidth = 1) +
    ggplot2::geom_line(ggplot2::aes(y = pred, colour = id), na.rm = TRUE) +
    ggplot2::geom_vline(xintercept = FLOAT_DATE, linetype = "dashed") +
    ggplot2::labs(title = paste(cc, "5-day forecasts around the March 2022 float (black = actual)"),
                  y = "LKR per foreign-currency unit", x = "Target date", colour = "Model") +
    ggplot2::theme_minimal()
  show_gg(pp, paste0("05_", cc, "_2022_float_forecasts.png"), width = 11, height = 6)
}

# 7. FINAL TEST: 2024-2025 -------------------------------------------------
# The configurations selected in section 6 are now evaluated, unchanged, on the
# test period. No setting is adjusted using test results.

test_rows <- list()
for (k in seq_len(nrow(selected))) {
  pick <- selected[k, ]                                       # one selected model
  cfg <- configs[configs$id == pick$id, , drop = FALSE]       # its settings
  for (i in test_origins) {
    h <- pick$h
    cc <- pick$currency
    test_rows[[length(test_rows) + 1L]] <- data.frame(
      phase = "test", currency = cc, id = cfg$id, family = cfg$family,
      h = h, origin = rates$date[i], target = rates$date[i + h],
      actual = rates[[cc]][i + h], pred = safe_point(cc, i, h, cfg, "test"))
  }
  message("Test completed: ", pick$currency, " / ", pick$h, " / ", cfg$id)
}
testing <- dplyr::bind_rows(test_rows)

# Test accuracy, then improvement relative to the naive benchmark:
# RMSE improvement (%) = 100 * (1 - model RMSE / naive RMSE); positive = better.
test_scores <- point_scores(testing)
baseline_scores <- test_scores %>% dplyr::filter(family == "Naive") %>%
  dplyr::select(currency, h, naive_RMSE = RMSE)
test_scores <- test_scores %>% dplyr::left_join(baseline_scores, by = c("currency", "h")) %>%
  dplyr::mutate(RMSE_improvement_pct = ifelse(naive_RMSE > 0,
                                              100 * (1 - RMSE / naive_RMSE), NA_real_))
save_table(testing, "06_test_predictions.xlsx")
save_table(test_scores, "06_test_scores.xlsx", max_print = Inf)
save_table(dplyr::bind_rows(failure_log), "fit_failures.xlsx")

# Positive improvement / R2_vs_naive = better than naive; not an accuracy percentage.
# Plot the test forecasts of every family against the actual rate (black line).
for (cc in CURRENCIES) {
  pp <- ggplot2::ggplot(dplyr::filter(testing, currency == cc),
                       ggplot2::aes(x = target)) +
    ggplot2::geom_line(ggplot2::aes(y = actual), colour = "black") +
    ggplot2::geom_line(ggplot2::aes(y = pred, colour = family), na.rm = TRUE) +
    ggplot2::facet_grid(h ~ family, scales = "free_y") +
    ggplot2::labs(title = paste(cc, "test forecasts; black = actual"),
                  y = "LKR per foreign-currency unit", x = "Target date") +
    ggplot2::theme_minimal() + ggplot2::theme(legend.position = "none")
  show_gg(pp, paste0("06_", cc, "_test_forecasts.png"), width = 22, height = 7)
}

# Bar chart: test RMSE improvement of every family over the naive forecast.
pp <- ggplot2::ggplot(dplyr::filter(test_scores, family != "Naive"),
                      ggplot2::aes(x = family, y = RMSE_improvement_pct, fill = family)) +
  ggplot2::geom_col() + ggplot2::geom_hline(yintercept = 0) +
  ggplot2::facet_grid(h ~ currency, labeller = ggplot2::label_both) +
  ggplot2::labs(title = "Test RMSE improvement over the naive forecast (positive = better)",
                y = "Improvement (%)", x = NULL) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
                 legend.position = "none")
show_gg(pp, "06_rmse_improvement_vs_naive.png", width = 12, height = 7)

# 8. VOLATILITY: ZERO-MEAN RETURNS, EWMA AND GARCH(1,1) -----------------------
# Separate task: forecast one-step variance in squared percentage-return units.
# Zero mean is an explicit simple assumption. Proxy = next percentage return^2.
# QLIKE = log(v) + proxy/v (equivalent ranking up to proxy-only constants).
# This form allows zero proxies, which occur with unchanged observed rates.

# Candidate volatility models: EWMA with three decay values and a zero-mean
# GARCH(1,1) with normal or Student-t errors.
vol_configs <- data.frame(
  id = c("EWMA_094", "EWMA_097", "EWMA_099", "GARCH_normal", "GARCH_student"),
  family = c("EWMA", "EWMA", "EWMA", "GARCH", "GARCH"),
  lambda = c(0.94, 0.97, 0.99, NA, NA),
  distribution = c(NA, NA, NA, "norm", "std"))

# Fixed recent window for volatility; selected point-model windows are separate.
# return_history(): percentage log returns available at forecast origin i.
return_history <- function(cc, i) {
  100 * diff(log(rates[[cc]][window_start(i, "recent"):i]))
}

# fit_garch(): fits a zero-mean GARCH(1,1) model to the returns r.
fit_garch <- function(r, dist) {
  spec <- rugarch::ugarchspec(
    variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
    mean.model = list(armaOrder = c(0, 0), include.mean = FALSE),
    distribution.model = dist)
  fit <- rugarch::ugarchfit(spec, data = r, solver = "hybrid",
                           fit.control = list(stationarity = 1))
  if (fit@fit$convergence != 0) stop("GARCH convergence failure")
  fit
}

# variance_predict(): one-step-ahead variance forecast.
#   EWMA : exponentially weighted average of squared returns
#   GARCH: conditional variance forecast from the fitted model
variance_predict <- function(r, cfg) {
  if (cfg$family == "EWMA") {
    v <- mean(head(r, min(50L, length(r)))^2)     # starting value
    for (rr in r) v <- cfg$lambda * v + (1 - cfg$lambda) * rr^2
    return(max(v, 1e-10))
  }
  fit <- fit_garch(r, cfg$distribution)
  v <- as.numeric(sigma(rugarch::ugarchforecast(fit, n.ahead = 1)))^2
  if (length(v) != 1 || !is.finite(v) || v <= 0) stop("Invalid GARCH variance")
  v
}

# vol_evaluate(): walk-forward evaluation of the volatility models. The proxy
# for the true variance is the squared percentage return at the target date.
# QLIKE and MSE are the two loss functions (lower is better).
vol_evaluate <- function(cc, ids, cfgs, phase) {
  rows <- list()
  for (k in seq_len(nrow(cfgs))) for (i in ids) {
    cfg <- cfgs[k, , drop = FALSE]
    err <- ""
    v <- tryCatch(variance_predict(return_history(cc, i), cfg),
                  error = function(e) {err <<- conditionMessage(e); NA_real_})
    proxy <- (100 * log(rates[[cc]][i + 1L] / rates[[cc]][i]))^2
    rows[[length(rows) + 1L]] <- data.frame(
      phase = phase, currency = cc, origin = rates$date[i], target = rates$date[i + 1L],
      id = cfg$id, family = cfg$family, variance = v, proxy = proxy,
      QLIKE = log(v) + proxy / v, MSE = (proxy - v)^2, error = err)
  }
  dplyr::bind_rows(rows)
}

# vol_scores(): average QLIKE and MSE for each volatility model.
vol_scores <- function(x) {
  x %>% dplyr::group_by(currency, family, id) %>%
    dplyr::summarise(n = dplyr::n(), failed = sum(!is.finite(variance)),
                     QLIKE = mean(QLIKE), MSE = mean(MSE), .groups = "drop")
}

# Validation: evaluate every volatility model on the validation origins.
vol_validation <- dplyr::bind_rows(lapply(CURRENCIES, function(cc) {
  message("Volatility validation: ", cc)
  vol_evaluate(cc, val_origins, vol_configs, "validation")
}))
save_table(vol_validation, "07_volatility_validation_predictions.xlsx")
vol_validation_scores <- vol_scores(vol_validation)
save_table(vol_validation_scores, "07_volatility_validation_scores.xlsx", max_print = Inf)

# Select the best EWMA and the best GARCH setting (lowest QLIKE) per currency.
vol_selected <- vol_validation_scores %>% dplyr::filter(failed == 0, is.finite(QLIKE)) %>%
  dplyr::group_by(currency, family) %>% dplyr::arrange(QLIKE, id, .by_group = TRUE) %>%
  dplyr::slice_head(n = 1) %>% dplyr::ungroup()
if (nrow(vol_selected) != length(CURRENCIES) * 2L) {
  stop("No fully successful volatility candidate for at least one family/currency. Inspect 07_volatility_validation_predictions.xlsx.")
}
save_table(vol_selected, "07_frozen_volatility_choices.xlsx", max_print = Inf)

# Test: evaluate the frozen volatility settings on the test origins.
vol_test_rows <- list()
for (k in seq_len(nrow(vol_selected))) {
  pick <- vol_selected[k, ]
  cfg <- vol_configs[vol_configs$id == pick$id, , drop = FALSE]
  vol_test_rows[[k]] <- vol_evaluate(pick$currency, test_origins, cfg, "test")
}
vol_testing <- dplyr::bind_rows(vol_test_rows)
save_table(vol_testing, "08_volatility_test_predictions.xlsx")
save_table(vol_scores(vol_testing), "08_volatility_test_scores.xlsx", max_print = Inf)

# Plot forecast volatility (lines) against absolute daily returns (points).
for (cc in CURRENCIES) {
  pp <- ggplot2::ggplot(dplyr::filter(vol_testing, currency == cc),
                        ggplot2::aes(x = target)) +
    ggplot2::geom_point(ggplot2::aes(y = sqrt(proxy)), colour = "grey50", size = 0.8, na.rm = TRUE) +
    ggplot2::geom_line(ggplot2::aes(y = sqrt(variance), colour = id), na.rm = TRUE) +
    ggplot2::labs(title = paste(cc, "test: forecast volatility (lines) vs absolute daily return (points)"),
                  y = "Daily % change", x = "Target date", colour = "Model") +
    ggplot2::theme_minimal()
  show_gg(pp, paste0("08_", cc, "_volatility_forecasts.png"), width = 11, height = 5)
}

# 9. GARCH RATE INTERVALS: SIMULATE COMPLETE RETURN PATHS --------------------
# These intervals belong to the ZERO-MEAN GARCH model, not the ARIMA/RF/XGB
# point forecasts. Do not attach them to another model and call them its interval.
# Conditional on fitted parameters; parameter uncertainty and new regimes omitted.
# Student-t log returns have no finite exponential mean. Use median/quantiles,
# not the simulated mean exchange rate, for this GARCH-based scenario output.

# garch_intervals(): fits the selected GARCH model at origin i, simulates
# 2,000 return paths for 20 reporting days, and returns the median and the
# 2.5% / 97.5% quantiles of the exchange rate at the 5-day and 20-day horizons.
garch_intervals <- function(cc, i, cfg) {
  fit <- fit_garch(return_history(cc, i), cfg$distribution)
  set.seed(3000 + i + match(cc, CURRENCIES))
  sim <- rugarch::ugarchsim(fit, n.sim = max(HORIZONS), n.start = 0,
                           m.sim = SIMULATIONS, startMethod = "sample")
  rr <- as.matrix(fitted(sim))                    # simulated daily % returns
  stopifnot(nrow(rr) == max(HORIZONS), ncol(rr) == SIMULATIONS)
  cumulative <- apply(rr / 100, 2, cumsum)       # cumulative log change per path
  rows <- lapply(HORIZONS, function(h) {
    # Quantiles in log space avoid unnecessary exponentiation of extreme paths.
    q <- exp(log(rates[[cc]][i]) +
               stats::quantile(cumulative[h, ], c(0.025, 0.5, 0.975), names = FALSE))
    if (any(!is.finite(q))) stop("Non-finite simulated interval")
    data.frame(h = h, lower95 = q[1], median = q[2], upper95 = q[3])
  })
  dplyr::bind_rows(rows)
}

# Backtest: build an interval at every test origin and keep the actual rate so
# that coverage can be measured.
interval_rows <- list()
for (cc in CURRENCIES) {
  pick <- dplyr::filter(vol_selected, currency == cc, family == "GARCH")
  cfg <- vol_configs[vol_configs$id == pick$id, , drop = FALSE]
  for (i in test_origins) {
    ans <- tryCatch(garch_intervals(cc, i, cfg), error = function(e) {
      warning(cc, " interval failed at ", rates$date[i], ": ", conditionMessage(e))
      data.frame(h = HORIZONS, lower95 = NA_real_, median = NA_real_, upper95 = NA_real_)
    })
    ans$currency <- cc
    ans$id <- cfg$id
    ans$origin <- rates$date[i]
    ans$target <- rates$date[i + ans$h]
    ans$actual <- rates[[cc]][i + ans$h]
    interval_rows[[length(interval_rows) + 1L]] <- ans
  }
  message("Interval backtest completed: ", cc)
}
interval_test <- dplyr::bind_rows(interval_rows)

# Coverage = percentage of actual rates inside the interval (target about 95%);
# mean_width = average interval width in LKR.
interval_scores <- interval_test %>% dplyr::group_by(currency, h, id) %>%
  dplyr::summarise(n = dplyr::n(), failed = sum(!is.finite(lower95) | !is.finite(upper95)),
                   coverage95_pct = 100 * mean(actual >= lower95 & actual <= upper95),
                   mean_width = mean(upper95 - lower95), .groups = "drop")
save_table(interval_test, "09_garch_interval_test_predictions.xlsx")
save_table(interval_scores, "09_garch_interval_test_scores.xlsx", max_print = Inf)

# Plot the intervals (shaded) against the actual rate (black line).
for (cc in CURRENCIES) {
  d <- dplyr::filter(interval_test, currency == cc, is.finite(lower95))
  pp <- ggplot2::ggplot(d, ggplot2::aes(x = target)) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = lower95, ymax = upper95), alpha = 0.25) +
    ggplot2::geom_line(ggplot2::aes(y = median), linetype = "dashed") +
    ggplot2::geom_line(ggplot2::aes(y = actual), colour = "black") +
    ggplot2::facet_wrap(~ h, ncol = 1, scales = "free_y", labeller = ggplot2::label_both) +
    ggplot2::labs(title = paste(cc, "GARCH 95% interval backtest (black = actual, dashed = median)"),
                  y = "LKR per foreign-currency unit", x = "Target date") +
    ggplot2::theme_minimal()
  show_gg(pp, paste0("09_", cc, "_interval_backtest.png"), width = 11, height = 7)
}

# 10. REFIT AFTER EVALUATION AND PRODUCE AN AS-OF-END-2025 FORECAST -----------
# These are forecasts from the final observed 2025 date, NOT today's forecast.

# Row number of the last observation (31 December 2025) = the forecast origin.
last_i <- max(which(rates$date <= TEST_END))

# Point forecasts: the validation-selected model for each currency and horizon
# is refitted with all data up to the origin, and the forecast rate is
# multiplied by the tuition fee to give the cost in LKR.
final_points <- list()
for (k in seq_len(nrow(chosen))) {
  pick <- chosen[k, ]
  cc <- pick$currency
  cfg <- configs[configs$id == pick$id, , drop = FALSE]
  pred <- safe_point(cc, last_i, pick$h, cfg, "final")
  final_points[[k]] <- data.frame(
    as_of = rates$date[last_i], currency = cc, reporting_days_ahead = pick$h,
    selected_model = cfg$id, foreign_fee = unname(TUITION[cc]),
    forecast_rate = pred, tuition_point_LKR = unname(TUITION[cc]) * pred,
    latest_rate_cost_LKR = unname(TUITION[cc]) * rates[[cc]][last_i])
}
final_points <- dplyr::bind_rows(final_points)
save_table(final_points, "10_tuition_point_forecasts.xlsx", max_print = Inf)

# GARCH scenarios: median and 95% interval of the tuition cost. The planning
# buffer is the upper bound minus the median cost.
final_intervals <- list()
for (cc in CURRENCIES) {
  pick <- dplyr::filter(vol_selected, currency == cc, family == "GARCH")
  cfg <- vol_configs[vol_configs$id == pick$id, , drop = FALSE]
  ans <- garch_intervals(cc, last_i, cfg)
  ans$as_of <- rates$date[last_i]
  ans$currency <- cc
  ans$model <- cfg$id
  ans$foreign_fee <- unname(TUITION[cc])
  ans$tuition_median_LKR <- ans$median * ans$foreign_fee
  ans$tuition_lower95_LKR <- ans$lower95 * ans$foreign_fee
  ans$tuition_upper95_LKR <- ans$upper95 * ans$foreign_fee
  ans$buffer_above_median_LKR <- ans$tuition_upper95_LKR - ans$tuition_median_LKR
  final_intervals[[cc]] <- ans
}
final_intervals <- dplyr::bind_rows(final_intervals)
save_table(final_intervals, "10_garch_tuition_scenarios.xlsx", max_print = Inf)

# Chart of the tuition-cost scenarios (median with 95% interval).
pp <- ggplot2::ggplot(final_intervals,
                      ggplot2::aes(x = factor(h), y = tuition_median_LKR, colour = currency)) +
  ggplot2::geom_pointrange(ggplot2::aes(ymin = tuition_lower95_LKR, ymax = tuition_upper95_LKR),
                           position = ggplot2::position_dodge(width = 0.4)) +
  ggplot2::labs(title = "Tuition cost scenario: GARCH median and 95% interval",
                x = "Reporting days ahead", y = "Tuition cost (LKR)") +
  ggplot2::theme_minimal()
show_gg(pp, "10_tuition_scenarios.png", width = 9, height = 6)

# 11. FINAL FIT DIAGNOSTICS AND REPRODUCIBILITY ------------------------------
# Descriptive checks only; do not retune after inspecting the final test.

# arch_lm(): ARCH-LM test for remaining volatility clustering. It regresses the
# squared residuals on their own lags; a small p-value means ARCH effects remain.
arch_lm <- function(e, lags = 10L) {
  e <- as.numeric(e)
  e <- e[is.finite(e)]
  mm <- embed(e^2, lags + 1L)
  mod <- lm(mm[, 1] ~ mm[, -1])
  stat <- nrow(mm) * summary(mod)$r.squared
  c(statistic = stat, p_value = pchisq(stat, df = lags, lower.tail = FALSE))
}
residual_rows <- list()
for (cc in CURRENCIES) {
  # ARIMA models chosen within that family, even when another family won overall.
  picks <- selected %>% dplyr::filter(currency == cc, family == "ARIMA") %>%
    dplyr::distinct(id, .keep_all = TRUE)
  for (k in seq_len(nrow(picks))) {
    cfg <- configs[configs$id == picks$id[k], , drop = FALSE]
    # Refit the ARIMA model on data through 31 December 2025 and test residuals.
    fit <- fit_arima(rates[[cc]][window_start(last_i, cfg$window):last_i], cfg)
    e <- as.numeric(residuals(fit))
    e <- e[is.finite(e)]
    residual_rows[[length(residual_rows) + 1L]] <- data.frame(
      currency = cc, id = cfg$id,
      ljung_box_p = Box.test(e, lag = 20, type = "Ljung-Box", fitdf = cfg$p + cfg$q)$p.value,
      arch_lm_p = unname(arch_lm(e)["p_value"]))
    saveRDS(fit, file.path(OUT, paste0("11_", cc, "_", cfg$id, ".rds")))
  }
  # Standardised residuals of the selected GARCH model.
  pick <- dplyr::filter(vol_selected, currency == cc, family == "GARCH")
  cfg <- vol_configs[vol_configs$id == pick$id, , drop = FALSE]
  fit <- fit_garch(return_history(cc, last_i), cfg$distribution)
  e <- as.numeric(residuals(fit, standardize = TRUE))
  e <- e[is.finite(e)]
  residual_rows[[length(residual_rows) + 1L]] <- data.frame(
    currency = cc, id = cfg$id,
    ljung_box_p = Box.test(e, lag = 20, type = "Ljung-Box")$p.value,
    arch_lm_p = unname(arch_lm(e)["p_value"]))
  saveRDS(fit, file.path(OUT, paste0("11_", cc, "_garch.rds")))
}
save_table(dplyr::bind_rows(residual_rows), "11_final_residual_checks.xlsx", max_print = Inf)
save_table(dplyr::bind_rows(failure_log), "fit_failures.xlsx")

# Save the R version and package versions used (for reproducibility).
capture.output(sessionInfo(), file = file.path(OUT, "11_sessionInfo.txt"))
message("DONE. Results saved in: ", normalizePath(OUT))
cat("\nFiles written to the results folder:\n")
print(list.files(OUT))
cat("\nFailures logged:", length(failure_log), "\n")
