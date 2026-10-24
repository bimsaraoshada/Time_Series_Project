# EXCHANGE-RATE FORECASTING FOR TUITION PLANNING  (VERSION 3)
# Run numbered sections in order in RStudio. No GitHub writes are performed.
# Put this script and exchange_rates_lkr.csv in one RStudio Project folder.
# File > New Project > Existing Directory, then open this script.
# Select a section and press Ctrl+Enter. Do not run inside PowerShell.
# Package installation needs internet. Fitting takes a long time.
# Data: observed reporting dates, not calendar dates. Never pad weekends.
# Forecast origin = after that date's exchange rate has become available.
# No full-sample imputation, scaling, winsorisation or outlier deletion is used.
#
# WHAT IS NEW IN VERSION 3
#   * Every table is printed in the R Console AND saved as an Excel (.xlsx) file.
#   * Every plot is drawn in the RStudio Plots pane AND saved as a PNG.
#     (Use the arrows in the Plots pane to flip back through earlier plots.
#      If R says "figure margins too large", make the Plots pane bigger.)
#   * R-squared columns: R2_vs_naive (out-of-sample, use this to compare models),
#     R2_level (plain R-squared on the rate level, near 1 for every model because
#     rates trend; shown only for completeness) and R2_returns_insample (ARIMA).
#   * Lecture-style steps: ACF/PACF plots, AIC/BIC table, residual plot + ACF +
#     checkresiduals + Ljung-Box, auto.arima(trace = TRUE), forecast vs actual plot
#     with legend, and accuracy().
#
# HOW THE ARIMA ORDERS (p, d, q) ARE USED
#   d = 1 is fixed after the training KPSS test (section 4 stops if it disagrees).
#   p and q come from a FIXED candidate list (arima_specs, section 4B):
#   (0,1,0), (0,1,0)+drift, (1,1,0), (0,1,1), (1,1,1).
#   The ACF/PACF plots and the significant-lag columns in section 4 are guidance for
#   you to justify that list. The script does NOT read them automatically.
#   auto.arima() is run as a cross-check and tells you if it prefers another order.
#   The order finally used is chosen by validation RMSE (section 6), not by AIC.
#
# NOT IMPLEMENTED ON PURPOSE (explain in the report): SARIMA, Holt-Winters,
# SARIMAX/VARMAX and Prophet. Daily exchange rates show no seasonality (see 4C).

# 1. INSTALL AND LOAD PACKAGES -----------------------------------------------
packages <- c("readr", "dplyr", "ggplot2", "forecast", "tseries",
              "zoo", "ranger", "xgboost", "rugarch", "vars", "writexl")
missing_packages <- packages[!vapply(packages, requireNamespace,
                                   logical(1), quietly = TRUE)]
if (length(missing_packages)) {
  install.packages(missing_packages, repos = "https://cloud.r-project.org")
}
invisible(lapply(packages, function(p) library(p, character.only = TRUE)))
set.seed(2026)
options(width = 200)   # wider Console so tables are not wrapped

# 2. SETTINGS: CHANGE THESE BEFORE STARTING ---------------------------------
DATA_FILE <- "exchange_rates_lkr.csv"
# If the file is elsewhere, use forward slashes, for example:
# DATA_FILE <- "C:/Users/ASUS/Downloads/exchange_rates_lkr.csv"

# QUICK_RUN = TRUE : USD only, forecast every 20 observations (fast rehearsal,
#                    the VAR and pooled extensions are skipped automatically).
# QUICK_RUN = FALSE: all three currencies, every 5 observations (final results).
# Set QUICK_RUN <- FALSE BEFORE selecting or reporting models.
QUICK_RUN <- FALSE
CURRENCIES <- if (QUICK_RUN) "USD" else c("USD", "GBP", "EUR")
ORIGIN_STEP <- if (QUICK_RUN) 20L else 5L

RUN_VAR <- TRUE      # multivariate VAR on the three return series (extension)
RUN_POOLED <- TRUE   # pooled RF / XGBoost with one-hot currency columns (extension)

HORIZONS <- c(5L, 20L)
TRAIN_END <- as.Date("2021-12-31")
VAL_START <- as.Date("2022-01-01")
VAL_END <- as.Date("2023-12-31")
TEST_START <- as.Date("2024-01-01")
TEST_END <- as.Date("2025-12-31")
RECENT_N <- 1250L  # approximately five reporting years
SIMULATIONS <- 2000L
# Clearly hypothetical fees; replace with your specified fees if needed.
TUITION <- c(USD = 5000, GBP = 5000, EUR = 5000)
stopifnot(ORIGIN_STEP >= 1, all(TUITION > 0))
# A new output folder each run avoids overwriting previous results.
OUT <- file.path("results", paste0("run_", format(Sys.time(), "%Y%m%d_%H%M%S")))
dir.create(OUT, recursive = TRUE, showWarnings = FALSE)

# Output helpers --------------------------------------------------------------
# save_table: writes an Excel file AND prints the table in the Console.
# Long tables print only the first 10 rows in the Console (full table is in Excel).
save_table <- function(x, filename, max_print = 60) {
  x <- as.data.frame(x)
  if (ncol(x) == 0L) x <- data.frame(note = "no rows")
  file <- sub("\\.csv$", ".xlsx", filename)
  writexl::write_xlsx(x, file.path(OUT, file))
  cat("\n===== ", file, " (", nrow(x), " rows) =====\n", sep = "")
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
  plot_fun()
  grDevices::dev.copy(grDevices::png, filename = file.path(OUT, filename),
                      width = width, height = height, res = res)
  invisible(grDevices::dev.off())
  par(mfrow = c(1, 1))
}
# show_gg: prints a ggplot in the Plots pane, then saves a PNG copy.
show_gg <- function(p, filename, width = 13, height = 7) {
  print(p)
  ggplot2::ggsave(file.path(OUT, filename), p, width = width, height = height)
}
saveRDS(list(currencies = CURRENCIES, horizons = HORIZONS, step = ORIGIN_STEP,
             recent_n = RECENT_N, seed = 2026, tuition = TUITION,
             run_var = RUN_VAR, run_pooled = RUN_POOLED),
        file.path(OUT, "settings.rds"))

# 3. IMPORT AND CHECK THE DATA ----------------------------------------------
rates <- readr::read_csv(DATA_FILE, show_col_types = FALSE)
stopifnot(all(c("date", "USD", "GBP", "EUR") %in% names(rates)))
rates$date <- as.Date(rates$date)
rates <- rates[order(rates$date), c("date", "USD", "GBP", "EUR")]
stopifnot(!anyNA(rates), !anyDuplicated(rates$date))
stopifnot(all(vapply(rates[c("USD", "GBP", "EUR")], is.numeric, logical(1))))
stopifnot(all(is.finite(as.matrix(rates[-1]))), all(as.matrix(rates[-1]) > 0))
stopifnot(min(rates$date) < TRAIN_END, max(rates$date) >= TEST_END)
# Deliberately keep genuine 2022 observations. Never cap large returns here.
print(head(rates))
print(tail(rates))
split_counts <- data.frame(
  period = c("Initial training", "Validation", "Final testing"),
  n = c(sum(rates$date <= TRAIN_END),
        sum(rates$date >= VAL_START & rates$date <= VAL_END),
        sum(rates$date >= TEST_START & rates$date <= TEST_END)))
save_table(split_counts, "01_split_counts.xlsx")

# 4. TRAINING-ONLY DIAGNOSTICS: ACF / PACF -----------------------------------
# Full-sample EDA is descriptive; do not use test outcomes to choose a model.
# ADF H0: unit root. KPSS H0 here: level stationarity.
# Breaks can complicate both tests. No single p-value proves stationarity.
# ACF of the log rate decays slowly when the series is non-stationary (needs d = 1).
# On the log-return ACF/PACF, a spike at lag k beyond the blue band suggests an
# MA(k) term (ACF) or AR(k) term (PACF). The significant-lag columns list those
# lags (|value| > 1.96 / sqrt(n)) as guidance only.
sig_lags <- function(v, n) {
  idx <- which(abs(v) > 1.96 / sqrt(n))
  if (length(idx)) paste(idx, collapse = ",") else "none"
}
diagnostics <- list()
for (cc in CURRENCIES) {
  x <- rates[[cc]][rates$date <= TRAIN_END]
  z <- log(x)
  r <- 100 * diff(z)
  acf_vals <- as.numeric(acf(r, lag.max = 10, plot = FALSE)$acf)[-1]
  pacf_vals <- as.numeric(pacf(r, lag.max = 10, plot = FALSE)$acf)
  diagnostics[[cc]] <- data.frame(
    currency = cc,
    adf_log_level_p = suppressWarnings(tseries::adf.test(z)$p.value),
    kpss_log_level_p = suppressWarnings(tseries::kpss.test(z, null = "Level")$p.value),
    adf_return_p = suppressWarnings(tseries::adf.test(r)$p.value),
    kpss_return_p = suppressWarnings(tseries::kpss.test(r, null = "Level")$p.value),
    recommended_d_kpss = forecast::ndiffs(z, test = "kpss", max.d = 2),
    return_acf_sig_lags_1to10 = sig_lags(acf_vals, length(r)),
    return_pacf_sig_lags_1to10 = sig_lags(pacf_vals, length(r)))
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
arima_specs <- data.frame(
  model = c("ARIMA_010", "ARIMA_010_drift", "ARIMA_110", "ARIMA_011", "ARIMA_111"),
  p = c(0, 0, 1, 0, 1),
  q = c(0, 0, 0, 1, 1),
  drift = c(FALSE, TRUE, FALSE, FALSE, FALSE))
arima_ic <- list()
fits_keep <- list()
for (cc in CURRENCIES) {
  z <- log(rates[[cc]][rates$date <= TRAIN_END])
  r_ret <- diff(z)
  for (k in seq_len(nrow(arima_specs))) {
    sp <- arima_specs[k, ]
    fit <- tryCatch(
      forecast::Arima(z, order = c(sp$p, 1L, sp$q),
                      include.drift = sp$drift, method = "ML"),
      error = function(e) NULL)
    if (is.null(fit)) next
    e <- as.numeric(residuals(fit))
    fits_keep[[paste(cc, sp$model)]] <- fit
    arima_ic[[length(arima_ic) + 1L]] <- data.frame(
      currency = cc, model = sp$model, p = sp$p, d = 1L, q = sp$q,
      drift = sp$drift, AIC = AIC(fit), BIC = BIC(fit),
      ljung_box_p = Box.test(e, lag = 10, type = "Ljung-Box",
                             fitdf = sp$p + sp$q)$p.value,
      R2_returns_insample = 1 - sum(e[-1]^2) / sum((r_ret - mean(r_ret))^2))
  }
}
arima_ic <- dplyr::bind_rows(arima_ic) %>%
  dplyr::group_by(currency) %>%
  dplyr::mutate(best_AIC = AIC == min(AIC), best_BIC = BIC == min(BIC)) %>%
  dplyr::ungroup()
save_table(arima_ic, "02b_arima_aic_bic_ljungbox.xlsx")

# Lecture-style residual checks for the lowest-AIC candidate of each currency:
# plot(res), acf(res), checkresiduals() and the Ljung-Box test.
for (cc in CURRENCIES) {
  best <- arima_ic %>% dplyr::filter(currency == cc) %>%
    dplyr::arrange(AIC) %>% dplyr::slice(1)
  fit <- fits_keep[[paste(cc, best$model)]]
  res <- residuals(fit)
  cat("\nResidual checks for ", cc, " using ", best$model,
      " (lowest training AIC)\n", sep = "")
  show_and_save(paste0("02b_", cc, "_", best$model, "_residuals.png"), function() {
    par(mfrow = c(1, 2))
    plot(res, type = "l", main = paste(cc, best$model, "residuals"), ylab = "Residual")
    acf(res, main = paste(cc, "ACF of residuals"))
  }, width = 1600, height = 700)
  show_and_save(paste0("02b_", cc, "_", best$model, "_checkresiduals.png"),
                function() forecast::checkresiduals(fit, lag = 10),
                width = 1400, height = 1000)
}

# auto.arima cross-check (same as the lecture, with trace = TRUE). It searches
# p and q with d = 1 on the log training series and prints every model it tries.
# If in_candidate_list is FALSE, consider adding that order to arima_specs above.
auto_rows <- list()
for (cc in CURRENCIES) {
  z <- log(rates[[cc]][rates$date <= TRAIN_END])
  cat("\nauto.arima trace for ", cc, " (log rate, training period)\n", sep = "")
  auto_fit <- forecast::auto.arima(z, d = 1, seasonal = FALSE, trace = TRUE)
  ao <- forecast::arimaorder(auto_fit)
  has_drift <- "drift" %in% names(coef(auto_fit))
  auto_rows[[cc]] <- data.frame(
    currency = cc, auto_p = ao[["p"]], auto_d = ao[["d"]], auto_q = ao[["q"]],
    auto_drift = has_drift, AIC = AIC(auto_fit),
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
  ym <- format(d$date, "%Y-%m")
  m <- tapply(log(d[[cc]]), ym, mean)
  first_month <- as.Date(paste0(names(m)[1], "-01"))
  last_month <- as.Date(paste0(names(m)[length(m)], "-01"))
  if (length(seq(first_month, last_month, by = "month")) != length(m)) {
    warning(cc, ": monthly series has gaps; seasonality check skipped.")
    next
  }
  tsm <- ts(as.numeric(m),
            start = c(as.integer(substr(names(m)[1], 1, 4)),
                      as.integer(substr(names(m)[1], 6, 7))),
            frequency = 12)
  dec <- stl(tsm, s.window = "periodic")
  comp <- dec$time.series
  fs <- max(0, 1 - var(comp[, "remainder"]) /
              var(comp[, "seasonal"] + comp[, "remainder"]))
  seasonality_rows[[cc]] <- data.frame(
    currency = cc, months = length(tsm), seasonal_strength = fs)
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
  tr <- rates[[cc]][rates$date <= TRAIN_END]
  te <- rates[[cc]][rates$date >= VAL_START & rates$date <= VAL_END]
  cat("\nLecture-style auto.arima for ", cc, " (training data, lambda = 0)\n", sep = "")
  auto_model1 <- forecast::auto.arima(tr, lambda = 0, trace = TRUE)
  forecast_values <- forecast::forecast(auto_model1, h = length(te))
  cat("\nFirst forecast values for ", cc, ":\n", sep = "")
  print(utils::head(as.data.frame(forecast_values)))
  show_and_save(paste0("02d_", cc, "_lecture_forecast_vs_actual.png"), function() {
    plot(forecast_values, include = 300,
         main = paste(cc, "forecasts vs actual values (lecture style)"),
         xlab = "Observation", ylab = "LKR per unit")
    lines(length(tr) + seq_along(te), te, lwd = 3)
    legend("topleft", legend = c("Forecast", "Actual"),
           lty = 1, lwd = c(2, 3), col = c("blue", "black"))
  }, width = 1400, height = 800)
  acc <- forecast::accuracy(forecast_values, te)
  lecture_rows[[cc]] <- cbind(currency = cc, set = rownames(acc), as.data.frame(acc))
}
save_table(dplyr::bind_rows(lecture_rows), "02d_lecture_style_accuracy.xlsx")

# 5. HELPER FUNCTIONS AND CANDIDATES ----------------------------------------
# One-step and 20-step targets must both remain within each evaluation period.
# All models use the same origins. Last origins without a 20-step target are omitted.
origins_for <- function(start, end) {
  n <- nrow(rates)
  ids <- seq_len(n - max(HORIZONS))
  ids <- ids[rates$date[ids + 1L] >= start &
               rates$date[ids + max(HORIZONS)] <= end]
  if (!length(ids)) stop("No forecast origins in requested period")
  ids[seq.int(1L, length(ids), by = ORIGIN_STEP)]
}
val_origins <- origins_for(VAL_START, VAL_END)
test_origins <- origins_for(TEST_START, TEST_END)
window_start <- function(i, window) {
  if (window == "expanding") 1L else max(1L, i - RECENT_N + 1L)
}

# Candidate models. Families:
#   Naive, ARIMA, ETS (exponential smoothing), LM (linear regression), RF, XGB
#   VAR, RF_pooled, XGB_pooled  (extensions; need at least 2 currencies)
candidate_base <- dplyr::bind_rows(
  data.frame(arima_specs, family = "ARIMA"),
  data.frame(model = c("RF_leaf5", "RF_leaf20"), family = "RF",
             leaf = c(5, 20), drift = FALSE),
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
configs <- merge(candidate_base, data.frame(window = c("expanding", "recent")))
configs$id <- paste(configs$model, configs$window, sep = "__")
naive_config <- data.frame(model = "Naive", family = "Naive", drift = FALSE,
                           window = "expanding", id = "Naive")
configs <- dplyr::bind_rows(naive_config, configs)
save_table(configs, "03_candidate_settings.xlsx", max_print = Inf)
N_FAMILIES <- length(unique(configs$family))
message("Candidate configurations: ", nrow(configs), " in ", N_FAMILIES, " families.")

# Features use rates observed through origin t, with no future observations.
# Return-based inputs avoid dependence on absolute currency price scale.
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
features <- lapply(CURRENCIES, function(cc) make_features(rates[[cc]]))
names(features) <- CURRENCIES

# One-hot currency columns (Currency_USD, Currency_GBP, Currency_EUR) for pooled models.
onehot <- function(cur, n) {
  m <- as.data.frame(matrix(0, nrow = n, ncol = length(CURRENCIES)))
  names(m) <- paste0("Currency_", CURRENCIES)
  m[[paste0("Currency_", cur)]] <- 1
  m
}

# Cache so VAR and pooled models (one fit serves every currency) are fitted once
# per origin and horizon, not once per currency.
pred_cache <- new.env(parent = emptyenv())

fit_arima <- function(s, cfg) {
  fit <- forecast::Arima(s, order = c(cfg$p, 1L, cfg$q), lambda = 0,
                         include.drift = cfg$drift, method = "ML", biasadj = TRUE)
  if (!is.null(fit$code) && fit$code != 0) stop("ARIMA convergence failure")
  fit
}

# Fit one supervised model and predict the h-step log return for the rows in newx.
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

point_predict <- function(cc, i, h, cfg) {
  s <- rates[[cc]]
  if (cfg$family == "Naive") return(s[i])
  first <- window_start(i, cfg$window)

  if (cfg$family == "ARIMA") {
    fit <- fit_arima(s[first:i], cfg)
    # lambda=0 logs internally; biasadj=TRUE gives approximate mean rate forecasts.
    return(as.numeric(forecast::forecast(fit, h = h, biasadj = TRUE)$mean[h]))
  }

  if (cfg$family == "ETS") {
    # Fitted to the log rate; exp() of the forecast is a median-type rate forecast.
    fit <- forecast::ets(log(s[first:i]), model = cfg$ets_model)
    return(as.numeric(exp(forecast::forecast(fit, h = h)$mean[h])))
  }

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

  if (cfg$family %in% c("RF_pooled", "XGB_pooled")) {
    key <- paste(cfg$id, i, h)
    if (!exists(key, envir = pred_cache, inherits = FALSE)) {
      X <- list(); Y <- list(); NEWX <- list()
      for (c2 in CURRENCIES) {
        f2 <- features[[c2]]
        s2 <- rates[[c2]]
        js2 <- seq.int(first, i - h)
        js2 <- js2[complete.cases(f2[js2, , drop = FALSE])]
        stopifnot(length(js2) >= 100, max(js2 + h) <= i,
                  complete.cases(f2[i, , drop = FALSE]))
        X[[c2]] <- cbind(f2[js2, , drop = FALSE], onehot(c2, length(js2)))
        Y[[c2]] <- log(s2[js2 + h] / s2[js2])
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
  y <- log(s[js + h] / s[js])
  x <- f[js, , drop = FALSE]
  newx <- f[i, , drop = FALSE]
  if (cfg$family == "LM" && cfg$feat == "lags") {
    keep <- c("ret_0", paste0("ret_lag", 1:5))
    x <- x[, keep, drop = FALSE]
    newx <- newx[, keep, drop = FALSE]
  }
  set.seed(2026 + i + h)
  yhat <- fit_predict_ml(cfg$family, x, y, newx, cfg, seed = 2026 + i + h)
  # A transformed point prediction; not automatically an expected exchange rate.
  as.numeric(s[i] * exp(yhat))
}

# Explicitly record failed fits. Do not rank partially successful candidates.
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
evaluate_points <- function(cc, ids, candidate_rows, phase) {
  rows <- list()
  for (j in seq_len(nrow(candidate_rows))) {
    cfg <- candidate_rows[j, , drop = FALSE]
    message(phase, ": ", cc, " / ", cfg$id)
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
validation <- dplyr::bind_rows(lapply(CURRENCIES, function(cc) {
  ans <- evaluate_points(cc, val_origins, configs, "validation")
  save_table(ans, paste0("04_validation_predictions_", cc, ".xlsx"))
  ans
}))
validation_scores <- point_scores(validation)
save_table(validation_scores, "04_validation_scores.xlsx", max_print = Inf)
validation_by_year <- validation %>%
  dplyr::mutate(year = as.integer(format(target, "%Y"))) %>%
  dplyr::group_by(currency, h, id, year) %>%
  dplyr::summarise(n = dplyr::n(), failed = sum(!is.finite(pred)),
                   MAE = mean(abs(actual - pred)), RMSE = sqrt(mean((actual - pred)^2)),
                   .groups = "drop")
save_table(validation_by_year, "04_validation_2022_vs_2023.xlsx")
# Choose one configuration per model family / currency / horizon by RMSE.
selected <- validation_scores %>% dplyr::filter(failed == 0, is.finite(RMSE)) %>%
  dplyr::group_by(currency, h, family) %>%
  dplyr::arrange(RMSE, MAE, id, .by_group = TRUE) %>%
  dplyr::slice_head(n = 1) %>% dplyr::ungroup()
if (nrow(selected) != length(CURRENCIES) * length(HORIZONS) * N_FAMILIES) {
  save_table(dplyr::bind_rows(failure_log), "fit_failures.xlsx")
  stop("At least one model family has no fully successful validation candidate. Inspect fit_failures.xlsx.")
}
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
# No model anticipates the jump; they follow the rate only after it appears.
FLOAT_DATE <- as.Date("2022-03-08")
for (cc in CURRENCIES) {
  ids <- selected$id[selected$currency == cc & selected$h == 5L]
  d <- validation %>%
    dplyr::filter(currency == cc, h == 5L, id %in% ids,
                  target >= as.Date("2022-01-15"), target <= as.Date("2022-08-31"))
  act <- dplyr::distinct(d, target, actual)
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
# Same-origin forecasts for every family. No tuning on test scores.
test_rows <- list()
for (k in seq_len(nrow(selected))) {
  pick <- selected[k, ]
  cfg <- configs[configs$id == pick$id, , drop = FALSE]
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
vol_configs <- data.frame(
  id = c("EWMA_094", "EWMA_097", "EWMA_099", "GARCH_normal", "GARCH_student"),
  family = c("EWMA", "EWMA", "EWMA", "GARCH", "GARCH"),
  lambda = c(0.94, 0.97, 0.99, NA, NA),
  distribution = c(NA, NA, NA, "norm", "std"))
# Fixed recent window for volatility; selected point-model windows are separate.
return_history <- function(cc, i) {
  100 * diff(log(rates[[cc]][window_start(i, "recent"):i]))
}
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
variance_predict <- function(r, cfg) {
  if (cfg$family == "EWMA") {
    v <- mean(head(r, min(50L, length(r)))^2)
    for (rr in r) v <- cfg$lambda * v + (1 - cfg$lambda) * rr^2
    return(max(v, 1e-10))
  }
  fit <- fit_garch(r, cfg$distribution)
  v <- as.numeric(sigma(rugarch::ugarchforecast(fit, n.ahead = 1)))^2
  if (length(v) != 1 || !is.finite(v) || v <= 0) stop("Invalid GARCH variance")
  v
}
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
vol_scores <- function(x) {
  x %>% dplyr::group_by(currency, family, id) %>%
    dplyr::summarise(n = dplyr::n(), failed = sum(!is.finite(variance)),
                     QLIKE = mean(QLIKE), MSE = mean(MSE), .groups = "drop")
}
vol_validation <- dplyr::bind_rows(lapply(CURRENCIES, function(cc) {
  message("Volatility validation: ", cc)
  vol_evaluate(cc, val_origins, vol_configs, "validation")
}))
save_table(vol_validation, "07_volatility_validation_predictions.xlsx")
vol_validation_scores <- vol_scores(vol_validation)
save_table(vol_validation_scores, "07_volatility_validation_scores.xlsx", max_print = Inf)
vol_selected <- vol_validation_scores %>% dplyr::filter(failed == 0, is.finite(QLIKE)) %>%
  dplyr::group_by(currency, family) %>% dplyr::arrange(QLIKE, id, .by_group = TRUE) %>%
  dplyr::slice_head(n = 1) %>% dplyr::ungroup()
if (nrow(vol_selected) != length(CURRENCIES) * 2L) {
  stop("No fully successful volatility candidate for at least one family/currency. Inspect 07_volatility_validation_predictions.xlsx.")
}
save_table(vol_selected, "07_frozen_volatility_choices.xlsx", max_print = Inf)
vol_test_rows <- list()
for (k in seq_len(nrow(vol_selected))) {
  pick <- vol_selected[k, ]
  cfg <- vol_configs[vol_configs$id == pick$id, , drop = FALSE]
  vol_test_rows[[k]] <- vol_evaluate(pick$currency, test_origins, cfg, "test")
}
vol_testing <- dplyr::bind_rows(vol_test_rows)
save_table(vol_testing, "08_volatility_test_predictions.xlsx")
save_table(vol_scores(vol_testing), "08_volatility_test_scores.xlsx", max_print = Inf)
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
garch_intervals <- function(cc, i, cfg) {
  fit <- fit_garch(return_history(cc, i), cfg$distribution)
  set.seed(3000 + i + match(cc, CURRENCIES))
  sim <- rugarch::ugarchsim(fit, n.sim = max(HORIZONS), n.start = 0,
                           m.sim = SIMULATIONS, startMethod = "sample")
  rr <- as.matrix(fitted(sim))
  stopifnot(nrow(rr) == max(HORIZONS), ncol(rr) == SIMULATIONS)
  cumulative <- apply(rr / 100, 2, cumsum)
  rows <- lapply(HORIZONS, function(h) {
    # Quantiles in log space avoid unnecessary exponentiation of extreme paths.
    q <- exp(log(rates[[cc]][i]) +
               stats::quantile(cumulative[h, ], c(0.025, 0.5, 0.975), names = FALSE))
    if (any(!is.finite(q))) stop("Non-finite simulated interval")
    data.frame(h = h, lower95 = q[1], median = q[2], upper95 = q[3])
  })
  dplyr::bind_rows(rows)
}
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
interval_scores <- interval_test %>% dplyr::group_by(currency, h, id) %>%
  dplyr::summarise(n = dplyr::n(), failed = sum(!is.finite(lower95) | !is.finite(upper95)),
                   coverage95_pct = 100 * mean(actual >= lower95 & actual <= upper95),
                   mean_width = mean(upper95 - lower95), .groups = "drop")
save_table(interval_test, "09_garch_interval_test_predictions.xlsx")
save_table(interval_scores, "09_garch_interval_test_scores.xlsx", max_print = Inf)
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
# Select the point model using validation, not by picking the best test result.
# Do not assign calendar payment dates: future reporting holidays are unspecified.
last_i <- max(which(rates$date <= TEST_END))
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
    fit <- fit_arima(rates[[cc]][window_start(last_i, cfg$window):last_i], cfg)
    e <- as.numeric(residuals(fit))
    e <- e[is.finite(e)]
    residual_rows[[length(residual_rows) + 1L]] <- data.frame(
      currency = cc, id = cfg$id,
      ljung_box_p = Box.test(e, lag = 20, type = "Ljung-Box", fitdf = cfg$p + cfg$q)$p.value,
      arch_lm_p = unname(arch_lm(e)["p_value"]))
    saveRDS(fit, file.path(OUT, paste0("11_", cc, "_", cfg$id, ".rds")))
  }
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
capture.output(sessionInfo(), file = file.path(OUT, "11_sessionInfo.txt"))
message("DONE. Results saved in: ", normalizePath(OUT))
cat("\nFiles written to the results folder:\n")
print(list.files(OUT))
cat("\nFailures logged:", length(failure_log), "\n")

# 12. HOW TO INTERPRET YOUR OUTPUTS -----------------------------------------
# 02_training_stationarity.xlsx: ADF/KPSS p-values, d from KPSS, and the lags where
#   the return ACF / PACF cross the 95% band (guidance for choosing q and p).
# 02b_arima_aic_bic_ljungbox.xlsx: lecture-style order comparison on training data.
#   Smaller AIC/BIC is better. Ljung-Box p > 0.05 supports white-noise residuals.
#   R2_returns_insample near 0 = daily returns are almost unpredictable.
# 02b_auto_arima_crosscheck.xlsx: the (p,d,q) that auto.arima prefers.
# 02c_seasonality_check.xlsx: seasonal_strength near 0 = no seasonality, so no
#   SARIMA / Holt-Winters / SARIMAX. Suggested report wording:
#   "STL decomposition of monthly mean log rates gave seasonal strength of X,
#    so seasonal models were not fitted. Holt-Winters seasonal smoothing reduces
#    to simple/Holt exponential smoothing without a seasonal component (see ETS)."
# 02d_lecture_style_accuracy.xlsx: accuracy() of the one-shot lecture-style forecast.
# 04_validation_2022_vs_2023.xlsx: compare crisis and subsequent-year performance.
# 05_chosen_point_models.xlsx: your pre-test model choices (RMSE selection).
# 06_test_scores.xlsx: lower MAE/RMSE is better; compare each currency/h separately.
#   R2_vs_naive > 0 means the model beats "no change". R2_level is near 1 for every
#   model (rates trend) and must not be read as forecasting accuracy.
#   Families: Naive, ARIMA, ETS, LM, RF, XGB, plus VAR, RF_pooled, XGB_pooled.
#   Compare RF vs RF_pooled and XGB vs XGB_pooled to see whether pooling helps.
# 08_volatility_test_scores.xlsx: lower QLIKE/MSE is better, not higher.
# 09_garch_interval_test_scores.xlsx: nominal 95% coverage should be near 95%,
#   but short, overlapping samples give uncertain estimates. Consider width too.
# 10_tuition_point_forecasts.xlsx: validation-selected point model results.
# 10_garch_tuition_scenarios.xlsx: separate GARCH median and 95% cost intervals.
# fit_failures.xlsx: inspect failures; affected score rows remain NA, not hidden.
# Do not claim that R-squared or 100-MAPE is a forecasting accuracy percentage.
# Do not claim the models predict a crisis or guarantee the cheapest payment day.
# Bank spreads and transfer charges are not included.
# 5/20-step errors overlap when ORIGIN_STEP < horizon; observations are not
# independent, so avoid naive significance tests using the error rows.
# Initial d=1 diagnostics are a gate, not proof that breaks are solved.
# VAR is fitted to log returns of all currencies; GBP and EUR are derived from USD,
# so the three series are related by construction. Treat VAR as an extension.
# The ML grid is deliberately small, and EGARCH is outside this starter scope.
#
# Documentation checked while preparing this script:
# https://pkg.robjhyndman.com/forecast/reference/Arima.html
# https://pkg.robjhyndman.com/forecast/reference/ets.html
# https://pkg.robjhyndman.com/forecast/reference/auto.arima.html
# https://xgboost.readthedocs.io/en/latest/r_docs/R-package/docs/reference/xgb.train.html
# https://alexiosg.r-universe.dev/rugarch/doc/manual.html
# https://otexts.com/fpp3/tscv.html
