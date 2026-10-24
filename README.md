# Time_Series_Project
# Exchange-Rate Forecasting for Overseas Tuition Planning

Forecasting **USD/LKR, GBP/LKR and EUR/LKR** exchange rates to help Sri Lankan students understand the rupee cost and uncertainty of overseas tuition payments.

Developed by third-year **Data Science and Business Analytics undergraduates at General Sir John Kotelawala Defence University (KDU)** for **CM3023 — Financial Time Series Analysis**.

## Project overview

Even when a university's tuition fee stays unchanged, its cost in Sri Lankan rupees changes with the exchange rate. This project compares statistical and machine-learning forecasting methods, evaluates exchange-rate volatility, and translates forecasts into illustrative tuition-payment budgets.

The analysis covers **5- and 20-reporting-day horizons**. Reporting days are observed dates in the dataset, rather than calendar days.

## Objectives

- Explore exchange-rate trends, stationarity, autocorrelation and volatility.
- Compare forecasting models against a naïve, no-change benchmark.
- Evaluate performance using chronological validation and walk-forward testing.
- Examine the effect of the large exchange-rate movements observed in 2022.
- Translate exchange-rate forecasts and simulated uncertainty into LKR tuition-cost scenarios.

## Dataset

The supplied processed dataset contains **6,518 observations**, from **3 January 2000 to 31 December 2025**, with columns `date`, `USD`, `GBP` and `EUR`. All three rate columns represent **LKR per one unit of foreign currency**.

The project uses the following Federal Reserve Economic Data (FRED) series:

| Source series | Original measurement | Conversion used |
| --- | --- | --- |
| [DEXSLUS](https://fred.stlouisfed.org/series/DEXSLUS) | LKR per USD | USD/LKR = DEXSLUS |
| [DEXUSUK](https://fred.stlouisfed.org/series/DEXUSUK) | USD per GBP | GBP/LKR = DEXSLUS × DEXUSUK |
| [DEXUSEU](https://fred.stlouisfed.org/series/DEXUSEU) | USD per EUR | EUR/LKR = DEXSLUS × DEXUSEU |

GBP/LKR and EUR/LKR are derived cross-rates. They are not executable bank selling rates.

The modelling script checks for missing values, duplicate dates, non-numeric rates and non-positive values. It retains genuine 2022 observations and does not pad weekends or remove large movements as outliers.

## Methodology

### Chronological evaluation

| Period | Purpose |
| --- | --- |
| 2000–2021 | Initial training and diagnostics |
| 2022–2023 | Validation and model selection |
| 2024–2025 | Final walk-forward testing |

Models are selected using **validation RMSE**, with their configurations fixed before final testing. At each walk-forward origin, models are refitted using information available at that time. The full run uses origins spaced five observations apart and compares expanding windows with recent windows of 1,250 observations.

The 2022 exchange-rate shock remains in the validation period. Separate 2022 and 2023 validation summaries help examine performance across different conditions. Updating models after new observations become available does not imply that an unexpected shock could have been predicted in advance.

### Exchange-rate models

| Model family | Implementation |
| --- | --- |
| Naïve / random walk | Latest observed rate |
| ARIMA | Five candidate specifications on log rates, including a drift variant |
| Exponential smoothing | Simple exponential smoothing and automatic non-seasonal ETS |
| Linear regression | Lagged-return and broader feature specifications |
| Random Forest | Currency-specific models using `ranger` |
| XGBoost | Currency-specific boosted-tree models |
| VAR | Joint modelling of the three return series |
| Pooled Random Forest and XGBoost | Shared models with currency indicator features |

Features include lagged log returns, rolling means, rolling standard deviations and deviations from rolling log-rate means. Training diagnostics include ADF/KPSS tests, ACF/PACF plots, ARIMA information criteria and residual checks. `auto.arima()` is also used as a cross-check.

### Volatility and uncertainty

- EWMA variance forecasts with candidate decay factors.
- GARCH(1,1) with normal and Student-t innovations.
- Validation-based volatility selection using QLIKE.
- GARCH simulations for 95% exchange-rate and tuition-cost intervals.
- Interval backtesting using empirical coverage and mean width.

GARCH intervals are separate simulation-based scenarios; they are not prediction intervals for the selected Random Forest, ARIMA or ETS point model.

### Evaluation metrics

- **MAE and RMSE:** exchange-rate forecast errors, measured in LKR per foreign-currency unit.
- **R² versus naïve:** `1 − SSE(model) / SSE(naïve)`; positive values indicate improvement over the benchmark.
- **QLIKE and MSE:** volatility losses evaluated against squared-return proxies.
- **95% interval coverage and width:** observed uncertainty performance.

A high R² on exchange-rate levels alone does not establish useful forecasting performance. Benchmark-relative errors are the main comparison.

## Results reported in the assignment

The following values come from the supplied completed report and have not been independently rerun for this README. These are the models selected during validation, evaluated on the final test period.

| Currency | Horizon | Validation-selected model | Test RMSE | RMSE improvement over naïve |
| --- | --- | --- | ---: | ---: |
| USD/LKR | 5 reporting days | Pooled Random Forest, expanding window | 1.6706 | 0.27% |
| GBP/LKR | 5 reporting days | Pooled Random Forest, expanding window | 4.5327 | −4.04% |
| EUR/LKR | 5 reporting days | Pooled Random Forest, expanding window | 3.6850 | 0.10% |
| USD/LKR | 20 reporting days | ARIMA(1,1,1), recent window | 3.4980 | 7.77% |
| GBP/LKR | 20 reporting days | Automatic ETS, recent window | 8.5528 | 1.61% |
| EUR/LKR | 20 reporting days | Automatic ETS, recent window | 7.8823 | 1.68% |

Positive improvement means lower RMSE than naïve; negative improvement means worse performance. These are not classification accuracy percentages.

No model consistently dominated across all currencies and horizons. Five-day improvements were small, and the selected GBP model underperformed the benchmark. The selected twenty-day models showed larger gains. The report also found that volatility rankings and interval coverage varied by currency, including undercoverage for USD twenty-day intervals.

## Project files

The supplied repository archive contains the preparation and EDA files below. The modelling script and completed report were supplied separately and should be added to the repository if they are not already present.

| Path | Purpose |
| --- | --- |
| `data/raw/` | Original exchange-rate CSV files |
| `data/processed/exchange_rates_lkr.csv` | Combined exchange-rate dataset |
| `notebooks/01_data_preparation.ipynb` | Python data-preparation notebook |
| `notebooks/02_exploratory_data_analysis.ipynb` | Python EDA notebook |
| `R/01_data_preparation.R` | R data-preparation script |
| `R/02_exploratory_data_analysis.R` | R EDA script |
| `R/helpers.R` | Shared R helpers |
| `exchange_rate_modelling_v3.R` | Main modelling and evaluation script; add at repository root |
| `FTS_Assignment_completed_copy.docx` | Completed assignment report; supplied separately |
| `results/run_YYYYMMDD_HHMMSS/` | Output directory created by each modelling run |

## How to run the modelling workflow

### 1. Open the project

Download or clone the repository and open its root folder as an RStudio project. Place `exchange_rate_modelling_v3.R` in that folder.

### 2. Set the dataset path

In section 2 of the script, point `DATA_FILE` to the processed dataset:

```r
DATA_FILE <- "data/processed/exchange_rates_lkr.csv"
```

Alternatively, place the separately supplied CSV beside the script and rename `exchange_rates_lkr(1).csv` to `exchange_rates_lkr.csv` to use the script's default path.

### 3. Review the run settings

```r
QUICK_RUN <- FALSE
RUN_VAR <- TRUE
RUN_POOLED <- TRUE
HORIZONS <- c(5L, 20L)
TUITION <- c(USD = 5000, GBP = 5000, EUR = 5000)
```

Set `QUICK_RUN <- TRUE` only for a faster rehearsal: it uses USD, fewer origins, and skips VAR and pooled extensions. Use `FALSE` for the full analysis and reported model comparisons.

The tuition amounts are hypothetical inputs and can be changed to the fees being considered.

### 4. Run the script in order

Run the numbered sections in RStudio using **Ctrl + Enter**. Section 1 installs missing packages, so an internet connection is required for package installation. Full walk-forward fitting can take considerable time.

Main dependencies:

```r
c("readr", "dplyr", "ggplot2", "forecast", "tseries", "zoo",
  "ranger", "xgboost", "rugarch", "vars", "writexl")
```

The script stops if its training-period KPSS check does not support the configured `d = 1` ARIMA family. Review the saved diagnostics before changing that modelling assumption.

### 5. Inspect the outputs

Each run creates a timestamped folder under `results/`. Tables are printed in the console and saved as Excel files; figures are displayed in RStudio and saved as PNGs.

| Output | Contents |
| --- | --- |
| `04_validation_scores.xlsx` | Validation model comparisons |
| `05_chosen_point_models.xlsx` | Validation-selected models |
| `06_test_scores.xlsx` | Final test performance |
| `08_volatility_test_scores.xlsx` | Volatility evaluation |
| `09_garch_interval_test_scores.xlsx` | Interval coverage and width |
| `10_tuition_point_forecasts.xlsx` | Point forecasts converted into LKR tuition costs |
| `10_garch_tuition_scenarios.xlsx` | Simulated tuition-cost ranges |
| `fit_failures.xlsx` | Recorded model-fitting failures |
| `11_sessionInfo.txt` | R and package versions for reproducibility |

Additional outputs include diagnostics, prediction tables, plots, run settings and fitted model objects. The base random seed is `2026`.

## Tuition-cost application

**Estimated tuition cost in LKR = foreign-currency tuition fee × forecast exchange rate.**

For an illustrative USD 5,000 fee, the application multiplies each USD/LKR forecast by 5,000. Simulated rate quantiles are converted into tuition-cost ranges in the same way.

The script's final scenarios are issued **as of 31 December 2025**. They are historical forecast-origin scenarios, not live exchange-rate forecasts. Their horizons refer to subsequent reporting observations, not assigned calendar payment dates.

## Limitations

- Unexpected shocks and changes in exchange-rate regimes can reduce forecast reliability.
- The three series share an LKR component, so their movements are related.
- The evaluated model families use limited candidate configurations.
- Overlapping forecast horizons create dependence between errors.
- Squared returns are noisy proxies for unobserved volatility.
- Simulated intervals omit parameter uncertainty and unforeseen regimes.
- Tuition scenarios assume fixed foreign-currency fees and exclude bank spreads, transfer charges, living costs and fee increases.

The project supports budgeting analysis. It does not demonstrate realised savings or establish the cheapest payment date.

## Academic context and acknowledgements

**Module:** Financial Time Series Analysis (CM3023)  
**Programme:** Data Science and Business Analytics — Third Year  
**Department:** Computational Mathematics  
**Faculty:** Computing  
**University:** General Sir John Kotelawala Defence University, Sri Lanka

We sincerely thank **Ms. Khas Harischandra** for her guidance, feedback and encouragement throughout this assignment. We also acknowledge the Federal Reserve Bank of St. Louis for providing FRED data, the developers of the open-source software used in the analysis, and all group members for their contributions.
