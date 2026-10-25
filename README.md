# Exchange-Rate Forecasting for Overseas Tuition Planning

Forecasting exchange rates and volatility to help Sri Lankan students estimate the rupee cost and uncertainty of overseas tuition payments.

Developed for **CM3023 — Financial Time Series Analysis** by third-year **Data Science and Business Analytics undergraduates at General Sir John Kotelawala Defence University (KDU)**.

## Project overview

Sri Lankan students paying overseas tuition face exchange-rate uncertainty. Even when a university’s foreign-currency fee remains unchanged, the amount needed in Sri Lankan rupees can increase or decrease.

This project compares traditional time-series and machine-learning models for forecasting **LKR per USD, GBP and EUR**, then converts those forecasts into illustrative tuition-payment budgets.

Forecast horizons are **5 and 20 reporting days ahead**. Reporting days refer to observed dates in the dataset, rather than calendar days.

The project also models volatility and produces simulated tuition-cost ranges to support budgeting.

## Objectives

- Examine exchange-rate trends, stationarity, autocorrelation and volatility.
- Compare traditional and machine-learning models against a naïve forecast.
- Select models using chronological validation.
- Evaluate selected models using walk-forward testing.
- Compare EWMA and GARCH volatility forecasts.
- Convert exchange-rate forecasts into tuition costs in LKR.
- Present the results through an interactive tuition-cost dashboard.

## Dataset

The processed dataset contains **6,518 reporting-day observations**, covering **3 January 2000 to 31 December 2025**.

Its columns are:

| Column | Meaning |
| --- | --- |
| `date` | Observation date |
| `USD` | LKR per US dollar |
| `GBP` | LKR per British pound |
| `EUR` | LKR per euro |

### Data sources

The project uses the following Federal Reserve Economic Data (FRED) series:

| Series | Original measurement | Conversion |
| --- | --- | --- |
| [DEXSLUS](https://fred.stlouisfed.org/series/DEXSLUS) | LKR per USD | LKR per USD = DEXSLUS |
| [DEXUSUK](https://fred.stlouisfed.org/series/DEXUSUK) | USD per GBP | LKR per GBP = DEXSLUS × DEXUSUK |
| [DEXUSEU](https://fred.stlouisfed.org/series/DEXUSEU) | USD per EUR | LKR per EUR = DEXSLUS × DEXUSEU |

The GBP and EUR series are derived cross-rates. They do not represent the exact selling rates charged by Sri Lankan banks.

### Data preparation

The preparation workflow:

- Converts dates and exchange rates into suitable formats.
- Checks missing values, duplicate dates and invalid rates.
- Aligns the three source series by date.
- Calculates the GBP/LKR and EUR/LKR cross-rates.
- Retains observations from 2000 onward.
- Saves the combined dataset for analysis.

Missing reporting dates are not replaced with artificial weekend observations. Genuine large exchange-rate movements, including those in 2022, are retained.

## Methodology

### Chronological data split

| Period | Purpose |
| --- | --- |
| 2000–2021 | Initial training and diagnostics |
| 2022–2023 | Validation and model selection |
| 2024–2025 | Final testing |

The data are split chronologically, rather than randomly.

Model configurations are selected using **validation RMSE** and fixed before final testing. Models are refitted at each forecast origin using only information available at that time.

The full evaluation uses origins spaced five observations apart and compares:

- Expanding training windows.
- Recent training windows containing 1,250 observations.

The 2022 exchange-rate shock remains in the validation period. Updating models as new observations become available helps them adapt, but does not mean an unexpected shock could have been predicted in advance.

### Log transformations and features

Log transformations express exchange-rate movements on a proportional scale. Log returns are used to analyse changes and volatility, while forecasts are converted back into exchange-rate levels for evaluation and tuition calculations.

Machine-learning features include:

- Current and lagged log returns.
- Rolling mean returns.
- Rolling standard deviations.
- Deviations from rolling log-rate means.

Pooled machine-learning models also include **one-hot-encoded currency indicators**, allowing a shared model to distinguish USD, GBP and EUR.

Features are constructed using information available at the forecast origin.

### Exchange-rate forecasting models

| Model family | Description |
| --- | --- |
| Naïve / random walk | Future rate equals the latest observed rate |
| ARIMA | Candidate specifications fitted to log exchange rates |
| Exponential smoothing | Simple exponential smoothing and automatic non-seasonal ETS |
| Linear regression | Models using lagged returns and other constructed features |
| Random Forest | Currency-specific models |
| XGBoost | Currency-specific boosted-tree models |
| VAR | Joint modelling of the three return series |
| Pooled Random Forest | Shared model with currency indicators |
| Pooled XGBoost | Shared boosted-tree model with currency indicators |

Training diagnostics include stationarity tests, ACF/PACF plots and residual checks.

### Volatility modelling

Volatility is evaluated separately from exchange-rate point forecasting.

The models compared are:

- **EWMA**, using candidate decay factors.
- **GARCH(1,1)**, using normal and Student-t innovations.

Volatility configurations are selected using validation QLIKE. Forecast variances are evaluated against squared returns, which are a noisy proxy for unobserved volatility.

### Probabilistic forecasts

GARCH simulations produce 95% exchange-rate ranges. These are converted into tuition-cost ranges by multiplying by the specified foreign-currency fee.

The GARCH ranges are separate simulation-based scenarios. They are not prediction intervals for the selected Random Forest, ARIMA or ETS point forecasts.

## Evaluation metrics

| Metric | Purpose |
| --- | --- |
| MAE | Average absolute exchange-rate forecast error |
| RMSE | Forecast error measure that gives greater weight to large errors |
| R² versus naïve | Improvement in squared error relative to the naïve benchmark |
| RMSE improvement (%) | Percentage reduction in RMSE compared with naïve |
| QLIKE | Volatility forecast loss |
| Volatility MSE | Squared error against the squared-return proxy |
| Interval coverage | Percentage of actual rates inside the forecast range |
| Mean interval width | Average width of the forecast range |

Lower MAE, RMSE and volatility losses indicate better performance.

Positive RMSE improvement means a model outperformed the naïve forecast. Negative improvement means it performed worse. These percentages are not classification accuracy scores.

## Main results

The following models were selected using **2022–2023 validation data** and evaluated on **2024–2025 test data**.

| Currency | Horizon | Validation-selected model | Test RMSE | RMSE improvement over naïve |
| --- | --- | --- | ---: | ---: |
| USD | 5 reporting days | Pooled Random Forest, expanding window | 1.6706 | 0.27% |
| GBP | 5 reporting days | Pooled Random Forest, expanding window | 4.5327 | −4.04% |
| EUR | 5 reporting days | Pooled Random Forest, expanding window | 3.6850 | 0.10% |
| USD | 20 reporting days | ARIMA(1,1,1), recent window | 3.4980 | 7.77% |
| GBP | 20 reporting days | Automatic ETS, recent window | 8.5528 | 1.61% |
| EUR | 20 reporting days | Automatic ETS, recent window | 7.8823 | 1.68% |

### Interpretation

- No model consistently outperformed the others across all currencies and horizons.
- Five-day improvements over the naïve benchmark were small.
- The selected five-day GBP model performed worse than naïve.
- The selected twenty-day models achieved larger improvements.
- Machine learning did not consistently outperform traditional models.
- Volatility-model performance varied by currency.
- USD twenty-day GARCH intervals showed coverage below the nominal 95% target.

The models used in the tuition application remain those selected during validation. They are not replaced retrospectively using test-period results.

## Project organisation

| File or folder | Purpose |
| --- | --- |
| `data/raw/` | Original exchange-rate datasets |
| `data/processed/` | Prepared datasets |
| `01_data_preparation.R` | Data cleaning, alignment and cross-rate construction |
| `02_exploratory_data_analysis.R` | Exploratory analysis and visualisations |
| `03_exchange_rate_modelling.R` | Model fitting, validation, testing and tuition scenarios |
| `dashboard.R` | Interactive tuition-cost dashboard |
| `results/` | Saved Excel results and figures |
| `Documents/` | Project report and supporting documents |
| `README.md` | Project description and running instructions |

## How to run the project

### 1. Download or clone the repository

Open the repository folder in RStudio.

Keep the working directory at the repository root so relative file paths work correctly.

### 2. Prepare the dataset

Run:

```r
# Clean and combine the raw exchange-rate data
source("01_data_preparation.R")
```

If the processed dataset is already supplied, it can be used directly.

### 3. Run the exploratory analysis

```r
# Produce exploratory summaries and graphs
source("02_exploratory_data_analysis.R")
```

### 4. Check the modelling settings

In `03_exchange_rate_modelling.R`, confirm that the dataset path matches its location:

```r
# Location of the prepared dataset
DATA_FILE <- "data/processed/exchange_rates_lkr.csv"
```

Review the main settings:

```r
# Use FALSE for the full analysis
QUICK_RUN <- FALSE

# Include joint and pooled model comparisons
RUN_VAR <- TRUE
RUN_POOLED <- TRUE

# Forecast horizons in reporting days
HORIZONS <- c(5L, 20L)

# Illustrative foreign-currency tuition amounts
TUITION <- c(USD = 5000, GBP = 5000, EUR = 5000)
```

A quick run is intended for checking the workflow and does not reproduce the complete reported analysis.

### 5. Run the modelling script

```r
# Fit models and save evaluation results
source("03_exchange_rate_modelling.R")
```

Alternatively, run its numbered sections in order using **Ctrl + Enter**.

The full walk-forward analysis can take considerable time. An internet connection is required when installing missing packages.

If the training-only KPSS check stops the script, review the saved diagnostics before changing the configured ARIMA differencing assumption.

### 6. Open the dashboard

In `dashboard.R`, set `RUN_FOLDER` to the folder containing the saved tuition forecasts. For example:

```r
# Folder containing the tuition forecast Excel files
RUN_FOLDER <- "results/run_20261024_113027"
```

Then run:

```r
# Launch the interactive tuition calculator
source("dashboard.R")
```

The dashboard requires:

- `10_tuition_point_forecasts.xlsx`
- `10_garch_tuition_scenarios.xlsx`

Keep these files in the expected results folder.

## Software and packages

The project uses **R and RStudio**.

Main modelling packages include:

```r
c(
  "readr", "dplyr", "ggplot2", "forecast", "tseries",
  "zoo", "ranger", "xgboost", "rugarch", "vars", "writexl"
)
```

The tuition dashboard also uses `shiny` and `readxl`.

R and package versions are recorded in the saved session information.

## Saved results

The modelling workflow saves tables as Excel files and figures as PNG images.

Important outputs include:

| Output | Contents |
| --- | --- |
| `04_validation_scores.xlsx` | Validation performance |
| `05_chosen_point_models.xlsx` | Models selected using validation |
| `06_test_scores.xlsx` | Final test performance |
| `08_volatility_test_scores.xlsx` | Volatility forecast evaluation |
| `09_garch_interval_test_scores.xlsx` | Interval coverage and width |
| `10_tuition_point_forecasts.xlsx` | Point forecasts and LKR tuition costs |
| `10_garch_tuition_scenarios.xlsx` | Simulated tuition-cost ranges |
| `fit_failures.xlsx` | Recorded fitting failures |
| `11_sessionInfo.txt` | R and package versions |

Excel files can be downloaded and opened in Excel or another compatible spreadsheet application. PNG figures can be viewed directly in the repository.

## Tuition-cost dashboard

The dashboard allows users to:

- Select USD, GBP or EUR.
- Select a 5- or 20-reporting-day horizon.
- Enter a foreign-currency tuition fee.
- View the estimated tuition cost in LKR.
- Compare the latest rate with the forecast rate.
- View the GARCH-based low and high scenarios.

**Estimated tuition cost in LKR = foreign-currency fee × forecast exchange rate.**

The supplied forecasts are issued **as of 31 December 2025**. They are historical forecast-origin scenarios, rather than live forecasts.

Changing the fee in the dashboard recalculates the tuition amount using saved forecasts; it does not retrain the models.

## Limitations

- Unexpected shocks and exchange-rate regime changes can reduce forecast reliability.
- The three currency series share an LKR component and are related.
- Only a limited set of model configurations was evaluated.
- Overlapping forecast horizons create dependence between forecast errors.
- Squared returns are noisy proxies for volatility.
- Simulated intervals do not include all sources of uncertainty.
- Tuition scenarios assume the foreign-currency fee remains unchanged.
- Bank spreads, transfer charges, living costs and fee increases are excluded.

The project supports budgeting and risk awareness. It does not guarantee savings or identify the cheapest payment date.

## Academic context and acknowledgements

**Module:** CM3023 — Financial Time Series Analysis  
**Programme:** Data Science and Business Analytics — Third Year  
**Department:** Computational Mathematics  
**Faculty:** Computing  
**University:** General Sir John Kotelawala Defence University, Sri Lanka  

We sincerely thank **Ms. KHAS Harischandra** for her guidance, feedback and encouragement throughout this assignment. We also acknowledge the Federal Reserve Bank of St. Louis for providing access to FRED data and the developers of the open-source software used in this project.
