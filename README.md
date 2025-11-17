# DS-340W Fantasy Football Prediction

This repository contains the code, data, and outputs for a DS 340W (Computational / Data Science) project on forecasting fantasy football performance using time-series and machine learning models in **R**.

The core goal is to build **player-level weekly fantasy projections** and compare different modeling approaches:

- **Univariate ARIMA** (baseline)
- **ARIMAX** with contextual/exogenous features (opponent, game environment, etc.)
- **Neural network time-series models** with exogenous inputs

The project focuses on **QB/RB/WR/TE/K/DEF** fantasy scoring and evaluates models using historical data and backtesting.

---

## Repository Structure

**Top-level R scripts**

- `DS340_Parent_Code.R`  
  Main end-to-end pipeline:
  - Loads and cleans the fantasy football dataset.
  - Builds per-player time series.
  - Fits forecasting models (ARIMA / ARIMAX / NN, depending on configuration).
  - Runs backtests over historical weeks.
  - Writes forecasts and evaluation summaries to CSV.

- `ARIMAX_DS340W_Code.R`  
  Standalone script focusing on **ARIMAX** models:
  - Uses player fantasy history as the response.
  - Adds contextual/exogenous regressors (e.g., opponent indicators, game features).
  - Produces offense forecasts and evaluation outputs for ARIMAX only.

- `NN_340w_Code.R`  
  Standalone script focusing on the **neural network time-series** model:
  - Uses player history + the same/similar exogenous features as ARIMAX.
  - Trains and evaluates the NN model for player-week fantasy projections.
  - Writes out NN forecast CSVs.

- `ARIMA_ARIMAX_NN_340W_Comparison.R`  
  Comparison script:
  - Reads model outputs (ARIMA, ARIMAX, NN) from CSV files.
  - Joins them on player-week.
  - Computes and summarizes forecast error metrics and “win rate” style comparisons between models.
  - Can be used to generate tables/plots for the final report.

- `NN_ARIMAX_340W_Comparison.R`  
  Focused comparison of **ARIMAX vs Neural Network**:
  - Loads the relevant forecast files.
  - Compares performance head-to-head across players and weeks.
  - Useful for isolating the effect of adding non-linear structure (NN) vs linear ARIMAX.

- `DS340W_scratchwork.R`  
  Scratch / exploratory script:
  - Ad-hoc analysis.
  - Prototype code snippets, feature engineering ideas, etc.

- `Tempcode_DS340W.R`  
  Temporary experimentation script:
  - Extra tests, debugging, or quick model variants that did not make it into the main pipeline.

**Data & outputs**

- `cleaned_fantasy_football_data.xlsx`  
  - Cleaned player-week fantasy dataset used as the main modeling input  
    (player identifiers, week, position, fantasy points, and contextual features).

- Forecast output files (examples):
  - `forecast_offense.csv`
  - `forecast_offense_ARIMAX_xreg.csv`
  - `forecast_offense_ARIMA_uni.csv`
  - `forecast_offense_NN_xreg.csv`
  - `forecast_offense_arimax_fast.csv`
  - `forecast_offense_nn_fast.csv`
  - `forecast_kickers.csv`
  - `forecast_defense.csv`

  These contain model predictions by player and week for different model variants.

- Backtest summary files:
  - `backtes
