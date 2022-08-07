# Time-Series-Forecasting-Project

## Goal: Build a time series model that predicts PM2.5 daily levels
* 1. Obtain PM2.5 measures in Beijing from March 2013 to June 2017
* 2. Fit 4 models: ARIMA, ARIMA errors, VAR, and TBATS to the PM2.5 time series
* 3. Cross-validate best fitted model from each model type
* 4. Recommend the best performing model

## Methodology and Tools
* The Decomposition
    * Pythons statsmodels function seasonal_decompose
* Hypothesis Testing
  * Augmented Dickey-Fuller Test
  * KPSS Test for Level Stationarity
  * KPSS Test for Trend Stationarity
* Transformation
  * Box Cox Transformation
  * Differencing
* Time Series Models
  * ARIMA Model
  * Regression with ARIMA Errors
  * VAR Model
  * TBATS Model
* Model Evaluation
  * Cross Validation with Sliding Window & Expanding Window
  * MAE over 30 horizon
  * AIC over 80 CV iterations
## Conclusion
* TBATS was the best on predicting PM2.5. It was an automated model, so almost no adjustments was made.
* Rain doesn’t have much correlation with PM2.5, temperature, and pressure, making it less useful as a predictor. The other three set of time series data can be used to predict each other due to correlation.
* ARIMA model does not capture other variables correlations with PM2.5.
* Regression with ARIMA errors better captures correlations than 34 ARIMA but there are still patterns in the data that are not exploited by the model.
* VAR is good at predicting temperature and pressure but not the PM2.5.
## Improvement and Future Work
* Try advanced models:
  * RNN, ARCH, and GARCH
* Better data selection:
  * Try using other data combinations to train the VAR model when predict PM 2.5
  * Consider using alternative data sources
* Try using different tools:
  * Meta has an interesting library - Prophet
  * Get some experience with Python time series (for practice)
