# Improving Taylor Rule Forecasts via ECM & ARDL Models

This repository provides an econometric analysis and R implementation for enhancing monetary policy forecasts using the Taylor Rule. The methodology applies Error Correction Models (ECM), Autoregressive Distributed Lag (ARDL), and ARIMA specifications, utilizing the Engle-Granger two-step method to test for co-integration and model short- vs. long-run dynamics in macroeconomic data.

---

## 📄 Repository Structure

* **`Improvement_of_Taylor_rule_forecast_ECM_ARDL.R`**  
  R script containing data preprocessing, ADF unit root tests, Engle-Granger cointegration testing, model estimation (ECM, ARDL, ARIMA), expanding window forecasting, and out-of-sample forecast evaluation.

* **`Improvement_of_Taylor_rule_forecast_ECM_ARDL.pdf`**  
  Seminar paper detailing the theoretical framework, empirical findings (Engle-Granger cointegration, ECM performance), and out-of-sample forecast evaluation using RMSE, MAE, MSE, and Clark and West (2007) tests.

---

## 🛠️ Requirements & R Packages

To run the script, ensure you have R installed along with the required packages:

```R
# Install required dependencies
install.packages(c("dynamac", "ARDL", "vars", "tseries", "urca", "forecast", "ggplot2", "tidyverse"))
