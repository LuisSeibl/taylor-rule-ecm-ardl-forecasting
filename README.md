# Improving Taylor Rule Forecasts via ECM & ARDL Models

This repository provides an econometric analysis and R implementation for enhancing monetary policy forecasts using the Taylor Rule. The methodology applies **Error Correction Models (ECM)** and **Autoregressive Distributed Lag (ARDL)** bounds testing to model co-integration and short- vs. long-run dynamics in macro-data[cite: 1].

---

## 📄 Repository Structure

* `Improvement_of_Taylor_rule_forecast_ECM_ARDL.R`: R script containing data preprocessing, ARDL/ECM model estimation, bounds testing, and forecasting evaluation[cite: 1].
* `Improvement_of_Taylor_rule_forecast_ECM_ARDL.pdf`: Accompanying research paper/documentation detailing the theoretical framework, empirical findings, and forecast evaluation[cite: 1].

---

## 🛠️ Requirements & R Packages

To run the script, ensure you have R installed along with the required packages:

```R
# Install required dependencies
install.packages(c("dynamac", "ARDL", "vars", "tseries", "urca", "forecast", "ggplot2", "tidyverse"))
