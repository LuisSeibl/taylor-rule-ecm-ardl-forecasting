#Load packages
library(readr)
library(stats)
library(readxl)
library(urca)
library(tseries)
library(ARDL)
library(forecast)
library(dynlm)
library(Metrics)
library(remotes)
library(oosanalysis)

#Load data
Fedfund <- read_csv("C:/Users/gijsn/Desktop/Documents/Macroeconometrics/Fedfund.csv")
Fedfunds <- Fedfundsrate[, 2]
ts_fedfunds <- ts(Fedfunds, start = 1959, frequency = 1)

GDPCA <- read_excel("C:/Users/gijsn/Desktop/Documents/Macroeconometrics/GDPCA.xlsx")
GDP <- GDPCA[, 2]

Inflat <- read_excel("C:/Users/gijsn/Desktop/Documents/Macroeconometrics/Inflat.xlsx")
inflation <- Inflat[, 2]

ts_gdp <- ts(GDP, start = 1959, end = 2024, frequency = 1)
ts_inflation <- ts(inflation, start = 1959, end = 2024, frequency = 1)

#OLS
reg <- lm(ts_fedfunds ~ ts_inflation + ts_gdp)
summary(reg)

#Save residuals and check stationarity
resid <- resid(reg)
plot(resid)
test3 <- ur.df(resid, type = "none", selectlags = "BIC")
summary(test3)
adf.test(resid)
kpss_test <- kpss.test(resid, null = "Level")

#Split data
train.start <- 1959
train.end <- 2018
test.start <- 2019
test.end <- 2024
end <- 2024

ts_gdp <- ts(GDP, start = 1959, end = end, frequency = 1)
ts_fedfunds <- ts(Fedfunds, start = 1959, end = end, frequency = 1)
ts_inflation <- ts(inflation, start = 1959, end = end, frequency = 1)

train.gdp <- ts(GDP, start = train.start, end = train.end, frequency = 1)
train.fedfunds <- ts(Fedfunds, start = train.start, end = train.end, frequency = 1)
train.inflation <- ts(inflation, start = train.start, end = train.end, frequency = 1)

training.data <- data.frame(
  fedfunds = as.numeric(train.fedfunds),
  inflation = as.numeric(train.inflation),
  gdp = as.numeric(train.gdp)
)

test.gdp <- ts(GDP, start = test.start, end = test.end, frequency = 1)
test.fedfunds <- ts(Fedfunds, start = test.start, end = test.end, frequency = 1)
test.inflation <- ts(inflation, start = test.start, end = test.end, frequency = 1)

#Autoregressive Distributed Lag models
maxlength <- 5
auto_model <- auto_ardl(fedfunds ~ inflation + gdp,
                        data = training.data,
                        max_order = c(maxlength, maxlength, maxlength),
                        selection = "BIC")
best_order <- auto_model$best_order

#Max leg length (p,d,q)
max_p <- 5
max_d <- 1
max_q <- 0
max_lags <- 5

a <- 1959
b <- 2024

full_data <- data.frame(
  year = a:b,
  fedfunds = as.numeric(ts_fedfunds),
  inflation = as.numeric(ts_inflation),
  gdp = as.numeric(ts_gdp)
)

#Forecast Autoregressive Distributed lag, ARIMA and Error correction models
forecast_results <- list()

for (forecast_year in 2019:2024) {
  train_data <- subset(full_data, year <= forecast_year - 1)
  test_data <- subset(full_data, year == forecast_year)
  
  y_train <- ts(train_data$fedfunds, start = 1959, frequency = 1)
  x1_train <- ts(train_data$inflation, start = 1959, frequency = 1)
  x2_train <- ts(train_data$gdp, start = 1959, frequency = 1)
  
  y_test <- as.numeric(test_data$fedfunds)
  x1_test <- as.numeric(test_data$inflation)
  x2_test <- as.numeric(test_data$gdp)
  
  for (p in 0:max_p) {
    for (d in 0:max_d) {
      for (q in 0:max_q) {
        fit <- tryCatch(Arima(y_train, order = c(p, d, q)), error = function(e) NULL)
        if (!is.null(fit)) {
          forecast_val <- predict(fit, n.ahead = 1)$pred
          model_name <- paste0("ARIMA(", p, ",", d, ",", q, ")")
          forecast_results[[model_name]] <- c(forecast_results[[model_name]], forecast_val)
        }
      }
    }
  }
  
  for (p in 1:max_lags) {
    for (q in 1:max_lags) {
      for (r in 1:max_lags) {
        tryCatch({
          ardl_model <- ardl(fedfunds ~ inflation + gdp, data = train_data, order = c(p, q, r))
          needed_lags <- max(p, q, r)
          last_rows <- tail(train_data, needed_lags)
          predict_data <- rbind(last_rows, test_data)
          forecast_val <- predict(ardl_model, newdata = predict_data)
          model_name <- paste0("ARDL(", p, ",", q, ",", r, ")")
          forecast_results[[model_name]] <- c(forecast_results[[model_name]], forecast_val[length(forecast_val)])
        }, error = function(e) NULL)
      }
    }
  }
  
  successful_ecms <- c()
  
  for (p in 1:max_lags) {
    for (q in 1:max_lags) {
      for (r in 1:max_lags) {
        model_name <- paste0("ECM(", p, ",", q, ",", r, ")")
        if (model_name %in% successful_ecms) next
        
        tryCatch({
          long_run_model <- lm(fedfunds ~ inflation + gdp, data = train_data)
          eq_errors <- residuals(long_run_model)
          train_data$eq_error <- eq_errors
          
          ecm_data <- data.frame(
            d_fedfunds = c(NA, diff(train_data$fedfunds)),
            L_eq_error = c(NA, eq_errors[-length(eq_errors)]),
            d_inflation = c(NA, diff(train_data$inflation)),
            d_gdp = c(NA, diff(train_data$gdp))
          )
          ecm_data <- na.omit(ecm_data)
          ecm_model <- lm(d_fedfunds ~ L_eq_error + d_inflation + d_gdp, data = ecm_data)
          
          latest_period <- tail(train_data, 1)
          latest_eq_error <- latest_period$fedfunds -
            (long_run_model$coefficients[1] +
               long_run_model$coefficients[2] * latest_period$inflation +
               long_run_model$coefficients[3] * latest_period$gdp)
          
          forecast_data <- data.frame(
            L_eq_error = latest_eq_error,
            d_inflation = test_data$inflation - latest_period$inflation,
            d_gdp = test_data$gdp - latest_period$gdp
          )
          
          forecast_change <- predict(ecm_model, newdata = forecast_data)
          forecast_val <- latest_period$fedfunds + as.numeric(forecast_change)
          forecast_val <- max(0, min(20, forecast_val))
          
          successful_ecms <- c(successful_ecms, model_name)
          forecast_results[[model_name]] <- c(forecast_results[[model_name]], forecast_val)
        }, error = function(e) NULL)
      }
    }
  }
  
  forecast_results[["actual"]] <- c(forecast_results[["actual"]], y_test)
}

actual_values <- forecast_results[["actual"]]
forecast_results$actual <- NULL

error_summary <- data.frame(Model = character(),
                            MAE = numeric(),
                            MSE = numeric(),
                            RMSE = numeric(),
                            stringsAsFactors = FALSE)

for (model_name in names(forecast_results)) {
  predictions <- forecast_results[[model_name]]
  if (length(predictions) == length(actual_values)) {
    mae_val <- mae(actual_values, predictions)
    mse_val <- mse(actual_values, predictions)
    rmse_val <- rmse(actual_values, predictions)
    error_summary <- rbind(error_summary, data.frame(
      Model = model_name,
      MAE = mae_val,
      MSE = mse_val,
      RMSE = rmse_val
    ))
  }
}

error_summary

act <- test.fedfunds
tr <- ts(c(5.38, 4.01, 8.21, 11.39, 7.43, 6.34), start = test.start, frequency = 1)
m1 <- tr
m2 <- forecast_results[["ECM(1,1,4)"]]

u1 <- m1 - act
u2 <- m2 - act
f <- (1/length(act)) * (sum(u1^2) - sum(u2^2) + sum((m1 - m2)^2))
sv <- (1/(length(act) - 1)) * sum((u1^2 - u2^2 + (m1 - m2)^2 - f)^2)
t <- (length(act)^(1/2)) * f / (sv^(1/2))
p <- 1 - pt(t, (length(act) - 1))

p_val <- list()
for (p in 1:max_lags) {
  for (q in 1:max_lags) {
    for (r in 1:max_lags) {
      model <- paste0("ECM(", p, ",", q, ",", r, ")")
      m2 <- forecast_results[[model]]
      if (!is.null(m2)) {
        u1 <- m1 - act
        u2 <- m2 - act
        f <- (1/length(act)) * (sum(u1^2) - sum(u2^2) + sum((m1 - m2)^2))
        sv <- (1/(length(act) - 1)) * sum((u1^2 - u2^2 + (m1 - m2)^2 - f)^2)
        t <- (length(act)^(1/2)) * f / (sv^(1/2))
        p <- 1 - pt(t, (length(act) - 1))
        p_val[[model]] <- c(model, p)
      }
    }
  }
}
p_val

