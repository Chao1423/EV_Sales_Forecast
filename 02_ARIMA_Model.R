library(fpp3)
library(tsibble)
library(lubridate)
library(forecast)


# Step 1: Check for stationarity (ADF Test) & differencing if necessary
# We have already known from ACF that original ts is not stationary
EV_Sales_Monthly |> 
  ACF(diff(EV), lag_max = 36) |>  
  autoplot() +
  labs(
    title = "ACF of EV Sales (First-time differencing)",
    x = "Lag",
    y = "Autocorrelation"
  ) +
  theme_minimal()

# We observed a peak in Lag 12, which might indicates a seasonality
EV_Sales_Monthly |> 
  ACF(diff(diff(EV)), lag_max = 36) |>  
  autoplot() +
  labs(
    title = "ACF of EV Sales (Second-time differencing)",
    x = "Lag",
    y = "Autocorrelation"
  ) +
  theme_minimal()

# Similarly, a PACF is needed
EV_Sales_Monthly |> 
  PACF(diff(EV), lag_max = 36) |>  
  autoplot() +
  labs(
    title = "PACF of EV Sales (First-time differencing)",
    x = "Lag",
    y = "Partial Autocorrelation"
  ) +
  theme_minimal()

# Then we conducted Ljung-box test for confirmation
Box.test(EV_Sales_Monthly$EV |> diff(), lag = 12, type = "Ljung-Box") # p-value << 0.05 There is a seasonality


# KPSS Method and Augmented Dickey-Fuller Test
library(urca)
library(tseries)
EV_Sales_Monthly$EV |> ur.kpss() |> summary()
# Value of test-statistic is: 2.3071 -- Not Stationary
EV_Sales_Monthly$EV %>% diff() |> ur.kpss() |> summary()
# Value of test-statistic is: 0.7371  -- Weak Stationary? To be checked in adf
EV_Sales_Monthly$EV |> diff() |> diff() |> ur.kpss() |> summary()
# Value of test-statistic is: 0.0504  -- Good Enough

adf.test(EV_Sales_Monthly$EV)
adf.test(diff(EV_Sales_Monthly$EV)) # Result: Stationary

# Step 2: Fit SARIMA model
# Automatically find best SARIMA parameters (p, d, q)(P, D, Q)[m]

ev_ts <- ts(EV_Sales_Monthly$EV, frequency = 12)
ev_ts |> diff(lag=12) |> ggtsdisplay()

fit_sarima <- auto.arima(ev_ts, seasonal = TRUE)
checkresiduals(fit_sarima) #(1,1,4)(0,0,1) 
summary(fit_sarima) #AICc=4360.25 RMSE=54026.45


# Try some different hyperparameters
fit_sarima_1 <- Arima(ev_ts, order = c(1, 1, 4), seasonal = c(0, 1, 1)) # I'll talk some about the parameter selection
summary(fit_sarima_1) #AICc=4018.6  RMSE=46457.85

fit_sarima_2 <- Arima(ev_ts, order = c(1, 1, 4), seasonal = c(1, 2, 2))
summary(fit_sarima_2) #AICc=3748.7 RMSE=43362.42

checkresiduals(fit_sarima_2) #Conclusion: SARIMA Model cannot capture all the characters of the time series, to solve this a xreg is required

#Let's compare with random walk

fit_rw <- Arima(ev_ts, order = c(0, 1, 0))
summary(fit_rw)

residuals_rw <- residuals(fit_rw)

residuals_sarima <- residuals(fit_sarima)

rw_rmse <- sqrt(mean(residuals_rw^2))
rw_mae <- mean(abs(residuals_rw))
sarima_rmse <- sqrt(mean(residuals_sarima^2))
sarima_mae <- mean(abs(residuals_sarima))

cat("Random Walk RMSE:", rw_rmse, "\n")
cat("Random Walk MAE:", rw_mae, "\n")
cat("SARIMA RMSE:", sarima_rmse, "\n")
cat("SARIMA MAE:", sarima_mae, "\n")

residuals_df <- data.frame(
  Time = time(ev_ts),
  RW = residuals_rw,
  SARIMA = residuals_sarima
)

ggplot(residuals_df, aes(x = Time)) +
  geom_line(aes(y = RW, color = "Random Walk Residuals")) +
  geom_line(aes(y = SARIMA, color = "SARIMA Residuals")) +
  labs(title = "Residual Comparison: Random Walk vs SARIMA",
       y = "Residuals", x = "Time") +
  theme_minimal()

#Rolling Window Test (OOS)

train_size <- 60
total_length <- length(ev_ts)
test_size <- total_length - train_size 

predictions <- numeric(test_size)
actuals <- ev_ts[(train_size + 1):total_length]  

for (i in 1:test_size) {
  train_data <- ev_ts[i:(train_size + i - 1)]
  model <- auto.arima(train_data, seasonal = TRUE)
  predictions[i] <- forecast(model, h = 1)$mean
}

mae <- mean(abs(predictions - actuals))  
rmse <- sqrt(mean((predictions - actuals)^2))  
cat("Mean Absolute Error (MAE):", mae, "\n")
cat("Root Mean Square Error (RMSE):", rmse, "\n")

plot_data <- data.frame(
  Time = time(actuals),
  Actual = actuals,
  Predicted = predictions
)

ggplot(plot_data, aes(x = Time)) +
  geom_line(aes(y = Actual, color = "Actual")) +
  geom_line(aes(y = Predicted, color = "Predicted")) +
  labs(title = "Rolling Window 1-step Ahead Forecasts",
       y = "Values", x = "Time", color = "Legend") +
  theme_minimal()

# Step 4: Forecast using the SARIMA model
ev_ts |>
  Arima(order=c(1,1,4),seasonal=c(1,2,2)) |>
  forecast() |>
  autoplot() +
  ylab("EV Sales") + xlab("year")
