
# Step 1: Check for stationarity (ADF Test) & differencing if necessary
# We have already known from ACF that original ts is not stationary
EV_Sales_Monthly_Logged |> 
  ACF(diff(EV_log), lag_max = 36) |>  
  autoplot() +
  labs(
    title = "ACF of EV Sales (First-time differencing)",
    x = "Lag",
    y = "Autocorrelation"
  ) +
  theme_minimal()

# PACF
EV_Sales_Monthly_Logged |> 
  PACF(diff(EV_log), lag_max = 36) |>  
  autoplot() +
  labs(
    title = "PACF of EV Sales (First-time differencing)",
    x = "Lag",
    y = "Autocorrelation"
  ) +
  theme_minimal()

# Ljung-box test
Box.test(EV_Sales_Monthly$EV |> diff(), lag = 12, type = "Ljung-Box")

# KPSS Method and Augmented Dickey-Fuller Test
library(urca)
library(tseries)
EV_Sales_Monthly_Logged$EV_log |> ur.kpss() |> summary()
# Value of test-statistic is: 3.4137 -- Not Stationary
EV_Sales_Monthly_Logged$EV_log  |> diff() |> ur.kpss() |> summary()
# Value of test-statistic is: 0.1743  -- Good Stationary

adf.test(EV_Sales_Monthly_Logged$EV_log)
adf.test(diff(EV_Sales_Monthly_Logged$EV_log)) # Result: Stationary


# Step 2: Fit SARIMA model
# Automatically find best SARIMA parameters (p, d, q)(P, D, Q)[m]

ev_ts_log <- ts(EV_Sales_Monthly_Logged$EV_log, frequency = 12)
ev_ts_log |> diff(lag=12) |> ggtsdisplay()

fit_sarima_log <- auto.arima(ev_ts_log, seasonal = TRUE)
checkresiduals(fit_sarima_log) #(2,1,2)(1,0,0) 
summary(fit_sarima_log) #AICc=246.33 RMSE=0.4608843

# Try some different parameters
fit_sarima_log_1 <- Arima(ev_ts_log, order = c(1, 1, 2), seasonal = c(1, 1, 1))
summary(fit_sarima_log_1) #AICc=212.37  RMSE=0.3969841 So we take this model
checkresiduals(fit_sarima_log_1)
fit_sarima_log_2 <- Arima(ev_ts_log, order = c(1, 1, 2), seasonal = c(1, 2, 1))
summary(fit_sarima_log_2) #AICc=280.78 RMSE=0.4658363
checkresiduals(fit_sarima_log_2) #Conclusion: choose one-time differencing

#Let's compare with random walk
fit_rw_log <- Arima(ev_ts_log, order = c(0, 1, 0))
summary(fit_rw_log)

residuals_rw_log <- residuals(fit_rw_log)

residuals_sarima_log <- residuals(fit_sarima_log)

rw_rmse_log <- sqrt(mean(residuals_rw_log^2))
rw_mae_log <- mean(abs(residuals_rw_log))
sarima_rmse_log <- sqrt(mean(residuals_sarima_log^2))
sarima_mae_log <- mean(abs(residuals_sarima_log))

cat("Random Walk RMSE:", rw_rmse_log, "\n")
cat("Random Walk MAE:", rw_mae_log, "\n")
cat("SARIMA RMSE:", sarima_rmse_log, "\n")
cat("SARIMA MAE:", sarima_mae_log, "\n")

residuals_df_log <- data.frame(
  Time = time(ev_ts_log),
  RW = residuals_rw_log,
  SARIMA = residuals_sarima_log
)

ggplot(residuals_df_log, aes(x = Time)) +
  geom_line(aes(y = RW, color = "Random Walk Residuals")) +
  geom_line(aes(y = SARIMA, color = "SARIMA Residuals")) +
  labs(title = "Residual Comparison in Log: Random Walk vs SARIMA",
       y = "Residuals", x = "Time") +
  theme_minimal()

#Rolling Window Test (OOS)

train_size <- 60
total_length <- length(ev_ts_log)
test_size <- total_length - train_size 

predictions <- numeric(test_size)
actuals <- ev_ts_log[(train_size + 1):total_length]  

for (i in 1:test_size) {
  train_data <- ev_ts_log[i:(train_size + i - 1)]
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

# Step 4: Forecast using the SARIMA model in the next 2 years

forecast_log <- ev_ts_log |>
  Arima(order = c(1, 1, 2), seasonal = c(1, 1, 1)) |>
  forecast()

# Forecast on log data
forecast_log |>
  autoplot() +
  ylab("EV Sales") + xlab("year")

# Forecast on real data (mean only)
forecast_real <- exp(forecast_log$mean)

data_forecast <- data.frame(
  Date = c(time(ev_ts), time(forecast_real)),
  Value = c(ev_ts, forecast_real),
  Type = c(rep("Historical", length(ev_ts)), rep("Forecast", length(forecast_real)))
)

ggplot(data_forecast, aes(x = Date, y = Value, color = Type)) +
  geom_line() +
  scale_y_continuous(labels = scales::comma_format()) + 
  ylab("EV Sales (Real Scale)") +
  xlab("Year") +
  ggtitle("EV Sales Forecast (Back to Original Scale)") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  scale_color_manual(values = c("Historical" = "blue", "Forecast" = "red"))



