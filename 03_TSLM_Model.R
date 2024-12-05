panel_ts <- panel_data |>
  as_tsibble(index = year)

fit <- panel_ts |>
  model(
    tslm_model = TSLM(EV_Sales ~ EV_charging_points + GDP_per_capita +
                        Consumer_Confidence_Index + Oil_price)
  )

report(fit)

#Rough forecast based on TSLM
future_data <- new_data(panel_ts, n = 5) |>
  mutate(
    EV_charging_points = seq(3000000, 4000000, length.out = 5),  
    GDP_per_capita = seq(90000, 95000, length.out = 5),
    Consumer_Confidence_Index = seq(85, 90, length.out = 5),
    Oil_price = seq(1.0, 1.2, length.out = 5)
  )

forecast <- fit |>
  forecast(new_data = future_data)

forecast |>
  autoplot(panel_ts, level = NULL) +
  labs(
    title = "EV Sales Forecast with TSLM",
    x = "Year",
    y = "EV Sales",
    caption = "Source: Panel Data"
  ) +
  theme_minimal()

#结论. TSLM 模型无法有效捕捉历史数据特征，模型过拟合说明可能存在多重共线性的问题
#解决方案：选取更多数据（Sample） 以及使用SARIMA来预测

#Lasso (ML method) to deal with multicolinearity
library(glmnet)

X <- panel_data |>
  select(EV_charging_points, GDP_per_capita, Consumer_Confidence_Index, Oil_price) %>%
  as.matrix()

y <- panel_data$EV_Sales
X_scaled <- scale(X)

set.seed(42)
lasso_model <- cv.glmnet(X_scaled, y, alpha = 1)

optimal_lambda <- lasso_model$lambda.min
print(paste("Optimal Lambda:", optimal_lambda))

lasso_coefficients <- coef(lasso_model, s = "lambda.min")
print(lasso_coefficients)


### TSLM with quarterly data: 43 observations

panel_ts_quarterly <- panel_data_quarterly |>
  mutate(quarter = yearquarter(quarter)) |>
  as_tsibble(index = quarter)

fit_1 <- panel_ts_quarterly |>
  model(
    tslm_model = TSLM(EV ~ GDP+ CCI + Gasoline_Price)
  )
report(fit_1)

fit_log <- panel_ts_quarterly |>
  model(
    tslm_log_model = TSLM(log(EV) ~ log(GDP) + log(CCI) + log(Gasoline_Price))
  )
report(fit_log)

# Then forecast
future_data <- new_data(panel_ts_quarterly, n = 8) |>
  mutate(
    GDP= seq(380000, 400000, length.out = 8),
    CCI= seq(82,78, length.out = 8),
    Gasoline_Price = seq(0.9, 0.96, length.out = 8)
  )

forecast <- fit_1 |>
  forecast(new_data = future_data)

forecast |>
  autoplot(panel_ts_quarterly, level = NULL) +
  labs(
    title = "EV Sales Forecast with TSLM",
    x = "Quarters",
    y = "EV Sales",
    caption = "Source: Panel Data Quarterly"
  ) +
  theme_minimal()

# LASSO here
library(glmnet)

X <- panel_data_quarterly |>
  select(GDP, CCI, Gasoline_Price)  |>
  as.matrix()

y <- panel_data_quarterly$EV
X_scaled <- scale(X)

set.seed(42)
lasso_model_q <- cv.glmnet(X_scaled, y, alpha = 1)

optimal_lambda_q <- lasso_model_q$lambda.min
print(paste("Optimal Lambda:", optimal_lambda_q))

lasso_coefficients_q <- coef(lasso_model_q, s = "lambda.min")
print(lasso_coefficients_q)
