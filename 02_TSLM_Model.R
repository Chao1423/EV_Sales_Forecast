panel_ts <- panel_data |>
  as_tsibble(index = year)

fit <- panel_ts %>%
  model(
    tslm_model = TSLM(EV_Sales ~ EV_charging_points + GDP_per_capita +
                        Consumer_Confidence_Index + Oil_price)
  )

report(fit)


#Rough forecast based on TSLM
future_data <- new_data(panel_ts, n = 5) %>%
  mutate(
    EV_charging_points = seq(3000000, 4000000, length.out = 5),  
    GDP_per_capita = seq(90000, 95000, length.out = 5),
    Consumer_Confidence_Index = seq(85, 90, length.out = 5),
    Oil_price = seq(1.0, 1.2, length.out = 5)
  )

forecast <- fit %>%
  forecast(new_data = future_data)

forecast %>%
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