# Log Transformation for EV Monthly Sales
EV_Sales_Monthly_Logged <- EV_Sales_Monthly |>
  mutate(
    EV_log = log(if_else(EV == 0, 1, EV)), 
    TV_log = log(if_else(TV == 0, 1, TV)), 
    Total_sales_log = log(if_else(Total_sales == 0, 1, Total_sales))
  )

# Time Series Decomposition (STL)
EV_decomp_log <- EV_Sales_Monthly_Logged |>
  model(
    STL(EV_log ~ trend(window = 13) +
          season(window = "periodic"),
        robust = TRUE)) |>
  components() |>
  autoplot()

EV_decomp_log

# AutoCorrelation Function
EV_Sales_Monthly_Logged |>
  ACF(EV_log, lag_max = 24) |>  
  autoplot() +
  labs(
    title = "ACF of EV Sales",
    x = "Lag",
    y = "Autocorrelation"
  ) +
  theme_minimal() # The result is very similar to the one not taking log

#Seasonal Plot
EV_Sales_Monthly_Logged |> gg_season(EV_log)
