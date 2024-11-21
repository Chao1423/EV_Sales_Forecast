
library(ggplot2)
library(dplyr)
library(fpp3)
library(forecast)
library(tsibble)
library(readxl)


#Load Chinese Vehicle Monthly Sales by Types
CV_Sales_Monthly <- read_xlsx("Data/EV_China_by_Powertrain.xlsx")


#Clean data by replacing N/As by 0
CV_Sales_Monthly <- CV_Sales_Monthly[,-1]|>
  replace_na(list(EV = 0, FCV = 0, HV = 0, ICE = 0, `Mild HV` = 0, `N/A` = 0, PHV = 0))

colnames(CV_Sales_Monthly)[colnames(CV_Sales_Monthly) == "N/A"] <- "Others"
#view(CV_Sales_Monthly)
CV_Sales_Monthly$date <- as.Date(CV_Sales_Monthly$date)

#Visualize the original data
ggplot(CV_Sales_Monthly, aes(x = date)) +
  geom_line(aes(y = EV, color = "EV")) +
  geom_line(aes(y = FCV, color = "FCV")) +
  geom_line(aes(y = HV, color = "HV")) +
  geom_line(aes(y = ICE, color = "ICE")) +
  geom_line(aes(y = `Mild HV`, color = "Mild HV")) +
  geom_line(aes(y = PHV, color = "PHV")) +
  geom_line(aes(y = Others, color = "Others")) +
  labs(title = "Time Series Trend of Vehicle Sales by Powertrain Type",
       x = "Date",
       y = "Sales",
       color = "Vehicle Type") +
  theme_minimal()



#Let's divide the whole market into New Energy Vehicles and Traditional Power Vehicles

EV_Sales_Monthly <- data.frame(
  date = CV_Sales_Monthly$date,  
  EV = rowSums(CV_Sales_Monthly[, c(2, 3, 4, 6, 8)]),
  TV = rowSums(CV_Sales_Monthly[, c(5,7)]),
  Total_sales = rowSums(CV_Sales_Monthly[,-1])
)

EV_Sales_Monthly <- EV_Sales_Monthly |> 
  #filter(EV > 5000) |>
  mutate(date = yearmonth(date)) |>
  as_tsibble(index = date) |> 
  fill_gaps()

EV_Sales_Long <- EV_Sales_Monthly |> 
  pivot_longer(
    cols = c(EV, TV, Total_sales),   
    names_to = "Category",  
    values_to = "Sales"     
  )

autoplot(EV_Sales_Long, Sales) +
  aes(colour = Category) +
  labs(
    title = "Monthly Sales Trends for EV, TV, and Total Sales",
    x = "Date",
    y = "Sales",
    colour = "Category", 
    caption = "Source: CV Sales Data"
  ) +
  theme_minimal() 

# Time Series Decomposition (STL)

EV_decomp <- EV_Sales_Monthly |>
  model(
    STL(EV ~ trend(window = 13) +
          season(window = "periodic"),
        robust = TRUE)) |>
  components() |>
  autoplot()

TV_decomp <- EV_Sales_Monthly |>
  model(
    STL(TV ~ trend(window = 13) +
          season(window = "periodic"),
        robust = TRUE)) |>
  components() |>
  autoplot()

TS_decomp <- EV_Sales_Monthly |>
  model(
    STL(Total_sales ~ trend(window = 13) +
          season(window = "periodic"),
        robust = TRUE)) |>
  components() |>
  autoplot()

EV_decomp
TV_decomp
TS_decomp

#AutoCorrelation Function
EV_Sales_Monthly |>
  ACF(TV, lag_max = 24) |>  
  autoplot() +
  labs(
    title = "ACF of EV Sales",
    x = "Lag",
    y = "Autocorrelation"
  ) +
  theme_minimal()

#Seasonal Strength
seasonal_strength <- function(data) {
  decomposition <- data |> model(STL(TV ~ trend(window = 13) + season(window = "periodic")))
  components <- decomposition |> components()
  var_seasonal <- var(components$season_year)
  var_total <- var(components$trend + components$remainder + components$season_year)
  F_s <- var_seasonal / var_total
  return(F_s)
}

seasonal_strength(EV_Sales_Monthly) #Not strong
