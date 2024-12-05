# To obtain more observations, we transform the yearly data to quarterly

ev_monthly <- EV_Sales_Monthly |>
  select(date, EV)

library(forecast)

start_year <- 2010
start_month <- 1
ev_monthly_ts <- ts(ev_monthly$EV, start = c(start_year, start_month), frequency = 12)
ev_quarterly_ts <- aggregate(ev_monthly_ts, nfrequency = 4, FUN = sum)

ev_quarterly <- data.frame(
  quarter = time(ev_quarterly_ts),
  EV = as.numeric(ev_quarterly_ts)
) |>
  filter(quarter > 2013.75) |>
  mutate(
    year = floor(quarter), 
    quarter_number = (quarter %% 1) * 4 + 1,  
    quarter = paste0(year, "Q", quarter_number) 
  ) |>
  select(quarter, EV)  

ev_quarterly
# GDP in quarters
gdp_quarterly <- read.csv("Data/Quarterly_GDP.csv",skip = 2)

gdp <- gdp_quarterly |>
  pivot_longer(
    cols = -Indicators,  
    names_to = "quarter",   
    values_to = "value" 
  ) |>
  filter(Indicators == "Gross Domestic Product Current Quarter(100 million yuan)") |> 
  select(-Indicators) |>  
  mutate(
    quarter = gsub("^X(\\d)Q\\.(\\d{4})$", "\\2Q\\1", quarter),  
  ) |> 
  arrange(quarter) 

# Confidence Index in quarters
library(lubridate)
cci_data <- read.csv("Data/Consumer_Confidence_Index.csv")
cci <- cci_data |>
  mutate(
    DateTime = as.Date(DateTime),          
    year_quarter = paste0(year(DateTime), "Q", quarter(DateTime))  
  ) |>
  filter(year(DateTime) >= 2014)  |>
  group_by(year_quarter) |>      
  summarize(
    average_price = mean(Close, na.rm = TRUE)  
  ) |>
  ungroup()   

cci

# Oil price in quarters
gasoline_data <- read.csv("Data/Gasoline Price.csv")

gasoline <- gasoline_data |>
  mutate(
    DateTime = as.Date(DateTime),          
    year_quarter = paste0(year(DateTime), "Q", quarter(DateTime))  
  ) |>
  filter(year(DateTime) >= 2014) |>   
  group_by(year_quarter)|>        
  summarize(
    average_price = mean(Close, na.rm = TRUE)  
  ) |>
  ungroup()   |>
  filter(year_quarter != "2024Q4") 
                     

gasoline

# Combine all 4 variables to a panel data
gdp <- gdp  |>
  rename(GDP = value)  |>
  mutate(quarter = as.character(quarter))  # Ensure quarter is character for merging

cci <- cci  |>
  rename(CCI = average_price)  |>
  mutate(year_quarter = as.character(year_quarter))  # Rename and ensure type consistency

gasoline <- gasoline  |>
  rename(Gasoline_Price = average_price)  |>
  mutate(year_quarter = as.character(year_quarter))  # Rename and ensure type consistency

panel_data_quarterly <- ev_quarterly  |>
  left_join(gdp, by = "quarter")  |>
  left_join(cci, by = c("quarter" = "year_quarter"))  |>
  left_join(gasoline, by = c("quarter" = "year_quarter"))

# Visualize the panel data

panel_data_standardized <- panel_data_quarterly  |>
  mutate(across(-quarter, scale)) 

panel_data_long <- panel_data_standardized  |>
  pivot_longer(
    cols = -quarter,     
    names_to = "variable",  
    values_to = "value"   
  )

ggplot(panel_data_long, aes(x = quarter, y = value, color = variable, group = variable)) +
  geom_line(size = 1) +
  scale_color_manual(
    values = c("EV" = "red", "other" = "grey"),  
    guide = guide_legend(override.aes = list(size = 2))
  ) +
  labs(
    title = "Standardized Panel Data Quarterly",
    x = "Quarter",
    y = "Standardized Value",
    color = "Variables"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom"
  )



  
  
  
  
  
  
  
  
