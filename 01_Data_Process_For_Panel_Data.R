EV_Yearly <- read.csv("Data/IEA Global EV Data 2024.csv") 

#EV yearly data for TSLM Y Variable
EV_Yearly_China <- EV_Yearly |> 
  filter(region == "China", category == "Historical") |>
  group_by(powertrain,parameter,year,unit) |>
  summarise(total_value = sum(value), .groups = 'drop')

EV_Yearly_China

EV_Sales_Yearly <- EV_Yearly_China |>
  filter(grepl("EV sales", parameter) & grepl("Vehicles", unit)) |>
  group_by(year) |>
  summarise(value = sum(total_value, na.rm = TRUE)) |>
  filter (year >= 2014)

#Predictors
#1. Charging Points with in the data set

EV_charging_points <- EV_Yearly_China |>
  filter(grepl("EV charging points", parameter)) |>
  group_by(year,powertrain) |>
  summarise(total_value = sum(total_value, na.rm = TRUE))

#Visualization of the trend
ggplot(EV_charging_points, aes(x = year, y = total_value, color = powertrain))  +
  geom_line(size = 1) + 
  geom_vline(xintercept = 2023, linetype = "dashed", color = "red") + 
  annotate("text", x=2023.5, y=2e+05, label="As of 2024", angle=90, size = 3) +
  labs(title = "EV Charging Points in China: Fast & Slow", 
       x = "Year", 
       y = "Number of Charging Points",
       color = "Charging Type") +
  theme_minimal()

EV_charging_points_yearly <- EV_charging_points |>
  group_by(year) |>
  summarise(value = sum(total_value, na.rm = TRUE))

#2. Grand Economics Indicators

GDP_Yearly <- read.csv("Data/Annual GDP.csv",skip = 2)

gdp_per_capita <- GDP_Yearly |>
  pivot_longer(
    cols = -Indicators,  
    names_to = "year",   
    values_to = "value"  
  ) |>
  filter(Indicators == "Per Capita GDP(yuan)") |>
  select(-Indicators) |>  
  mutate(year = as.numeric(gsub("^X(\\d)", "\\1", year))) |>
  arrange(year)


CCI <- read.csv("Data/Consumer_Confidence_Index.csv")

CCI <- CCI[,c(3,4)] |>
  mutate(DateTime = as.Date(DateTime, format = "%Y-%m-%d"))  |>
  mutate(year = year(DateTime)) |>
  group_by(year) |>               
  summarise(value = mean(Close, na.rm = TRUE)) |>
  filter (year >= 2014)

Oil_price <- read.csv("Data/Gasoline Price.csv") 

Oil_price <- Oil_price[,c(3,4)] |>
  mutate(DateTime = as.Date(DateTime, format = "%Y-%m-%d"))  |>
  mutate(year = year(DateTime)) |>
  group_by(year) |>               
  summarise(value = mean(Close, na.rm = TRUE)) |>
  filter (year >= 2014)


panel_data <- EV_Sales_Yearly |>
  left_join(EV_charging_points_yearly, by = "year") |>
  left_join(gdp_per_capita, by = "year") |>
  left_join(CCI, by = "year") |>
  left_join(Oil_price, by = "year") 
colnames(panel_data) <- c("year","EV_Sales","EV_charging_points","GDP_per_capita","Consumer_Confidence_Index","Oil_price")
  
write.csv(panel_data, "Data/Panel_Data.csv")

#Visualisation of the panel data
normalized_data <- panel_data %>%
  mutate(across(c(EV_Sales, EV_charging_points, GDP_per_capita, Consumer_Confidence_Index, Oil_price),
                ~ (. - min(.)) / (max(.) - min(.)))) %>%
  pivot_longer(cols = -year, names_to = "Variable", values_to = "Value")

highlight_color <- c("EV_Sales" = "red", "EV_charging_points" = "gray10", 
                     "GDP_per_capita" = "gray30", "Consumer_Confidence_Index" = "gray50", 
                     "Oil_price" = "gray70")

# Plot the trends, emphasizing EV_Sales
ggplot(normalized_data, aes(x = year, y = Value, group = Variable, color = Variable)) +
  geom_line(aes(size = ifelse(Variable == "EV_Sales", 1.5, 0.8))) +
  geom_point(aes(size = ifelse(Variable == "EV_Sales", 3, 1.5))) +
  scale_color_manual(values = highlight_color) +
  scale_size_identity() +
  labs(
    title = "Trends in EV Sales and Predictors (EV Sales Highlighted)",
    x = "Year",
    y = "Normalized Value",
    color = "Variable"
  ) +
  theme_minimal() +
  theme(
    text = element_text(size = 12),
    legend.position = "bottom"
  )



