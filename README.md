
# **EV_Sales_Forecast**

## Overview
This project analyzes and forecasts **electronic vehicles according to the data from International Energy Association(IEA), marklines.com, and National Statstic Bureau of China** using R. It includes data preprocessing, visualization, and statistical modeling, with a focus on time series analysis and predictive modeling.

---

## Features
- **Data Preprocessing**: Clean and structure raw data for analysis.
- **Visualization**: Explore trends and relationships using ggplot2 and other visualization tools.
- **Time Series Modeling**: Build SARIMA, TSLM, and LASSO models to forecast trends.
- **Insights**: Generate actionable insights from historical data and predictions.

---

## Getting Started

### Prerequisites
Ensure you have the following software installed:
- **R** (≥ 4.0.0)
- **RStudio** (optional, but recommended)

Install the required R packages:

```r
# Install missing packages
packages <- c("ggplot2", "dplyr", "tidyr", "lubridate", "fpp3", "forecast", "tsibble", "urca", "tseries", "readr", "readxl", "glmnet")
for (pkg in packages) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg, dependencies = TRUE)
    library(pkg, character.only = TRUE)
  }
}
```

### Initializing and running
- To start running the project, please download the file "data" from our project Google drive and put it in the "Data" file in your local Rproject file.
- To run the ARIMA model, process the data using 00 and 00(1). Go through the 02 file because "ev_ts" variable here will be needed. Then running 03 is recommended since ARIMA in 03 used logged data.
- To run the TSLM model, process the data using 01 for yearly panel data and 01(1) for quarterly panel data. Then run 04 for TSLM models for forecasting both.


## Contributing
Contributions are welcome! Please fork this repository, create a feature branch, and submit a pull request.

## License
This project is licensed under the MIT License.

## Contact
If you have any questions or feedback, feel free to contact me at [wanchzhao3-c@my.cityu.edu.hk].