
# setwd("C:/Users/carys/gatech/mgt6203/Team-104/")
library('readxl')
library('tidyverse')
library('lubridate')

# data imports
real_gasoline <- read.csv(
  'Data/raw/real-prices/Regular_Gasoline_Retail_Prices.csv',
  skip=4
)
real_diesel <- read.csv(
  'Data/raw/real-prices/Diesel_Fuel_Retail_Prices.csv',
  skip=4
)


# parse "Month" column to datetime format
# h/t https://stackoverflow.com/questions/41122645/lubridate-how-to-parse-month-year
real_gasoline$date <- parse_date_time(real_gasoline$Month, orders = 'b Y') %>% as_date()
real_diesel$date <- parse_date_time(real_diesel$Month, orders = 'b Y') %>% as_date()

# filter out forecasts
real_gasoline <- real_gasoline %>%
  filter(date <= '2023-04-01')

real_diesel <- real_diesel %>% 
  filter(date <= '2023-04-01')

# merge into one df
data <- inner_join(
  real_gasoline, real_diesel,
  by=join_by(date),
  suffix=c(".gas", ".diesel")
) %>%
  select(date, Real.Values.gas, Real.Values.diesel)

ggplot(data, aes(x=date)) +
  geom_line(aes(y=Real.Values.gas, color='Gasoline')) +
  geom_line(aes(y=Real.Values.diesel, color='Diesel')) +
  labs(
    title = 'Plot of real gasoline and diesel prices, 1976-2023',
    subtitle = 'Price adjusted to 2023 value of the dollar',
    caption = 'Data from and adjusted for inflation by the U.S. Energy Information Administration',
    x = 'Time',
    y = 'Real price per gallon, USD ($)'
  )
ggsave('Visualizations/real-gas-and-diesel-prices.png')

# Install and load the rugarch package
install.packages("rugarch")
library(rugarch)

# Create a time series object for the gas prices
gas_ts <- ts(data$Real.Values.gas, frequency = 12)
# Create a time series object for the diesel prices
diesel_ts <- ts(data$Real.Values.diesel, frequency = 12)

# Specify the GARCH model
gas_spec <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 1)), 
                       mean.model = list(armaOrder = c(0, 0)))
diesel_spec <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 1)), 
                          mean.model = list(armaOrder = c(0, 0)))

# Fit the GARCH model to the gas prices
gas_fit <- ugarchfit(spec = gas_spec, data = gas_ts)
# Fit the GARCH model to the diesel prices
diesel_fit <- ugarchfit(spec = diesel_spec, data = diesel_ts)

# Extract the conditional volatility from the gas GARCH model
gas_volatility <- sigma(gas_fit)

# Extract the conditional volatility from the diesel GARCH model
diesel_volatility <- sigma(diesel_fit)

# Plot the conditional volatility of the gas GARCH model
plot(gas_volatility, col = "blue", lwd = 2, ylab = "Conditional Volatility")

# Add the conditional volatility of the diesel GARCH model to the same graph
lines(diesel_volatility, col = "red", lwd = 2)


