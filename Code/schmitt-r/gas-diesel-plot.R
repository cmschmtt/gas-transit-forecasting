setwd("C:/Users/carys/gatech/mgt6203/Team-104/")
library(tidyverse)

data <- read.csv('Data/real-gas-diesel.csv')
data$date <- as.Date(data$date)

ggplot(data, aes(x=date)) +
  # lines for gas and diesel
  geom_line(aes(y=Real.Values.gas, color='gas')) +
  geom_line(aes(y=Real.Values.diesel, color='diesel')) +
  # most labels
  labs(
    title = 'Plot of real gasoline and diesel prices, 1976-2023',
    subtitle = 'Price adjusted to 2023 value of the dollar',
    caption = 'Data from and adjusted for inflation by the U.S. Energy Information Administration',
    x = 'Time',
    y = 'Real price per gallon, USD ($)'
  ) +
  geom_vline(
    xintercept=as.Date('2008-01-01'), linetype='dotted'
  ) +
  geom_text(
    aes(x=as.Date('2008-01-01'), y=6), label='2008 recession'
  )
