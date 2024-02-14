#install.packages("changepoint")
#install.packages("lubridate")
library(changepoint)
library(lubridate)

combined_data <- read.csv('National_Combined_Data.csv')
combined_data$Date <- mdy(combined_data$Date)

# Plot decomposition of bus data
bus_ts <- ts(combined_data$Bus, frequency=12, start=c(2002, 1))
bus_ts_dc <- decompose(bus_ts, type='additive')
plot(bus_ts_dc$seasonal, main='Seasonal Component of Bus Commuting Data', xlab='Year', ylab='Number of Commuters')

bus_ts_sa <- bus_ts - bus_ts_dc$seasonal
plot(bus_ts_sa,xlab='Year', ylab='Number of Commuters', main='Bus Commuters by Year, Seasonally Adjusted')

# Plot decomposition of commuter rail data
cr_ts <- ts(combined_data$CommuterRail, frequency=12, start=c(2002, 1))
cr_ts_dc <- decompose(cr_ts, type='additive')
plot(cr_ts_dc$seasonal, main='Seasonal Component of Commuter Rail Commuting Data', xlab='Year', ylab='Number of Commuters')

cr_ts_sa <- cr_ts - cr_ts_dc$seasonal
plot(cr_ts_sa, xlab='Year', ylab='Number of Commuters', main='Commuter Rail Commuters by Year, Seasonally Adjusted')

# Remove COVID-19 related outliers
combined_data_ex_c19 <- combined_data[combined_data['Date'] < '2020-01-01',]

# Plot seasonally adjusted bus data with changepoints
bus_ts_ex_c19 <- ts(combined_data_ex_c19$Bus, frequency=12, start=c(2002, 1))
bus_ts_dc_ex_c19 <- decompose(bus_ts_ex_c19, type='additive')
bus_ts_sa_ex_c19 <- bus_ts_ex_c19 - bus_ts_dc_ex_c19$seasonal
plot(bus_ts_sa_ex_c19, xlab='Year', ylab='Number of Commuters', main='Bus Commuters by Year, Seasonally Adjusted')
chg_pt_bus <- cpt.mean(bus_ts_sa_ex_c19, method='BinSeg')
plot(chg_pt_bus)

# Identify dates of changepoints for bus
print(chg_pt_bus)
combined_data_ex_c19$Date[34]
combined_data_ex_c19$Date[91]
combined_data_ex_c19$Date[123]
combined_data_ex_c19$Date[168]
combined_data_ex_c19$Date[182]

# Plot seasonally adjusted commuter rail data with changepoints
cr_ts_ex_c19 <- ts(combined_data_ex_c19$CommuterRail, frequency=12, start=c(2002, 1))
cr_ts_dc_ex_c19 <- decompose(cr_ts_ex_c19, type='additive')
cr_ts_sa_ex_c19 <- cr_ts_ex_c19 - cr_ts_dc_ex_c19$seasonal
plot(cr_ts_sa_ex_c19, xlab='Year', ylab='Number of Commuters', main='Commuter Rail Commuters by Year, Seasonally Adjusted')
chg_pt_cr <- cpt.mean(cr_ts_sa_ex_c19, method='BinSeg')
plot(chg_pt_cr)

# Identify dates of changepoints for commuter rail
print(chg_pt_cr)
combined_data_ex_c19$Date[34]
combined_data_ex_c19$Date[60]
combined_data_ex_c19$Date[118]
combined_data_ex_c19$Date[147]
combined_data_ex_c19$Date[207]