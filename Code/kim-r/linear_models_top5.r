library(lubridate)
library(dplyr)

# Import data
regular_gasoline_df <-  read.csv('C:/Users/mkim0/Documents/MGT 6203/Regular_Gasoline_Retail_Prices.csv', skip=4)
regular_gasoline_df$DATE <- parse_date_time(regular_gasoline_df$Month, orders='b Y')

diesel_gasoline_df <- read.csv('C:/Users/mkim0/Documents/MGT 6203/Diesel_Fuel_Retail_Prices.csv', skip=4)
diesel_gasoline_df$DATE <- parse_date_time(diesel_gasoline_df$Month, orders='b Y')

# Select time period
regular_gasoline_df <- regular_gasoline_df[regular_gasoline_df['DATE'] >= '2002-01-01' & regular_gasoline_df['DATE'] <= '2022-12-01',]
diesel_gasoline_df <- diesel_gasoline_df[diesel_gasoline_df['DATE'] >= '2002-01-01' & diesel_gasoline_df['DATE'] <= '2022-12-01',]

# Combine and format regular and diesel price dataframes
combined_df <- data.frame(regular_gasoline_df['DATE'])
combined_df$GASOLINE_PRICE_REAL <- regular_gasoline_df$Real.Values
combined_df <- inner_join(combined_df, diesel_gasoline_df)
combined_df <- subset(combined_df, , -c(Month, Nominal.Values))
names(combined_df)[names(combined_df) == 'Real.Values'] <- 'DIESEL_PRICE_REAL'
print(combined_df)

# Import UPT by city, filter out dates not in 20-year time period
upt_df <- read.csv('C:/Users/mkim0/Documents/MGT 6203/upt_city_sums_groupby_city.csv')
upt_df$DATE <- parse_date_time(upt_df$DATE, orders='Y m d')
upt_df <- upt_df[upt_df['DATE'] >= '2002-01-01' & upt_df['DATE'] <= '2022-12-01',]

# Model 1: NYC
ny_df <- upt_df[upt_df['UZA_NAME'] == 'New York-Newark, NY-NJ-CT',]
ny_df <- merge(ny_df, combined_df, by='DATE')
ny_model <- lm(formula=UNLINKED_PASSENGER_TRIPS~GASOLINE_PRICE_REAL+DIESEL_PRICE_REAL, data=ny_df)

# Model 2: Chicago
chi_df <- upt_df[upt_df['UZA_NAME'] == 'Chicago, IL-IN',]
chi_df <- merge(chi_df, combined_df, by='DATE')
chi_model <- lm(formula=UNLINKED_PASSENGER_TRIPS~GASOLINE_PRICE_REAL+DIESEL_PRICE_REAL, data=chi_df)

# Model 3: Los Angeles
la_df <- upt_df[upt_df['UZA_NAME'] == 'Los Angeles-Long Beach-Anaheim, CA',]
la_df <- merge(la_df, combined_df, by='DATE')
la_model <- lm(formula=UNLINKED_PASSENGER_TRIPS~GASOLINE_PRICE_REAL+DIESEL_PRICE_REAL, data=la_df)

# Model 4: San Francisco
sf_df <- upt_df[upt_df['UZA_NAME'] == 'San Francisco-Oakland, CA',]
sf_df <- merge(sf_df, combined_df, by='DATE')
sf_model <- lm(formula=UNLINKED_PASSENGER_TRIPS~GASOLINE_PRICE_REAL+DIESEL_PRICE_REAL, data=sf_df)

# Model 5: Washington DC
dc_df <- upt_df[upt_df['UZA_NAME'] == 'Washington--Arlington, DC--VA--MD',]
dc_df <- merge(dc_df, combined_df, by='DATE')
dc_model <- lm(formula=UNLINKED_PASSENGER_TRIPS~GASOLINE_PRICE_REAL+DIESEL_PRICE_REAL, data=dc_df)

# Print model summaries
summary(ny_model)
summary(chi_model)
summary(la_model)
summary(sf_model)
summary(dc_model)