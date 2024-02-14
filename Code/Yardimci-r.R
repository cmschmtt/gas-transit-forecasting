# Load the necessary library
library(tidyverse)

# Read the data (make sure the file path is correct)
data <- read_csv("/Users/osmanyardimci/Desktop/Business Analytics Master/MGT 6203/Team-104/Data/All_Gas_Grades_Reformattedcsv.csv")

# Check for missing values
missing_values <- data %>% is.na() %>% colSums()

print(missing_values)

# Get summary statistics
summary_stats <- data %>% summary()

print(summary_stats)

# Load the necessary libraries
library(tidyverse)
library(lubridate)

# Convert the 'Date' column to datetime
data$Date <- mdy(data$Date)

# Plotting the trends over time for different types of gasoline
ggplot(data, aes(x = Date)) +
  geom_line(aes(y = `All Grades Formulations`), color = "blue", na.rm = TRUE) +
  geom_line(aes(y = `Regular All Formulations`), color = "red", na.rm = TRUE) +
  geom_line(aes(y = `Midgrade All Formulation`), color = "green", na.rm = TRUE) +
  geom_line(aes(y = `Premium All Grades`), color = "purple", na.rm = TRUE) +
  geom_line(aes(y = `Diesel No. 2`), color = "orange", na.rm = TRUE) +
  labs(x = "Date", y = "Price", title = "Gasoline Price Trends Over Time") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  scale_color_manual("", 
                     breaks = c("All Grades Formulations", "Regular All Formulations", "Midgrade All Formulation", "Premium All Grades", "Diesel No. 2"),
                     values = c("blue", "red", "green", "purple", "orange"))

# We can see from the graph:
#   
#   Over time, all gasoline and diesel types exhibit a similar tendency. They all 
#   fluctuate, but they seem to rise and fall in lockstep, implying that the same 
#   reasons may be driving all of their values.
# All of the curves include many peaks and troughs, signifying price highs and 
# lows. Changes in crude oil prices, refining expenses, distribution and marketing
# costs, and gasoline taxes might all contribute to this.
# 
# Formulations for All Grades and Premium Prices for all grades of gasoline are 
# often higher than for other kinds of fuel. Diesel No. 2 costs the same as 
# Regular All Formulations.
# 
# Prices for all forms of fuel seem to be growing over time, which might be due to
# inflation, greater demand, or other economic issues.
# 
# Please let me know if you want further analysis or have particular queries 
# regarding the data.


# Convert the 'Date' column to datetime and extract year
data <- read_csv("/Users/osmanyardimci/Desktop/Business Analytics Master/MGT 6203/Team-104/Data/All_Gas_Grades_Reformattedcsv.csv")
data$Date <- mdy(data$Date)
data$Year <- year(data$Date)

# Compute yearly average prices
yearly_avg_prices <- data %>% 
  group_by(Year) %>% 
  summarise(across(where(is.numeric), mean, na.rm = TRUE))

# Plotting the yearly average prices for each type of gasoline
ggplot(yearly_avg_prices, aes(x = Year)) +
  geom_line(aes(y = `All Grades Formulations`), color = "blue") +
  geom_line(aes(y = `Regular All Formulations`), color = "red") +
  geom_line(aes(y = `Midgrade All Formulation`), color = "green") +
  geom_line(aes(y = `Premium All Grades`), color = "purple") +
  geom_line(aes(y = `Diesel No. 2`), color = "orange") +
  labs(x = "Year", y = "Yearly Average Price", title = "Yearly Average Gasoline Price Trends") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  scale_color_manual("", 
                     breaks = c("All Grades Formulations", "Regular All Formulations", "Midgrade All Formulation", "Premium All Grades", "Diesel No. 2"),
                     values = c("blue", "red", "green", "purple", "orange"))

# Volatility analysis
# For volatility analysis, we'll compute and compare the standard deviation of the prices for each type of gasoline
volatility <- data %>% 
  summarise(across(where(is.numeric), sd, na.rm = TRUE))

print(volatility)


# The trend analysis plot shows the yearly average price for each type of gasoline
# and diesel. We can observe a generally increasing trend over the years for all 
# fuel types. There are some periods of sharp increase followed by decreases, which
# could be due to various economic factors.
# 
# For volatility analysis, we computed the standard deviation of the prices for 
# each type of gasoline and diesel. The standard deviation is a measure of the 
# amount of variation or dispersion in a set of values. A low standard deviation 
# means that the values tend to be close to the mean, while a high standard 
# deviation means that the values are spread out over a wider range.
# 
# Here are the standard deviations for each type of fuel:
# 
# All Grades Formulations: 0.739
# All Grade Conventional: 0.725
# All Grades Reformulated: 0.773
# Regular All Formulations: 0.732
# Regular Convential: 0.721
# Regular Reformulated: 0.762
# Midgrade All Formulation: 0.775
# Midgrade Conventional: 0.749
# Midgrade Reformulated: 0.820
# Premium All Grades: 0.809
# Premium Conventional: 0.782
# Premium Reformulated: 0.841
# Diesel No. 2: 0.921
# These numbers show that Diesel No. 2 has the highest volatility, while Regular 
# Convential has the lowest volatility. The reformulated types of gasoline 
# generally have higher volatility than their conventional counterparts.


# Load the data
data1 <- read.csv('/Users/osmanyardimci/Desktop/Business Analytics Master/MGT 6203/Team-104/Data/National_Combined_Data.csv')

# Display the first few rows of the dataframe
head(data1)


# The dataset contains monthly national data (from the United States) regarding 
# various modes of transportation and fuel prices. Here's a brief description of 
# the columns:
# 
# Date: The date on which the data was collected, presumably on a monthly basis.
# Reg Gas: The average price of regular gasoline in that month.
# Diesel: The average price of diesel fuel in that month.
# Bus: The number of bus rides taken in that month.
# CommuterRail: The number of commuter rail rides taken in that month.
# DemandResponse: The number of demand response rides taken in that month.
# FerryBoat: The number of ferry boat rides taken in that month.
# HeavyRail: The number of heavy rail rides taken in that month.
# LightRail: The number of light rail rides taken in that month.
# Monorail_AutomatedGuideway: The number of monorail or automated guideway rides 
# taken in that month.
# Trolley: The number of trolley rides taken in that month.
# VanPool: The number of vanpool rides taken in that month.
# Total: The total number of rides taken in that month across all modes of transport.


# Descriptive statistics
summary(data1)


# Here are some basic statistical insights from the dataset:
# 
# Reg Gas: The average price of regular gasoline over the entire dataset is 
# approximately $3.56, with a minimum of $1.89 and a maximum of $5.66.
# 
# Diesel: The average price of diesel over the entire dataset is approximately 
# $3.92, with a minimum of $1.97 and a maximum of $6.53.
# 
# Bus: The average number of bus rides taken per month is about 381 million, 
# with a minimum of 108 million and a maximum of 506 million.
# 
# CommuterRail: The average number of commuter rail rides taken per month is about
# 35.4 million, with a minimum of 2.83 million and a maximum of 46.3 million.
# 
# DemandResponse: The average number of demand response rides taken per month is 
# about 7.39 million, with a minimum of 2.30 million and a maximum of 9.24 million.
# 
# FerryBoat: The average number of ferry boat rides taken per month is about 5.15 
# million, with a minimum of 0.94 million and a maximum of 8.92 million.
# 
# HeavyRail: The average number of heavy rail rides taken per month is about 274 
# million, with a minimum of 30 million and a maximum of 360 million.
# 
# LightRail: The average number of light rail rides taken per month is about 34.2 
# million, with a minimum of 9.27 million and a maximum of 44.9 million.
# 
# Monorail_AutomatedGuideway: The average number of monorail or automated guideway
# rides taken per month is about 1.27 million, with a minimum of 0.13 million and 
# a maximum of 2.20 million.
# 
# Trolley: The average number of trolley rides taken per month is about 7.41 million,
# with a minimum of 1.22 million and a maximum of 10.4 million.
# 
# VanPool: The average number of vanpool rides taken per month is about 2.22 million,
# with a minimum of 0.81 million and a maximum of 3.44 million.
# 
# Total: The average total number of rides taken per month across all modes of 
# transport is about 749 million, with a minimum of 156 million and a maximum of 947 million.

# Assuming data1 is your loaded data frame, you can check its structure with str(data1)
# Make sure you have only numeric columns for fuel prices and transportation types

# Select only numeric columns. Exclude 'Date' or other non-numeric columns.
data_numeric <- data1 %>% select_if(is.numeric)

# Calculate correlation matrix
correlation_matrix <- cor(data_numeric)

# Check the column names of your data
print(colnames(data_numeric))

# Adjust the column names and indices below based on the output of the above command

# Extract the correlations between fuel prices and transportation types
gas_correlations <- correlation_matrix['Reg.Gas', 1:(ncol(data_numeric)-2)]
diesel_correlations <- correlation_matrix['Diesel', 1:(ncol(data_numeric)-2)]

# Combine the correlations into a data frame
fuel_correlations <- data.frame('Regular Gasoline' = gas_correlations, 'Diesel' = diesel_correlations)

# Print the correlations
print(fuel_correlations)

# Here are the correlation coefficients between fuel prices and various types of transportation:
#   
#   CommuterRail:
#   
#   Regular Gasoline: 0.25
# Diesel: 0.17
# DemandResponse:
#   
#   Regular Gasoline: 0.42
# Diesel: 0.39
# FerryBoat:
#   
#   Regular Gasoline: 0.14
# Diesel: 0.09
# HeavyRail:
#   
#   Regular Gasoline: 0.32
# Diesel: 0.25
# LightRail:
#   
#   Regular Gasoline: 0.38
# Diesel: 0.32
# Monorail_AutomatedGuideway:
#   
#   Regular Gasoline: 0.32
# Diesel: 0.30
# Trolley:
#   
#   Regular Gasoline: 0.14
# Diesel: -0.03
# VanPool:
#   
#   Regular Gasoline: 0.41
# Diesel: 0.45
# It appears that for most types of transportation, there is a positive 
# correlation between fuel prices and ridership, although the strength of the
# correlation varies. The strongest correlations appear to be between the prices
# of both types of fuel and DemandResponse and VanPool usage. This suggests that
# as fuel prices increase, so does the usage of these types of public 
# transportation. However, the correlation is still moderate, and there could 
# be other factors influencing these types of transportation usage.
# 
# For Trolley, there is a weak positive correlation with regular gasoline prices 
# and a weak negative correlation with diesel prices.