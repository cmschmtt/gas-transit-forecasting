install.packages("dplyr")
install.packages("ggplot2")
library(dplyr)
library(ggpubr)

data <- read.csv('upt_city_sums_groupby_city_CLEAN.csv')
head(data)

SM_Corr_Highest <- 0.0
SM_Corr_Lowest <- 99.0
city_name <- ""
city_name_low <- ""
lst_high = list()
lst_low = list()

# 154 unique cities in data set
for (row in 1:154) {
  
  if (row == 1) {
    temp_data <- slice(data, 1:218)
  }
  else {
    temp_data <- slice(data, (row-1)*218+1:(row*218))
  }
  
  # Spearman Ranks/Delta Added to Data
  data_SM <- temp_data %>% mutate (
    rank_reg_gas = rank(Reg.Gas),
    rank_total = rank(UNLINKED_PASSENGER_TRIPS),
    d = rank_reg_gas - rank_total,
    d_square = d^2) %>%
    select (DATE, Reg.Gas, rank_reg_gas, UNLINKED_PASSENGER_TRIPS, rank_total, d, d_square)
    
  # Spearman rank correlation calculation
  d_square_sum <- sum(data_SM$d_square)
  n <- nrow(data_SM)
  SM_Corr <- (1-(6*(sum(d_square_sum)))/(n*(n^2-1))) %>% round(2)
    
  # A 5% sig level with n=256 -> 0.123 correlation value
  # SM_Corr > 0.123 so we reject the null hypthosis that the national avg. reg. gas price is correlated to total transit ridership in the US
  
  if (SM_Corr > SM_Corr_Highest) {
    SM_Corr_Highest <- SM_Corr
    city_name <- temp_data$UZA_NAME[1]
  }
  
  if (SM_Corr < SM_Corr_Lowest) {
    SM_Corr_Lowest <- SM_Corr
    city_name_low <- temp_data$UZA_NAME[1]
  }
  
  # checking if any other cities have same value - they dont
  if (SM_Corr == 0.3) {
    lst_high <- append(lst, city_name)
  }
  
  if (SM_Corr == 0.02) {
    lst_low <- append(lst_low, city_name_low)
  }
}

data_SM
SM_Corr_Highest
SM_Corr_Lowest
city_name
city_name_low
lst_high
lst_low
