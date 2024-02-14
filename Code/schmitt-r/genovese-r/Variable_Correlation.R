install.packages("dplyr")
install.packages("ggplot2")
library(dplyr)
library(ggpubr)

combined_data <- read.csv('National_Combined_Data.csv')
head(combined_data)

# Spearman Ranks/Delta Added to Data
combined_data_SM <- combined_data %>% mutate (
  rank_reg_gas = rank(Reg.Gas),
  rank_total = rank(Total),
  d = rank_reg_gas - rank_total,
  d_square = d^2) %>%
  select (Date, Reg.Gas, rank_reg_gas, Total, rank_total, d, d_square)
# Sum of delta test, shoud be 0
sum(combined_data_SM$d)

# Spearman rank correlation calculation
d_square_sum <- sum(combined_data_SM$d_square)
n <- nrow(combined_data_SM)
SM_Corr <- (1-(6*(sum(d_square_sum)))/(n*(n^2-1))) %>% round(2)

# A 5% sig level with n=256 -> 0.123 correlation value
# SM_Corr > 0.123 so we reject the null hypthosis that the national avg. reg. gas price is correlated to total transit ridership in the US
SM_Corr

# Plot Reg Gas vs Total Ridership
ggscatter (combined_data, x="Total", y="Reg.Gas",
           color = "blue", cor.coef = TRUE, cor.method = "spearman",
           xlab = "Total Ridership in US", ylab = "Reg. Gas Price ($)")

## Check if Covid years affect ridership numbers ##
covid_test <- arrange(combined_data, Total)
# All 38 of the data points in our scatter plot are made up of the months 3/1/2020 (covid impact starts) to the end of the time series
head(covid_test, 38)


### Testing Correlation Pre-Covid ###
# Create dataset just to 2/1/2020
data_pre_covid <- slice(combined_data, 1:(n() - 38))

# Rerun the Spearman rank correlation process with adjusted dataset
combined_data_SM_adj <- data_pre_covid %>% mutate (
  rank_reg_gas = rank(Reg.Gas),
  rank_total = rank(Total),
  d = rank_reg_gas - rank_total,
  d_square = d^2) %>%
  select (Date, Reg.Gas, rank_reg_gas, Total, rank_total, d, d_square)
# Sum of delta test, shoud be 0
sum(combined_data_SM_adj$d)

# Spearman rank correlation calculation
d_square_sum_adj <- sum(combined_data_SM_adj$d_square)
n_adj <- nrow(combined_data_SM_adj)
SM_Corr_adj <- (1-(6*(sum(d_square_sum_adj)))/(n_adj*(n_adj^2-1))) %>% round(2)

# A 5% sig level with n=218 -> ~0.16 correlation value
# SM_Corr > 0.16 so we reject the null hypthosis that the national avg. reg. gas price is correlated to total transit ridership in the US
SM_Corr_adj

# Although not strongly correlated, there does still seem to be a positive trend between the two variables
ggscatter (data_pre_covid, x="Total", y="Reg.Gas",
           color = "blue", cor.coef = TRUE, cor.method = "spearman",
           xlab = "Total Ridership in US", ylab = "Reg. Gas Price ($)")


### Testing Correlation of One Specific Month ###
# We've also seen that our ridership numbers are cyclical with the time of year.  Colder winter monthes have lower ridership.
# Let's create a dataset of one specific month year after year until the start of covid to see if there is a correlation
jan_data <- data_pre_covid[seq(1, nrow(data_pre_covid), 12),]

# Test correlation again
# Rerun the Spearman rank correlation process with adjusted dataset
jan_data_SM <- jan_data %>% mutate (
  rank_reg_gas = rank(Reg.Gas),
  rank_total = rank(Total),
  d = rank_reg_gas - rank_total,
  d_square = d^2) %>%
  select (Date, Reg.Gas, rank_reg_gas, Total, rank_total, d, d_square)
# Sum of delta test, shoud be 0
sum(jan_data_SM$d)

# Spearman rank correlation calculation
d_square_sum_jan <- sum(jan_data_SM$d_square)
n_jan <- nrow(jan_data_SM)
SM_Corr_jan <- (1-(6*(sum(d_square_sum_jan)))/(n_jan*(n_jan^2-1))) %>% round(2)

# A 5% sig level with n=19 -> 0.46 correlation value
# SM_Corr > 0.46 so we reject the null hypthosis that the national avg. reg. gas price is correlated to total transit ridership in the US
SM_Corr_jan

# Although not strongly correlated, there does still seem to be a positive trend between the two variables
ggscatter (jan_data, x="Total", y="Reg.Gas",
           color = "blue", cor.coef = TRUE, cor.method = "spearman",
           xlab = "Total Ridership in US", ylab = "Reg. Gas Price ($)")


### Test correlation in fair weather city (San Diego) where weather is close to consistant ###
SanDiego_data <- read.csv('SanDiego_Combined_Data.csv')
head(SanDiego_data)
SanDiego_pre_covid <- slice(SanDiego_data, 1:(n() - 38))

# Spearman Ranks/Delta Added to Data
SanDiego_pre_covid_SM <- SanDiego_pre_covid %>% mutate (
  rank_reg_gas = rank(Reg.Gas),
  rank_total = rank(Total),
  d = rank_reg_gas - rank_total,
  d_square = d^2) %>%
  select (ï..Date, Reg.Gas, rank_reg_gas, Total, rank_total, d, d_square)
# Sum of delta test, shoud be 0
sum(SanDiego_pre_covid_SM$d)

# Spearman rank correlation calculation
d_square_sum <- sum(SanDiego_pre_covid_SM$d_square)
n <- nrow(SanDiego_pre_covid_SM)
SM_Corr <- (1-(6*(sum(d_square_sum)))/(n*(n^2-1))) %>% round(2)

# A 5% sig level with n=256 -> 0.123 correlation value
# SM_Corr > 0.123 so we reject the null hypthosis that the national avg. reg. gas price is correlated to total transit ridership in the US
SM_Corr

# Plot Reg Gas vs Total Ridership
ggscatter (SanDiego_pre_covid, x="Total", y="Reg.Gas",
           color = "blue", cor.coef = TRUE, cor.method = "spearman",
           xlab = "Total Ridership in US", ylab = "Reg. Gas Price ($)")


### Granger Causality Test ###
library(lmtest)
grangertest(Total ~ Reg.Gas, order = 1, data = combined_data)
grangertest(Total ~ Reg.Gas, order = 1, data = data_pre_covid)
grangertest(Total ~ Reg.Gas, order = 1, data = jan_data)
grangertest(Total ~ Reg.Gas, order = 1, data = SanDiego_data)

grangertest(Reg.Gas ~ Total, order = 1, data = combined_data)
grangertest(Reg.Gas ~ Total, order = 1, data = data_pre_covid)
grangertest(Reg.Gas ~ Total, order = 1, data = jan_data)
grangertest(Reg.Gas ~ Total, order = 1, data = SanDiego_data)
