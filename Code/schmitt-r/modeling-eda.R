# MODELING EDA ------------------------------------------------------------

# This script file contains code for model-specific exploratory data analysis,
# including line plots, partial autocorrelation function plots, and partial
# autocorrelation function plots.

# imports
setwd("C:/Users/carys/gatech/mgt6203/Team-104/")
library(tidyverse)
library(stats)
library(fpp3)
library(astsa)

# MAIN DATASETS -----------------------------------------------------------

# inflation-adjusted gas prices:
real_gas_diesel <- read.csv('Data/real-gas-diesel.csv')
real_gas_diesel$date <- as.Date(real_gas_diesel$date)
real_gas_diesel <- real_gas_diesel %>%
  filter(date >= '2002-01-01' & date <= '2022-12-01') %>%
  dplyr::select(date, Real.Values.gas)

# ACF and PACF of gas:
acf(real_gas_diesel$Real.Values.gas,
    main = 'Autocorrelation of real gas prices')
pacf(real_gas_diesel$Real.Values.gas,
     main = 'Partial autocorrelation of real gas prices')

# unlinked passenger trips:
upt <- read.csv('Data/monthly_upt_sums.csv')
upt$date <- as.Date(parse_date_time(upt$date, orders = c("m/Y")))
upt <- upt %>%
  filter(date >= '2002-01-01' & date <= '2022-12-01')

# NYC line plot, ACF, & PACF:
nyc_eda <- dplyr::select(upt, date, New.York.Newark..NY.NJ.CT)
plot(
  x = nyc_eda$date,
  y = nyc_eda$New.York.Newark..NY.NJ.CT,
  type = 'l',
  main = 'Weekly unlinked passenger trips, NYC tri-state area'
)
acf(nyc_eda$New.York.Newark..NY.NJ.CT,
    main = "Autocorrelation of NYC UPT")
pacf(nyc_eda$New.York.Newark..NY.NJ.CT,
     main = "Partial autocorrelation of NYC UPT")

# CHI EDA:
chi_eda <- dplyr::select(upt, date, Chicago..IL.IN)
plot(
  x = nyc_eda$date,
  y = chi_eda$Chicago..IL.IN,
  type = 'l',
  main = 'Weekly unlinked passenger trips, Chicago'
)
acf(chi_eda$Chicago..IL.IN, main = "Autocorrelation of Chicago UPT")
pacf(chi_eda$Chicago..IL.IN, main = "Partial autocorrelation of Chicago UPT")

# LA EDA:
la_eda <-
  dplyr::select(upt, date, Los.Angeles..Long.Beach..Anaheim..CA)
plot(
  x = la_eda$date,
  y = la_eda$Los.Angeles..Long.Beach..Anaheim..CA,
  type = 'l'
)
acf(
  la_eda$Los.Angeles..Long.Beach..Anaheim..CA,
  main = "Autocorrelation of LA-Long Beach-Anaheim UPT"
)
pacf(
  la_eda$Los.Angeles..Long.Beach..Anaheim..CA,
  main = "Partial autocorrelation of LA-Long Beach-Anaheim UPT"
)

# SF EDA:
sf_eda <- dplyr::select(upt, date, San.Francisco.Oakland..CA)
plot(
  x = sf_eda$date,
  y = sf_eda$San.Francisco.Oakland..CA,
  type = 'l',
  main = 'Weekly unlinked passenger trips, San Francisco/Oakland'
)
acf(sf_eda$San.Francisco.Oakland..CA, main = "Autocorrelation of San Francisco/Oakland")
pacf(sf_eda$San.Francisco.Oakland..CA, main = "Partial autocorrelation of San Francisco/Oakland")

# DC EDA:
dc_eda <-
  dplyr::select(upt, date, Washington..Arlington..DC..VA..MD)
plot(
  x = dc_eda$date,
  y = dc_eda$Washington..Arlington..DC..VA..MD,
  type = 'l'
)
acf(dc_eda$Washington..Arlington..DC..VA..MD, main = "Autocorrelation of D.C. tri-state area")
pacf(dc_eda$Washington..Arlington..DC..VA..MD, main = "Partial autocorrelation of D.C. tri-state area")

# DECOMPOSITION EDA -------------------------------------------------------
# Univariate decomposition models for the purpose of better understanding
# market trends.

plot(decompose(ts(chi_eda$Chicago..IL.IN, freq = 52)))
title('\nChicago')

plot(decompose(ts(
  nyc_eda$New.York.Newark..NY.NJ.CT, freq = 52
)))
title('\nNYC tri-state area')

plot(decompose(ts(
  la_eda$Los.Angeles..Long.Beach..Anaheim..CA, freq = 52
)))
title('\nLA-Long Beach-Anaheim')