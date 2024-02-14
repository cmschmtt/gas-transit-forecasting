setwd("C:/Users/carys/gatech/mgt6203/Team-104/")
library(astsa)
library(vars)
library(tidyverse)
library(Metrics)

# for 70/30 train-test split
TRAIN_IX <- 176
TEST_IX <- 76

# MAIN DATASETS -----------------------------------------------------------

# real gas price - import & cast to date-time
real_gas_diesel <- read.csv('Data/real-gas-diesel.csv')
real_gas_diesel$date <- as.Date(real_gas_diesel$date)
# use only gas price & relevant dates
real_gas_diesel <- real_gas_diesel %>%
  filter(date >= '2002-01-01' & date <= '2022-12-01') %>% 
  dplyr::select(date, Real.Values.gas,)

# unlinked passenger trips - import & cast to date-time
upt <- read.csv('Data/monthly_upt_sums.csv')
upt$date <- as.Date(parse_date_time(upt$date, orders=c("m/Y")))
# filter based on dates
upt <- upt %>% 
  filter(date >= '2002-01-01' & date <= '2022-12-01')


# TOP FIVE CITIES: MODELS ETC. ---------------------------------------------------------


# NYC: MODEL --------------------------------------------------------------

# nyc + tristate area
nyc <- left_join(
  dplyr::select(upt, date, New.York.Newark..NY.NJ.CT),
  real_gas_diesel,
  ) %>% 
  rename('upt' = 'New.York.Newark..NY.NJ.CT')
nyc_data <- dplyr::select(nyc, upt, Real.Values.gas)
nyc_data <- scale(nyc_data)

# train-test split:
nyc_train <- head(nyc_data, TRAIN_IX)
nyc_test <- tail(nyc_data, TEST_IX)

# double-checking lag order
acf(nyc_train)

# fit initial model
var_nyc <-  VAR(nyc_train, p=2, type='both')
summary(var_nyc)

# preds & metrics
nyc_train_preds <- as.numeric(var_nyc$varresult$upt$fitted.values)
mae(nyc_train_preds, nyc_train[3:TRAIN_IX])

nyc_test_preds <- predict(var_nyc, n.ahead=TEST_IX)
upt_preds <- data.frame(nyc_test_preds$fcst$upt)$fcst
mae(nyc_test[1:TEST_IX], upt_preds)



# NYC: PLOT ---------------------------------------------------------------
# train and test are plotted separately in this section.
# CHI has code for one plot.

train_dates <- head(nyc, TRAIN_IX)$date
train_dates <- nyc_train_dates[3:TRAIN_IX]

nyc_train_actuals <- head(as.data.frame(nyc_data), TRAIN_IX)$upt[3:TRAIN_IX]
nyc_train_actuals_df <- data.frame(train_dates, nyc_train_actuals)
nyc_train_preds_df <- data.frame(train_dates, nyc_train_preds)

test_dates <- tail(nyc, TEST_IX)$date

nyc_test_actuals <- tail(as.data.frame(nyc_data), TEST_IX)$upt
nyc_test_actuals_df <- data.frame(test_dates, nyc_test_actuals)
nyc_test_preds_df <- data.frame(test_dates, upt_preds)

plot(nyc_train_actuals_df, type='l',
     main='VAR performance on scaled training data, NYC',
     xlab='time',
     ylab='Scaled NYC unlinked passenger trips',
     col='blue'
)
lines(nyc_train_preds_df, type='l', col='orange')
legend("topleft", legend=c("Actual", "Predicted"), col=c("blue", "orange"), lty=c(1,1))

plot(
  nyc_test_actuals_df,
  main='VAR performance on scaled testing data, NYC',
  type='l',
  xlab='time',
  ylab='Scaled NYC unlinked passenger trips',
  col='blue'
)
lines(nyc_test_preds_df, type='l', col='orange')
legend("bottomleft", legend=c("Actual", "Predicted"), col=c("blue", "orange"), lty=c(1,1))

plot(nyc_train[3:TRAIN_IX], type='l')
lines(nyc_train_preds)
lines(nyc_test[1:TEST_IX], type='l')
lines(upt_preds)
legend(1, 95, legend=c('training data'))


plot(head(nyc, TRAIN_IX)$c(date, upt), type='l')
lines(nyc_train_preds_df)


# NYC: ARIMA --------------------------------------------------------------

# didn't have time to complete this section, but strongly believe it would be
# fruitful to compare ARIMA and VAR performance. it's possible that adding gas
# prices *doesn't* help compared to using just UPT to predict UPT.

nyc_ridership <- nyc$upt
nyc_ridership <- scale(nyc_ridership)
nyc_arima <- arima(head(nyc_ridership, TRAIN_IX), order=c(1, 0, 0))
nyc_arima_preds <- predict(nyc_arima, n.ahead=TEST_IX, se.fit=FALSE)


mae(tail(nyc_ridership, TEST_IX), as.numeric(nyc_arima_preds))

# MODEL: CHICAGO ----------------------------------------------------------

# chicago
chi <- merge(
  select(upt, date, Chicago..IL.IN),
  real_gas_diesel
) %>% 
  rename('upt' = 'Chicago..IL.IN')
chi
chi_data <- dplyr::select(chi, upt, Real.Values.gas)
chi_data <- scale(chi_data)

# train-test split:
chi_train <- head(chi_data, TRAIN_IX)
chi_test <- tail(chi_data, TEST_IX)

acf(nyc_train)

# fit initial model
var_chi <-  VAR(chi_train, p=1, type='both')
summary(var_chi)

chi_train_preds <- as.numeric(var_chi$varresult$upt$fitted.values)
mae(chi_train_preds, chi_train[2:TRAIN_IX])

chi_test_preds <- predict(var_chi, n.ahead=TEST_IX)
upt_preds <- data.frame(chi_test_preds$fcst$upt)$fcst
mae(chi_test[1:TEST_IX], upt_preds)


# PLOT: CHICAGO -----------------------------------------------------------

# plotting preds
train_dates <- head(chi, TRAIN_IX)$date
train_dates <- train_dates[2:TRAIN_IX]

chi_train_actuals <- head(as.data.frame(chi_data), TRAIN_IX)$upt[2:TRAIN_IX]
chi_train_actuals_df <- data.frame(train_dates, chi_train_actuals)
chi_train_preds_df <- data.frame(train_dates[2:TRAIN_IX], chi_train_preds)

test_dates <- tail(chi, TEST_IX)$date

chi_test_actuals <- tail(as.data.frame(chi_data), TEST_IX)$upt
chi_test_actuals_df <- data.frame(test_dates, chi_test_actuals)
chi_test_preds_df <- data.frame(test_dates, upt_preds)

# deeply ashamed of this workaround. would love to know a better way to do this.
# purpose of the white lineplot is to force the x-axis of the plot object to
# span dates of both training and testing set
plot(x=nyc$date, y=rep(0, 252),
     main='VAR performance on scaled training data, CHI',
     xlab='time',
     ylab='Scaled CHI unlinked passenger trips',
     col='white'
)
# plot training and testing actual and predicted onto above plot:
lines(chi_train_actuals_df, type='l', col='blue')
lines(chi_train_preds_df, type='l', col='orange')
lines(chi_test_actuals_df, type='l', lty=2, col='blue')
lines(chi_test_preds_df, type='l', lty=2, col='orange')
# add legend:
legend(
  "bottomleft",
  legend=c("training actual", "training predicted", "testing actual", "testing predicted"),
  col=c("blue", "orange", "blue", "orange"),
  lty=c(1,1,2,2)
)


# MODEL: LA -------------------------------------------------------------------------

# la
losang <- merge(
  select(upt, date, Los.Angeles..Long.Beach..Anaheim..CA),
  real_gas_diesel
)%>% 
  rename('upt' = 'Los.Angeles..Long.Beach..Anaheim..CA')

la_data <- dplyr::select(losang, upt, Real.Values.gas)
la_data <- scale(la_data)

# train-test split:
la_train <- head(la_data, TRAIN_IX)
la_test <- tail(la_data, TEST_IX)

# fit initial model
var_la <-  VAR(la_train, p=2, type='both')
summary(var_la)

la_train_preds <- as.numeric(var_la$varresult$upt$fitted.values)
mae(la_train_preds, la_train[3:TRAIN_IX])

la_test_preds <- predict(var_la, n.ahead=TEST_IX)
upt_preds <- data.frame(la_test_preds$fcst$upt)$fcst
mae(la_test[1:TEST_IX], upt_preds)


# MODEL: SAN FRANCISCO ----------------------------------------------------

# sf
sanfran <- merge(
  select(upt, date, San.Francisco.Oakland..CA),
  real_gas_diesel
)%>% 
  rename('upt' = 'San.Francisco.Oakland..CA')

sf_data <- dplyr::select(sanfran, upt, Real.Values.gas)
sf_data <- scale(sf_data)

# train-test split:
sf_train <- head(sf_data, TRAIN_IX)
sf_test <- tail(sf_data, TEST_IX)

# fit initial model
sf_la <-  VAR(sf_train, p=2, type='both')
summary(sf_la)

sf_train_preds <- as.numeric(sf_la$varresult$upt$fitted.values)
mae(sf_train_preds, sf_train[3:TRAIN_IX])

sf_test_preds <- predict(sf_la, n.ahead=TEST_IX)
upt_preds <- data.frame(sf_test_preds$fcst$upt)$fcst
mae(sf_test[1:TEST_IX], upt_preds)

# MODEL: WASHINGTON D.C. --------------------------------------------------

# dc
dc <- merge(
  select(upt, date, Washington..Arlington..DC..VA..MD),
  real_gas_diesel
)%>% 
  rename('upt' = 'Washington..Arlington..DC..VA..MD')

dc_data <- dplyr::select(dc, upt, Real.Values.gas)
dc_data <- scale(dc_data)

# train-test split:
dc_train <- head(dc_data, TRAIN_IX)
dc_test <- tail(dc_data, TEST_IX)

# fit initial model
dc_la <-  VAR(dc_train, p=1, type='both')
summary(dc_la)

dc_train_preds <- as.numeric(dc_la$varresult$upt$fitted.values)
mae(dc_train_preds, dc_train[2:TRAIN_IX])

dc_test_preds <- predict(dc_la, n.ahead=TEST_IX)
upt_preds <- data.frame(dc_test_preds$fcst$upt)$fcst
mae(dc_test[1:TEST_IX], upt_preds)
