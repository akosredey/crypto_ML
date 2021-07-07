################################################################################
#        CAS Data Engineering Modul 2 - "Crypto Gruppe"                        #
#        Time Series Analysis of Bitcoin Prices with R                         #
#         -> Statistical Tests and ARIMA Experiment <-                         #
#                                                                              #
#                         09 July 2021                                         #
#                                                                              #
################################################################################
# References: 
#
# SUMNER, T. 14/11/2019. Forecasting Bitcoin in R. 
# Available from: https://rstudio-pubs-static.s3.amazonaws.com/549884_39fa223876e448608b7a7fa79337feba.html
#
# JAQUART, P. DANN, D. WEINHARDT, Ch. Short-term bitcoin market prediction via machine learning
# Available online at www.sciencedirect.com
#
# URAS, N. MARCHESI, L. MARCHESI, M. TONELLI, R. Forecasting Bitcoin closing price series using linear regression and neural networks models
# Uras et al. (2020), PeerJ Comput. Sci., DOI 10.7717/peerj-cs.279
#
#
################################################################################


# Remove Data & Libraries
rm(list=ls())

#-------------------------------------------------------------------------------
# Load Libraries
#-------------------------------------------------------------------------------

library(foreign)
library(psych)
library(dplyr)
library(tidyverse)
library(zoo)
library(lubridate)
library(fpp2)
library(astsa)
library(plotly)
library(tseries)

#-------------------------------------------------------------------------------
# Initial configurations
#-------------------------------------------------------------------------------

# Set default font
windowsFonts(Georgia = windowsFont("Georgia"))


## Set Original plot theme
my_theme = theme(panel.grid = element_line(color = '#e6e6e6'),
                 panel.background = element_rect(fill = 'white'),
                 plot.title = element_text(hjust = .5, size = 28, 
                                           colour = '#ffa500'),
                 text = element_text(family = 'Georgia'),
                 axis.text = element_text(size = 10),
                 axis.title = element_text(size = 18, family = 'Georgia', 
                                           face = 'bold'),
                 axis.line = element_line(colour = '#737373', size = 1),
                 strip.background = element_rect(colour = "black", 
                                                 fill = "white"),
                 strip.text = element_text(face = 'bold'))  


#-------------------------------------------------------------------------------
# Read Data
#-------------------------------------------------------------------------------


# Read Original Bitcoin Dataset
bitcoin <- read_csv('bitcoin_full_daily_prices.csv')
names(bitcoin)

# Read United BTC Dataset (used in the Python Modelling)
btc <- read_csv('bitcoin_full_daily_returns.csv')
names(btc)


#-------------------------------------------------------------------------------
# Create Time series Dataset
#-------------------------------------------------------------------------------

# Create original time series Dataset
bit_ts = bitcoin %>%
  filter(Date > as.Date('2017-01-01')) %>%
  arrange(Date) %>%
  select(WeightedPrice) %>%
  as.matrix() %>%
  ts()


# Create time series from BTC returns Dataset
bit_ret_ts = btc %>%
  filter(Date > as.Date('2017-01-01')) %>%
  arrange(Date) %>%
  select(WeightedPrice_return) %>%
  as.matrix() %>%
  ts()

#-------------------------------------------------------------------------------
# Plot BTC Prices (full Dataset)
#-------------------------------------------------------------------------------

# Plot BTC Data
ggplotly(ggplot(bitcoin, aes(Date, WeightedPrice)) + geom_line(col = '#ffa500') + 
           labs(title = 'Bitcoin Weighted Prices 2014 -2021', x = '') +
           scale_y_continuous(breaks = c(0, 5000, 10000, 15000, 30000, 60000), 
                              labels = c('$0', '$5,000', '$10,000', '$15,000', 
                                         '$30,000', '$60,000')) + my_theme)

#-------------------------------------------------------------------------------
# Plot BTC Prices after 2017
#-------------------------------------------------------------------------------


ggplotly(bitcoin %>%
           filter(Date > as.Date('2017-01-01')) %>% ggplot(aes(Date, 
                                                               WeightedPrice)) + 
           geom_line(col = '#ffa500') + 
           labs(title = 'Bitcoin Weighted Prices after 2017', x = '') +
           scale_y_continuous(breaks = c(0, 5000, 10000, 15000, 30000, 60000), 
                              labels = c('$0', '$5,000', '$10,000', '$15,000', 
                                         '$30,000', '$60,000')) + my_theme)




#-------------------------------------------------------------------------------
# Correlation plots for BTC Prices & its lags
#-------------------------------------------------------------------------------


gglagplot(bit_ts, do.lines = F) + my_theme +
  scale_color_continuous(low = "#b37400", high = "#ffc04d", 
                         breaks = c(1, 366, 731, 1097, 1463), 
                         labels = c('2017', '2018', '2019', '2020', '2021')) + 
  scale_y_continuous(breaks = c(0, 5000, 10000, 15000, 30000, 60000), 
                     labels = c('$0', '$5,000', '$10,000', '$15,000', 
                                '$30,000', '$60,000')) +
  scale_x_continuous(breaks = c(5000, 10000, 15000, 30000, 60000), 
                     labels = c('$5,000', '$10,000', '$15,000', 
                                '$30,000', '$60,000'))


#-------------------------------------------------------------------------------
# Autocorrelation (ACF) and Partial Autocorrelation (PACF) plots
#-------------------------------------------------------------------------------

ggAcf(bit_ts, lag.max = 200) + my_theme + labs(title = 'ACF' , y = 'Correlation')

ggPacf(bit_ts, lag.max = 200) + my_theme + labs(title = 'PACF', y = '')


#-------------------------------------------------------------------------------
# Autocorrelation (ACF) and Partial Autocorrelation (PACF) after differencing
#-------------------------------------------------------------------------------

ggAcf(diff(bit_ts), lag.max = 200) + my_theme + 
  labs(title = 'ACF with First Differnce' , y = 'Correlation') 

ggPacf(diff(bit_ts), lag.max = 200) + my_theme + 
  labs(title = 'PACF with First Difference', y = '')


#-------------------------------------------------------------------------------
# Autocorrelation (ACF) and Partial Autocorrelation (PACF) with daily returns 
#-------------------------------------------------------------------------------

ggAcf(bit_ret_ts, lag.max = 200) + my_theme + 
  labs(title = 'ACF with Daily Returns' , y = 'Correlation') 

ggPacf(bit_ret_ts, lag.max = 200) + my_theme + 
  labs(title = 'PACF with Daily Returns', y = '')


#-------------------------------------------------------------------------------
# Plot First Difference after 2017
#-------------------------------------------------------------------------------

cut_bit_df = bitcoin %>%
  filter(Date > as.Date('2017-01-01'))

ggplotly(cut_bit_df[-1,] %>%
           mutate(WeightedPrice = diff(cut_bit_df$WeightedPrice)) %>%
           ggplot(aes(Date, WeightedPrice)) + geom_line(col = '#ffa500') + 
           my_theme + labs(x = '', title = 'Bitcoin Differenced By One', 
                           y = 'Difference'))

#-------------------------------------------------------------------------------
# Plot Daily Returns after 2017
#-------------------------------------------------------------------------------

cut_bit_ret_df = btc %>%
  filter(Date > as.Date('2017-01-01'))

ggplotly(cut_bit_ret_df[-1,] %>%
           mutate(WeightedPrice_return = diff(
             cut_bit_ret_df$WeightedPrice_return)) %>%
           ggplot(aes(Date, WeightedPrice_return)) + geom_line(col = '#ffa500') + 
           my_theme + labs(x = '', title = 'Bitcoin Daily Returns', 
                           y = '% Returns'))

#-------------------------------------------------------------------------------
# Box-Cox Normalization of Bitcoin Prices
#-------------------------------------------------------------------------------

BoxCox.lambda(bit_ts)

ggplotly(cut_bit_df %>%
           mutate(WeightedPrice = BoxCox(cut_bit_df$WeightedPrice, 
                                         lambda=BoxCox.lambda(
                                           cut_bit_df$WeightedPrice))) %>%
           ggplot(aes(Date, WeightedPrice)) + geom_line(col = '#ffa500') + 
           my_theme + labs(x = '', title = 'Bitcoin Box-Cox transformed', 
                           y = 'BTC Price Transformed'))


#-------------------------------------------------------------------------------
# Plot First Difference, Transformed First Difference & Daily Returns
#-------------------------------------------------------------------------------

## Original Price
cut_bit_df[-1,] %>%
  mutate(WeightedPrice = diff(cut_bit_df$WeightedPrice)) %>%
  ggplot(aes(Date, WeightedPrice)) + geom_line(col = '#650fba') + my_theme + 
  labs(x = '', title = 'Original BTC Price', y = 'Difference')

## Transformed Price
cut_bit_df[-1,] %>%
  mutate(WeightedPrice = diff(BoxCox(cut_bit_df$WeightedPrice, 
                                     lambda = BoxCox.lambda(
                                       cut_bit_df$WeightedPrice)))) %>%
  ggplot(aes(Date, WeightedPrice)) + geom_line(col = '#650fba') + my_theme + 
  labs(x = '', title = 'Transformed BTC Price', y = '')

## Daily Returns of Price
cut_bit_ret_df[-1,] %>%
  ggplot(aes(Date, WeightedPrice_return)) + geom_line(col = '#650fba') + my_theme + 
  labs(x = '', title = 'Daily Returns of BTC Price', y = '')


#-------------------------------------------------------------------------------
# Autocorrelation (ACF) and Partial Autocorrelation (PACF) transformed Prices 
#-------------------------------------------------------------------------------

bit_ts_tran = BoxCox(bit_ts, lambda = BoxCox.lambda(bit_ts))

ggAcf(diff(bit_ts_tran), lag.max = 200) + my_theme + labs(title = 'ACF' , y = 'Correlation') 

ggPacf(diff(bit_ts_tran), lag.max = 200) + my_theme + labs(title = 'PACF', y = '')


#-------------------------------------------------------------------------------
# Test for Stationarity
#-------------------------------------------------------------------------------   

# Daily Prices Dataset
adf.test(bit_ts) # p-value < 0.05 indicates the TS is stationary

# Box-Cox Transformed Dataset
adf.test(bit_ts_tran) # p-value < 0.05 indicates the TS is stationary

# Daily Returns Dataset
adf.test(bit_ret_ts) # p-value < 0.05 indicates the TS is stationary


#-------------------------------------------------------------------------------
# Try an Autoarima with transformed data
#-------------------------------------------------------------------------------

arima_tr <- auto.arima(bit_ts_tran)

checkresiduals(arima_tr)

#-------------------------------------------------------------------------------
# Try an Autoarima with daily returns data
#-------------------------------------------------------------------------------

arima_ret <- auto.arima(bit_ret_ts)

checkresiduals(arima_ret)

#-------------------------------------------------------------------------------
# Taking only 2020
#-------------------------------------------------------------------------------

cut2_bit_df = cut_bit_df %>%
  filter(Date >= ymd('2020-01-01'))

ggplotly(cut2_bit_df %>%
           mutate(WeightedPrice = BoxCox(cut2_bit_df$WeightedPrice, 
                                         lambda = BoxCox.lambda(
                                           cut2_bit_df$WeightedPrice))) %>%
           ggplot(aes(Date, WeightedPrice)) + geom_line(col = '#ffa500') + 
           labs(title = 'Bitcoin', x = '', y = 'Price (Transformed)') + my_theme)


ggplotly(cut2_bit_df[-1,] %>%
           mutate(WeightedPrice = diff(BoxCox(cut2_bit_df$WeightedPrice, 
                                      lambda = BoxCox.lambda(
                                        cut2_bit_df$WeightedPrice)))) %>%
           ggplot(aes(Date, WeightedPrice)) + geom_line(col = '#ffa500') + 
           my_theme + labs(x = '', title = 'Transformed Price', y = 'Difference'))


#-------------------------------------------------------------------------------
# ACF, PCF only for 2020
#-------------------------------------------------------------------------------

bit_ts2 = bitcoin %>%
  filter(Date >= as.Date('2020-01-01')) %>%
  arrange(Date) %>%
  select(WeightedPrice) %>%
  as.matrix() %>%
  ts()

bit_ts_tran2 = BoxCox(bit_ts2, lambda = BoxCox.lambda(bit_ts2))

ggAcf(diff(bit_ts_tran2), lag.max = 200) + my_theme + labs(title = 'ACF' , 
                                                           y = 'Correlation') 

ggPacf(diff(bit_ts_tran2), lag.max = 200) + my_theme + labs(title = 'PACF', y = '')

#-------------------------------------------------------------------------------
# Autoarima for Data only 2020
#-------------------------------------------------------------------------------                                                           

arima_2020_tr <- auto.arima(bit_ts_tran2)

checkresiduals(arima_2020_tr)

#-------------------------------------------------------------------------------
# Taking only 2021
#-------------------------------------------------------------------------------

cut3_bit_df = cut_bit_df %>%
  filter(Date >= ymd('2021-01-01'))

ggplotly(cut3_bit_df %>%
           mutate(WeightedPrice = BoxCox(cut3_bit_df$WeightedPrice, 
                                         lambda = BoxCox.lambda(
                                           cut3_bit_df$WeightedPrice))) %>%
           ggplot(aes(Date, WeightedPrice)) + geom_line(col = '#ffa500') + 
           labs(title = 'Bitcoin', x = '', y = 'Price (Transformed)') + my_theme)


ggplotly(cut3_bit_df[-1,] %>%
           mutate(WeightedPrice = diff(BoxCox(cut3_bit_df$WeightedPrice, 
                                              lambda = BoxCox.lambda(
                                                cut3_bit_df$WeightedPrice)))) %>%
           ggplot(aes(Date, WeightedPrice)) + geom_line(col = '#ffa500') + 
           my_theme + labs(x = '', title = 'Transformed Price', y = 'Difference'))


#-------------------------------------------------------------------------------
# ACF, PCF only for 2021
#-------------------------------------------------------------------------------

bit_ts3 = bitcoin %>%
  filter(Date >= as.Date('2021-01-01')) %>%
  arrange(Date) %>%
  select(WeightedPrice) %>%
  as.matrix() %>%
  ts()

bit_ts_tran3 = BoxCox(bit_ts3, lambda = BoxCox.lambda(bit_ts2))

ggAcf(diff(bit_ts_tran3), lag.max = 200) + my_theme + labs(title = 'ACF' , 
                                                           y = 'Correlation') 

ggPacf(diff(bit_ts_tran3), lag.max = 200) + my_theme + labs(title = 'PACF', y = '')

#-------------------------------------------------------------------------------
# Autoarima for Data only 2021
#-------------------------------------------------------------------------------                                                           

arima_2021_tr <- auto.arima(bit_ts_tran3)

checkresiduals(arima_2021_tr)

#-------------------------------------------------------------------------------
# ARIMA Model Fits
#-------------------------------------------------------------------------------   

## Data after 2017
bit_ts_past_2017 = bitcoin %>%
  filter(Date >= as.Date('2017-01-01')) %>%
  arrange(Date) %>%
  select(WeightedPrice) %>%
  as.matrix() %>%
  ts()

bit_ts_past_2017 %>%
  BoxCox(lambda = BoxCox.lambda(bit_ts_past_2017)) %>%
  Arima(order = c(0,1,0), include.drift = T) %>%
  checkresiduals()

summary(Arima(bit_ts_tran, order = c(0,1,0), include.drift = T))



## Daily Return Data after 2017
bit_ts_ret_past_2017 = btc %>%
  filter(Date >= as.Date('2017-01-01')) %>%
  arrange(Date) %>%
  select(WeightedPrice_return) %>%
  as.matrix() %>%
  ts()

bit_ts_ret_past_2017 %>%
  Arima(order = c(0,1,0), include.drift = T) %>%
  checkresiduals()


summary(Arima(bit_ret_ts, order = c(0,1,0), include.drift = T))


## Random Walk Test on Daily Data only 2020
bit_ts_tran2 %>%
  Arima(order = c(0,1,0), include.drift = T) %>%
  checkresiduals()


summary(Arima(bit_ret_ts, order = c(0,1,0), include.drift = T))
  

## Random Walk Test on Daily Data only 2021
bit_ts_tran3 %>%
  Arima(order = c(0,1,0), include.drift = T) %>%
  checkresiduals()


summary(Arima(bit_ret_ts, order = c(0,1,0), include.drift = T))



#-------------------------------------------------------------------------------
# Check errors
#-------------------------------------------------------------------------------   

# Transformed Data
err_tr = residuals(Arima(bit_ts_tran, order = c(0,1,0), include.drift = T))
cat('Standard Deviation = ', sd(err_tr))
cat('Mean =', mean(err_tr))


invers_BoxCox = function(ts_data, lambda){
  original_ts = (ts_data * lambda + 1) ** (1/lambda)
  return(original_ts)
}

invers_BoxCox(sd(err_tr), BoxCox.lambda(bit_ts))



# Daily Return Data
err_ret = residuals(Arima(bit_ret_ts, order = c(0,1,0), include.drift = T))
cat('Standard Deviation = ', sd(err_ret))
cat('Mean =', mean(err_ret))


invers_BoxCox(sd(err_ret), BoxCox.lambda(bit_ret_ts))

#-------------------------------------------------------------------------------
# Forecast with ARIMA
#-------------------------------------------------------------------------------   


## h is the the length you want the prediction to be in units of days

fit_model = function(bitcoin_data, h){
  bitcoin_df = bitcoin_data %>%
    filter(Date >= as.Date('2017-01-01')) %>%
    arrange(Date)
  
  time_series = bitcoin_df %>%
    select(WeightedPrice) %>%
    ts()
  
  predictions = time_series %>%
    BoxCox(lambda = BoxCox.lambda(time_series)) %>% 
    auto.arima() %>%
    forecast(h)
  
  forecast_df = cbind(data.frame(predictions[4]), 
                      data.frame(predictions[5]), 
                      data.frame(predictions[6]))
  
  the_forecast = invers_BoxCox(forecast_df, lambda = BoxCox.lambda(time_series))
  
  the_forecast = the_forecast %>%
    mutate(Date = tail(bitcoin_df$Date, h) + h) %>%
    as_tibble()
  
  return(the_forecast)
}



# read the updated data for BTC prices

bitcoin_new <- read_csv('btc_base_dataset_NEW.csv')


# Plot the new BTC Data
ggplotly(ggplot(bitcoin_new, aes(Date, WeightedPrice)) + 
           geom_line(col = '#ffa500') + 
           labs(title = 'Bitcoin Weighted Prices 2014 -2021 (new)', x = '') +
           scale_y_continuous(breaks = c(0, 5000, 10000, 15000, 30000, 60000), 
                              labels = c('$0', '$5,000', '$10,000', '$15,000', 
                                         '$30,000', '$60,000')) + my_theme)

## Predict the BTC Price for the next 30 Days

ggplotly(fit_model(bitcoin, 30) %>%
           ggplot(aes(x = Date, y = mean)) + geom_line(col = '#ff2500') +
           geom_ribbon(aes(ymin = lower.80., ymax = upper.80.), alpha = .3, 
                       fill = '#ffc04c') +
           geom_ribbon(aes(ymin = lower.95., ymax = upper.95.), alpha = .3, 
                       fill = '#ffe4b2') +
           geom_line(data = bitcoin_new, aes(Date, WeightedPrice)) +
           geom_line(data = filter(bitcoin, Date >= as.Date('2015-01-01')), 
                     aes(Date, WeightedPrice), col = '#ffa500') + my_theme +
           labs(title = 'Bitcoin Prediction of 30 Days', y = 'Price', x = '') +
           scale_y_continuous(breaks = c(0, 5000, 10000, 15000, 20000, 25000, 
                                         30000, 35000, 40000, 45000, 50000, 
                                         55000, 60000), 
                              labels = c('$0', '$5,000', '$10,000', '$15,000',
                                         '$20,000', '$25,000', '$30,000', 
                                         '$35,000', '$40,000', '$45,000', 
                                         '$50,000', '$55,000', '$60,000')))



