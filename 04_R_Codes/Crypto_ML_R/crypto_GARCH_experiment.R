# Remove Data & Libraries
rm(list=ls())

#-------------------------------------------------------------------------------
# Load Libraries
#-------------------------------------------------------------------------------

library(rugarch)
library(tseries)
library(fBasics)
library(zoo)
library(lmtest) 
library(forecast)

#-------------------------------------------------------------------------------
# Initial configurations
#-------------------------------------------------------------------------------

# Set default font
windowsFonts(Georgia = windowsFont("Georgia"))

# Set working directory
setwd("/Users/akosr/CAS_DAENG/Modul_02/crypto_project_ML/00_Data/")


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
# Load and transform data
#-------------------------------------------------------------------------------

bitcoin         <- read_csv('bitcoin_full_daily_prices.csv')

# Create time series object
bitcoin_ts      <- zoo(bitcoin$WeightedPrice, as.Date(as.character(bitcoin$Date),
                                                 format=c("%Y-%m-%d")))

# Create Log Return time series
bitcoin_rets    <- log(bitcoin_ts/lag(bitcoin_ts, -1))

# strip off the dates and create a numeric object
bitcoin_ret_num <- coredata(bitcoin_rets)

# compute statistics
basicStats(bitcoin_ts)


# Plot time series
plot(bitcoin_ts, type='l', ylab = "BTC Price", 
     main="Plot of 2014-2021 daily Bitcoin prices", col = 'red')

#-------------------------------------------------------------------------------
# Plot Autocorrelation and Partial Autocorrelation
#-------------------------------------------------------------------------------

acf(coredata(bitcoin_ts), main="ACF plot of 2014-2021 daily Bitcoin prices")
pacf(coredata(bitcoin_ts), main="ACF plot of 2014-2021 daily Bitcoin prices")

#-------------------------------------------------------------------------------
# Daily Return Data - Statistics
#-------------------------------------------------------------------------------

basicStats(bitcoin_rets)

#Histogram
hist(bitcoin_rets, xlab="Daily return of Bitcoin prices", prob=TRUE, 
     main="Histogram for daily return of Bitcoin prices")
xfit<-seq(min(bitcoin_rets),max(bitcoin_rets),length=30)
yfit<-dnorm(xfit,mean=mean(bitcoin_rets),sd=sd(bitcoin_rets))
lines(xfit, yfit, col="blue", lwd=2)

#QQ-plot
qqnorm(bitcoin_rets)
qqline(bitcoin_rets, col = 2) 


#-------------------------------------------------------------------------------
# Daily Return Data - Plots
#-------------------------------------------------------------------------------

#Time plot of log return of prices
plot(bitcoin_rets, type='l', ylab = "BTC price return", 
     main="Plot of 2014-2021 daily Bitcoin price return")

#Time plot of square of log return of prices
plot(bitcoin_rets^2,type='l', ylab = "Squares of BTC price return", 
     main="Plot of 2014-2021 daily Bitcoin price squared return")


#Time plot of absolute value of log return of prices
plot(abs(bitcoin_rets),type='l', ylab = "abs value of BTC price return", 
     main="Plot of 2014-2021 daily Bitcoin price abs return")

#-------------------------------------------------------------------------------
# Plot Autocorrelation and Partial Autocorrelation for Daily Returns
#-------------------------------------------------------------------------------

#ACF plot of log return of prices
par(mfrow=c(3,1))
acf(bitcoin_ret_num)

#ACF plot of square of log return of prices
acf(bitcoin_ret_num^2)

#ACF plot of absolute value of log return of prices
acf(abs(bitcoin_ret_num))

#-------------------------------------------------------------------------------
# Test for Autocorrelation
#-------------------------------------------------------------------------------

#Test of independence
#Compute the Ljung's Box Test on BTC price returns
#Ljung Box test on ret


#Ljung Box test on squared values of the stock price returns
Box.test(bitcoin_ret_num^2, lag=2, type="Ljung")
Box.test(bitcoin_ret_num^2, lag=4, type="Ljung")
Box.test(bitcoin_ret_num^2, lag=6, type="Ljung")

#Ljung Box test on absolute values of the stock price returns
Box.test(abs(bitcoin_ret_num), lag=2, type="Ljung")
Box.test(abs(bitcoin_ret_num), lag=4, type="Ljung")
Box.test(abs(bitcoin_ret_num), lag=6, type="Ljung")

# The null hypothesis is there exists no autocorrelation. 
# We perform the Ljung Box’s test to test the independence of BTC price return.
# From all the above Ljung Box Tests, we observe that the log returns are not
# correlated as the p-values>>0.05 and hence we cant reject the null hypothesis
# of no autocorrelation. 

# However, it shows signs of ARCH effect on the log returns of the BTC prices
# since the Ljung Box test on both the squared values of the BTC price returns 
# and the absolute values of the BTC price returns are significant.

#-------------------------------------------------------------------------------
# Determine the lag order based on the PACF Plot
#-------------------------------------------------------------------------------

#Determine the order of the model
#PACF plot on the log return of the BTC prices
par(mfrow=c(1,1))
pacf(bitcoin_ret_num, lag=10, main="PACF plot of the log return of BTC prices")

#PACF plot on the squared return of the BTC prices
pacf(bitcoin_ret_num^2, lag=10, main="PACF plot of the squared return of BTC prices")

#PACF plot on the absolute return of the BTC prices
pacf(abs(bitcoin_ret_num), lag=10, main="PACF plot of the absolute return of BTC prices")


#-------------------------------------------------------------------------------
# GARCH Model Fitting
#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
## GARCH(1,1), ARMA(0,0)
#-------------------------------------------------------------------------------

garch11.spec=ugarchspec(variance.model=list(garchOrder=c(1,1)), 
                        mean.model=list(armaOrder=c(0,0)))
#estimate model 
garch11.fit=ugarchfit(spec=garch11.spec, data=bitcoin_rets)
garch11.fit

# Weighted Ljung-Box test on St Residuals: p-values are 0, we reject the H0 
# which is: there is no autocorrelation

# Test for ARCH-behaviour in residuals: p-values are >0, we fail to reject H0
# which is: no serial correlation in the squared residuals

#using extractors
#estimated coefficients:
coef(garch11.fit)

#unconditional mean in mean equation
uncmean(garch11.fit)

#unconditional varaince: omega/(alpha1+beta1)
uncvariance(garch11.fit)

#persistence = alpha1+beta1
persistence(garch11.fit)


#Constraints on parameters < 1

#half-life: ln(0.5)/ln(alpha1+beta1)
halflife(garch11.fit)

#create selection list of plots for garch(1,1) fit
plot(garch11.fit, which = "all")


#conditional volatility plot
plot.ts(sigma(garch11.fit), ylab="sigma(t)", col="blue")

#Compute information criteria using infocriteria() function for model selecton
infocriteria(garch11.fit)


#-------------------------------------------------------------------------------
## GARCH(1,1), ARMA(0,0) with t-distribution
#-------------------------------------------------------------------------------

garch11.t.spec=ugarchspec(variance.model=list(garchOrder=c(1,1)), 
                        mean.model=list(armaOrder=c(0,0)), 
                        distribution.model = "std")
#estimate model 
garch11.t.fit=ugarchfit(spec=garch11.t.spec, data=bitcoin_rets)
garch11.t.fit

#using extractors
#estimated coefficients:
coef(garch11.t.fit)

#unconditional mean in mean equation
uncmean(garch11.t.fit)

#unconditional varaince: omega/(alpha1+beta1)
uncvariance(garch11.fit)

#persistence = alpha1+beta1
persistence(garch11.t.fit)


#plot of residuals
plot(garch11.t.fit, which = "all")

#-------------------------------------------------------------------------------
## GARCH(1,1), ARMA(0,0) with skewed t-distribution
#-------------------------------------------------------------------------------

garch11.skt.spec=ugarchspec(variance.model=list(garchOrder=c(1,1)), 
                          mean.model=list(armaOrder=c(0,0)), 
                          distribution.model = "sstd")
#estimate model 
garch11.skt.fit=ugarchfit(spec=garch11.skt.spec, data=bitcoin_rets)
garch11.skt.fit

#using extractors
#estimated coefficients:
coef(garch11.skt.fit)

#unconditional mean in mean equation
uncmean(garch11.skt.fit)

#unconditional varaince: omega/(alpha1+beta1)
uncvariance(garch11.skt.fit)

#persistence = alpha1+beta1
persistence(garch11.skt.fit)


#plot of residuals
plot(garch11.skt.fit, which = "all")


#-------------------------------------------------------------------------------
## eGARCH(1,1), ARMA(0,0) with t-distribution
#-------------------------------------------------------------------------------

egarch11.t.spec=ugarchspec(variance.model=list(model="eGARCH", garchOrder=c(1,1)), 
                            mean.model=list(armaOrder=c(0,0)), 
                            distribution.model = "std")
#estimate model 
egarch11.t.fit=ugarchfit(spec=egarch11.t.spec, data=bitcoin_rets)
egarch11.t.fit

#using extractors
#estimated coefficients:
coef(egarch11.t.fit)

#unconditional mean in mean equation
uncmean(egarch11.t.fit)

#unconditional varaince: omega/(alpha1+beta1)
uncvariance(egarch11.t.fit)

#persistence = alpha1+beta1
persistence(egarch11.t.fit)


#plot of residuals
plot(egarch11.t.fit, which = "all")


#-------------------------------------------------------------------------------
## fGARCH(1,1), ARMA(0,0) with t-distribution
#-------------------------------------------------------------------------------

fgarch11.t.spec=ugarchspec(variance.model=list(model="fGARCH", garchOrder=c(1,1),
                                               submodel = "APARCH"),
                           mean.model=list(armaOrder=c(0,0)), 
                           distribution.model = "std")
#estimate model 
fgarch11.t.fit=ugarchfit(spec=fgarch11.t.spec, data=bitcoin_rets)
fgarch11.t.fit

#using extractors
#estimated coefficients:
coef(fgarch11.t.fit)

#unconditional mean in mean equation
uncmean(fgarch11.t.fit)

#unconditional varaince: omega/(alpha1+beta1)
uncvariance(fgarch11.t.fit)

#persistence = alpha1+beta1
persistence(fgarch11.t.fit)


#plot of residuals
plot(fgarch11.t.fit, which = "all")


#-------------------------------------------------------------------------------
## iGARCH(1,1), ARMA(0,0) with t-distribution
#-------------------------------------------------------------------------------

igarch11.t.spec=ugarchspec(variance.model=list(model="iGARCH", garchOrder=c(1,1)),
                           mean.model=list(armaOrder=c(0,0)), 
                           distribution.model = "std")
#estimate model 
igarch11.t.fit=ugarchfit(spec=igarch11.t.spec, data=bitcoin_rets)
igarch11.t.fit

#using extractors
#estimated coefficients:
coef(igarch11.t.fit)

#unconditional mean in mean equation
uncmean(igarch11.t.fit)

#unconditional varaince: omega/(alpha1+beta1)
uncvariance(igarch11.t.fit)

#persistence = alpha1+beta1
persistence(igarch11.t.fit)


#plot of residuals
plot(igarch11.t.fit, which = "all")


infocriteria(garch11.fit)
infocriteria(garch11.t.fit)
infocriteria(garch11.skt.fit)
infocriteria(egarch11.t.fit)
infocriteria(fgarch11.t.fit)
infocriteria(igarch11.t.fit)

#-------------------------------------------------------------------------------
## Forecasting and Model Diagnostics
#-------------------------------------------------------------------------------

#5-step ahead forecast
#Fit ARMA(0,0)-iGARCH(1,1) model with t-distribution
egarch11.t.spec=ugarchspec(variance.model=list(model = "eGARCH", 
                                               garchOrder=c(1,1)), 
                           mean.model=list(armaOrder=c(0,0)), 
                           distribution.model = "std")
#estimate model 
egarch11.t.fit=ugarchfit(spec=egarch11.t.spec, data=bitcoin_rets)
egarch11.t.fit

#plot(egarch11.t.fit)
f=ugarchforecast(egarch11.t.fit, n.ahead=20)
f


#plot(f, which="all")

#rolling forecasts
rff=ugarchfit(spec=egarch11.t.spec, data=bitcoin_rets, out.sample=500)
rf=ugarchforecast(rff, n.ahead=20, n.roll=450)
rf

#plot(rf, which="all")

## Backtesting method to validate EGARCH model:
mod_egarch = ugarchroll(egarch11.t.spec, data = bitcoin_rets, n.ahead = 1,
                        n.start = 2500, refit.every = 200, refit.window = "recursive")

mod_egarch
