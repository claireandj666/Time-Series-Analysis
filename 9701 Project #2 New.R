library(gdata)
library(forecast)
library(TSA)
library(tseries)
library(astsa)
library(xts)

install.packages("rugarch")
library(rugarch)
install.packages("fGarch")
library(fGarch)
install.packages("PerformanceAnalytics")
library(PerformanceAnalytics)

#date1 <- function(TSLA)as.Date(TSLA, origin = "2010-6-28") 
#TSLA2 = read.zoo("/Users/yandichen/Downloads/TSLA.csv", header = TRUE, sep=",", FUN = date1)


TSLA2 = read.csv("/Users/yandichen/Downloads/TSLA.csv", header = TRUE)
names(TSLA2)

# Adj.close
#
#
#

# plot time series using daily adj.close
tsla = ts(TSLA2$Adj.Close)
plot.ts(TSLA2$Adj.Close, main = "Time series plot of Tesla stock price")

# log of adj.close
#a = log(TSLA2$Adj.Close)

a = log(tsla)
plot.ts(a, main = "Log of Tesla stock price")

# difference the log of TSLA 
b = diff(a)
plot.ts(b, main = "Difference Log of TSLA")

sampleACF1 = acf(b, lag.max = 500, main="sample ACF")
samplePACF2 = pacf(b, lag.max = 500,main="sample PACF")

acf2(b, main="TSLA Returns")
acf2(b^2, main="TSLA Squared Log Returns")
eacf(b^2)

tslart1 = CalculateReturns(tsla)
plot(tslart1, main= 'TSLA Returns')
sd(tslart1)

tslart = na.omit(tslart1)
m = mean(tslart)
err = tslart -m

plot(abs(err), main= 'TSLA Absolute Prediction Errors')
pacf(abs(err), main= 'PACF of Absolute Errors')
acf(abs(err), main= 'ACF of Absolute Errors')


garchspec = ugarchspec(mean.model = list(armaOrder=c(0,0), 
                                         variance.model = list(model='sGARCH'),
                                         distribution.model = 'sstd'))
garchfit = ugarchfit(spec = garchspec, data=tslart)
garchvol = sigma(garchfit)

# compute unconditional volatility
sqrt(uncvariance(garchfit))

# forecast 5 days
#garchforecast = ugarchforecast(modelfit1, n.ahead = 10)

garchforecast = ugarchforecast(fitORspec = garchfit, n.head=10)
print(sigma(garchforecast))

plot(garchforecast)


