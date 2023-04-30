# Assignment 6
library(tseries)
par(mfrow=c(2,1))
data<-read.table("/Users/lukebeebe/Documents/School/Rutgers/Spring 2023/Regression Methods/Rocket_Motors-1.txt",header = T)
data$newDate<-as.Date(data$Date,"%d-%b-%y")
attach(data)
Price<-ts(Price_in_USD,start=2020,frequency=365)
plot(Price,main="Rocket Motors Stock",ylab="Price USD",xlab="Year") #1 Plot time series
# It looks like it's exponentially growing.
change<-Price_in_USD[2:length(Price_in_USD)]-Price_in_USD[1:(length(Price_in_USD)-1)]
change<-change[2:length(change)]-change[1:(length(change)-1)]
#2 arima model, how did you decide pdq?
d2<-ts(change,start=2020,frequency = 365)
plot(d2,ylab='change',main='d=2') # change twice, d = 2
acf(change) # going towards 0 as lag->infinity, p = 0
pacf(change) # 3 spikes, q = 3, rest is white noise
# p = AR, Auto regressive, lags of the variable itself
# d = I, Integrated, differencing steps required to make stationary (mean and variance are constant over set)
# q = MA, Moving average, lags of previous information shocks
arima(Price, order=c(0,2,3))
#3 equation
# Y = 0.8565e(t-1)+0.7081e(t-2)+0.3223e(t-3)
auto.arima(Price)
# Funny, the auto.arima function gives me p,d,q of 3,2,3