#library(fpp3)

library(fpp2)
library(seasonal)
library(ggplot2)
data(elecequip)
autoplot(window(elecequip, start=c(2001,1), end=c(2001,12)))

plot(decompose(elecequip, type="additive"))

plot(stl(elecequip, s.window="periodic"))

ts_remainder <- decompose(elecequip, type="additive")$random
anyNA(ts_remainder)

ts_remainder <- ts_remainder[!is.na(ts_remainder)]

autoplot(ts_remainder)

#Unit root testing.

library(urca)

PP.test(ts_remainder)
summary(ur.kpss(ts_remainder))
library(tseries)

kpss.test(ts_remainder)
adf.test(ts_remainder)

dts <- diff(ts_remainder)
anyNA(dts)
autoplot(dts)

plot(dts, type="l")

kpss.test(dts)

library(data.table)
#sp500 <- fread("SP500.csv")
#plot(sp500)
#decompose(sp500)
#library(xts)
#as.xts(sp500)
#library(fpp3)
#library(forecast)
summary(arma(ts_remainder)) # AIC is 866

summary(arma(ts_remainder, order=c(1,2))) #AIC is larger! ARMA(1,2)

summary(arma(ts_remainder, order=c(1,0))) #ARMA(1,0)

summary(arma(ts_remainder, order=c(0,1))) #ARMA(0,1)

#ARMA(0,0) white noise

library(fpp2)

auto.arima(ts_remainder)

auto.arima(sp500[,2]) #AICC criterion to select lag order; 
# uses the KPSS test to test stationarity;
# treats missing values automatically. Kalman filter methods to impute missing values.
plot(forecast(auto.arima(sp500[,2])))

#library(forecast)
plot(forecast(auto.arima(ts_remainder)))

plot(forecast(arima(ts_remainder, order=c(1,0,1)))
)

acf(ts_remainder)
pacf(ts_remainder)




