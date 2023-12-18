library(quantmod)
library(TTR)
library(tseries)
library(TSA)
library(rugarch)
library(FinTS)
library(ggplot2)
library(moments)
library(PerformanceAnalytics)
# Get Symbol 
symbol <- "AMZN"

# Get the date
start_date <- "2019-11-01"
end_date <- "2020-10-31"
getSymbols(symbol, src = "yahoo", from = start_date, to = end_date)

head(AMZN)
colnames(AMZN)
plot(AMZN[, "AMZN.Close"], main="AMZN", col="red")


# Log return
log_return <- c(diff(log(AMZN$AMZN.Close)))
#ELiminate the lost data
log_return <- na.omit(log_return) 
plot(log_return, col="blue")
hist(log_return)
chart.Histogram(log_return,
                methods=c('add.density', 'add.normal'),
                colorset=c('blue', 'green', 'red'))
chartSeries(log_return, theme='white')
# ACF and PACF
acf_values <- acf(log_return)
pacf_values <- pacf(log_return)

adf.test(log_return) 

#Dari plot diketahui bahwa stasioner
#Number 1b
sq_price_data<-log_return^2
acf_sq_price_data<-acf(sq_price_data)
pacf_sq_price_data<-pacf(sq_price_data)
plot(sq_price_data)
#Number 1c
abs_price_data<-abs(log_return)
acf_abs_price_data<-acf(abs_price_data)
pacf_abs_price_data<-pacf(abs_price_data)
plot(abs_price_data)

ArchTest(log_return)
#
eacf(log_return)

mu<-mean(log_return)  
sigma<-sd(log_return)
x<-seq(min(log_return),max(log_return),length=80) 
y<-dnorm(x,mu,sigma) 
lines(x,y,lwd=2,col="red")

#Dari output EACF, diduga model GARCH(1,1), ARCH(2), GARCH(3,1)

####ARCH(2)####
arch_spec <- ugarchspec(mean.model =list(armaOrder = c(0,0)), variance.model = list(model = "sGARCH", garchOrder = c(0, 2)),
                        distribution.model = 'std')
arch_model <- ugarchfit(spec = arch_spec, data = log_return)
print(arch_model)

#Residual ARCH
residuals_arch<-residuals(arch_model)
qqnorm(residuals_arch)
qqline(residuals_arch)
jarque.bera.test(residuals_arch)


####GARCH(1,1)####
garch_spec <- ugarchspec(mean.model =list(armaOrder = c(0,0)), variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
                         distribution.model = 'std')
# Estimation GARCH(1,1)
garch_model <- ugarchfit(spec = garch_spec, data = log_return)
print(garch_model)

#Residual GARCH(1,1)
residuals_garch<-residuals(garch_model)
qqnorm(residuals_garch)
qqline(residuals_garch, col = "red")
jarque.bera.test(residuals_garch)

##Plot Volatility Model GARCH(1,1)
vol<-ts(garch_model@fit$sigma^2, start = c(2019, 11), end = c(2020,10), frequency = 12)
plot(vol, xlab='', ylab='',main="Volatilitas AMZN (GARCH[1,1])")


## Forecasting GARCH(1,1)
garch_forecast <- ugarchforecast(fitORspec = garch_model, n.ahead = 10000)
garch_forecast
plot(fitted(garch_forecast))
plot(sigma(garch_forecast))

####GARCH(3,1)####
garch_spec1 <- ugarchspec(mean.model =list(armaOrder = c(0,0)), variance.model = list(model = "sGARCH", garchOrder = c(3, 1)),
                          distribution.model = 'std')

# Estimation model GARCH(3,1)
garch_model1 <- ugarchfit(spec = garch_spec1, data = log_return)
print(garch_model1)

#Residual GARCH(3,1)
residuals_garch1<-residuals(garch_model1)
qqnorm(residuals_garch1)
qqline(residuals_garch1, col = "blue")
jarque.bera.test(residuals_garch1)

