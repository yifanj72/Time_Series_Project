library(fpp)
library(TSA)
library(ggplot2)
library(vars)
defaultW <- getOption("warn") 
options(warn = -1)

data <- read.csv("/Users/amyzhang/Desktop/MScA/MSCA 31006/Project/Wanliu_data_daily.csv")
airq <- ts(data$PM2.5, start=c(2013,3), frequency=365.25)


win.graph(width=6, height=6, pointsize=12)
part(mfrow=c(2,1))
plot(airq,xlab='Time',ylab='Air Quality')
periodogram(airq,ylab='Variable Air Quality Periodogram',xlim=c(0,0.045))
abline(h=0)
temp<-periodogram(airq,ylab='Variable Air Quality Periodogram',xlim=c(0,0.045))


Max_freq <- temp$freq[which.max(temp$spec)]
seasonality <-1/ Max_freq
seasonality


# Exploring dataset
autoplot(airq,ylab='PM2.5')
tsdisplay(airq) 
### The ACF of airq is not decreasing exponentially showing signs of non-stationarity as well as auto correlation through out the dataset.
### There seems to be yearly seasonality 
BoxCox.lambda(airq) 
### 0.8812519

airq %>% BoxCox(lambda = 0.88) %>% autoplot(main='Air Quality Data (Box Cox Transform, lambda=0.88)')


# Differencing dataset
airq_ts_bc <- BoxCox(airq, lambda = 0.88)
airq_ts_ds <- diff(airq_ts_bc,lag=365)
airq_ts_d1 <- diff(airq_ts_bc, differences=1)

tsdisplay(airq_ts_ds, main='Seasonal Differencing')
tsdisplay(airq_ts_d1, main='1st Order Differencing')

kpss.test(airq_ts_bc) ### P-value = 0.01 < 0.05 ==> Reject Null hypothesis ==> The process is not stationary. 
kpss.test(airq_ts_ds) ### P-value = 0.09 > 0.05 ==> Accept Null hypothesis ==> The process is stationary
kpss.test(airq_ts_d1) ### P-value = 0.1 > 0.05 ==> Accept Null hypothesis ==> The process is stationary


# Fitting Auto Arima model
fit.arima <- auto.arima(airq, d=1,lambda='auto')
fit.arima
### ARIMA(4,1,2)
### AICc = 14328.73

# Checking residuals of model 
checkresiduals(fit.arima)
### Ljung-Box test: P-value = 0.9561 > 0.05 ==> Accept Null Hypothesis ==> Residuals are independently correlated. 

# Forecasting for next 30 days
Model_Arima_forecast <-forecast(fit.arima, h=30)
Model_Arima_forecast

autoplot(airq) +
  autolayer(Model_Arima_forecast$mean, series="Forecast")+
  ggtitle("Forecast ARIMA(4,1,2) for air quality") +
  xlab("Year") + ylab("PM2.5")

# CV
k=1381
h=30
p=365.25
n <- length (airq)

st <- tsp(airq)[1]+(k-2)/p #  gives the start time in time units,

error_1_arima <- matrix(NA,n-k,h)
error_2_arima <- matrix(NA,n-k,h)

AIC_1_arima <- matrix(NA,n-k)
AIC_2_arima <- matrix(NA,n-k)

i<-1
for(i in (1:80))
{
  ### One Month rolling forecasting
  # Expanding Window 
  train_1 <- window(airq, end=st + i/p)  ## Window Length: k+i
  # Sliding Window - keep the training window of fixed length. 
  # The training set always consists of k observations.
  train_2 <- window(airq, start=st+(i-k+1)/p, end=st+i/p) ## Window Length: k
  
  test <- window(airq, start=st + (i+1)/p, end=st + (i+h)/p) ## Window Length: H
  
  if (i<4) {
    cat(c("*** CV", i,":","len(Expanding Window):",length(train_1), "len(Sliding Window):",length(train_2), "len(Test):",length(test),'\n'  ))
    cat(c("*** TRAIN -  Expanding WIndow:",tsp(train_1)[1],'-',tsp(train_1)[2],'\n'))
    cat(c("*** TRAIN - Sliding WIndow:",tsp(train_2)[1],'-',tsp(train_2)[2],'\n'))
    #cat(c("*** TEST:",tsp(test)[1],'-',tsp(test)[2],'\n'))
    cat("*************************** \n \n")
  }
  
  # M1 - ARIMA(4,1,2) with expanding window
  fit_1 <- Arima(train_1, order=c(4,1,2), method="ML")
  
  fcast_1 <- forecast(fit_1, h=h)
  AIC_1_arima[i] <- fit_1$aic 
  
  # M2 - ARIMA(4,1,2) with sliding window
  fit_2 <- Arima(train_2, order=c(4,1,2), method="ML")
  fcast_2 <- forecast(fit_2, h=h)
  AIC_2_arima[i] <- fit_2$aic
  
  error_1_arima[i,1:length(test)] <- fcast_1[['mean']]-test
  error_2_arima[i,1:length(test)] <- fcast_2[['mean']]-test
  
  i<-i+1
}


#For each of the four models above, calculate and plot the
#1) Mean Absolute Forecast Error (MAE) vs forecast horizon.
#Takes the error table from each model, calculate the absolute error and then take the column mean to get to MAE
plot(1:30, colMeans(abs(error_1_arima),na.rm=TRUE), type="l",col=1,xlab="horizon", ylab="MAE",
     ylim=c(50,800))
lines(1:30, colMeans(abs(error_2_arima),na.rm=TRUE), type="l",col=2)
lines(1:30, colMeans(abs(error_1_arima_errors),na.rm=TRUE), type="l",col=3)
lines(1:30, colMeans(abs(error_2_arima_errors),na.rm=TRUE), type="l",col=4)
lines(1:30, colMeans(abs(error_1_TBATS),na.rm=TRUE), type="l",col=5)
lines(1:30, colMeans(abs(error_2_TBATS),na.rm=TRUE), type="l",col=6)
lines(1:30, colMeans(abs(error_2_var),na.rm=TRUE), type="l",col=7)
lines(1:30, colMeans(abs(error_2_var),na.rm=TRUE), type="l",col=8)
legend("topleft",legend=c("ARIMA - Expanding Window","ARIMA - Sliding Window", "ARIMAerors - Expanding Window","ARIMAerors - Sliding Window", "TBATS - Expanding Window","TBATS - Sliding Window", "VAR - Expanding Window","VAR - Sliding Window"),col=1:8,lty=1, xpd=TRUE, inset=c(0, -.25), cex=.8)

# 2) Root-square Forecast Error (RMSE) vs forecast horizon
#Takes the error table from each model, calculate the squared error, take the column mean and then take the sqrt to get to RMSE
plot(1:30, sqrt(colMeans((error_1_arima)^2,na.rm=TRUE)), type="l",col=1,xlab="horizon", ylab="RMSE",
     ylim=c(50,1500))
lines(1:30, sqrt(colMeans((error_2_arima)^2,na.rm=TRUE)), type="l",col=2)
lines(1:30, sqrt(colMeans((error_1_arima_errors)^2,na.rm=TRUE)), type="l",col=3)
lines(1:30, sqrt(colMeans((error_2_arima_errors)^2,na.rm=TRUE)), type="l",col=4)
lines(1:30, sqrt(colMeans((error_1_TBATS)^2,na.rm=TRUE)), type="l",col=5)
lines(1:30, sqrt(colMeans((error_2_TBATS)^2,na.rm=TRUE)), type="l",col=6)
lines(1:30, sqrt(colMeans((error_2_var)^2,na.rm=TRUE)), type="l",col=7)
lines(1:30, sqrt(colMeans((error_2_var)^2,na.rm=TRUE)), type="l",col=8)
legend("topleft",legend=c("ARIMA - Expanding Window","ARIMA - Sliding Window", "ARIMAerors - Expanding Window","ARIMAerors - Sliding Window", "TBATS - Expanding Window","TBATS - Sliding Window", "VAR - Expanding Window","VAR - Sliding Window"),col=1:8,lty=1, xpd=TRUE, inset=c(0, -.25), cex=.8)

# 3) AIC vs iteration number
plot(1:80, AIC_1_arima, type="l",col=1,xlab="iteration", ylab="AIC",
     ylim=c(10000,26000))
lines(1:80, AIC_2_arima, type="l",col=2)
lines(1:80, AIC_1_arima_errors, type="l",col=3)
lines(1:80, AIC_2_arima_errors, type="l",col=4)
lines(1:80, AIC_1_TBATS, type="l",col=5)
lines(1:80, AIC_2_TBATS, type="l",col=6)
lines(1:30, AIC_1_var, type="l",col=7)
lines(1:30, AIC_2_var, type="l",col=8)
legend("topleft",legend=c("ARIMA - Expanding Window","ARIMA - Sliding Window", "ARIMAerors - Expanding Window","ARIMAerors - Sliding Window", "TBATS - Expanding Window","TBATS - Sliding Window", "VAR - Expanding Window","VAR - Sliding Window"),col=1:8,lty=1, xpd=TRUE, inset=c(0, -.25), cex=.8)
