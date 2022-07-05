library(fpp)
library(TSA)
library(ggplot2)
library(vars)
defaultW <- getOption("warn") 
options(warn = -1)

data <- read.csv("/Users/amyzhang/Desktop/MScA/MSCA 31006/Project/Wanliu_data_daily.csv")

data_ts <- ts(data, start=c(2013,3), frequency=365.25)

##### Plotting Target Variable with and without Box Cox #####

pm25 <- data_ts[,'PM2.5']
autoplot(pm25, main='PM2.5')
BoxCox.lambda(pm25)
pm25 %>% BoxCox(lambda = 0.88) %>% autoplot(main='PM2.5 BoxCox (lambda=0.88)')
summary(pm25)

##### Checking Stationarity with and without seasonal differencing #####

pm25_ds <- diff(pm25, lag=365)

tsdisplay(pm25, main='Original Dataset')
tsdisplay(pm25_ds, main='Seasonal Differencing')

adf.test(pm25) # stationary
kpss.test(pm25) # stationary

adf.test(pm25_ds) # stationary
kpss.test(pm25_ds) # stationary

##### Plot variables with and without Box Cox, check for stationarity #####

temp <- data_ts[,'TEMP']
autoplot(temp, main='Temp')
temp %>% BoxCox(lambda = 0.88) %>% autoplot(main='Temp BoxCox (lambda=0.88)')
temp_d1 <- diff(temp, differences=1)
autoplot(temp_d1, main='Temp (Differenced)')

adf.test(temp) # non-stationary
adf.test(temp_d1) # stationary

pres <- data_ts[,'PRES']
autoplot(pres, main='Pres')
pres %>% BoxCox(lambda = 0.88) %>% autoplot(main='Pres BoxCox (lambda=0.88)')
pres_d1 <- diff(pres, differences=1)
autoplot(pres_d1, main='Pressure (Differenced)')

adf.test(pres) # non-stationary
adf.test(pres_d1) # stationary

rain <- data_ts[,'RAIN']
autoplot(rain, main='Rain')
rain %>% BoxCox(lambda = 0.88) %>% autoplot(main='Rain BoxCox (lambda=0.88)')
rain_d1 <- diff(rain, differences=1)
autoplot(rain_d1, main='Rain (Differenced)')

adf.test(rain) # stationary
adf.test(rain_d1) # stationary

##### Models #####
#Regression with ARIMA(1,1,2) errors 
arima_d1 <- auto.arima(pm25, xreg=cbind(temp,pres,rain), lambda="auto", d=1, seasonal=TRUE)
summary(arima_d1)
checkresiduals(arima_d1)

arima_D1 <- auto.arima(pm25, xreg=cbind(temp,pres,rain), lambda="auto", D=1, seasonal=TRUE)
summary(arima_D1)
checkresiduals(arima_D1)

# Use the first model -> Lower AIC/AICC/BIC, seasonal differencing is accounted for in regression

##### Forecasting #####

temp_forecast <- snaive(temp, h=30)
pres_forecast <- snaive(pres, h=30)
rain_forecast <- naive(rain, h=30)

pm25_forecast <- forecast(arima_D0, h=30, xreg=cbind(temp_forecast$mean,pres_forecast$mean,rain_forecast$mean))

autoplot(temp) +
  autolayer(temp_forecast) + 
  ggtitle("Temp Forecast")

autoplot(pres) +
  autolayer(pres_forecast) + 
  ggtitle("Pres Forecast")

autoplot(rain) +
  autolayer(rain_forecast) + 
  ggtitle("Rain Forecast")

autoplot(pm25) +
  autolayer(pm25_forecast) + 
  ggtitle("PM2.5 Forecast")


# CV
k=1381
h=30
p=365.25
n <- length(pm25)

st <- tsp(pm25)[1]+(k-2)/p #  gives the start time in time units,

error_1_arima_errors <- matrix(NA,n-k,h)
error_2_arima_errors <- matrix(NA,n-k,h)

AIC_1_arima_errors <- matrix(NA,n-k)
AIC_2_arima_errors <- matrix(NA,n-k)

i<-1
for(i in (1:80))
{
  ### One Month rolling forecasting
  # Expanding Window 
  train_1 <- window(pm25, end=st + i/p)  ## Window Length: k+i
  temp_1 <- window(temp, end=st + i/p)
  pres_1 <- window(pres, end=st + i/p)
  rain_1 <- window(rain, end=st + i/p)
  
  # Sliding Window - keep the training window of fixed length. 
  # The training set always consists of k observations.
  train_2 <- window(pm25, start=st+(i-k+1)/p, end=st+i/p) ## Window Length: k
  temp_2 <- window(temp, start=st+(i-k+1)/p, end=st+i/p)
  pres_2 <- window(pres, start=st+(i-k+1)/p, end=st+i/p)
  rain_2 <- window(rain, start=st+(i-k+1)/p, end=st+i/p)
  
  test <- window(pm25, start=st + (i+1)/p, end=st + (i+h)/p) ## Window Length: H
  
  if (i<4) {
    cat(c("*** CV", i,":","len(Expanding Window):",length(train_1), "len(Sliding Window):",length(train_2), "len(Test):",length(test),'\n'  ))
    cat(c("*** TRAIN -  Expanding WIndow:",tsp(train_1)[1],'-',tsp(train_1)[2],'\n'))
    cat(c("*** TRAIN - Sliding WIndow:",tsp(train_2)[1],'-',tsp(train_2)[2],'\n'))
    #cat(c("*** TEST:",tsp(test)[1],'-',tsp(test)[2],'\n'))
    cat("*************************** \n \n")
  }
train_1
  # M1 - ARIMA(4,1,2) with expanding window
  fit_1 <- Arima(train_1, model=arima_d1, xreg=cbind(temp_1,pres_1,rain_1), lambda="auto")
  temp_forecast_1 <- snaive(temp, h=h)
  pres_forecast_1 <- snaive(pres, h=h)
  rain_forecast_1 <- naive(rain, h=h)

  fcast_1 <- forecast(fit_1, h=h, xreg=cbind(temp_forecast_1$mean,pres_forecast_1$mean,rain_forecast_1$mean))

  AIC_1_arima_errors[i] <- fit_1$aic 
  
  # M2 - ARIMA(4,1,2) with sliding window
  fit_2 <- Arima(train_2, model=arima_d1, xreg=cbind(temp_2,pres_2,rain_2), lambda="auto")
  temp_forecast_2 <- snaive(temp, h=h)
  pres_forecast_2 <- snaive(pres, h=h)
  rain_forecast_2 <- naive(rain, h=h)
  
  fcast_2 <- forecast(fit_2, h=h, xreg=cbind(temp_forecast_2$mean,pres_forecast_2$mean,rain_forecast_2$mean))

  AIC_2_arima_errors[i] <- fit_2$aic
  
  error_1_arima_errors[i,1:length(test)] <- fcast_1[['mean']]-test
  error_2_arima_errors[i,1:length(test)] <- fcast_2[['mean']]-test
  
  i<-i+1
  
}


#For each of the four models above, calculate and plot the
#1) Mean Absolute Forecast Error (MAE) vs forecast horizon.
#Takes the error table from each model, calculate the absolute error and then take the column mean to get to MAE
plot(1:30, colMeans(abs(error_1_arima_errors),na.rm=TRUE), type="l",col=1,xlab="horizon", ylab="MAE",
     ylim=c(50,800))
lines(1:30, colMeans(abs(error_2_arima_errors),na.rm=TRUE), type="l",col=2)
legend("topleft",legend=c("ARIMA - Expanding Window","ARIMA - Sliding Window"),col=1:2,lty=1, xpd=TRUE, inset=c(0, -.25), cex=.8)

# 2) Root-square Forecast Error (RMSE) vs forecast horizon
#Takes the error table from each model, calculate the squared error, take the column mean and then take the sqrt to get to RMSE
plot(1:30, sqrt(colMeans((error_1_arima_errors)^2,na.rm=TRUE)), type="l",col=1,xlab="horizon", ylab="RMSE",
     ylim=c(50,1500))
lines(1:30, sqrt(colMeans((error_2_arima_errors)^2,na.rm=TRUE)), type="l",col=2)
legend("topleft",legend=c("ARIMA - Expanding Window","ARIMA - Sliding Window"),col=1:2,lty=1, xpd=TRUE, inset=c(0, -.25), cex=.8)

# 3) AIC vs iteration number
plot(1:80, AIC_1_arima_errors, type="l",col=1,xlab="iteration", ylab="AIC",
     ylim=c(10000,26000))
lines(1:80, AIC_2_arima_errors, type="l",col=2)
legend("topleft",legend=c("ARIMAerrors - Expanding Window","ARIMAerrors - Sliding Window"),col=1:2,lty=1, xpd=TRUE, inset=c(0, -.25), cex=.8)

