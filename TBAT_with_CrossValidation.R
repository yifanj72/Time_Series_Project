library(forecast)

data <- read.csv("/Users/amyzhang/Desktop/MScA/MSCA 31006/Project/Wanliu_data_daily.csv")
firstHour <- 24*(as.Date("2013-03-01 00:00:00")-as.Date("2013-1-1 00:00:00"))
data_ts <- ts(data=data[c('PM2.5', 'PM10', 'TEMP', 'RAIN')], start=c(2013, firstHour), frequency=365.25)

plot(data_ts[, 'TEMP'], xlab='Year', ylab='Daily Average Temperture (°C)')  
plot(data_ts[, 'RAIN'], xlab='Year', ylab='Daily Average Precipitation (mm)') #Should I use SUM() 
plot(data_ts[, 'PM2.5'], xlab='Year', ylab='Daily Average PM2.5 Concentration')  
plot(data_ts[, 'PM10'], xlab='Year', ylab='Daily Average PM10 Concentration') 

PM2.5 <- data_ts[, 'PM2.5']
fit <- tbats(PM2.5)
fit # BATS(0.038, {2,1}, 0.8, -) # No trigonometric terms

# CV
k=1381
h=30
p=365.25
n <- length(PM2.5)

st <- tsp(PM2.5)[1]+(k-2)/p #  gives the start time in time units,

error_1_TBATS <- matrix(NA,n-k,h)
error_2_TBATS <- matrix(NA,n-k,h)

AIC_1_TBATS <- matrix(NA,n-k)
AIC_2_TBATS <- matrix(NA,n-k)

i<-1
for(i in (1:80))
{
  ### One Month rolling forecasting
  # Expanding Window 
  train_1 <- window(PM2.5, end=st + i/p)  ## Window Length: k+i
  
  # Sliding Window - keep the training window of fixed length. 
  # The training set always consists of k observations.
  train_2 <- window(PM2.5, start=st+(i-k+1)/p, end=st+i/p) ## Window Length: k
  
  test <- window(PM2.5, start=st + (i+1)/p, end=st + (i+h)/p) ## Window Length: H
  
  if (i<4) {
    cat(c("*** CV", i,":","len(Expanding Window):",length(train_1), "len(Sliding Window):",length(train_2), "len(Test):",length(test),'\n'  ))
    cat(c("*** TRAIN -  Expanding WIndow:",tsp(train_1)[1],'-',tsp(train_1)[2],'\n'))
    cat(c("*** TRAIN - Sliding WIndow:",tsp(train_2)[1],'-',tsp(train_2)[2],'\n'))
    #cat(c("*** TEST:",tsp(test)[1],'-',tsp(test)[2],'\n'))
    cat("*************************** \n \n")
  }
  train_1
  # M1 - ARIMA(4,1,2) with expanding window
  fit_1 <- tbats(train_1, model=fit)
  fcast_1 <- forecast(fit_1, h=h)
  AIC_1_TBATS[i] <- fit_1$AIC 

    # M2 - ARIMA(4,1,2) with sliding window
  fit_2 = tbats(train_2, model=fit)
  fcast_2 <- forecast(fit_2, h=h)
  AIC_2_TBATS[i] <- fit_2$AIC
  
  error_1_TBATS[i,1:length(test)] <- fcast_1[['mean']]-test
  error_2_TBATS[i,1:length(test)] <- fcast_2[['mean']]-test
  
  i<-i+1
  
}


#For each of the four models above, calculate and plot the
#1) Mean Absolute Forecast Error (MAE) vs forecast horizon.
#Takes the error table from each model, calculate the absolute error and then take the column mean to get to MAE
plot(1:30, colMeans(abs(error_1_TBATS),na.rm=TRUE), type="l",col=1,xlab="horizon", ylab="MAE",
     ylim=c(50,100))
lines(1:30, colMeans(abs(error_2_TBATS),na.rm=TRUE), type="l",col=2)
legend("topleft",legend=c("ARIMA - Expanding Window","ARIMA - Sliding Window"),col=1:2,lty=1, xpd=TRUE, inset=c(0, -.25), cex=.8)

# 2) Root-square Forecast Error (RMSE) vs forecast horizon
#Takes the error table from each model, calculate the squared error, take the column mean and then take the sqrt to get to RMSE
plot(1:30, sqrt(colMeans((error_1_TBATS)^2,na.rm=TRUE)), type="l",col=1,xlab="horizon", ylab="RMSE",
     ylim=c(80,120))
lines(1:30, sqrt(colMeans((error_2_TBATS)^2,na.rm=TRUE)), type="l",col=2)
legend("topleft",legend=c("ARIMA - Expanding Window","ARIMA - Sliding Window"),col=1:2,lty=1, xpd=TRUE, inset=c(0, -.25), cex=.8)

# 3) AIC vs iteration number
plot(1:80, AIC_1_TBATS, type="l",col=1,xlab="iteration", ylab="AIC",
     ylim=c(21000,22000))
lines(1:80, AIC_2_TBATS, type="l",col=2)
legend("topleft",legend=c("ARIMA - Expanding Window","ARIMA - Sliding Window"),col=1:2,lty=1, xpd=TRUE, inset=c(0, -.25), cex=.8)

