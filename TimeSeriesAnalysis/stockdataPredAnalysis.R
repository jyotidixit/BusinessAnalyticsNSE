setwd("C:/Users/jyoti/Desktop/BusinessAnalyticsNSE/TimeSeriesAnalysis")

data <- read.csv("stock_pred.csv") #import the data

View(data) #View the data

#creating the mape function
mape = function(actual, predicted){
  result = mean(abs((actual-predicted)/actual))
  return(result)
}

#Converting data into a time-series object
stock_ts <- ts(data[, 1], start=2008, frequency = 12)
head(stock_ts)

#plotting the time-series
plot(stock_ts)

#Seeing components of time-series
stock_1 <- stl(stock_ts, s.window = 'periodic')
stock_1

#Plotting the components
plot(stock_1)

#Predicting the component/ alpha, beta and gamma values
stock_hws = HoltWinters(stock_ts)
stock_hws

#calculating the MAPE
pred = as.numeric(stock_hws$fitted[, 1])
act = as.numeric(stock_ts[(1:96)])

#Calculating the in-sample error
mape(act, pred)

#Prediction for forecast for next 12 months
prediction = predict(stock_hws, n.ahead = 12, prediction.interval = TRUE, level=0.95)
prediction

#Plotting forecasted values
plot(stock_hws, prediction)

#Calculating error
err = act - as.numeric(prediction[, 1])
err

#Errors should be randomly distributed
par(mfrow = c(1,2))
hist(err)
qqnorm(err)

#Ljung-Box test for errors
par(mfrow = c(1,1))
acf(err, lag.max = 30)

Box.test(err, lag = 30, type = "Ljung-Box")
#p-value greater than 5%. Thus, the errors are not statistically significant.