#load necessary packages
library(ggplot2)
library(readxl)
library(tidyverse)
library(dplyr)
library(lubridate)
library(zoo)
library(forecast)
library(writexl)
library(fpp2)
library(Metrics)

#Second, we will read file from excel that we saved before in "Stohastic interpolation" script.
setwd("C:/Users/User/Desktop/TEZA")
Data <- read_xlsx("data_interpolated_combinedALL.xlsx")
#delete column with NAs that we needed before for plotting 
Data$sales_with_na <- NULL

#Add rest of the approximation techniques to the Data that contains only interpolation techniques
#We have these values from earlier, from our Approximation, deterministic_interpolation, and Stochastic interpolation scripts which can be found on Git
#We will now add our first method called Average of last know periods
Data$Average_of_last_know_periods <- sales_data$Sales

#Now we will add spline interpolation
Data$spline_interpolation <- sales_data_spline$Sales

#Lastly, we will add polynomial interpolation
Data$polynomial_interpolation <- sales_data_polynomial$Sales

# Split the data
train_data <- subset(Data, Date < as.Date("2023-01-01"))
test_data <- subset(Data, Date >= as.Date("2023-01-01"))

results <- list()

# Forecasting and Accuracy Measurement
for (col in names(Data)[!names(Data) %in% "Date"]) {
  
  # Triple Exponential Smoothing (HoltWinters)
  hw_model <- HoltWinters(ts(train_data[[col]], frequency = 365))
  hw_forecast <- forecast(hw_model, h = nrow(test_data))
  
  # auto.arima
  auto_arima <- auto.arima(ts(train_data[[col]], frequency = 365))
  auto_arima_forecast <- forecast(auto_arima, h = nrow(test_data))

  test_actuals <- test_data[[col]]
  
  # Measure forecasting accuracy
  hw_metrics <- c(
    MAE = mae(test_actuals, hw_forecast$mean),
    MSE = mse(test_actuals, hw_forecast$mean),
    RMSE = rmse(test_actuals, hw_forecast$mean),
    MAPE = mape(test_actuals, hw_forecast$mean)
  )
  
  sarima_metrics <- c(
    MAE = mae(test_actuals, auto_arima_forecast$mean),
    MSE = mse(test_actuals, auto_arima_forecast$mean),
    RMSE = rmse(test_actuals, auto_arima_forecast$mean),
    MAPE = mape(test_actuals, auto_arima_forecast$mean)
  )
  
  results[[col]] <- list(hw_metrics = hw_metrics, sarima_metrics = sarima_metrics)
}

print(results)


