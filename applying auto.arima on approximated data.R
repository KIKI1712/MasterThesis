setwd("C:/Users/User/teza")
library(forecast)
library(Metrics)
load("Data_final.RData")

Data$Date <- as.Date(Data$Date)
# Split the data
train_data <- subset(Data, Date < as.Date("2023-01-01"))
test_data <- subset(Data, Date >= as.Date("2023-01-01"))
test_actuals <- test_data$Optimized_ALKP

# Create an empty dataframe to store parameter choices for auto.arima
auto_arima_results <- data.frame(COL = character(), p = integer(), d = integer(), q = integer(),
                                 P = integer(), D = integer(), Q = integer(), stringsAsFactors = FALSE)

# Create an empty list to store results
results_arima <- list()

# Forecasting and Accuracy Measurement
for (col in names(Data)[!names(Data) %in% "Date"]) {
  
  # auto.arima
  auto_arima <- auto.arima(ts(train_data[[col]], frequency = 365))
  auto_arima_forecast <- forecast(auto_arima, h = nrow(test_data))
  
  # Measure forecasting accuracy
  auto_arima_metrics <- c(
    MAE = mae(test_data[[col]], auto_arima_forecast$mean),
    MSE = mse(test_data[[col]], auto_arima_forecast$mean),
    RMSE = rmse(test_data[[col]], auto_arima_forecast$mean),
    MAPE = mape(test_data[[col]], auto_arima_forecast$mean)
  )
  
  # Save parameter choices to the dataframe
  auto_arima_params <- coefficients(auto_arima)
  auto_arima_results <- rbind(auto_arima_results, c(col, auto_arima_params))
  
  # Save results in the list
  results_arima[[col]] <- list(auto_arima_metrics = auto_arima_metrics)
}

# Print results
print(auto_arima_results)
summary(results_arima)

# Save auto.arima parameter choices dataframe
write.csv(auto_arima_results, "auto_arima_results.csv", row.names = FALSE)

# Save results
#save(results_arima, file = "auto_arima_results.RData")
load("auto_arima_results.RData")



# auto.arima get all
aa_Optimized_ALKP <- auto.arima(ts(train_data$Optimized_ALKP, frequency = 365))
aa_PI <- auto.arima(ts(train_data$Polynomial_Interpolation, frequency = 365)) 
aa_CSI <- auto.arima(ts(train_data$Spline_Interpolation, frequency = 365))
aa_kalmanARIMA <- auto.arima(ts(train_data$Imputed_Kalman_ARIMA_TS, frequency = 365))
aa_kalmanSTS <- auto.arima(ts(train_data$Imputed_Kalman_Struct_TS, frequency = 365))
best_forecast_aa <- forecast(aa_kalmanSTS, h = nrow(test_data))
aa_mats <- auto.arima(ts(train_data$Imputed_MA_TS, frequency = 365))
worst_forecast_aa <- forecast(aa_worst, h = nrow(test_data))
#save(worst_forecast_aa, file = "worst_forecast_aa.RData")
load("worst_forecast_aa.RData")