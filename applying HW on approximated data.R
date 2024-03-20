setwd("C:/Users/User/teza")
library(forecast)
load("Data_final.RData")

Data$Date <- as.Date(Data$Date)
# Split the data
train_data <- subset(Data, Date < as.Date("2023-01-01"))
test_data <- subset(Data, Date >= as.Date("2023-01-01"))
test_actuals <- test_data$Optimized_ALKP
Data$Sales_Original <- NULL
results_hw <- list()
results_hw_seasonal <- list()
# Create an empty dataframe to store parameter choices
parameter_results <- data.frame(COL = character(), alpha = numeric(), beta = numeric(), gamma = numeric(),
                                stringsAsFactors = FALSE)

# Forecasting and Accuracy Measurement
for (col in names(Data)[!names(Data) %in% "Date"]) {
  
  alpha_values <- seq(0.1, 1, by = 0.1)
  beta_values <- seq(0.1, 1, by = 0.1)
  gamma_values <- seq(0.1, 1, by = 0.1)
  
  best_mape <- Inf
  best_params <- c()
  
  for (alpha in alpha_values) {
    for (beta in beta_values) {
      for (gamma in gamma_values) {
        
        # Fit Holt-Winters model
        hw_model <- HoltWinters(ts(train_data[[col]], frequency = 365), alpha = alpha, beta = beta, gamma = gamma, seasonal = "multiplicative")
        
        # Make forecast
        hw_forecast <- forecast(hw_model, h = nrow(test_data))
        
        # Calculate all forecasting error metrics
        hw_metrics <- c(
          MAE = mae(test_data[[col]], hw_forecast$mean),
          MSE = mse(test_data[[col]], hw_forecast$mean),
          RMSE = rmse(test_data[[col]], hw_forecast$mean),
          MAPE = mape(test_data[[col]], hw_forecast$mean)
        )
        
        # Check if current combination yields a better MAPE
        current_mape <- hw_metrics["MAPE"]
        if (current_mape < best_mape) {
          best_mape <- current_mape
          best_params <- c(alpha = alpha, beta = beta, gamma = gamma)
        }
      }
    }
  }
  
  # Save parameter choices to the dataframe
  parameter_results <- rbind(parameter_results, c(col, best_params))
  
  # Print results for the current column
  cat("Column:", col, "\n")
  cat("Best Parameters:", best_params, "\n")
  cat("Best MAPE:", best_mape, "\n")
  
  # Save forecasting errors
  results_hw_seasonal[[col]] <- list(hw_metrics = hw_metrics)
}

# Save the parameter choices dataframe
write.csv(parameter_results, "parameter_results.csv", row.names = FALSE)

# Save forecasting errors
#save(results_hw, file = "results_hw.RData")
load("results_hw.RData")
results_hw
#save(results_hw_seasonal, file = "results_hw_seasonal.RData")
load("results_hw_seasonal.RData")

# Extract the Holt-Winters metrics for Sales_Original
sales_original_metrics <- results_hw$Sales_Original$hw_metrics

# Remove the Sales_Original entry from the list
results_hw_seasonal <- results_hw_seasonal[!names(results_hw_seasonal) %in% "Sales_Original"]

# Add the Sales_Original metrics at the beginning of the list
results_hw_seasonal <- c(list(Sales_Original = list(hw_metrics = sales_original_metrics)), results_hw_seasonal)




#DROP LATER
# Fit Holt-Winters model
hw_model <- HoltWinters(ts(train_data$Imputed_MA_TS, frequency = 365), alpha = 0.1, beta = 0.1, gamma = 0.2, seasonal = "multiplicative")

# Make forecast
hw_forecast <- forecast(hw_model, h = nrow(test_data))


hw_metrics <- c(
  MAE = mae(test_data$Imputed_MA_TS, hw_forecast$mean),
  MSE = mse(test_data$Imputed_MA_TS, hw_forecast$mean),
  RMSE = rmse(test_data$Imputed_MA_TS, hw_forecast$mean),
  MAPE = mape(test_data$Imputed_MA_TS, hw_forecast$mean)
)

results_hw_seasonal$Imputed_MA_TS <- list(hw_metrics = hw_metrics)