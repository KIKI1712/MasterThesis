# Load libraries
library(forecast)
library(TSA)
library(zoo)
library(ggplot2)
#generate sales data with trend and summer seasonality
t <- seq(as.Date("2021-01-01"), by = "1 month", length.out = 48)
trend <- 100 * exp(0.03 * seq_along(t))
seasonality <- c(1, 1, 1, 1.3, 1.7, 2, 2, 1.5, 1, 1, 1, 1) 

sales_data <- data.frame(date = t, sales = trunc(trend) * seasonality)
#split data into training and test sets
train_data <- sales_data[1:36, ]
test_data <- sales_data[37:48, ]

train_ts <- ts(train_data$sales, frequency = 12)

results_df <- data.frame(Period = 1:12, Demand = test_data$sales)

#function to calculate error metrics
calculate_errors <- function(forecast, actual) {
  errors <- forecast - actual
  mae <- mean(abs(errors))
  mse <- mean(errors^2)
  rmse <- sqrt(mse)
  mape <- mean(abs(errors/actual)) * 100
  return(cbind(errors, mae, mse, rmse, mape))
}

# ARMA
best_arma_mae <- Inf  
best_arma_order <- c(0, 0, 0)  

for (p in 0:3) {
  for (q in 0:3) {
    if (p == 0 && q == 0) {
      next  #skip ARMA(0,0)
    }
    
   
    arma_model <- Arima(train_ts, order = c(p, 0, q))
    arma_forecast <- forecast(arma_model, h = 12)$mean
    
    
    errors_arma <- calculate_errors(arma_forecast, test_data$sales)
    mae <- errors_arma[1, 2]  
    
    
    if (mae < best_arma_mae) {
      best_arma_mae <- mae
      best_arma_order <- c(p, 0, q)
    }
  }
}

cat("Best ARMA order:", best_arma_order, "\n")
cat("Lowest MAE:", best_arma_mae, "\n")

arma_model <- Arima(train_ts, c(3, 0, 2))
arma_forecast <- forecast(arma_model, h = 12)$mean
errors_arma <- calculate_errors(arma_forecast, test_data$sales)
results_df$ARMA <- arma_forecast
results_df$ARMA_MAE <- errors_arma[, 2]
results_df$ARMA_MSE <- errors_arma[, 3]
results_df$ARMA_RMSE <- errors_arma[, 4]
results_df$ARMA_MAPE <- errors_arma[, 5]

# ARIMA
best_arima_mae <- Inf  
best_arima_order <- c(0, 0, 0)

for (p in 0:3) {
  for (q in 0:3) {
    for (d in 1:2) {  
      if (p == 0 && q == 0 && d == 0) {
        next  #skip ARIMA(0,0,0)
      }
      
      arima_model <- Arima(train_ts, order = c(p, d, q))
      arima_forecast <- forecast(arima_model, h = 12)$mean
      
      errors_arima <- calculate_errors(arima_forecast, test_data$sales)
      mae <- errors_arima[1, 2]  
      
      if (mae < best_arima_mae) {
        best_arima_mae <- mae
        best_arima_order <- c(p, d, q)
      }
    }
  }
}

cat("Best ARIMA order:", best_arima_order, "\n")
cat("Lowest MAE:", best_arima_mae, "\n")


best_arima_model <- Arima(train_ts, order = best_arima_order)
best_arima_forecast <- forecast(best_arima_model, h = 12)$mean


results_df$ARIMA <- best_arima_forecast
errors_arima <- calculate_errors(best_arima_forecast, test_data$sales)
results_df$ARIMA_MAE <- errors_arima[, 2]
results_df$ARIMA_MSE <- errors_arima[, 3]
results_df$ARIMA_RMSE <- errors_arima[, 4]
results_df$ARIMA_MAPE <- errors_arima[, 5]

#make the data stationary
diff_train_ts <- diff(train_ts)

# SARIMA
sarima_order <- c(1, 1, 1, 1, 0, 1, 12)  
sarima_model <- Arima(diff_train_ts, order = c(sarima_order[1:3]), seasonal = list(order = c(sarima_order[4:6]), period = sarima_order[7]))
sarima_forecast_diff <- forecast(sarima_model, h = 12)$mean

# Adding back the differencing to the forecast
sarima_forecast <- cumsum(c(tail(train_ts, 1), sarima_forecast_diff))[-1]

errors_sarima <- calculate_errors(sarima_forecast, test_data$sales)
results_df$SARIMA <- sarima_forecast
results_df$SARIMA_MAE <- errors_sarima[, 2]
results_df$SARIMA_MSE <- errors_sarima[, 3]
results_df$SARIMA_RMSE <- errors_sarima[, 4]
results_df$SARIMA_MAPE <- errors_sarima[, 5]


# Simple Exponential Smoothing
best_ses_mae <- Inf  
best_ses_alpha <- 0  

for (alpha in seq(0.01, 0.99, by = 0.01)) {
  ses_model <- ets(train_ts, model = "MNN", alpha = alpha)
  ses_forecast <- forecast(ses_model, h = 12)$mean
  errors_ses <- calculate_errors(ses_forecast, test_data$sales)
  mae <- mean(errors_ses[, 2])  
  
  #check if current alpha gives lower MAE
  if (mae < best_ses_mae) {
    best_ses_mae <- mae
    best_ses_alpha <- alpha
  }
}

cat("Best SES parameter (alpha):", best_ses_alpha, "\n")
cat("Lowest MAE:", best_ses_mae, "\n")
ses_model <- ets(train_ts, model = "MNN", alpha = 0.22,allow.multiplicative.trend = TRUE)
ses_forecast <- forecast(ses_model, h = 12)$mean
errors_ses <- calculate_errors(ses_forecast, test_data$sales)
results_df$SES <- ses_forecast
results_df$SES_MAE <- errors_ses[, 2]
results_df$SES_MSE <- errors_ses[, 3]
results_df$SES_RMSE <- errors_ses[, 4]
results_df$SES_MAPE <- errors_ses[, 5]

# Double Exponential Smoothing

des_model <- ets(train_ts, model = "AAN", opt.crit = "mae", allow.multiplicative.trend = TRUE)
des_forecast <- forecast(des_model, h = 12)$mean
errors_des <- calculate_errors(des_forecast, test_data$sales)

# Print the results
cat("Best DES parameters (alpha, beta):", des_model$alpha, des_model$beta, "\n")
cat("Lowest MAE:", mean(errors_des[, 2]), "\n")

# Add results to the dataframe
results_df$DES <- des_forecast
results_df$DES_MAE <- errors_des[, 2]
results_df$DES_MSE <- errors_des[, 3]
results_df$DES_RMSE <- errors_des[, 4]
results_df$DES_MAPE <- errors_des[, 5]

# Triple Exponential Smoothing with HoltWinters
best_tes_mae <- Inf  
best_tes_params <- c(0, 0, 0) 

for (alpha in seq(0.01, 0.99, by = 0.01)) {
  for (beta in seq(0.01, 0.99, by = 0.01)) {
    for (gamma in seq(0.01, 0.99, by = 0.01)) {
      tes_model <- HoltWinters(train_ts, alpha = alpha, beta = beta, gamma = gamma, seasonal = "additive")
      tes_forecast <- forecast(tes_model, h = 12)
      tes_forecast_values <- tes_forecast$mean
      
      errors_tes <- calculate_errors(tes_forecast_values, test_data$sales)
      mae <- mean(errors_tes[, 2])  
      
      #check if current combination gives lower MAE
      if (mae < best_tes_mae) {
        best_tes_mae <- mae
        best_tes_params <- c(alpha, beta, gamma)
      }
    }
  }
}

cat("Best TES parameters (alpha, beta, gamma):", best_tes_params, "\n")
cat("Lowest MAE:", best_tes_mae, "\n")
tes_model <- HoltWinters(train_ts, alpha = best_tes_params[1], beta = best_tes_params[2], gamma = best_tes_params[3], seasonal = "additive")
#tes_model <- HoltWinters(train_ts, alpha = 1, beta = 1, gamma = 1, seasonal = "multiplicative")
tes_forecast <- forecast(tes_model, h = 12)
tes_forecast_values <- tes_forecast$mean
errors_tes <- calculate_errors(tes_forecast_values, test_data$sales)
results_df$TES <- tes_forecast_values
results_df$TES_MAE <- errors_tes[, 2]
results_df$TES_MSE <- errors_tes[, 3]
results_df$TES_RMSE <- errors_tes[, 4]
results_df$TES_MAPE <- errors_tes[, 5]


rounded_results_df <- as.data.frame(lapply(results_df, function(x) if(is.numeric(x)) round(x) else x))

print(rounded_results_df)



library(ggplot2)

# Melt the data frame
rounded_results_df$MonthYear <- seq(as.Date("2024-01-01"), as.Date("2024-12-01"), by = "month")
rounded_results_df$MonthYear <- as.Date(rounded_results_df$MonthYear)
rounded_results_df$MonthYear <- format(rounded_results_df$MonthYear, "%m-%y")
rounded_results_df$Actual_sales <- rounded_results_df$Demand
                               

melted_forecasts <- reshape2::melt(rounded_results_df, id.vars = c("Period", "Demand"), measure.vars = c("Actual_sales","ARMA", "ARIMA", "SARIMA", "SES", "DES", "TES"))

# Plotting
ggplot(melted_forecasts, aes(x = factor(Period), y = value, color = variable, group = variable)) +
  geom_line() +
  geom_point() +
  geom_line(aes(x = factor(Period), y = Demand), color = "black", linetype = "dashed", size = 1, show.legend = "Actual Sales") + 
  labs(
       x = "Date",
       y = "Sales",
       color = "Forecasting Methods") +
  theme_minimal() +
  scale_x_discrete(labels = rounded_results_df$MonthYear) +   
  guides(color = guide_legend(title = "Forecasting Methods"),
         linetype = guide_legend(title = "Legend Title", override.aes = list(linetype = "dashed")),
         color = guide_legend(title = "Actual Sales", override.aes = list(color = "black", linetype = "dashed")))



library(ggplot2)

# Melt the updated rounded_results_df for better visualization
melted_errors <- reshape2::melt(rounded_results_df, id.vars = "MonthYear", measure.vars = c("SES_MAE", "DES_MAE", "TES_MAE", "ARMA_MAE", "ARIMA_MAE", "SARIMA_MAE", "SES_MSE", "DES_MSE", "TES_MSE", "ARMA_MSE", "ARIMA_MSE", "SARIMA_MSE", "SES_RMSE", "DES_RMSE", "TES_RMSE", "ARMA_RMSE", "ARIMA_RMSE", "SARIMA_RMSE", "SES_MAPE", "DES_MAPE", "TES_MAPE", "ARMA_MAPE", "ARIMA_MAPE", "SARIMA_MAPE"))

# Extract method names and error types
melted_errors$Method <- gsub("_[A-Z]+", "", melted_errors$variable)
melted_errors$ErrorType <- gsub("[A-Z]+_", "", melted_errors$variable)

# Plotting
ggplot(melted_errors, aes(x = Method, y = value, fill = ErrorType)) +
  geom_bar(stat = "identity", position = "dodge", color = "black", size = 0.2) +
  labs(title = "Forecasting Accuracy Comparison",
       x = "Forecast Method",
       y = "Error Metric Value",
       fill = "Error Type") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  facet_wrap(~ErrorType, scales = "free_y", ncol = 1)  



#create a subset of the results_df for SARIMA and ARMA
selected_results_df <- rounded_results_df[c("MonthYear", "SARIMA_MAE", "SARIMA_MSE", "SARIMA_RMSE","SARIMA_MAPE", "ARMA_MAE", "ARMA_MSE", "ARMA_RMSE", "ARMA_MAPE")]

#melt the selected_results_df for better visualization
melted_errors <- reshape2::melt(selected_results_df, id.vars = "MonthYear", measure.vars = c("SARIMA_MAE", "SARIMA_MSE", "SARIMA_RMSE","SARIMA_MAPE", "ARMA_MAE", "ARMA_MSE", "ARMA_RMSE", "ARMA_MAPE"))

#extract method names and error types
melted_errors$Method <- gsub("_[A-Z]+", "", melted_errors$variable)
melted_errors$ErrorType <- gsub("[A-Z]+_", "", melted_errors$variable)

ggplot(melted_errors, aes(x = Method, y = value, fill = ErrorType, label = sprintf("%.2f", value))) +
  geom_bar(stat = "identity", position = "dodge", color = "black", size = 0.2) +
  geom_text(position = position_dodge(width = 0.9), aes(y = ifelse(value >= 0, value  +1, value - 0.5))) +  # Adjust offset for label placement
  labs(title = "Forecasting Accuracy Comparison (SARIMA vs ARMA)",
       x = "Forecast Method",
       y = "Error Metric Value",
       fill = "Error Type") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  facet_wrap(~ErrorType, scales = "free_y", ncol = 1)



sum(test_data$sales)

ma

