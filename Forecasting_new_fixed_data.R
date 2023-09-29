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
library(gridExtra)
library(reshape2)

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

#save it 
#save(Data, file = "Data_final.RData")
load("Data_final.RData")

# Split the data
train_data <- subset(Data, Date < as.Date("2023-01-01"))
test_data <- subset(Data, Date >= as.Date("2023-01-01"))
test_actuals <- test_data$imputed_ma_ts #they are all the same
#############make the for loop that will to forecast for each row and than measure accuracy using different accuracy methods
results <- list()

# Forecasting and Accuracy Measurement
for (col in names(Data)[!names(Data) %in% "Date"]) {
  
  # Triple Exponential Smoothing (HoltWinters)
  hw_model <- HoltWinters(ts(train_data[[col]], frequency = 365), alpha = 1, beta = 1, gamma = 1)
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
  
  auto_arima_metrics <- c(
    MAE = mae(test_actuals, auto_arima_forecast$mean),
    MSE = mse(test_actuals, auto_arima_forecast$mean),
    RMSE = rmse(test_actuals, auto_arima_forecast$mean),
    MAPE = mape(test_actuals, auto_arima_forecast$mean)
  )
  
  results[[col]] <- list(hw_metrics = hw_metrics, auto_arima_metrics = auto_arima_metrics)
}

print(results)
summary(results)
#save it since it is executing for a while
#save(results, file = "results.RData")
load("results.RData")
#save(auto_arima, file = "auto_arima.RData")
load("auto_arima.RData")


#write_xlsx(df_results, "final_results.xlsx")

################TEST FOR CHECKING THE FOR LOOP
#Triple expo smoothing 
tesst1 <- HoltWinters(ts(train_data$polynomial_interpolation, frequency = 365))
t_forecast <- forecast(tesst1, h = nrow(test_data))
mae(test_actuals, t_forecast$mean) 
##9.17 for imputed_ma_ts <- same as in for loop, we are good to go

# auto.arima
test2 <- auto.arima(ts(train_data$imputed_ma_ts, frequency = 365))
t2_forecast <- forecast(test2, h = nrow(test_data))
mae(test_actuals, t2_forecast$mean)
#2.810784 for imputed_ma_ts <- same as in for loop, we are good to go


###############PLOT IT ALL TOGETHER
#forecasts are too messy to plot together, so we won't
# Prepare data for plotting accuracy metrics
accuracy_data <- data.frame(Column = character(), Metric = character(), Method = character(), Value = numeric())

for (col in names(Data)[!names(Data) %in% "Date"]) {
  tmp <- data.frame(
    Column = col,
    Metric = rep(c("MAE", "MSE", "RMSE", "MAPE"), 2),
    Method = c(rep("HoltWinters", 4), rep("SARIMA", 4)),
    Value = c(unlist(results[[col]]$hw_metrics), unlist(results[[col]]$auto_arima_forecast))
  )
  accuracy_data <- rbind(accuracy_data, tmp)
}

# Plot
ggplot(accuracy_data, aes(x = Column, y = Value, fill = Method, group = Method)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ Metric, scales = "free_y") +
  labs(title = "Forecasting Error Metrics Comparison", y = "Value") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#write_csv(accuracy_data, "accuracy_data.csv")

#####################
##see the worst and best performing interpolation technique
accuracy_data %>%
  filter(Method %in% c("SARIMA", "HoltWinters"), 
         Metric %in% c("MAE", "MSE", "RMSE", "MAPE")) %>%
  group_by(Metric) %>%
  slice_max(Value) %>%
  ungroup() %>%
  select(Column, Metric, Method, Value)

##see the best performing approximation technique
accuracy_data %>%
  filter(Method %in% c("SARIMA", "HoltWinters"), 
         Metric %in% c("MAE", "MSE", "RMSE", "MAPE")) %>%
  group_by(Metric) %>%
  slice_min(Value) %>%
  ungroup() %>%
  select(Column, Metric, Method, Value)

accuracy_data %>% filter(Column %in% c("imputed_seadec_ma", "imputed_kalman_arima_ts"))

#########PLOT ANY APPROXIMATION TECHNIQUE
##we will plot worst and best one 

#Triple expo smoothing for worst performing
hw_worst <- HoltWinters(ts(train_data$polynomial_interpolation, frequency = 365), alpha = 1, beta = 1, gamma = 1)
worst_forecast <- forecast(hw_worst, h = nrow(test_data))

# auto.arima for worst performing
aa_worst <- auto.arima(ts(train_data$polynomial_interpolation, frequency = 365))
worst_forecast_aa <- forecast(aa_worst, h = nrow(test_data))
#save(worst_forecast_aa, file = "worst_forecast_aa.RData")
load("worst_forecast_aa.RData")

##best one 
#Triple expo smoothing for best performing
hw_best <- HoltWinters(ts(train_data$imputed_kalman_arima_ts, frequency = 365), alpha = 1, beta = 1, gamma = 1)
best_forecast <- forecast(hw_best, h = nrow(test_data))

# auto.arima for best performing
aa_best <- auto.arima(ts(train_data$imputed_kalman_arima_ts, frequency = 365))
best_forecast_aa <- forecast(aa_best, h = nrow(test_data))
#save(best_forecast_aa, file = "best_forecast_aa.RData")
load("best_forecast_aa.RData")

#PLOTTIGN THE FORECASTS
ggplot(test_data, aes(x = Date)) +
  
  geom_line(aes(y = worst_forecast$mean, color = "HoltWinters polynomial interpolation", linetype = "HoltWinters polynomial interpolation"), size = 1) +
  geom_line(aes(y = worst_forecast_aa$mean, color = "auto.arima polynomial interpolation", linetype = "auto.arima polynomial interpolation"), size = 1) +
  
  geom_line(aes(y = best_forecast$mean, color = "HoltWinters Kalman ARIMA", linetype = "HoltWinters Kalman ARIMA"), size = 1) +
  geom_line(aes(y = best_forecast_aa$mean, color = "auto.arima Kalman ARIMA", linetype = "auto.arima Kalman ARIMA"), size = 1) +
  
  geom_line(aes(y = test_actuals, color = "Actual Forecast", linetype = "Actual Forecast"), size = 1) +
  
  scale_color_manual(values = c("HoltWinters polynomial interpolation" = "lightblue", 
                                "HoltWinters Kalman ARIMA" = "darkblue", 
                                "auto.arima polynomial interpolation" = "green", 
                                "auto.arima Kalman ARIMA" = "darkgreen", 
                                "Actual Forecast" = "red")) +
  
  scale_linetype_manual(values = c("HoltWinters polynomial interpolation" = "dashed", 
                                   "HoltWinters Kalman ARIMA" = "dashed", 
                                   "auto.arima polynomial interpolation" = "dotdash", 
                                   "auto.arima Kalman ARIMA" = "dotdash", 
                                   "Actual Forecast" = "solid")) +
  
  labs(title = "Forecasts vs Actuals", y = "Value", x = "Date", color = "Series", linetype = "Series") +
  theme_minimal()

 



#COMPARING best and worst
# Calculating metrics
metrics <- data.frame(
  Metric = c("MSE", "MAPE", "MAE", "RMSE"),
 "polynomial_interpolation(Holt-Winters)" = c(
    mse(test_actuals, worst_forecast$mean),
    mape(test_actuals, worst_forecast$mean),
    mae(test_actuals, worst_forecast$mean),
    rmse(test_actuals, worst_forecast$mean)
  ),
  "imputed_kalman_arima(SARIMA)" = c(
    mse(test_actuals, best_forecast_aa$mean),
    mape(test_actuals, best_forecast_aa$mean),
    mae(test_actuals, best_forecast_aa$mean),
    rmse(test_actuals, best_forecast_aa$mean)
  )
)

# Reshaping the data for ggplot
melted_metrics <- melt(metrics, id.vars = "Metric")


ggplot(melted_metrics, aes(x = Metric, y = value, fill = variable)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = round(value,2)), position = position_dodge(width = 0.9), vjust = -0.5) +
  labs(y = "Value", x = "Metric", fill = "Approximation technique") +
  theme_minimal() +
  ggtitle("Comparison between least and most accurate technique")



#Function for calculating the percentage accuracy
calculate_accuracy <- function(Kolona, Metoda) {
 mape_vr <-  accuracy_data %>% filter(Column %in% c(Kolona),
                            Metric %in% c("MAPE"),
                            Method %in% c(Metoda)) %>% 
    select(Value)
  
  return (100 - (mape_vr*100))
  
}

calculate_accuracy("imputed_seadec_ma", "SARIMA")


########additional test for Holt-winters (too test alpha, beta, and gamma)
for (col in names(Data)[!names(Data) %in% "Date"]) {
  
  # Triple Exponential Smoothing (Holt-Winters)
  hw_model <- HoltWinters(ts(train_data[[col]], frequency = 365),alpha = 1, beta = 1, gamma = 1)
  hw_forecast <- forecast(hw_model, h = nrow(test_data))

  # Measure forecasting accuracy
  hw_metrics <- c(
    MAE = mae(test_actuals, hw_forecast$mean),
    MSE = mse(test_actuals, hw_forecast$mean),
    RMSE = rmse(test_actuals, hw_forecast$mean),
    MAPE = mape(test_actuals, hw_forecast$mean)
  )
  

  results_hw[[col]] <- list(hw_metrics = hw_metrics)
}




##########3
# Initialize an empty data frame to store forecast results
forecasts_df <- data.frame(Date = test_data$Date)

# Loop through each column, excluding "Date"
for (col in names(Data)[!names(Data) %in% "Date"]) {
  
  # Triple Exponential Smoothing (HoltWinters)
  hw_model <- HoltWinters(ts(train_data[[col]], frequency = 365), alpha = 1, beta = 1, gamma = 1)
  hw_forecast <- forecast(hw_model, h = nrow(test_data))
  
  # Store the forecast results with column names
  forecasts_df[[paste(col, "Forecast")]] <- hw_forecast$mean
}

# Convert forecast dataframe to a regular data frame
forecasts_df <- as.data.frame(forecasts_df)



# Convert data from wide to long format for plotting
plot_data_long <- reshape2::melt(forecasts_df, id.vars = "Date")


# Plotting
p <- ggplot(plot_data_long, aes(x = Date, y = value, color = variable)) +
  geom_line() +
  labs(title = "Forecasts vs Actuals",
       y = "Value", x = "Date", color = "Series") +
  theme_minimal()


print(p)
