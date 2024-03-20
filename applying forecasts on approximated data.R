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


#setwd("C:/Users/User/Desktop/TEZA")
#Data <- read_xlsx("data_interpolated_combinedALL.xlsx")
#delete column with NAs that we needed before for plotting 
#Data$sales_with_na <- NULL
sales_data_org <- read_xlsx("fake_data_m5.xlsx")

Data <- data.frame(
  Date = sales_data$Date,
  Sales_Original = sales_data_org$Sales,
  Optimized_ALKP = sales_data$Sales,
  Polynomial_Interpolation = sales_data_polynomial$Sales,
  Spline_Interpolation = sales_data_spline$Sales,
  Imputed_Kalman_ARIMA_TS = imputed_kalman_arima_ts,
  Imputed_Kalman_Struct_TS = imputed_kalman_struct_ts,
  Imputed_MA_TS = imputed_ma_ts
)

##This is used for auto.arima for Original Dataset, since it failed to capture otherwise
arima_model_original <- auto.arima(ts(train_data$Sales_Original, frequency = 365),D=1,trace=T,stepwise = F)
original_forecast_moj <- forecast(arima_model_original, h = nrow(test_data))

accuracy_data$Value[accuracy_data$Metric == "MAE" & accuracy_data$Method == "Auto ARIMA" & accuracy_data$Approximation_method == "Sales_Original"] <- mae(test_actuals, original_forecast_moj$mean)
accuracy_data$Value[accuracy_data$Metric == "MSE" & accuracy_data$Method == "Auto ARIMA" & accuracy_data$Approximation_method == "Sales_Original"] <- mse(test_actuals, original_forecast_moj$mean)
accuracy_data$Value[accuracy_data$Metric == "RMSE" & accuracy_data$Method == "Auto ARIMA" & accuracy_data$Approximation_method == "Sales_Original"] <- rmse(test_actuals, original_forecast_moj$mean)
accuracy_data$Value[accuracy_data$Metric == "MAPE" & accuracy_data$Method == "Auto ARIMA" & accuracy_data$Approximation_method == "Sales_Original"] <- mape(test_actuals, original_forecast_moj$mean)
save(original_forecast_moj, file ="original_forecast_moj.RData")
load("original_forecast_moj.RData")
#write_xlsx(df_results, "final_results.xlsx")

###############PLOT IT ALL TOGETHER
#forecasts are too messy to plot together, so we won't
# Prepare data for plotting accuracy metrics
accuracy_data <- data.frame(Approximation_method = character(), Metric = character(), Method = character(), Value = numeric())

for (col in names(Data)[!names(Data) %in% "Date"]) {
  tmp <- data.frame(
    Approximation_method = col,
    Metric = rep(c("MAE", "MSE", "RMSE", "MAPE"), 2),
    Method = c(rep("HoltWinters", 4), rep("Auto ARIMA", 4)),
    Value = round(c(unlist(results_hw_seasonal[[col]]$hw_metrics), unlist(results_arima[[col]]$auto_arima_metrics)), 5)
  )
  accuracy_data <- rbind(accuracy_data, tmp)
}

# Plot
ggplot(accuracy_data, aes(x = Approximation_method, y = Value, fill = Method, group = Method)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ Metric, scales = "free_y") +
  labs(title = "Forecasting Error Metrics Comparison", y = "Value") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

selected_columns <- c("Sales_Original", "Spline_Interpolation", "Polynomial_Interpolation")
filtered_data <- accuracy_data[accuracy_data$Approximation_method %in% selected_columns, ]

filtered_data$Approximation_method <- factor(filtered_data$Approximation_method, levels = selected_columns,
                               labels = c("Original sales", "Spline Interpolation", "Polynomial Interpolation"))

# Plotting code
ggplot(filtered_data, aes(x = Approximation_method, y = Value, fill = Method, group = Method)) +
  geom_bar(stat = "identity", position = "dodge", color = "white") +
  facet_wrap(~ Metric, scales = "free_y") +
  labs(y = "Value", x="Accuracy measurement") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "right",
        legend.title = element_blank(),
        panel.grid.major.x = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(size = 10)) 
#write_csv(accuracy_data, "accuracy_data.csv")

#####################
##see the worst performing approximation technique
accuracy_data %>%
  filter(Method %in% c("Auto ARIMA", "HoltWinters"), 
         Metric %in% c("MAE", "MSE", "RMSE", "MAPE")) %>%
  group_by(Metric) %>%
  slice_max(Value) %>%
  ungroup() %>%
  select(Approximation_method, Metric, Method, Value)

#accuracy, except original
accuracy_data %>%
  filter(Method %in% c("Auto ARIMA", "HoltWinters"), 
         Metric %in% c("MAE", "MSE", "RMSE", "MAPE"),
         Approximation_method != "Sales_Original") %>%
  group_by(Metric) %>%
  slice_max(Value) %>%
  ungroup() %>%
  select(Approximation_method, Metric, Method, Value)

##see the best performing approximation technique
accuracy_data %>%
  filter(Method %in% c("Auto ARIMA", "HoltWinters"), 
         Metric %in% c("MAE", "MSE", "RMSE", "MAPE")) %>%
  group_by(Metric) %>%
  slice_min(Value) %>%
  ungroup() %>%
  select(Approximation_method, Metric, Method, Value)

accuracy_data %>% filter(Approximation_method %in% c("seadec_moving_average", "Kalman_arima_ts"))

#########PLOT ANY APPROXIMATION TECHNIQUE
#ORGIINAL
#Triple expo smoothing for original
hw_original <- HoltWinters(ts(train_data$Sales_Original, frequency = 365), alpha = 0.8, beta = 0.7, gamma = 0.3)
original_forecast <- forecast(hw_original, h = nrow(test_data))
mape(test_data$Sales_Original, original_forecast$mean)
#this is used for original ARIMA
arima_model_original <- auto.arima(ts(train_data$Sales_Original, frequency = 365),D=1,trace=T,stepwise = F)
original_forecast_moj <- forecast(arima_model_original, h = nrow(test_data))
mape(test_data$Sales_Original, original_forecast_moj$mean)

#AUTO ARIMA FOR WORST PERFORMING OFFICIAL
aa_worst <- auto.arima(ts(train_data$Polynomial_Interpolation, frequency = 365))
worst_forecast_aa <- forecast(aa_worst, h = nrow(test_data))
#save(worst_forecast_aa, file = "worst_forecast_aa.RData")
load("worst_forecast_aa.RData")


##best one 
#HOLTWINTER FOR BEST PERFORMING OFFICIAL
hw_best <- HoltWinters(ts(train_data$Spline_Interpolation , frequency = 365), alpha = 0.9, beta = 0.5, gamma = 0.1, seasonal = "multiplicative")
best_forecast <- forecast(hw_best, h = nrow(test_data))

# auto.arima for best performing BEST OFFICIAL
#aa_kalmanSTS <- auto.arima(ts(train_data$Spline_Interpolation, frequency = 365))
#best_forecast_aa <- forecast(aa_kalmanSTS, h = nrow(test_data))
#save(best_forecast_aa, file = "best_forecast_aa.RData")
#load("best_forecast_aa.RData")

#PLOTTIGN THE FORECASTS
library(lubridate)

# Set the locale to English
Sys.setlocale("LC_TIME", "English")

test_data$Date <- as.Date(test_data$Date)

ggplot(test_data, aes(x = Date)) +
  #geom_line(aes(y = original_forecast_moj$mean, color = "sarima - Original Dataset"), size = 1, alpha = 0.8) +
  geom_line(aes(y = original_forecast_moj$mean, color = "Auto ARIMA - Original Dataset"), linewidth = 1, alpha = 0.8) +
  geom_line(aes(y = worst_forecast_aa$mean, color = "Auto ARIMA - Polynomial Interpolation"), linewidth = 1, alpha = 0.8) +
  geom_line(aes(y = best_forecast$mean, color = "HoltWinters - Spline Interpolation"), linewidth = 1, alpha = 0.8) +
  geom_line(aes(y = test_actuals, color = "Actual Sales"), linewidth = 1.2, linetype = "dashed") +  # Change the linetype here
  
  scale_color_manual(values = c(
    #"sarima - Original Dataset" = "blue",
    "Auto ARIMA - Original Dataset" = "#FF3333",
    "Auto ARIMA - Polynomial Interpolation" = "#3385FF",
    "HoltWinters - Spline Interpolation" = "#2E8B57",
    "Actual Sales" = "black"
  )) +
  
  labs(y = "Forecasted demand", x = "Date", color = "Series", linetype = "Series") +
  scale_x_date(date_breaks = "2 months", date_labels = "%m - %Y") +  
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.text = element_text(size = 7),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "#F0F0F0"),
    panel.border = element_blank(),
    axis.line = element_line(color = "black")
  )



 



#COMPARING best and worst
# Calculating metrics
metrics <- data.frame(
  Metric = c("MSE", "MAPE", "MAE", "RMSE"),
 "polynomial_interpolation - Holt-Winters" = c(
    mse(test_actuals, worst_forecast$mean),
    mape(test_actuals, worst_forecast$mean),
    mae(test_actuals, worst_forecast$mean),
    rmse(test_actuals, worst_forecast$mean)
  ),
  "Kalman_arima - SARIMA" = c(
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
  labs(y = "Value", x = "Metric", fill = "Approximation and forecasting method") +
  theme_minimal() +
  ggtitle("Comparison between least and most accurate method")



#Function for calculating the percentage accuracy
calculate_accuracy <- function(Kolona, Metoda) {
 mape_vr <-  accuracy_data %>% filter(Approximation_method %in% c(Kolona),
                            Metric %in% c("MAPE"),
                            Method %in% c(Metoda)) %>% 
    select(Value)
  
  return (100 - (mape_vr*100))
  
}

calculate_accuracy("Spline_Interpolation", "HoltWinters")
calculate_accuracy("Polynomial_Interpolation", "Auto ARIMA")
calculate_accuracy("Sales_Original", "Auto ARIMA")
calculate_accuracy("Sales_Original", "HoltWinters")
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

hw_modeltest <- HoltWinters(ts(train_data$Sales_Original, frequency = 365),alpha = 0.9, beta = 0.1, gamma =0.1, seasonal = "multiplicative" )
hw_forecasttest <- forecast(hw_modeltest, h = nrow(test_data))
mape(test_actuals, hw_forecasttest$mean)
##
hw_modeltest <- HoltWinters(ts(train_data$Polynomial_Interpolation, frequency = 365),alpha = 1, beta = 1, gamma = 1)
hw_forecasttest <- forecast(hw_modeltest, h = nrow(test_data))
mape(test_actuals, hw_forecasttest$mean)
##
hw_modeltest <- HoltWinters(ts(train_data$Imputed_Kalman_Struct_TS, frequency = 365),alpha =1, beta = 0.4, gamma = 0.4)
hw_forecasttest <- forecast(hw_modeltest, h = nrow(test_data))
mape(test_actuals, hw_forecasttest$mean)


sum(test_actuals)
sum(hw_forecasttest$mean)





