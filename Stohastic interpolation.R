library(ggplot2)
library(readxl)
library(tidyverse)
library(dplyr)
library(lubridate)
library(zoo)
library(forecast)
library(writexl)

# Install and load the imputTS package
#install.packages("imputeTS")
library(imputeTS)

#read data
sales_data <- read_xlsx("fake_sales_data.xlsx")

# Replace zeros with NA
sales_data$Sales[sales_data$Sales == 0] <- NA

#test
#sales_data$Sales <- na_interpolation(sales_data$Sales)

#ggplot_na_imputations(sales_data$Sales)

?na_seasplit()

# lets put it as a variable and time series variable
Sales <- sales_data$Sales
Sales_ts <- ts(Sales, frequency = 365)
summary(Sales_ts)
summary(Sales)

# 1. Missing Value Imputation by Weighted Moving Average
#imputed_ma <- na_ma(Sales, k=6, weighting = "exponential")
imputed_ma_ts <- na_ma(Sales_ts, k=6, weighting = "exponential")
summary(imputed_ma_ts)
summary(imputed_ma)

setwd("C:/Users/User/teza")
# 2. Missing Value Imputation by Kalman Smoothing with StructTS
#imputed_kalman_struct <- na_kalman(Sales, model = "StructTS")
imputed_kalman_struct_ts <- na_kalman(Sales_ts, model = "StructTS")
#make a format for excel, to save it since it is running for a long time
imputed_kalman_struct_ts_excel <- as.data.frame(imputed_kalman_struct_ts)
write_xlsx(imputed_kalman_struct_ts_excel,"C:\\Users\\User\\teza\\imputed_kalman_struct_ts.xlsx")
#read it from excel
imputed_kalman_struct_ts <- read_xlsx("imputed_kalman_struct_ts.xlsx")
#make it time series
imputed_kalman_struct_ts <- ts(imputed_kalman_struct_ts$x)
#summary(imputed_kalman_struct_ts)

# 3. Missing Value Imputation by Kalman Smoothing with auto.arima
#imputed_kalman_arima <- na_kalman(Sales, model = "auto.arima")
imputed_kalman_arima_ts <- na_kalman(Sales_ts, model = "auto.arima")
#make a format for excel, to save it since it is running for a long time
imputed_kalman_arima_ts_excel <- as.data.frame(imputed_kalman_arima_ts)
write_xlsx(imputed_kalman_arima_ts_excel,"C:\\Users\\User\\teza\\imputed_kalman_arima_ts.xlsx")
#read it from excel
imputed_kalman_arima_ts <- read_xlsx("imputed_kalman_arima_ts.xlsx")
#make it time-series
imputed_kalman_arima_ts <- ts(imputed_kalman_arima_ts$x)
#summary(imputed_kalman_arima_ts)

# 4. Seasonally Decomposed Missing Value Imputation with na_ma
imputed_seadec_ma <- na_seadec(Sales_ts,find_frequency = TRUE, algorithm = "ma", k=6, weighting = "exponential")

# 5. Seasonally Decomposed Missing Value Imputation with Kalman StructTS
#imputed_seadec_kalman_struct <- na_seadec(Sales_ts, algorithm = "kalman", model = "StructTS")
#make a format for excel, to save it since it is running for a long time
imputed_seadec_kalman_excel <- as.data.frame(imputed_seadec_kalman_struct)
write_xlsx(imputed_seadec_kalman_excel,"C:\\Users\\User\\Desktop\\TEZA\\imputed_seadec_kalman_struct.xlsx")
#read it from excel
imputed_seadec_kalman_struct <- read_xlsx("imputed_seadec_kalman_struct.xlsx")
#make it time-series variable
imputed_seadec_kalman_struct <- imputed_seadec_kalman_struct$x
imputed_seadec_kalman_struct <- ts(imputed_seadec_kalman_struct)

# 6. Seasonally Decomposed Missing Value Imputation with Kalman auto.arima
imputed_seadec_kalman_arima <- na_seadec(Sales_ts, find_frequency = TRUE, algorithm = "kalman", model = "auto.arima")
#make a format for excel, to save it since it is running for a long time
imputed_seadec_kalmanarima_excel <- as.data.frame(imputed_seadec_kalman_arima)
write_xlsx(imputed_seadec_kalmanarima_excel,"C:\\Users\\User\\Desktop\\TEZA\\imputed_seadec_kalman_arima.xlsx")

# 7. Seasonally Decomposed Missing Value Imputation with Interpolation
imputed_seadec_interpolation <- na_seadec(Sales_ts,find_frequency = TRUE, algorithm = "interpolation", option = "spline")


#Plotting the graph 
Date <- sales_data$Date



data_for_plot <- data.frame(
  Date = Date, 
  sales_with_na = ts(sales_data$Sales),
  imputed_ma_ts = round(imputed_ma_ts),
  imputed_kalman_struct_ts = round(imputed_kalman_struct_ts),
  imputed_kalman_arima_ts = round(imputed_kalman_arima_ts),
  imputed_seadec_ma = round(imputed_seadec_ma),
  imputed_seadec_kalman_struct = round(imputed_seadec_kalman_struct),
  imputed_seadec_kalman_arima = round(imputed_seadec_kalman_arima),
  imputed_seadec_interpolation = round(imputed_seadec_interpolation)
)

# Convert Date to a Date object, if it's not already
data_for_plot$Date <- as.Date(data_for_plot$Date)

write_xlsx(data_for_plot,"C:\\Users\\User\\Desktop\\TEZA\\data_interpolated_combinedALL.xlsx")

# Create a data frame with only the interpolated values 
data_interpolated <- data_for_plot %>%
  filter(is.na(sales_with_na)) %>%
  select(Date, starts_with("imputed"))

write_xlsx(data_interpolated,"C:\\Users\\User\\Desktop\\TEZA\\data_interpolated.xlsx")



ggplot(data_interpolated, aes(x = Date)) +
  geom_line(aes(y = imputed_ma_ts, color = "MA TS")) +
  geom_line(aes(y = imputed_kalman_struct_ts, color = "Kalman Struct TS")) +
  geom_line(aes(y = imputed_kalman_arima_ts, color = "Kalman ARIMA TS")) +
  geom_line(aes(y = imputed_seadec_ma, color = "Seadec MA")) +
  geom_line(aes(y = imputed_seadec_kalman_struct, color = "Seadec Kalman Struct")) +
  geom_line(aes(y = imputed_seadec_kalman_arima, color = "Seadec Kalman ARIMA")) +
  geom_line(aes(y = imputed_seadec_interpolation, color = "Seadec Interpolation")) +
  theme_minimal() +
  labs(color = "Method") + ylab("Sales") + ggtitle("Comparison of different interpolation techniques")
  theme(legend.position="bottom")










###ako budem koristio








# Group by Month and Year
monthly_data <- data_for_plot %>%
  mutate(MonthYear = format(Date, "%b-%Y")) %>%
  group_by(MonthYear) %>%
  summarise(
    Date = min(Date),
    Monthly_MA_TS = sum(imputed_ma_ts),
    Monthly_Kalman_Struct_TS = sum(imputed_kalman_struct_ts),
    Monthly_Kalman_ARIMA_TS = sum(imputed_kalman_arima_ts),
    Monthly_Seadec_MA = sum(imputed_seadec_ma),
    Monthly_Seadec_Kalman_Struct = sum(imputed_seadec_kalman_struct),
    Monthly_Seadec_Kalman_ARIMA = sum(imputed_seadec_kalman_arima),
    Monthly_Seadec_Interpolation = sum(imputed_seadec_interpolation)
  ) %>%
  arrange(Date)

p <- ggplot(monthly_data, aes(x = Date)) +
  geom_line(aes(y = Monthly_MA_TS, color = "MA TS")) +
  geom_line(aes(y = Monthly_Kalman_Struct_TS, color = "Kalman Struct TS")) +
  geom_line(aes(y = Monthly_Kalman_ARIMA_TS, color = "Kalman ARIMA TS")) +
  geom_line(aes(y = Monthly_Seadec_MA, color = "Seadec MA")) +
  geom_line(aes(y = Monthly_Seadec_Kalman_Struct, color = "Seadec Kalman Struct")) +
  geom_line(aes(y = Monthly_Seadec_Kalman_ARIMA, color = "Seadec Kalman ARIMA")) +
  geom_line(aes(y = Monthly_Seadec_Interpolation, color = "Seadec Interpolation")) +
  theme_minimal() +
  labs(color = "Method") +
  theme(legend.position="bottom")

print(p)
