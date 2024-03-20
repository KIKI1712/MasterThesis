#first graph

# Data
Week <- c(1:13)
Sales <- c(39, 41, 43, 40, 38, 39, 40, 42, 44, 46, NA, NA, NA)
Forecast_DES <- c(NA, 41, 43, 45, 45.2, 44.308, 43.66512, 43.2951968, 43.55853875, 44.36936579, 45.63478277, 46.41100949, 47.18723621)
Forecast_SES <- c(39, 39, 39.4, 40.12, 40.096, 39.6768, 39.54144, 39.633152, 40.1065216, 40.88521728, 40.88521728, 40.88521728, 40.88521728)
Forecast_DES_dumped_trend <- c(NA, 41, 42.344, 43.671488, 43.26236058, 41.96091428, 41.16432561, 40.84448879, 41.29417538, 42.34222893, 43.82817819, 44.25203607, 44.67589394)

# Plot
plot(Week, Sales, type = "o", pch = 16, ylim = c(35, 55), xlab = "Week", ylab = "Sales", main = "Sales and Forecasts")
lines(Week, Forecast_DES, type = "o", pch = 16, col = "blue")
lines(Week, Forecast_SES, type = "o", pch = 16, col = "red")
lines(Week, Forecast_DES_dumped_trend, type = "o", pch = 16, col = "orange")
legend("topleft", legend = c("Sales", "Forecast - DES", "Forecast - SES", "Forecast - DES with dumped trend"),
       col = c("black", "blue", "red", "orange"), lty = 1, pch = 16)


ggplot(df) + aes(x=Week, y=Sales) + geom_line(aes(Week, Sales, color = "Sales"), linetype = "solid", size = 1) +
  geom_line(aes(Week, Forecast_DES, color = "Forecast - DES"), linetype = "dotted", size = 1) +
  geom_line(aes(Week, Forecast_SES, color = "Forecast - SES"), linetype = "dotted", size = 1) +
  geom_line(aes(Week, Forecast_DES_dumped_trend, color = "Forecast - DES with dumped trend"), linetype = "dotted", size = 1) + 
  scale_color_manual(values = c("orange", "blue", "red", "black")) +
  theme_minimal() + ylim(35,50)

###########################


# Load required libraries
library(tidyverse)
library(scales)
library(lubridate)
library(forecast)

sales <- data.frame(
  Date = as.Date(seq(from = as.Date("2020-01-01"), to = as.Date("2022-12-01"), by = "month")),
  Sales = c(
    100, 110, 120, 130, 140, 150, 180, 200, 170, 150, 130, 120,
    110, 120, 130, 140, 150, 160, 190, 210, 180, 160, 140, 130,
    120, 130, 140, 150, 160, 170, 200, 220, 190, 170, 150, 140
  )
)

sales %>% 
  ggplot() +
  aes(Date, Sales) +
  geom_line() +
  xlab("Month") +
  ylab("Sales") +
  scale_x_date(labels = date_format(format = "%m-%Y"), breaks = "3 months") +
  geom_smooth(n = 10000)
  ggtitle("Observing trend and seasonality")

auto.arima(sales_ts)

###SARIMA
# Load the required packages
library(forecast)

# Convert the data to a time series object
sales_ts <- ts(sales$Sales, frequency = 12, start = c(2020, 1))

# Fit the SARIMA model
sarima_model <- arima(sales_ts, order = c(1, 1, 1), seasonal = list(order = c(1, 1, 1), period = 2))

summary(sarima_model)


#forecasting with sarima
model <- arima(sales_ts, order = c(1, 1, 1), seasonal = list(order = c(1, 1, 1), period = 2))

model %>% forecast(h=12) %>% autoplot()

###regression
advertising <- data.frame(
  Advertising = c(10, 15, 20, 25, 30),
  Sales = c(15, 20, 22, 28, 33)
) 
  model <- lm(Sales ~ Advertising, data = advertising)
  
ggplot(advertising) + aes(x=Advertising, y=Sales) + geom_line()
summary(model)

new_data <- data.frame(Advertising = 50)
predicted_sales <- predict(model, newdata = new_data)

#################
# Generate sample data
salary <- c(50000, 60000, 70000, 55000, 65000, 75000, 60000, 70000, 80000, 90000)
experience <- c(1, 2, 3, 2, 4, 5, 3, 4, 5, 6)

# Create a data frame
data <- data.frame(salary, experience)

# Plot the scatter plot
plot(data$experience, data$salary, 
     xlab = "Experience", ylab = "Salary",
     main = "Scatter Plot of Salary vs Experience",
     pch = 16, col = "blue")

# Add a regression line
abline(lm(salary ~ experience, data = data), col = "red")


###


# Fit a simple linear regression model
model <- lm(salary ~ experience)

# Extract the coefficient for experience
slope <- coef(model)["experience"]

# Calculate the expected salary increase for a specific increase in experience
increase_in_experience <- 5  # Example: 5-year increase
expected_salary_increase <- increase_in_experience * slope

# Print the expected salary increase
expected_salary_increase

summary(model)
####### multiple regressino
# Example data and fitted model
sales <- c(1000, 1200, 1300, 1100, 1400) # Historical sales data
independent_vars <- data.frame(var1 = c(10, 12, 13, 11, 14), var2 = c(20, 25, 30, 22, 28)) # Independent variables
model <- lm(sales ~ ., data = independent_vars) # Fitted regression model

# Obtain regression coefficients
coefficients <- coef(model)

predicted_sales <- predict(model, newdata = coefficients)


# Create electricity and temperature variables
electricity <- c(70, 65, 60, 45, 43, 45, 54, 55, 60, 65, 70, 75, 80, 75, 70)
temperature <- c(40, 35, 30, 25, 20, 15, 10, 5, 0, -5, -10, -15, -20, -25, -30)

data <- data.frame(x=electricity, y = temperature)

library(ggplot2)

ggplot(data) + aes(x= temperature, y= electricity) +geom_smooth() + ggtitle('Non-linear relationship')

###shortages

# Load required libraries
library(ggplot2)

# Set seed for reproducibility
set.seed(123)

# Generate sales data for 100 days
days <- 100
sales <- rnorm(days, mean = 50, sd = 10)

# Create stockout gaps
num_gaps <- 3
gap_length <- 10

# Generate random indices for stockout gaps
gap_indices <- sort(sample(1:(days - gap_length), num_gaps))

# Set sales to 0 for stockout gaps
for (i in 1:num_gaps) {
  sales[gap_indices[i]:(gap_indices[i] + gap_length - 1)] <- 0
}

# Create a data frame with the sales data
sales_data <- data.frame(Day = 1:days, Sales = sales)

# Plot the sales curve
ggplot(sales_data, aes(x = Day, y = Sales)) +
  geom_line() +
  labs(x = "Day", y = "Sales") +
  ggtitle("Sales Curve") +
  theme_minimal()

####
library(forecast)

# Convert sales data to a time series object
sales_ts <- ts(sales_data$Sales)

# Apply exponential smoothing and forecast for the next 10 days
forecast_result <- forecast(auto.arima(sales_ts), h = 10)

# Print the forecasted values
print(forecast_result)

# Plot the sales data and forecasted values
plot(forecast_result, main = "Sales Forecast")


#
#install.packages("tsintermittent")
library(tsintermittent)
library(readxl)

data <- read_xlsx('demand vs sales.xlsx')

data$Sales <- replace(data$Sales, is.na(data$Sales), 0)

library(ggplot2)
library(forecast)

# Convert the "Period" column to a Date type
data$Period <- as.Date(data$Period, format = "%m.%Y")

# Create a time series object for the "demand" column
demand_ts <- ts(data$demand, start = c(2019, 1), frequency = 12)

# Create a time series object for the "sales" column
sales_ts <- ts(data$Sales, start = c(2019, 1), frequency = 12)

# Perform the demand forecasting based on the "demand" column
#demand_arima <- Arima(demand_ts, order=c(3,1,1))
demand_ets <- hw(demand_ts, alpha = 0.6)
demand_forecast <- forecast(demand_ets, h = 12)
summary(forecast(demand_ets, h = 12))
autoplot(forecast(demand_ets, h = 12))
# Perform the demand forecasting based on the "sales" column
sales_ets <-hw(sales_ts, damped = TRUE, seasonal = "additive")
sales_forecast <- forecast(sales_ets, h = 12)
summary(forecast(sales_ets, h = 12))
autoplot(forecast(sales_ets, h = 12))
# Convert the forecasted values to a data frame
forecast_df <- data.frame(
  Period = seq(as.Date("2023-06-01"), by = "month", length.out = 12),
  demand = as.vector(demand_forecast),
  Sales = as.vector(sales_forecast)
)
forecast_df$Period <- as.Date(forecast_df$Period, format = "%m.%Y")

# Plot the combined data
ggplot(data) + 
  geom_line(aes(x = Period, y = Sales, colour = "Sales"), size = 0.8) + 
  geom_point(aes(x = Period, y = Sales, colour = "Sales"), size = 1) + 
  geom_line(aes(x = Period, y = demand, colour = "Demand")) +
  geom_point(aes(x = Period, y = demand, colour = "Demand"), size = 1) +
  geom_smooth(aes(x=Period, y=demand.Point.Forecast, colour = "Forecast by demand"), data = forecast_df, stat =  "identity") +
  geom_smooth(aes(x=Period, y=Sales.Point.Forecast, colour = "Forecast by sales"), data = forecast_df, stat =  "identity") +
  geom_vline(xintercept = as.Date("2023-05-01"), linetype="dashed", color = "grey50") +
  scale_colour_manual("", 
                      breaks = c("Sales", "Demand", "Forecast by demand", "Forecast by sales"),
                      values = c("#ff0033", "blue", "dark blue", "dark red")) +
  labs(title = "Demand Forecast for next 5 Months",
       x = "Period (Monthly Data)",
       y = "Demand/Sales") +
  scale_x_date(date_labels = "%m.%Y", date_breaks = "6 month") +
  expand_limits(y=0) +
  theme()

#######
#install.packages("zoo")
library(zoo)

data1$Sales <- replace(data1$Sales == 0, NA)
# Perform interpolation
data1$sales_interpolated <- na.approx(data1$Sales)

# Print the interpolated data
print(data_interplation)


