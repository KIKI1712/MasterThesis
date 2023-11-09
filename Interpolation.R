setwd("C:/Users/User/teza")
###SPLINE INTERPOLATION
library(ggplot2)
library(readxl)
library(tidyverse)
library(dplyr)
library(lubridate)
library(zoo)
library(forecast)
sales_data_spline <- read_xlsx("fake_sales_data.xlsx")

# Replace zeros with NA
sales_data_spline$Sales[sales_data_spline$Sales == 0] <- NA

# Use na.spline() to fill in the missing values using spline interpolation
sales_data_spline$Sales <- na.spline(ts(sales_data_spline$Sales), na.rm = FALSE)

#round data 
sales_data_spline$Sales <- round(sales_data_spline$Sales)

print(sales_data_spline)

# Plot the graph
monthly_sales <- sales_data_spline %>%
  mutate(MonthYear = format(Date, "%b-%Y")) %>%
  group_by(MonthYear) %>%
  summarise(Date = min(Date), TotalSales = sum(Sales)) %>%
  arrange(Date)

monthly_sales <- data.frame(monthly_sales)
monthly_sales$Date <- as.Date(sales_data_spline$Date)
# Plot the graph
ggplot(monthly_sales, aes(x = Date, y = TotalSales, group = 1)) +
  geom_line(color = "steelblue", size = 1, alpha = 0.8) +
  geom_smooth(color = "steelblue", size = 1, alpha = 0.3, span = 0.3) +
  #scale_x_discrete(breaks = monthly_sales$MonthYear[seq(1, nrow(monthly_sales), 3)]) +
  labs(x = "Monthly sales", y = "Total Sales", title = "Monthly Sales") +
  theme_minimal() +
  theme(plot.title = element_text(size = 16, face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12))


###POLYNOMIAL INTERPOLATION
library(ggplot2)
library(readxl)
library(tidyverse)
library(dplyr)
library(lubridate)
library(zoo)
library(forecast)
library()
sales_data_polynomial <- read_xlsx("fake_sales_data.xlsx")

# Replace zeros with NA
sales_data_polynomial$Sales[sales_data_polynomial$Sales == 0] <- NA

# Create a new column Days which will be used for the interpolation (used as predictor variable in the model)
sales_data_polynomial$Days <- 1:nrow(sales_data_polynomial)
#convert to time series
sales_data_polynomial$Sales <- ts(sales_data_polynomial$Sales)
#Firstly, we need to decide on degree of polynomial which we will use. We do it in following way:
# Initialize variables to store the results
best_degree <- 0
best_aic <- Inf

# Try different degrees of the polynomial
for (degree in 1:27) {
  # Fit the model
  model <- lm(Sales ~ poly(Days, degree), data = sales_data_polynomial, na.action = na.exclude)
  
  # Calculate the AIC
  aic <- AIC(model)
  
  # Update the best degree and AIC if necessary
  if (aic < best_aic) {
    best_degree <- degree
    best_aic <- aic
  }
}
print(paste("Best Degree:", best_degree))
print(paste("Best AIC:", best_aic))
#Best polynomial for our example is 26

# Fit a polynomial regression model to the non-NA data
model <- lm(Sales ~ poly(Days, 26), data = sales_data_polynomial, na.action = na.exclude)

# Predict the sales using the fitted model
sales_data_polynomial$Predicted_Sales <- predict(model, sales_data_polynomial)

# Replace the NA values in the Sales column with the predicted values
sales_data_polynomial$Sales[is.na(sales_data_polynomial$Sales)] <- sales_data_polynomial$Predicted_Sales[is.na(sales_data_polynomial$Sales)]

# Remove the Predicted_Sales column
sales_data_polynomial$Predicted_Sales <- NULL

#round Data
sales_data_polynomial$Sales <- round(sales_data_polynomial$Sales)
print(sales_data_polynomial)


## Compare the results
sales_data_polynomial$Days <- NULL

# Create a new data frame that combines both 'sales_data_polynomial' and 'sales_data_spline'
#add column with missing values
sales_data_withNA <- read_xlsx("fake_sales_data.xlsx")
# Replace zeros with NA
sales_data_withNA$Sales[sales_data_withNA$Sales == 0] <- NA

#Plotting the graph 
Date <- sales_data_withNA$Date


data_for_plot_deterministicINT <- data.frame(
  Date = Date, 
  sales_data_withNA = ts(sales_data_withNA$Sales),
  polynomial_interpolation = ts(sales_data_polynomial$Sales),
  spline_interpolation = ts(sales_data_spline$Sales)
)

# Convert Date to a Date object, if it's not already
data_for_plot_deterministicINT$Date <- as.Date(data_for_plot_deterministicINT$Date)


# Create a data frame with only the interpolated values 
data_only_interpolated <- data_for_plot_deterministicINT %>%
  filter(is.na(sales_data_withNA)) 

ggplot(data_only_interpolated, aes(x = Date)) +
  geom_line(aes(y = polynomial_interpolation, color = "Polynomial Interpolation")) +
  geom_line(aes(y = spline_interpolation, color = "Cubic Spline Interpolation")) +
  theme_minimal() +
  labs(color = "Method") + ylab("Sales") + ggtitle("Polynomial vs. Cubic Spline interpolation")
theme(legend.position="bottom")


#### another way to plot
sales_data_polynomial$Days <- NULL

# Create a new data frame that combines both 'sales_data_polynomial' and 'sales_data_spline'
combined_data <- rbind(
  cbind(sales_data_polynomial, Source = "sales_data_polynomial"),
  cbind(sales_data_spline, Source = "sales_data_spline")
)
monthly_sales_interpolation <- combined_data %>%
  mutate(MonthYear = format(Date, "%b-%Y")) %>%
  group_by(MonthYear, Source) %>%
  summarise(Date = min(Date), TotalSales = sum(Sales)) %>%
  arrange(Date)

ggplot(monthly_sales_interpolation, aes(x = Date, y = TotalSales, group = Source, color = Source)) +
  geom_line(size = 1, alpha = 1) +
  geom_smooth(span = 0.0001) +
  #scale_x_discrete(breaks = monthly_sales$MonthYear[seq(1, nrow(monthly_sales), 3)]) +
  labs(x = "Monthly sales", y = "Total Sales", title = "Monthly Sales") +
  theme_minimal() +
  theme(plot.title = element_text(size = 16, face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12)) 


#library("writexl")
#write_xlsx(combined_data,"C:\\Users\\User\\Desktop\\TEZA\\interp_combined_data2.xlsx")



