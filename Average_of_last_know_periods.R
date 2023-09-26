setwd("C:/Users/User/teza")

library(ggplot2)
library(readxl)
library(tidyverse)
library(dplyr)
sales_data <- read_xlsx("fake_sales_data.xlsx")


# Create a Month-Year column for grouping
sales_data$MonthYear <- format(sales_data$Date, "%b-%Y")

# Plot the graph
monthly_sales <- sales_data %>%
  mutate(MonthYear = format(Date, "%b-%Y")) %>%
  group_by(MonthYear) %>%
  summarise(Date = min(Date), TotalSales = sum(Sales)) %>%
  arrange(Date)

monthly_sales <- data.frame(monthly_sales)
monthly_sales$MonthYear <- as.Date(sales_data$Date)
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



####Approximation technique <- "Average of last know periods" 
library(ggplot2)
library(readxl)
library(tidyverse)
library(dplyr)
library(lubridate)
library(zoo)
# Load your data (replace 'fake_sales_data.xlsx' with your actual data file)
sales_data <- read_xlsx("fake_sales_data.xlsx")

# Create a binary column indicating stockouts (1 for stockout, 0 for no stockout)
sales_data$stockout <- ifelse(sales_data$Sales == 0, 1, 0)

# Define a function to calculate approximate demand for a given stockout period
calculate_approximate_demand <- function(data, stockout_index) {
  # Extract the date of the stockout
  stockout_date <- as.Date(data$Date[stockout_index])
  
  # Calculate the start date (15 days before the stockout)
  start_date <- stockout_date - days(15)
  
  # Calculate the end date (one day before the stockout)
  end_date <- stockout_date - days(1)
  
  # Filter data for non-stockout days within the last 15 days
  relevant_data <- data %>%
    filter(Date >= start_date & Date <= end_date & stockout == 0)
  
  # Calculate the average sales during these non-stockout days
  average_sales <- mean(relevant_data$Sales)
  
  # Adjust for seasonality based on the month and day of the stockout
  stockout_month <- format(stockout_date, "%m")
  stockout_day <- as.integer(format(stockout_date, "%d"))
  
  if (stockout_month == "05" && stockout_day >= 1 && stockout_day <= 10) {
    average_sales <- average_sales * 1.25
  } else if (stockout_month == "06" && stockout_day >= 1 && stockout_day <= 10) {
    average_sales <- average_sales * 1.28
  } else if (stockout_month == "09" && stockout_day >= 1 && stockout_day <= 10) {
    average_sales <- average_sales / 1.28
  } else if (stockout_month == "10" && stockout_day >= 1 && stockout_day <= 10) {
    average_sales <- average_sales / 1.25
  }
  
  return(average_sales)
}
#sales_data$Sales_adjusted <- sales_data$Sales
# Loop through each row of the data frame to calculate the approximate demand
for (i in 1:nrow(sales_data)) {
  if (sales_data$stockout[i] == 1) {
    sales_data$Sales[i] <- calculate_approximate_demand(sales_data, i)
  }
}

#library("writexl")
#write_xlsx(sales_data,"C:\\Users\\User\\Desktop\\TEZA\\data_from_R.xlsx")
#Round the Sales, since we can not sell half of an item :)
sales_data$Sales <- round(sales_data$Sales)

#remove stockout column since we do not need it anymore
sales_data$stockout <- NULL

#plot the the new data
# Create a Month-Year column for grouping
sales_data$MonthYear <- format(sales_data$Date, "%b-%Y")

# Plot the graph
monthly_sales <- sales_data %>%
  mutate(MonthYear = format(Date, "%b-%Y")) %>%
  group_by(MonthYear) %>%
  summarise(Date = min(Date), TotalSales = sum(Sales)) %>%
  arrange(Date)

monthly_sales <- data.frame(monthly_sales)
monthly_sales$MonthYear <- as.Date(sales_data$Date)
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

####2.separate data into train and test set
#load libraries 
library(ggplot2)
library(readxl)
library(tidyverse)
library(dplyr)
library(forecast)
# Convert the "Date" column to a Date object
sales_data$Date <- as.Date(sales_data$Date)


# Split the data
train_data <- subset(sales_data, Date < as.Date("2023-01-01"))
test_data <- subset(sales_data, Date >= as.Date("2023-01-01"))

# Create a time series object with a yearly seasonality (365 days)
ts_training_data <- ts(train_data$Sales, frequency = 365)
ggtsdisplay(ts_training_data)
