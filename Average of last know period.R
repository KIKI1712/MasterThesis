setwd("C:/Users/User/teza")

library(ggplot2)
library(readxl)
library(tidyverse)
library(dplyr)
library(scales)
#sales_data <- read_xlsx("fake_sales_data.xlsx")
sales_data <- read_xlsx("fake_data_m5.xlsx")

# Create a Month-Year column for grouping
sales_data$MonthYear <- format(sales_data$Date, "%b-%Y")

#Graph
monthly_sales <- sales_data %>%
  mutate(MonthYear = format(Date, "%b-%Y")) %>%
  group_by(MonthYear) %>%
  summarise(Date = min(Date), TotalSales = sum(Sales)) %>%
  arrange(Date)

monthly_sales <- data.frame(monthly_sales)
#monthly_sales$MonthYear <- as.Date(monthly_sales$MonthYear)
monthly_sales$Date <- as.Date(monthly_sales$Date)
# Plot the graph with points

ggplot(monthly_sales, aes(x = Date, y = TotalSales)) +
  geom_line(aes(color = "Actual Sales"), size = 1, alpha = 0.8) +
  geom_point(color = "darkred", size = 3, alpha = 0.8) +
  geom_smooth(aes(color = "Smoothed Estimate"), linetype = "dashed", size = 1, alpha = 0.3, span = 0.3) + # Assign linetype within aes for legend
  scale_x_date(breaks = seq(min(monthly_sales$Date), max(monthly_sales$Date), by = "6 months")) +
  scale_y_continuous(limits = c(1000, 6000), breaks = seq(1000, 6000, by = 1000)) +
  labs(x = "Month", y = "Total Sales",
       color = "Legend", linetype = "Legend") + 
  theme_minimal() +
  theme(plot.title = element_text(size = 16, face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        legend.position = "right") 

# Aggregate stockout periods monthly
stockout_data <- sales_data %>%
  mutate(MonthYear = format(Date, "%b-%Y")) %>%
  group_by(MonthYear) %>%
  summarise(Date = min(Date), StockoutPeriods = sum(Sales == 0)) %>%
  arrange(Date)

stockout_data <- data.frame(stockout_data)
stockout_data$Date <- as.Date(stockout_data$Date)

# Merge the two dataframes based on Date
merged_data <- merge(monthly_sales, stockout_data, by = "Date", all = TRUE)

merged_data$NormalizedStockoutPeriod <- (merged_data$StockoutPeriod / 15) * (6000 - 1000) + 1000

#Now plot  
ggplot(merged_data, aes(x = Date)) +
  geom_line(aes(y = TotalSales, color = "Actual Sales"), size = 1.2, alpha = 1) +
  geom_point(aes(y = TotalSales), color = "darkred", size = 3) +
  #geom_smooth(aes(y = TotalSales,color = "Smoothed Estimate"), linetype = "dashed", size = 1, alpha = 0.3, span = 0.3) +
  geom_line(aes(y = NormalizedStockoutPeriod, color = "Stockout Periods"), size = 1, alpha = 0.6) +
  geom_point(aes(y = NormalizedStockoutPeriod), color = "lightblue", size = 2) +
  scale_x_date(breaks = seq(min(merged_data$Date), max(merged_data$Date), by = "6 months")) +
  scale_y_continuous(
    name = "Actual Sales", 
    limits = c(1000, 6000), 
    breaks = seq(1000, 6000, by = 1000),
    sec.axis = sec_axis(~ . / ((6000 - 1000) / 15) - 1000 / ((6000 - 1000) / 15), name = "Stockout Periods", breaks = seq(0, 20, by = 5))
  ) +
  labs(x = "Date", y = "Actual Sales", color = "Legend", linetype = "Legend") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    legend.position = "bottom"
  )
#Replace zeros with NA
sales_data$Sales[sales_data$Sales == 0] <- NA
sales_data$Date <- as.Date(sales_data$Date)
ggplot_na_distribution(sales_data$Sales)

####Approximation technique <- "Average of last know periods" 
library(ggplot2)
library(readxl)
library(tidyverse)
library(dplyr)
library(lubridate)
library(zoo)

#sales_data <- read_xlsx("fake_sales_data.xlsx")
sales_data <- read_xlsx("fake_data_m5.xlsx")
#reate a binary column indicating stockouts (1 for stockout, 0 for no stockout)
sales_data$stockout <- ifelse(sales_data$Sales == 0, 1, 0)

#Define a function to calculate approximate demand for a given stockout period
calculate_approximate_demand <- function(data, stockout_index) {
  #Extract the date of the stockout
  stockout_date <- as.Date(data$Date[stockout_index])
  
  #Calculate the start date (15 days before the stockout)
  start_date <- stockout_date - days(15)
  
  #Calculate the end date (one day before the stockout)
  end_date <- stockout_date - days(1)
  
  #Filter data for non-stockout days within the last 15 days
  relevant_data <- data %>%
    filter(Date >= start_date & Date <= end_date & stockout == 0)
  
  #Calculate the average sales during these non-stockout days
  average_sales <- mean(relevant_data$Sales)
  
  #Adjust for seasonality based on the month and day of the stockout
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
monthly_sales$Date <- as.Date(monthly_sales$Date)

monthly_sales <- monthly_sales[monthly_sales$Date < '2023-01-01',] 
# Plot the graph
ggplot(monthly_sales, aes(x = Date, y = TotalSales, group = 1)) +
  geom_line(color = "steelblue", size = 1, alpha = 0.8) +
  geom_point(color = "steelblue", size = 3, alpha = 0.8) +  
  geom_smooth(color = "steelblue", size = 1, alpha = 0.3, span = 0.3) +
  scale_x_date(breaks = seq(min(monthly_sales$Date), max(monthly_sales$Date), by = "6 months")) +  
  labs(x = "Monthly sales", y = "Total Sales", title = "Monthly Sales") +
  theme_minimal() +
  theme(plot.title = element_text(size = 16, face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1),   
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12))

merged_data <- merged_data[merged_data$Date < '2023-01-01',]

ggplot() +
  geom_line(data = monthly_sales, aes(x = Date, y = TotalSales, group = 1, color = "Sales with approximated stockouts"), size = 1, alpha = 0.8) +
  geom_point(data = monthly_sales, aes(x = Date, y = TotalSales, color = "Sales with approximated stockouts"), size = 3, alpha = 0.8) +
  geom_line(data = merged_data, aes(x = Date, y = TotalSales, group = 1, color = "Sales without approximated stockouts"), size = 1, alpha = 0.8) +
  scale_x_date(breaks = seq(min(monthly_sales$Date), max(monthly_sales$Date), by = "6 months")) +
  scale_color_manual(values = c("Sales with approximated stockouts" = "steelblue", "Sales without approximated stockouts" = "red")) +
  labs(x = "Date", y = "Actual Sales",
       color = "Legend") +
  theme_minimal() +
  theme(plot.title = element_text(size = 16, face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1),   
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        legend.position = "bottom")


