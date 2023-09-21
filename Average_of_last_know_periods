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
sales_data$Sales2 <- sales_data$Sales
# Loop through each row of the data frame to calculate the approximate demand
for (i in 1:nrow(sales_data)) {
  if (sales_data$stockout[i] == 1) {
    sales_data$Sales2[i] <- calculate_approximate_demand(sales_data, i)
  }
}
