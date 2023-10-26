# import libraries
from faker import Faker
import pandas as pd
import numpy as np

fake = Faker()

# Set random seed for reproducibility
np.random.seed(0)

# Generate fake sales data with seasonality and increasing trend
start_date = pd.to_datetime('2019-01-01')
end_date = pd.to_datetime('2023-06-30')
dates = pd.date_range(start=start_date, end=end_date, freq='D')

# Generate sales values with seasonality and increasing trend
trend = np.linspace(0, 1, len(dates))
starting_sales = 30

# Calculate sales as a combination of trend, seasonality, and starting sales
sales = (trend * 80 + starting_sales).round(0) 

# Introduce gaps of 6 to 10 days with zero sales for 25 periods (stockouts)
gap_periods = 25
gap_length = np.random.randint(4, 7, gap_periods)
gap_start_indices = np.random.choice(np.where(dates.year != 2023)[0], gap_periods, replace=False)

for i in range(gap_periods):
    gap_start = gap_start_indices[i]
    gap_end = gap_start + gap_length[i]
    sales[gap_start:gap_end] = 0

# Multiply sales by a scaling factor for higher sales during summer months
summer_months = (dates.month >= 6) & (dates.month <= 8)
sales[summer_months] *= 1.6  # Adjust the scaling factor as needed

# Multiply sales by a scaling factor for higher sales during almost-summer months
almost_summer_months = (dates.month == 5) | (dates.month == 9)
sales[almost_summer_months] *= 1.25  # Adjust the scaling factor as needed

# Create a DataFrame with generated data
df = pd.DataFrame({'Date': dates, 'Sales': sales.round(0), 'Customer Name': [fake.name() for _ in range(len(dates))]})

# Display the DataFrame
print(df.head())

# Get the data to excel
df.to_excel('fake_data_m5.xlsx', index=False)
