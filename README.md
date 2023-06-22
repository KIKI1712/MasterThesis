## MasterThesis
Code used in my Master thesis with the title: "Demand Forecasting from sales data with the presence of stockouts".

The goal of the master thesis is to show how to do successful demand forecasting when you are facing a product that has frequent stockouts.
Stockouts can harm the forecast since they will give a false picture of the demand. Hence, regular forecasting techniques can not be applied, and various adjustments to the dataset are needed. Firstly, we need to approximate the demand during the stockout period, so we can then do the forecast on those adjusted data. I will test various methods on the sales data characterized by trend and seasonality, making the problem more complex.

For the purpose of this master thesis both Python and R are being used. 

Python is used in the first part to generate fake data which will have trend and seasonality. Furtherly, R is used for the approximation of demand during stockouts, forecasting, and error measurements. 
