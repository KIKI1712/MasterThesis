## MasterThesis
Code used in my Master thesis with the title: "Demand Forecasting from sales data with the presence of stockouts".

The goal of the master thesis is to show how to do successful demand forecasting when you are facing a product that has frequent stockouts.
Stockouts can harm the forecast since they will give a false picture of the demand. Hence, regular forecasting techniques can not be applied, and various adjustments to the dataset are needed. Firstly, we need to approximate the demand during the stockout period, so we can then do the forecast on those adjusted data. I will test various methods on the sales data characterized by trend and seasonality, making the problem more complex.

For the purpose of this master thesis both Python and R are being used. 

Python is used in the first part to generate fake data which will have trend and seasonality. Furtherly, R is used for the approximation of demand during stockouts, forecasting, and error measurements. 

The dataset used in this master thesis can be found on my Kaggle profile: https://www.kaggle.com/datasets/kerimmasovic/fakesalesdata. This dataset is generated by Python and saved to Excel. 

This is how original data with stockouts looks like:

<img src="https://github.com/KIKI1712/MasterThesis/assets/82513917/85edbff3-ebcb-4eb2-8c9c-82f4e9cbba6a" width="600" height="600">

The generated sales data reflects an increasing trend over the years, alongside the presence of seasonality. The seasonality component specifically manifests as heightened sales during the summer months, spanning from June to August where sales are multiplied by 1.6, with a slight surge of 1.25 in May and September. The dataset captures monthly sales from the start of 2019 until the end of May 2023, providing sample data to account for trend and seasonality. Sales commence at a value of 30 on January 1, 2019, and conclude at 176 on June 30, 2023. Random stockout periods, totaling 25 periods where we stockouts are four to seven days long, were incorporated into the dataset using Python. The trend array represents a linear progression of numbers ranging from 0 to 1, and its length is determined by the number of dates generated. For the purpose of growth, the trend value is multiplied by 80. The sales figures are generated by summing the trend and the starting sales value (which is 30). Data is aggregated monthly for more readable display.

#### After applying the first sales approximation technique "Average of last known periods", we can see a significant change in data:
<img src="https://github.com/KIKI1712/MasterThesis/assets/82513917/bb5d7b8d-ea14-4d62-88f5-be3fc164ca96" width="600" height="600">

#### Second sales approximation technique "Interpolation": 
I have now applied two models of interpolation: Classic polynomial interpolation and Spline interpolation: Interestingly, they both yield similar results, mostly due to the high degree of polynomials, which was determined by the loop function that I created in R (you can see more in deterministic_interpolation.R file). A higher-degree polynomial can capture local variations in the data better, making it more similar to spline interpolation, which is “local” by nature, meaning that it is designed to capture local variations by fitting separate polynomials to different segments of the data.

<img src="https://github.com/KIKI1712/MasterThesis/assets/82513917/d71dafb3-bfc0-4aa9-ac27-2735a7e1b996" width="600" height="600">


#### Application of different imputation methods from ImputTS package in R 

<img src="https://github.com/KIKI1712/MasterThesis/assets/82513917/9ad5756c-6468-4f63-9525-0df54e9b9deb" width="600" height="600">



### This is how HoltWinters and SARIMA forecasting methods behaved when using data from different approximation methods  

<img src="https://github.com/KIKI1712/MasterThesis/assets/82513917/8d438806-b562-43fd-982e-3021fee08da9" width="600" height="600">


### Comparison of forecast accuracy between least and most accurate approximation technique

<img src="https://github.com/KIKI1712/MasterThesis/assets/82513917/1c1d9e91-cb4a-46bf-b013-54ba282e8db5" width="600" height="600">


In the practical part of this master's thesis, our objective was to demonstrate the process of effectively predicting demand based on sales data, particularly in instances of stockouts. We successfully implemented several approximation techniques and then we applied some forecasting techniques to those “fixed” data. All of them showed really good results.

Drawing conclusions from our research, the approximation techniques showcased remarkable performance, with all reporting an accuracy surpassing 90%. Some even reached an accuracy of approximately 97%. Two methods stood out notably: the Holt-Winters method (also termed as 'triple exponential smoothing') and the SARIMA model which was chosen by auto.arima function from R. Their efficacy was anticipated given the presence of both trend and seasonality in our data. Our research further showed that we can rely on selected approximation techniques for data characterized by seasonality and trend. Consequently, this led to precise forecasting when combined with apt methods such as SARIMA and Holt-Winters. We choose our approximation techniques based on this specific dataset and we choose only those that can handle seasonality patterns and trend. That is why every approximation techniques have similar result we do not have situations where some of the approximation techniques got completely false results.

A deeper dive into our techniques revealed that the Holt-Winters method had the highest accuracy, particularly in the forecasting part where there was no seasonality present. However, it did fall short in capturing data from June to July, where seasonality came into play. We attribute our high degree of success to a profound comprehension and thorough analysis of our data, enabling the accurate application of both forecasting and approximation techniques.

For businesses to achieve optimal demand forecasting, a comprehensive understanding of their data is crucial. Recognizing elements such as seasonality and when does it occur, trend magnitude, and similar is essential. For instance, while auto.arima displayed significant success, it was not as easy as just applying the code to the data. When deployed with our approximated data without specific frequency, it failed to capture seasonality, choosing a regular ARIMA model. Once we specified its frequency parameters, its performance dramatically improved.

