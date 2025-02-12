# Traffic Crash Analysis and Prediction

This script analyzes traffic crash data to explore factors contributing to serious accidents and predict crash frequency. The analysis involves categorizing crashes by injury severity and environmental conditions, such as weather and roadway surface. 

## Key Steps:

1. **Data Categorization**: The crash data is categorized into crash types based on the severity of injuries (e.g., serious crashes, bad weather conditions, and poor road conditions).
   
2. **Summary Dataset Creation**: A summary dataset is created by grouping crashes by date. The dataset calculates various metrics such as:
   - Number of serious crashes
   - Proportions of crashes under poor road and weather conditions
   - Temporal variables like day of the week and month

3. **Negative Binomial Regression Model**: A negative binomial regression model is built to predict the daily count of serious crashes using predictors such as road and weather conditions, day of the week, and month.

4. **Model Evaluation**: Model performance is evaluated using Root Mean Squared Error (RMSE). Predictions are compared to observed crash frequencies.

## Key Libraries:
- `tidyverse` for data manipulation and visualization
- `dplyr` for data cleaning and transformations
- `MASS` for building the negative binomial regression model

## Visualizations:
- **Observed vs Predicted Crashes**: The script includes a paired bar chart that compares the observed and predicted number of serious crashes across different days of the week.
- **First 7 Days Analysis**: A bar chart shows the observed vs predicted serious crashes for the first seven days in the dataset.
