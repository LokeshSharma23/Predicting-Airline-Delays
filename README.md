# Predicting-Airline-Delays

Predicting Airline arrival and departure delay using Machine Learning



Summary and Motivation: 
Flight delays are a major problem for individual customers and also for the airlines and the US economy in general.
In 2016, there has been a 17% delay in the arrival of flights and 18% delay in the departure of flights consequently causing in 78% propagational delays leading to a major loss in US economy.



Highlights:
In our project we are trying to understand the causes for delay and expected delays for a given flight in the future. Our project deals with the following questions:

Exploratory Data Analysis:
EDA was done to find the answers to the following questions:
1. What is the percentage of Arrival Delay out of all flights?
2. What is the percentage of Departure Delay out of all flights?
3. What is the percentage of Propagation Delay out of all flights?
4. Which airline is best or worst?
5. Which airport is best or worst?
6. Which route is best or worst?
7. Which airport has most weather delays?
8. Which airline has most carrier delays?
9. Which airline had most late aircraft delays?


Model Building: 
1. Whether you flight will encounter delay or not based on various parameters such as Time of day, Day of Week, Month, etc.?
2. How much delay will occur quantitatively for a given flight?


Data Sets:
The official flight database for every domestic flight in the US, using 2016 data. 
http://www.transtats.bts.gov/

Cleaned data and R code can be found at:
https://drive.google.com/drive/folders/1DlqE5DgZ22W4h7Ma_snJ2907otgsC_ZO?usp=sharing



Softwares Used:
R, Palmetto Cluster (Clemson University Supercomputer), Tableau and MS Excel


Model Building Steps
1. Loading the monthly US Airline data from US DOT and saving it in the file name ‘data_16’.
2. Cleaning the data and omitting NA values.
3. Performing data wrangling on the features and converting them into categorial variables.
4. Splitting the data into training set and testing set.
5. Filtering the data and choosing the two busiest airports of 2016 i.e. Hartsfield–Jackson Atlanta International Airport and Los Angeles International Airport. 
6. Performed Principal Component Analysis for feature selection and reduced the number of features from 64 to 20.
7. Build three classification models to categorize flights as Delay or Non-Delay. Models used: Logistics Regression, Random Forest and Support Vector Machine.
8. Build two regression models to quantify the amount of delay for a given flight. Models used: Ordinary Least Square and Support Vector Regression.

Classification and Regression Results:
Random Forest was the best model for classification with Average Precision of 92.5%, Average Accuracy of 87.76%, Average Recall of 94%.
Support Vector Regression was the best model for quantitative prediction based on Residual Standard Error with RMSE for Arrival Delay : 4.22 minutes and RMSE for Departure Delay: 3.63 minutes.

Exploratory Data Analysis Results:
1. Best Airline: Hawaiian Airlines
2. Worst Airline: Frontier Airlines
3. Best Arrival Airport: Hilo International Airport, Hawaii
4. Worst Arrival Airport: Trenton–Mercer Airport, New Jersey
5. Best Departure Airport: Hilo International Airport, Hawaii
6. Worst Departure Airport: Laredo International Airport, Texas
7. Worst Weather Delay: Adak Airport, Alaska
8. Worst Carrier Delay: Jet Blue Airways
9. Worst Late Aircraft Delays: Jet Blue Airways

