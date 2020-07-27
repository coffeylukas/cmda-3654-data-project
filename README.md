# CMDA 3654 Data Analysis & Machine Learning Project
### By Lukas Coffey
Using the statistical programming language R, I analyzed a dataset of sales transactions for an online superstore. I performed exploratory data analysis and found a possible logistical area for improvement. Using unsupervised clustering methods I isolated the busiest times of the year and used location data to pinpoint the best locations for product distribution centers.

## Project Structure
The data for this project, original and cleaned, is in the `Data` folder. 

## Project Details
For this project, I took zip codes from the original sales data and joined the dataset with a list of latitudes and longitudes. Using those coordinates I was able to plot the data points across the U.S. and get a sense of the types of products being sold and shipped in different locations. Using different clustering methods and factors I pinpointed locations for distribution centers that made the most sense. 

## Data Cleaning and EDA
I performed the data cleaning and EDA in `analytics_sales.R`. I performed some simple linear regression and logistic regression and made a few line plots to explore the data.

## Clustering 
The clustering was performed in the `Coffey_Lukas_Final_Project_Report.Rmd` file.