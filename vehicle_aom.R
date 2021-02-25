# # --- Steer Group: Data Analysis with R - Project 1 - Exploratory Data Analysis with the NHTS 2017 datasets --- #
# --- Date: 26-February-2021
# --- Team: 

# --- Part 1 - Importing and understanding of data --- #
# 1. Importing and understanding the data
# 2. Examples of data aggregations between multiple variables (summaries for different levels of categorical variables)
# 3. Cross-tabulations and frequency tests for chosen variables
# 4. Recoding values into new values (e.g. adding labels to numberic data) and converting character/categorical data into factors and ordered factors
# 5. Screening and dealing with missing or unusual values (e.g. extreme/outliers)

# ==== Importing data and understanding the data === #

library(readr)


getwd()
setwd("~/00 Steer R Training/Project 1/Data")

survey_vehicles <- read_csv("survey_vehicles.csv") #loading the dataset

head(survey_vehicles, 10)       # checking for the first values of the dataset
str(survey_vehicles)            # revising the structure of the dataset
dim(survey_vehicles)            # checking for dimensionality of the dataset 30 columns with  52,197 observations
names(survey_vehicles)          # checking the names of the columns (which can also be done with str)
sum(is.na(survey_vehicles))     # checking for NA values in the dataset. There are 104, 459! 
sapply(survey_vehicles, function (x) sum(is.na(x)))  # checking for NA values on each column. Mostly of the values come for the redacted parts of model and model_o. And some from hfuel_o

# ==== Recoding values and dealing with missing or unusual values === #







# ==== Data summaries, cross tabulations and frequency tests === #



# --- The end of Part 1 --- #



# --- Part 2 - Merging datasets and visualizations --- #
# 6. Efficient merging/joining datasets by common column(s)
# 7. Visualizing data using gglplot2 (editing parameters, applying labels, adding annotations, etc)
# 8. (extra) Visualizing data using interactive plots

# ==== Merging datasets === #



# ==== visualizations === #



# ==== Interactive visualizations === #



# --- The end of Part 2 --- #



