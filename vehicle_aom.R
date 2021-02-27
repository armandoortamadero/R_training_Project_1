# # --- Steer Group: Data Analysis with R - Project 1 - Exploratory Data Analysis with the NHTS 2017 datasets --- #
# --- Date: 26-February-2021
# --- Team: 

# --- Part 1 - Importing and understanding of data --- #
# 1. Importing and understanding the data
# 2. Examples of data aggregations between multiple variables (summaries for different levels of categorical variables)
# 3. Cross-tabulations and frequency tests for chosen variables
# 4. Recoding values into new values (e.g. adding labels to numberic data) and converting character/categorical data into factors and ordered factors
# 5. Screening and dealing with missing or unusual values (e.g. extreme/outliers)

# ==== Importing and understanding the data === #

library(readr)
library(readxl)
library(dplyr)
library (psych)
library(Hmisc)
library(summarytools)
library(dplyr)
library(ggplot2)
library(RColorBrewer)

getwd()
setwd("~/00 Steer R Training/Project 1/Data")

survey_vehicles <- read_csv("survey_vehicles.csv") #loading the dataset

head(survey_vehicles, 10)       # checking for the first values of the dataset
str(survey_vehicles)            # revising the structure of the dataset
dim(survey_vehicles)            # checking for dimensionality of the dataset 30 columns with  52,197 observations
names(survey_vehicles)          # checking the names of the columns (which can also be done with str)
sum(is.na(survey_vehicles))     # checking for NA values in the dataset. There are 104, 459! 
sapply(survey_vehicles, function (x) sum(is.na(x)))  # checking for NA values on each column. Mostly of the values come for the redacted parts of model and model_o. And some from hfuel_o

excel_sheets("thsc-nhts17-caltrans-codebook.xlsx") #exploring the codebook sheets
NHTS_codebook <- read_excel("thsc-nhts17-caltrans-codebook.xlsx", sheet = 2, col_names = TRUE) # loading values from the codebook
str(NHTS_codebook)
names(NHTS_codebook)
View(NHTS_codebook)
unique(NHTS_codebook$TABLE)

NHTS_codebook_vehicle <- NHTS_codebook %>% filter(TABLE=="VEHICLE") # extracting the values related to the Vehicle survey


# ==== Data summaries, cross tabulations and frequency tests === #

#Estimate the top 10 most popular car makers
top10 <- survey_vehicles %>%
  group_by(make) %>%
  summarise(N = n()) %>%
  arrange(-N) %>%
  top_n(10) 
top10

#Extract the column make to use in further filtering
top10 <- pull(top10,make)  
str(top10)
 
#Create a subset of the survey to consider only the top 10 vehicles
survey_vehicles_top10 <- survey_vehicles %>% filter(make %in% top10) 
str(survey_vehicles_top10)

#Estimating the average age of the top 10 vehicle models
avgVehAge <- survey_vehicles_top10 %>% 
  group_by(make) %>%
  summarise(N = n(), 
            avgAge = round(mean(vehage, na.rm = TRUE), 2)) %>% 
            arrange (-N) %>%
            as.data.frame()
avgVehAge

#Extract the names 

#transform the VALUE column of the codebook to allow for other matchings with other datasets 
#(and avoid problems of a character value 07 vs 7 in numeric on the surveys dataset)
NHTS_codebook_vehicle$VALUE <- as.numeric(NHTS_codebook_vehicle$VALUE)
NHTS_codebook_vehicle <- NHTS_codebook_vehicle %>% filter (NAME != "MODEL")


#extraction of the names of the top 10 vehicles from codebook
top10_veh_names <-NHTS_codebook_vehicle %>% filter(NAME == "MAKE" & VALUE %in% top10)
top10_veh_names

#create a new column in the dataset to create a factor
survey_vehicles_top10 <- survey_vehicles_top10 %>% mutate (survey_vehicles_top10, make_labelled=make)

#factor conversion 
survey_vehicles_top10$make_labelled <-factor(survey_vehicles_top10$make_labelled, levels= top10,
                                    labels = c("Toyota","Ford", "Honda","Chevrolet","Nissan","Dodge","Subaru","BMW","Lexus","Volkswagen"),
                                    ordered = FALSE)
str(survey_vehicles_top10$make_labelled)

#Estimating the average age and vehicle miles of the top 10 vehicle models
avgVehStats <- survey_vehicles_top10 %>% 
  group_by(make_labelled) %>%
  summarise(N = n(), 
            avgVehAge = round(mean(vehage, na.rm = TRUE), 1),
            avgVehMiles = round(mean(vehmiles,na.rm=TRUE,1))) %>% 
  arrange (-N) %>%
  as.data.frame()
avgVehStats

#create a new column to add labels to the fuel type variable (another method from above)
survey_vehicles_top10<- survey_vehicles_top10 %>%
  mutate(fueltype_labelled = case_when(fueltype < 0 ~ 'NA',
                                        fueltype == 1 ~ 'Gas',
                                        fueltype == 2 ~ 'Diesel',
                                        fueltype == 3 ~ 'Hybrid',
                                        fueltype > 3 ~ 'Other fuel'))
as.factor(survey_vehicles_top10$fueltype_labelled)                                        
str(survey_vehicles_top10)
unique(survey_vehicles_top10$fueltype_labelled)

#understanding the distribution of fuel type by car manufacturer

table_maker_fueltype <- table(survey_vehicles_top10[c("make_labelled", "fueltype_labelled")])
 
# ==== Recoding values and dealing with missing or unusual values === #

#understanding the values on vehicle age 
unique(survey_vehicles_top10$vehage)

# negative numbers, that means those values are NA and need to be changed
survey_vehicles_top10 <- survey_vehicles_top10 %>%
  mutate(vehage = ifelse(vehage %in% c(-8, -7), NA, vehage))

table(survey_vehicles_top10$vehage, useNA = 'always')

#exploring some very large age variables 

survey_vehicles_top10 %>% filter(vehage>50) #they seem to be 457 cars older than 50 years. Vintage cars! But, how can we be sure?

#Test plot to see how the values are distributed. Very interesting 'two-peak' distribution!
ggplot(data = survey_vehicles_top10, 
       mapping = aes(x = vehage)) + 
  geom_bar()


# --- The end of Part 1 --- #

# --- Part 2 - Merging datasets and visualizations --- #
# 6. Efficient merging/joining datasets by common column(s)
# 7. Visualizing data using gglplot2 (editing parameters, applying labels, adding annotations, etc)
# 8. (extra) Visualizing data using interactive plots

# ==== Merging datasets === #

#shaping the vehicles dataset for merge with households dataset
vehicles <- survey_vehicles %>% select(sampno, vehno, vehyear,vehage,make, fueltype, vehmiles)

#extracting the list of maker and fuel type levels from the codebook
make_val <- NHTS_codebook %>% filter(NAME=='MAKE')
fuel_val <- NHTS_codebook %>% filter(NAME=='FUELTYPE')
make_val
fuel_val
str(fuel_val)
str(make_val)

#change column names for join
names(make_val)[3]<-'make'
names(make_val)[4]<-'make_label'
names(fuel_val)[3]<-'fueltype'
names(fuel_val)[4]<-'fueltype_label'
names(make_val)
names(fuel_val)

#transforming to numeric
make_val$make <-as.numeric(make_val$make)
fuel_val$fueltype <-as.numeric(fuel_val$fueltype)

#merge
vehicles_join <- dplyr::full_join(vehicles, make_val, by = 'make')
vehicles_join <- dplyr::full_join(vehicles_join, fuel_val, by = 'fueltype')
str(vehicles_join)


#import NTHS CA households data
households <- read.table("survey_households.csv", header=TRUE, sep=",")
str(households)

#pulling ID, househould size, household count and household income variables from Households
households$hhfaminc

hh_stats <- households %>% select(sampno, hhsize, hhvehcnt, hhfaminc)
hh_stats
str(hh_stats)

#extracting the list of income levels from the codebook
income_val <- NHTS_codebook %>% filter(NAME=='HHFAMINC')
income_val
str(income_val)

#changing the name of the income column for labels merge 
#(another way of matching raw numeric data values to a description from the codebook)
names(income_val)
names(income_val)[3]<-'hhfaminc'
names(income_val)

#changing the income columns from both dataframes to numeric to allow for match

hh_stats$hhfaminc <-as.numeric(hh_stats$hhfaminc)
income_val$hhfaminc <-as.numeric(income_val$hhfaminc)

#data join
hh_stats_join <- dplyr::full_join(hh_stats, income_val, by = 'hhfaminc')
str(hh_stats_join)

#renaming the join
hh_info <- hh_stats_join %>% select(sampno, hhsize, hhvehcnt, LABEL)
str(hh_info)

#changing the column names for easier ID
hh_info %>% 
  rename(
    'Household Size'= hhsize,
    'Number of Vehicles' = hhvehcnt,
    'Income' = LABEL 
  )

#right merging of the household and vehicle datasets 
#there are less variables in the household dataset. It is more relevant to compare that dataset. 

hh_veh_data <- dplyr::left_join(hh_info, vehicles_join, by ='sampno')
str(hh_veh_data) #this dataset is larger than hh_info beacuse it could register multiple cars per household id (sampno)
names(hh_veh_data)

# ==== visualizations === #

#age distribution of vehicles

#boxplot of age by top10 vehicles

#age of vehicles distribution by income


# ==== Interactive visualizations === #

#type of fuel by income


# --- The end of Part 2 --- #
