# ==== Initialization, importing data, exploring data, preparing codebook assignment === #

setwd("C:/Users/sarah.mcminimy/Desktop/R_Course/proj1/data")
getwd()
load("C:/Users/sarah.mcminimy/Desktop/R_Course/proj1/data/working1.Rdata")
save.image("C:/Users/sarah.mcminimy/Desktop/R_Course/proj1/data/working1.RData")

library(readr)
library(readxl)
library(dplyr)
library (psych)
library(Hmisc)
library(summarytools)
library(dplyr)
library(ggplot2)
library(RColorBrewer)


# import NTHS CA households data
households <- read.table("survey_households.csv", header=TRUE, sep=",")
households
str(households)

# import NTHS CA trips data
trips <- read.table("survey_trips.csv", header=TRUE, sep=",")
trips
str(trips)

# import Codebook
install.packages("readxl")
library(readxl)
excel_sheets("thsc-nhts17-caltrans-codebook.xlsx")
full_headings <- read_excel("thsc-nhts17-caltrans-codebook.xlsx",sheet=1,col_names=TRUE)
value_codes <- read_excel("thsc-nhts17-caltrans-codebook.xlsx",sheet=2,col_names=TRUE)

#Pull headings for only trips and household data - this is used to variables are available and their name in the data. 
str(full_headings)

trip_head <- full_headings %>% filter(`LABEL`!= 'NA', `TABLE:\r\nTRIP` >= 1)
str(trip_head)
trip_head
trip_head$LABEL
trip_head %>% select(NAME, LABEL) %>% print(n = 117) #We want to see all variables. Chose to use TRPTRANS17, TRPMILES17, TRIPPURP and HOUSEID for joining  


hh_head <- full_headings %>% filter(`LABEL`!= 'NA', `TABLE:\r\nHOUSEHOLD` != 'NA')
str(hh_head)
hh_head$LABEL
hh_head %>% select(NAME, LABEL) %>% print(n = 94) #Chose to use HH_CBSA, HHFAMINC, HHSIZE and HOUSEID for joining


#Retrieve descriptions for coded values from codebook
trip_mode <- value_codes %>% filter(NAME=='TRPTRANS17')
trip_mode
str(trip_mode)

purpose <- value_codes %>% filter(NAME=='TRIPPURP')
purpose
str(purpose)

hh_loc <- value_codes %>% filter(NAME=='HH_CBSA')
hh_loc
str(hh_loc)

income_val <- value_codes %>% filter(NAME=='HHFAMINC')
income_val
str(income_val)

#Reviewing Trips Data
trips       
str(trips)            
dim(trips)
names(trips)  #these should match the names observed in headings review although noting that HHID is actually sampo in the tibble
sum(is.na(trips))  #558810 missing values   
sapply(trips, function (x) sum(is.na(x)))  # checking for NA values location by column - trpmiles17, trptrans17 and trippurp do not have missing values - lucky for us!

#Reviewing Households Data
households       
str(households)            
dim(households)
names(households)  #these should match the names observed in headings review although noting that HHID is actually sampo in the tibble
sum(is.na(households))  #880 missing values  
sapply(households, function (x) sum(is.na(x)))  # looks like most columns are missing 10 records for households including sampno - will be impossible to match these to trips but the number is small


# ==== Data summaries, cross tabulations and frequency tests === #



#TO DELETE - Copied code from households
households$hhfaminc
str(value_codes)
library(dplyr)

hh_stats <- households %>% select(sampno, hhsize, hhvehcnt, hhfaminc)
hh_stats
str(hh_stats)

income_val <- value_codes %>% filter(NAME=='HHFAMINC')
income_val
str(income_val)

names(income_val)
names(income_val)[3]<-'hhfaminc'

hh_stats$hhfaminc <-as.numeric(hh_stats$hhfaminc)
income_val$hhfaminc <-as.numeric(income_val$hhfaminc)
hh_stats_join <- dplyr::full_join(hh_stats, income_val, by = 'hhfaminc')
str(hh_stats_join)
hh_info <- hh_stats_join %>% select(sampno, hhsize, hhvehcnt, LABEL)
str(hh_info)
hh_info

hh_info %>% 
  rename(
    'Household Size'= hhsize,
    'Number of Vehicles' = hhvehcnt,
    'Income' = LABEL 
  )
