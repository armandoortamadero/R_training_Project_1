#set working directory to project 1/nhts17/survey data
setwd("C:/Users/KBridges/Desktop/TRANSFER/R Programming/project 1/nhts17_caltrans/nhts17_caltrans_tsdc_download/survey_data")

#load packages for analysis
library(data.table)
library(dplyr)
library(stats)
search()

#import households data as new object
#check for structure and dimensions
#use head and tail functions to confirm that data has imported correctly
hhdata <-fread("survey_households.csv", header = T, sep = ",")
str(hhdata)
dim(hhdata)
options(max.print = 5000)
head(hhdata, 10)
tail(hhdata, 10)

#import the codebook headings using readxl package
install.packages("readxl")
library(readxl)
excel_sheets("thsc-nhts17-caltrans-codebook.xlsx")
full_headings <- read_excel("thsc-nhts17-caltrans-codebook.xlsx",sheet=1,col_names=TRUE)
value_codes <- read_excel("thsc-nhts17-caltrans-codebook.xlsx",sheet=2,col_names=TRUE)
unique(value_codes$TABLE)

#filter just the household values
hhvalues<- value_codes %>% filter(TABLE == 'HOUSEHOLD')
str(hhvalues)
dim(hhvalues)
head(hhvalues)

#create a subset with modes, use dplyr to select specific household variables
#related to our research questions: sampo, home location, mode share
#and number of vehicles

hhmodes <- hhdata %>% select(sampno, bike, walk, car, bus, train, taxi, para)
head(hhmodes, 20)

hhsubset <- hhdata %>% select(sampno, hhm, hh_cbsa, hhvehcnt, hhfaminc, bike, walk, car, bus, train, taxi, para)
#shows that there are some -9 values across the columns

#check for NA values in each of the columns
#shows that there are 17 missing values across each of the columns
apply(is.na(hhsubset), FUN = sum, MARGIN = 2)

#create one-way frequency tables for income, vehicle count, cbsa
library (stats)
freq_inc <- table(hhsubset$hhfaminc)
freq_vct <- table(hhsubset$hhvehcnt)
freq_cbsa <- table(hhsubset$hh_cbsa)


#create two-way freq tables for income and vehicle count
inc_veh_ct <- table(hhsubset$hhfaminc, hhsubset$hhvehcnt, useNA = 'always')

#create two-way freq tables for cbsas and income
hhcbsa_inc_ct <- table(hhsubset$hh_cbsa, hhsubset$hhfaminc, useNA = 'always')

#create two-way freq tables for cbas and vehicle ownership
hhcbsa_inc_ct <- table(hhsubset$hh_cbsa, hhsubset$hhvehcnt, useNA = 'always')

#create some simple plots
library(dplyr)
library(ggplot2)
library(RColorBrewer)

#change modal variables to factors - results in an unordered, numberic factor
class(hhmodes$bike)
mode(hhmodes$bike)
hhmodes$bike <- as.factor(hhmodes$bike)

#create a subset with the household characteristics (that could maybe be joined with other data later?)
hhchar<- hhdata %>% select(homeown, hhsize, hhvehcnt, hhfaminc, numadlt, youngchild, hh_race, hh_hisp)



#recode values to ordered factors  
hhmodes$bike <- dplyr::recode(hhmodes$bike, -8 = NA, -9 = NA, -7 = NA, 
                              1 = Daily, 2 = A few times a week, 
                              3 = A few times a month, 4 = A few times a year, 5 = Never)

#stop and save your work
savehistory(file="hhdata.Rhistory")
save.image("hhdata1.RData")
