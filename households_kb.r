#set working directory to project 1/nhts17/survey data
setwd("C:/Users/KBridges/Desktop/TRANSFER/R Programming/project 1/nhts17_caltrans/nhts17_caltrans_tsdc_download/survey_data")

#intall packages for importing file
install.packages("data.table")
library(data.table)
install.packages("dplyr")
library(dplyr)
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

#create a subset with modes, use dplyr to select
hhmodes <- hhdata %>% select(sampno, bike, walk, car, bus, train, taxi, para)
head(hhmodes, 20)
#shows that there are some -9 values across the columns

#check for NA values in each of the columns
apply(is.na(hhmodes), FUN = sum, Margin = 2)

#shows that there are 17 missing values across each of the columns

#change modal variables to factors - results in an unordered, numberic factor
class(hhmodes$bike)
mode(hhmodes$bike)
hhmodes$bike <- as.factor(hhmodes$bike)

#recode values to character type
hhmodes$bike <- dplyr::recode(hhmodes$bike, -8 = NA, -9 = NA, -7 = NA, 
                              1 = Daily, 2 = A few times a week, 
                              3 = A few times a month, 4 = A few times a year, 5 = Never)
#this did not work, need to covert to a character type first maybe??
hhmodes$bike <- as.character(hhmodes$bike)
class(hhmodes$bike)
mode(hhmodes$bike)

#create a subset with the household characteristics (that could maybe be joined with other data later?)
hhchar<- hhdata %>% select(homeown, hhsize, hhvehcnt, hhfaminc, numadlt, youngchild, hh_race, hh_hisp)

#stop and save your work
savehistory(file="hhdata.Rhistory")
save.image("hhdata1.RData")
