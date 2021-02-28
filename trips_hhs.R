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

library(readr)


# Import NTHS CA households data
households <- read.table("survey_households.csv", header=TRUE, sep=",")
households
str(households)


# Import NTHS CA trips data
trips <- read.table("survey_trips.csv", header=TRUE, sep=",")
trips
str(trips)


# Import Codebook
library(readxl)
excel_sheets("thsc-nhts17-caltrans-codebook.xlsx")
full_headings <- read_excel("thsc-nhts17-caltrans-codebook.xlsx",sheet=1,col_names=TRUE)
value_codes <- read_excel("thsc-nhts17-caltrans-codebook.xlsx",sheet=2,col_names=TRUE)


#Pull headings for only trips and household data - this is used to variables are available and their name in the data. 
library(dplyr)
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
head(trips, 10)
tail(trips, 10)
names(trips)  #these should match the names observed in headings review although noting that HHID is actually sampo in the tibble
sum(is.na(trips))  #558810 missing values   
sapply(trips, function (x) sum(is.na(x)))  # checking for NA values location by column - trpmiles17, trptrans17 and trippurp do not have missing values - lucky for us!


#Reviewing Households Data
households       
str(households)            
dim(households)
head(households, 10)
tail(households, 10)
names(households)  #these should match the names observed in headings review although noting that HHID is actually sampo in the tibble
sum(is.na(households))  #880 missing values  
sapply(households, function (x) sum(is.na(x)))  # looks like most columns are missing 10 records for households including sampno - will be impossible to match these to trips but the number is small


# ==== Creating usable subsets, recoding values and joining tables === #

#Creating subsets with dplyer for both tables with the variables we want to use

trip_stats <- trips %>% select(sampno, trptrans17, trpmiles17, trippurp)
trip_stats
str(trip_stats)
  #Cleaning up coded variables with -'ve values
unique(trip_stats$trptrans17)
trip_mode #codebook says that we should have -8 and -7 as non-answers but unique values show -8 and -9. I am converting both of these -'ves into NAs 
trip_stats <- trip_stats %>%
  mutate(trptrans17 = ifelse(trptrans17 %in% c(-8, -9), NA, trptrans17))
unique(trip_stats$trptrans17)
unique(trip_stats$trippurp)
purpose
trip_stats <- trip_stats %>%
  mutate(trippurp = ifelse(trippurp %in% "-9", NA, trippurp)) #changing 'Not ascertained' values to NA
unique(trip_stats$trippurp)

hh_stats <- households %>% select(sampno, hh_cbsa, hhsize, hhfaminc)
hh_stats
str(hh_stats)
  #Cleaning up coded variables with -'ve values or other codes for missing values
unique(hh_stats$hhfaminc)
income_val
hh_stats <- hh_stats %>%
  mutate(hhfaminc = ifelse(hhfaminc %in% c(-7, -8, -9), NA, hhfaminc)) #changing values with no answer to NA
unique(hh_stats$hhfaminc)
unique(hh_stats$hh_cbsa)
hh_stats <- hh_stats %>%
  mutate(hh_cbsa = ifelse(hh_cbsa %in% "XXXXX", NA, hh_cbsa)) %>% mutate(hh_cbsa = ifelse(hh_cbsa %in% "", NA, hh_cbsa))
sum(is.na(hh_stats$hh_cbsa)) # 651 NA values (out of 16,387) for CBSAs - will need to remember when summarizing by geography that not all records fall into a CBSA


#Assign recode existing coded variables with the label value from codebook using a join in dplyr - to do this must (1) ensure join fields names match, (2) convert codebook codes to characters (converting num to character will not work, or will take more steps because codebook characters have a "0" before number)
  #This will be applied to the columns from our subsets trip_stats and hh_stats, using summaries from codebook created earlier:
#trip_mode, purpose, hh_loc, income_val

str(trip_stats)
trip_mode
names(trip_mode)
names(trip_mode)[3]<-'trptrans17'

purpose
names(purpose)
names(purpose)[3]<-'trippurp'

trip_mode$trptrans17 <-as.integer(trip_mode$trptrans17)
trip_stats <- dplyr::full_join(trip_stats, trip_mode, by = 'trptrans17') %>%  mutate(trptrans17 = LABEL)
trip_stats <- dplyr::full_join(trip_stats, purpose, by = 'trippurp') %>%  mutate(trippurp = LABEL.y)
str(trip_stats) 
trip_stats <- trip_stats %>% select(sampno, trptrans17, trpmiles17, trippurp) #cleaning up to make sure it's only the columns we need
str(trip_stats)
trip_stats

###### HHs
hh_stats
str(hh_stats)
hh_loc
names(hh_loc)
names(hh_loc)[3]<-'hh_cbsa'
str(hh_loc) #mode matches - both are character so no need for conversion

income_val
names(income_val)
names(income_val)[3]<-'hhfaminc'
str(income_val) #mode mismatch - need to convert character in codebook (income_val) to num to match hh_stats
income_val$hhfaminc <-as.numeric(income_val$hhfaminc)

hh_stats <- dplyr::full_join(hh_stats, hh_loc, by = 'hh_cbsa') %>%  rename(
  'Location'= LABEL) #We want to keep the CBSA code for assigning geography but include the place name
hh_stats <- dplyr::full_join(hh_stats, income_val, by = 'hhfaminc') %>%  mutate(hhfaminc = LABEL) #we don't need the income code just the name of the income bracket
str(hh_stats) 
hh_stats <- hh_stats %>% select(sampno, hh_cbsa, Location, hhsize, hhfaminc) #cleaning up to make sure it's only the columns we need
str(hh_stats)
hh_stats

#Joining the Trip and Household information into one table
str(hh_stats$sampno)
str(trip_stats$sampno) #sampno is of different modes so will need to convert one to another. Also because there are many more record in Trips (185,637) than Households (17,245) we will need to join so that we're not loosing records (join to trips rather than joining trips to households.) To check this we will be looking for a join output with a number of records matching the number of observations in trips (185,637)
hh_stats$sampno <-as.integer(hh_stats$sampno)

?dplyr::full_join #check which join to use - for this one picking a right_joing where we have trips in y
hh_trips <- dplyr::right_join(hh_stats, trip_stats, by = 'sampno')
str(hh_trips) # 188,238 Observations
hh_trips

#Consolidating transport mode in trptrans17 (THIS DOESN'T WORK YET!!!!!)

unique(hh_trips$trptrans17)
hh_trips <- hh_trips %>%
  mutate(trptrans17 = ifelse(hh_trips %in% c("Pickup truck","SUV","Van","Rental car (Including Zipcar / Car2Go)"), 'Car', hh_trips)) %>%
  mutate(trptrans17 = ifelse(hh_trips %in% c("Something Else", "RV (motor home, ATV, snowmobile)","Golf cart / Segway","School bus"), 'Other', hh_trips)) %>%
  mutate(trptrans17 = ifelse(hh_trips %in% c("Private / Charter / Tour / Shuttle bus", "City-to-city bus (Greyhound, Megabus)"), 'Shuttle/Long-Distance Bus', hh_trips)) %>%
  mutate(trptrans17 = ifelse(hh_trips %in% c("I don't know","I prefer not to answer"), NA, hh_trips))

# ==== Data summaries, cross tabulations and frequency tests === #



# ==== Data Visualization === #

#Changing column names for styling

hh_trips <- hh_trips %>% 
  rename(
    'HHID'= sampno,
    'CBSA' = hh_cbsa,
    'HH Size' = hhsize,
    'Income' = hhfaminc,
    'Trip Mode' = trptrans17,
    'Trip Miles' = trpmiles17,
    'Trip Purpose' = trippurp 
  )


 
