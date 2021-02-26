# setting working directory
setwd("C:/Users/sarah.mcminimy/Desktop/R_Course/proj1/data")
getwd()
load("C:/Users/sarah.mcminimy/Desktop/R_Course/proj1/data/working1.Rdata")
save.image("C:/Users/sarah.mcminimy/Desktop/R_Course/proj1/data/working1.RData")

# import NTHS CA persons data
persons1 <- read.table("survey_person.csv", header=TRUE, sep=",")
persons1

install.packages("readr")
library(readr)
persons2 <- read.csv("survey_person.csv")
persons2

str(persons1)
persons1 == persons2

# import NTHS CA households data
households <- read.table("survey_households.csv", header=TRUE, sep=",")
households
str(households)

# import Codebook
install.packages("readxl")
library(readxl)
excel_sheets("thsc-nhts17-caltrans-codebook.xlsx")
full_headings <- read_excel("thsc-nhts17-caltrans-codebook.xlsx",sheet=1,col_names=TRUE)
value_codes <- read_excel("thsc-nhts17-caltrans-codebook.xlsx",sheet=2,col_names=TRUE)


#Filter the headings to find person data headings only

library(dplyr)
#test persons1 %>% select(drvr, educ)
str(full_headings)

p_head <- full_headings %>% filter(`QUESTION TEXT`!= 'NA', `TABLE:\r\nPERSON` >= 1)
str(p_head)
str(persons1)
p_head$LABEL

#Pulling select characteristics from Households
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
