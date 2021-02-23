# setting working directory
setwd("C:/Users/sarah.mcminimy/Desktop/R_Course/proj1/data")
getwd()
load("C:/Users/sarah.mcminimy/Desktop/R_Course/proj1/data/working1.Rdata")

# import NTHS CA persons data
persons1 <- read.table("survey_person.csv", header=TRUE, sep=",")
persons1

install.packages("readr")
library(readr)
persons2 <- read.csv("survey_person.csv")
persons2

str(persons1)
persons1 == persons2

install.packages("readxl")
library(readxl)
excel_sheets("thsc-nhts17-caltrans-codebook.xlsx")
full_headings <- read_excel("thsc-nhts17-caltrans-codebook.xlsx",sheet=1,col_names=TRUE)
value_codes <- read_excel("thsc-nhts17-caltrans-codebook.xlsx",sheet=2,col_names=TRUE)

# Recoding the values
library(dplyr)
library(car)
library(data.table)
library(readr)

names(persons1)
# ??? unique(person1$educ)

full_headings
full_headings$`QUESTION TEXT`

save.image
save.image("C:/Users/sarah.mcminimy/Desktop/R_Course/proj1/data/working1.RData")

#Filter the headings to find person data headings only

library(dplyr)
#test persons1 %>% select(drvr, educ)
str(full_headings)

##personhead <- subset(full_headings, person >= 1)
p_head <- full_headings %>% filter(`QUESTION TEXT`!= 'NA', `TABLE:\r\nPERSON` >= 1)
str(p_head)
str(persons1)
p_head$LABEL
