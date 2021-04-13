# # --- Steer Group: Data Analysis with R - Project 2 - Times Series analysis, forecasting and model optimization --- #
# --- Date: 12-April-2021
# --- Team: Kate Bridges, Sarah McMinimy, Derek Cheah, Armando Orta

# Part 1 - Feature Engineering and data visualization ---------------------------------

#load libraries 
library(readr)
library(zoo)
library(xts)
library(timetk)
library(tsibble)
library(dplyr)
library(forecast)
library(data.table)
library(lubridate)
library(ggplot2)
library(ggpubr)
library(dygraphs)
library(seasonal)
library(highcharter)
library(reshape2)
library(psych)
library(tidyverse)

#set working directory
# setwd("C:/Users/KBridges/Desktop/TRANSFER/R Programming/project 2")
# setwd("~/00 Steer R Training/Project 2/data")
setwd("C:/Users/sarah.mcminimy/Desktop/R_Course/proj2/data")

#load the bikeshare dataset 
load('capitalbikeshare_11_17.RData')
str(bikes_20112017)
head(bikes_20112017)
names(bikes_20112017)
sapply(bikes_20112017, function (x) sum(is.na(x))) 

#load the weatherdataset
weather_201112017 <- read_csv("weather_20112017.csv")
str(weather_201112017)
head(weather_201112017)
sapply(weather_201112017, function (x) sum(is.na(x))) 

#extract date and time information using data.table and as.IDate() and as.ITime() 
bikes_20112017 <- bikes_20112017 %>% 
  mutate(Date = as.IDate(Start_date, format = "%d/%m/%Y"),
         STime = as.ITime(Start_date, format = "%H:%M:%S"),
         ETime = as.ITime(End_date, format = "%H:%M:%S"))

str(bikes_20112017)
head(bikes_20112017)

#extract date and time info using data.table for the weather dataset so they match
weather_201112017 <- weather_201112017 %>% 
  mutate(Date = as.IDate(Datetime, format = "%d/%m/%Y"),
         Time = as.ITime(Datetime, format = "%H:%M:%S"))

str(weather_201112017)
head(weather_201112017)

#create a subset for just 2017
bikes_2017 <- bikes_20112017 %>%
  dplyr::filter(between(Date, "2017-01-01", "2017-12-31")) 
min(bikes_2017$Date)
max(bikes_2017$Date)

#extract day of the month information usign lubridate package
str(bikes_2017)
bikes_2017$DayOfWeek <- lubridate::wday(bikes_2017$Date, 
                                        label = TRUE, 
                                        abbr = FALSE, 
                                        week_start = 1) 

#extract month information using lubridate information
bikes_2017$MonthName <- lubridate::month(bikes_2017$Date, 
                                         label = TRUE, abbr = TRUE) 
bikes_2017$MonthInt <- lubridate::month(bikes_2017$Date, 
                                        label = FALSE) 

#extract day of the month using lubridate
bikes_2017$DayOfMonth <- lubridate::day(bikes_2017$Date) 

# Extract day of year using lubridate
bikes_2017$DayOfYear <- lubridate::yday(bikes_2017$Date)
tail(bikes_2017)

# Extract year information using lubridate
bikes_2017$Year <- lubridate::year(bikes_2017$Date)

# Extract hour information using lubridate
bikes_2017$Hour <- lubridate::hour(bikes_2017$STime)
str(bikes_2017)
head(bikes_2017)

# using base package extract the hour data for the 2015-2017 dataset
bikes_20112017$Hour <- data.table::hour(bikes_20112017$STime) 
str(bikes_20112017)
head(bikes_20112017)

#create new variable for trip duration in seconds/minutes using difftime()
bikes_2017 <- bikes_2017 %>% 
  mutate(Duration = difftime(bikes_2017$End_date,  bikes_2017$Start_date, units = "mins"))
str(bikes_2017)
head(bikes_2017)

bikes_20112017 <- bikes_20112017 %>%
  mutate(Duration = difftime(bikes_20112017$End_date, bikes_20112017$Start_date, units = "mins"))
str(bikes_20112017)
head(bikes_20112017)

#calculate the average trip duration per day for bike trips in 2017

stats_duration_2017 <- bikes_2017 %>% 
  group_by(Date) %>%
  summarise(N = n(), 
            Mean = mean(Duration, na.rm = TRUE), 
            SD = sd(Duration, na.rm = TRUE)) %>%
  as.data.frame()

head(stats_duration_2017)

# create TS and XTS of daily trips using all of the bike data for the last five years
trips_daily_ts <- bikes_20112017 %>% 
  group_by(Date) %>%
  summarise(N = n()) %>%
  tk_ts(select = N, start = c(2011, 1), frequency = 365.25)

str(trips_daily_ts)
tail(trips_daily_ts) 

trips_daily_xts <- bikes_20112017 %>% 
  group_by(Date) %>%
  summarise(N = n()) %>%
  tk_xts(select = N, start = c(2011, 1))

# create TS and XTS of average trip duration per day
duration_dailyts <- bikes_20112017 %>% 
  group_by(Date) %>% 
  summarise(N = n(), 
            trips_duration = as.numeric(mean(Duration, na.rm = TRUE)), 
            SD = sd(Duration, na.rm = TRUE)) %>%
  tk_ts(select = trips_duration, start = c(2011, 1), frequency = 365.25)

str(duration_dailyts)
head(duration_dailyts)

duration_daily_xts <- bikes_20112017 %>% 
  group_by(Date) %>%
  summarise(trips_duration = as.numeric(mean(Duration, na.rm = TRUE))) %>%
  tk_xts(select = trips_duration, start = c(2011, 1))

# Estimate the average trip duration by time of day 
duration_hourlyts <- bikes_20112017 %>% 
  group_by(Hour) %>% 
  summarise(N = n(), 
            trips_duration = mean(Duration, na.rm = TRUE), 
            SD = sd(Duration, na.rm = TRUE))

# create TS and XTS of monthly trips using all of the bike data for the last five years
trips_monthly_xts <- apply.monthly(trips_daily_xts, FUN = sum)
nmonths(trips_monthly_xts)

trips_monthly_ts <- trips_monthly_xts %>%
  tk_ts(start = c(2011, 1), frequency = 12)
trips_monthly_ts

#visualize the time series for daily trips using the forecast package
# 1) Total Daily Trips
trips_daily_ts %>% autoplot()

# 2) Duration of Daily Trips
duration_dailyts %>% autoplot()

# 3) Average Duration of Trips by Hour of Day
ggplot(duration_hourlyts, aes(Hour, trips_duration)) +
         geom_col() + 
  ggtitle("Average Trip Duration by Hour of Day") +
  xlab("Hour of Day") + 
  ylab("Average Trip Duration")

#zoom in on just the last 2 years
window(trips_daily_ts, start = c(2017, 1), end = c(2018, 31)) %>% 
  autoplot()
window(duration_dailyts, start = c(2017, 1), end = c(2018, 31)) %>% 
  autoplot()


# Join weather and trip datasets
daily_biketrips_all <- bikes_20112017 %>%
  group_by(Date) %>%
  summarise(alltrips = n(),
            trips_duration = mean(Duration, na.rm = T)) 

daily_biketrips_member <- bikes_20112017 %>%
  filter(Member == "Member") %>%
  group_by(Date) %>%
  summarise(membertrips = n(),
            membertrips_duration = mean(Duration, na.rm = T)) 

daily_biketrips_casual <- bikes_20112017 %>%
  filter(Member == "Casual") %>%
  group_by(Date) %>%
  summarise(casualtrips = n(),
            casualtrips_duration = mean(Duration, na.rm = T))

str(daily_biketrips_member)
str(daily_biketrips_casual)

weather_daily <- weather_201112017 %>% 
  mutate(Date = as.Date(Datetime, format = "%Y-%m-%d")) %>%
  group_by(Date) %>%
  summarise(avg_temp = mean(temp, na.rm = T),
            avg_wspd = mean(wspd, na.rm = T), 
            sum_precip = sum(precip_hrly, na.rm = T), 
            sum_snow = sum(snow_hrly, na.rm = T))

daily_biketrips_all
daily_biketrips_member
daily_biketrips_casual
weather_daily

join1 <- merge(daily_biketrips_all,  daily_biketrips_member, by.x = 'Date', by.y = 'Date')
join2 <- merge(join1, daily_biketrips_casual, by.x = 'Date', by.y = 'Date')
biketrip_weather <- merge(join2,  weather_daily, by.x = 'Date', by.y = 'Date')

str(biketrip_weather)

# Part 2 - Hypothesis testing ---------------------------------

#Hypothesis 1: Members will take more but shorter trips than Casual users
#Test Duration versus Member Type
biketrip_weather$casualtrips_duration <- as.numeric(biketrip_weather$casualtrips_duration)
biketrip_weather$membertrips_duration <- as.numeric(biketrip_weather$membertrips_duration)

hist(biketrip_weather$membertrips)
hist(biketrip_weather$casualtrips)
hist(biketrip_weather$membertrips_duration)
hist(biketrip_weather$casualtrips_duration)

plot(density(biketrip_weather$membertrips))
plot(density(biketrip_weather$casualtrips))
plot(density(biketrip_weather$membertrips_duration))
plot(density(biketrip_weather$casualtrips_duration))

stats_member <- biketrip_weather %>% 
  summarise(N = sum(membertrips),
            N_Mean = mean(membertrips, na.rm = TRUE), 
            Duration_Mean = mean(membertrips_duration, na.rm = TRUE),
            N_SD = sd(membertrips, na.rm = TRUE),
            Duration_SD = sd(membertrips_duration, na.rm = TRUE)) %>%
  as.data.frame()

stats_casual <- biketrip_weather %>% 
  summarise(N = sum(casualtrips),
            N_Mean = mean(casualtrips, na.rm = TRUE), 
            Duration_Mean = mean(casualtrips_duration, na.rm = TRUE),
            N_SD = sd(casualtrips, na.rm = TRUE),
            Duration_SD = sd(casualtrips_duration, na.rm = TRUE)) %>%
  as.data.frame()

#Kolmogorov-Smirnov test - none of the four samples appears to be have significant difference from normal distribution 

stats_member
stats_casual

set.seed(999)
sample_membertrips <- round(rnorm(14850526, mean = 5816.892, sd = 2664.989), 0)

set.seed(999)
sample_casualtrips <- round(rnorm(4151472, mean = 1626.115, sd = 1530.833), 0)

set.seed(999)
sample_membertrips_duration <- round(rnorm(14850526, mean = 11.90116, sd = 1.311446), 0)

set.seed(999)
sample_casualtrips_duration <- round(rnorm(4151472, mean = 37.31107, sd = 7.404463), 0)

ks.test(biketrip_weather$membertrips, 
        sample_membertrips) # p-value < .05

ks.test(biketrip_weather$casualtrips, 
        sample_casualtrips) # p-value < .05

ks.test(biketrip_weather$membertrips_duration, 
        sample_membertrips_duration) # p-value < .05

ks.test(biketrip_weather$casualtrips_duration, 
        sample_casualtrips_duration) # p-value < .05

#??? Kate/Armando any thoughts on these - can't figure out how to make work - variance tests & t test
var.test(biketrip_weather$membertrips_duration ~ biketrip_weather$casualtrips_duration, data = biketrip_weather$trips_duration)

meantrips_test <- bikes_20112017 %>%
  select (Member, Duration) 
meantrips_test$Duration <- as.numeric(meantrips_test$Duration)
unique(meantrips_test$Member)
mmeantrips_test <- meantrips_test%>%
  mutate(Member = ifelse(Member %in% c("Member","Casual"), Member, NA))

var.test(Duration ~ Member, data = meantrips_test) 
#p-value is <0.5 so the variance in duration of trips by Member and Casual users is statistically different 

# run independent (unpaired) t-test
t.test(Duration ~ Member, data = meantrips_test, alternative="two.sided")
# The difference is significant, p<.05







# Part 3 - Optimize forecasting models ---------------------------------

# Join with employment data from .....

# Create training and testing datasets




auto.arima(trips_monthly_ts)




