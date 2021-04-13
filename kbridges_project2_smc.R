#rproject 2

#set working directory
setwd("C:/Users/sarah.mcminimy/Desktop/R_Course/proj2/data")

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

#load the bikeshare dataset 
load('capitalbikeshare_11_17.RData')
str(bikes_20112017)
head(bikes_20112017)

#load the weatherdataset
weather_201112017 <- read_csv("weather_20112017.csv")
str(weather_201112017)
head(weather_201112017)

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

stats_duration <- bikes_2017 %>% 
  group_by(Date) %>%
  summarise(N = n(), 
            Mean = mean(Duration, na.rm = TRUE), 
            SD = sd(Duration, na.rm = TRUE)) %>%
  as.data.frame()

stats_duration

#create a time series of daily trips using all of the bike data for the last five years
trips_daily_ts <- bikes_20112017 %>% 
  group_by(Date) %>%
  summarise(N = n()) %>%
  tk_ts(select = N, start = c(2011, 1), frequency = 365.25)

tail(trips_daily_ts) 

#create a time series of average trip duration per day
duration_dailyts <- bikes_20112017 %>% 
  group_by(Date) %>% 
  summarise(N = n(), 
            Mean = mean(Duration, na.rm = TRUE), 
            SD = sd(Duration, na.rm = TRUE)) %>%
  tk_ts(select = N, start = c(2011, 1), frequency = 365.25)

duration_hourlyts <- bikes_20112017 %>% 
  group_by(Hour) %>% 
  summarise(N = n(), 
            Mean = mean(Duration, na.rm = TRUE), 
            SD = sd(Duration, na.rm = TRUE)) %>%
  tk_ts(select = N, start = c(2011, 1), frequency = 365.25)

#visualize the time series for daily trips using the forecast package
trips_daily_ts %>% autoplot()
duration_dailyts %>% autoplot()
duration_hourlyts %>% autoplot()

#zoom in on just the last 2 years
window(trips_daily_ts, start = c(2017, 1), end = c(2018, 31)) %>% 
  autoplot()
window(duration_dailyts, start = c(2017, 1), end = c(2018, 31)) %>% 
  autoplot()


# Summarizing Data by Day to Join Weather and Trips

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

daily_biketrips_member
daily_biketrips_casual

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
