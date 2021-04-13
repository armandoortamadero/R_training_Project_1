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

#set working directory
# setwd("C:/Users/KBridges/Desktop/TRANSFER/R Programming/project 2")
setwd("~/00 Steer R Training/Project 2/data")

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

# Join weather and trip datasets for Hypothesis Testing
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

# 1) extract day of the week 
biketrip_weather$DayOfWeek <- lubridate::wday(biketrip_weather$Date, 
                                              label = TRUE, 
                                              abbr = FALSE, 
                                              week_start = 1) 

# 2) create a subset with new variables for Weekday 
member_subset <- biketrip_weather %>%
  select (Date, membertrips, membertrips_duration, DayOfWeek) %>%
  mutate(Day_Group = if_else(DayOfWeek == "Saturday" | DayOfWeek == "Sunday", 'Weekend','Weekday'))

member_subset

# 3) calculate mean and sd for member trips by weekday

stats_members<- member_subset%>% 
  group_by(Day_Group) %>%
  summarise(N = n(), 
            Mean = mean(membertrips, na.rm = TRUE),
            SD = sd(membertrips, na.rm = TRUE)) %>%
  as.data.frame()

stats_members

summary(member_subset)
hist(member_subset)
plot(density(member_subset$membertrips))

# 4) use a Kolmogorov-Smirnov test because these are larger samples

set.seed(999)
sample_weekday <- round(rnorm(1823, mean = 6319.126, sd = 2748.938), 0)

set.seed(999)
sample_weekend <- round(rnorm(730, mean = 4562.685, sd = 1941.656), 0)

ks.test(member_subset[member_subset$Day_Group == 'Weekday', membertrips], 
        sample_weekday) 

ks.test(member_subset[member_subset$Day_Group == 'Weekend', membertrips], 
        sample_weekend) 

# 5) variance test
var.test( weekday ~ membertrips, data = member_subset) 

# 6) run independent (unpaired) t-test
t.test(membertrips ~ Day_Group, data=member_subset, alternative="two.sided")
plot(t.test(membertrips ~ Day_Group, data=member_subset, alternative="two.sided")) 

# 7) Plot simple boxplots with means to compare groups:
ggplot(member_subset, aes(x=Day_Group, y=membertrips, colour=Day_Group)) + 
  geom_boxplot(width=.5) + 
  stat_summary(fun=mean, geom="point", shape=23, 
               size=3, color="blue", fill="blue") +
  labs(title="Boxplots of member usage on weekdays and weekends with mean statistic", 
       x="Group", y="Member Trips") +
  theme_minimal()


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

# QUESTION: Simon, we couldn't figure out how to get an output from - variance tests & t test. What are we doing wrong?
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

# Confirming monthly bike trips TS file. If not loaded, load from Part 1

trips_monthly_ts

# Creating a daily weather dataset with the significant variables 'avg_temp' and 'avg_pressure' from Week 9 session

weather_daily_reg <- weather_201112017 %>% 
  mutate(Date = as.Date(Datetime, format = "%Y-%m-%d")) %>% # convert the datetime variable into day date only
  group_by(Date) %>%
  summarise(avg_temp = mean(temp, na.rm = T), 
            avg_pressure = mean(pressure, na.rm = T))

tail(weather_daily_reg)

col_names <- names(weather_daily_reg)
weather_daily_reg_xts <- weather_daily_reg %>% tk_xts(select = col_names, 
                                              start = c(2011, 1))

# Convert daily weather data into monthly using daily averages:
weather_monthly_reg_xts <- apply.monthly(weather_daily_reg_xts, FUN = mean)
weather_monthly_reg_xts
nmonths(weather_monthly_reg_xts)

# Join the monthly weather data with monthly no. of trips and convert into a ts object
bike_weather_reg_ts <- trips_monthly_xts %>% 
  merge.xts(weather_monthly_reg_xts['2011-1/2017-12']) %>%
  tk_ts(start = c(2011, 1), frequency = 12)

frequency(bike_weather_reg_ts)
nmonths(bike_weather_reg_ts)

# Load the Employment data for the District of Columbia from the Bureau of Labour Statistics. Not seasonally adjusted
# https://beta.bls.gov/dataViewer/view/timeseries/LAUST110000000000005

employment <- read_csv('Employment_DC.csv') 
head(employment)

# Note: As you can see in the head of employment, we couldn't reformat the Label Variable into date. We did the analysis outside R to get the Date column
# How would you have done it? Every format we tried using Label we got an NA

# Select relevant variables
employment_reg <- employment %>% select(Date, Value)
head(employment_reg)


# Convert to a date format
employment_reg <- employment_reg %>% 
  mutate(Date_1 = as.Date(Date, format = "%Y-%b-%d"))

# Rename Date_1 to Date for Join

employment_reg <- employment_reg %>% select(Date_1, Value)
employment_reg <- rename(employment_reg, Date = Date_1, employment = Value)
head(employment_reg)

# Create an XTS and TS for monthly employment in DC

col_names_empl <- names(employment_reg)

employment_reg_xts <- employment_reg %>% tk_xts(select = col_names_empl,
                                                start = c(2011,1))
employment_monthly_reg_xts <- apply.monthly(employment_reg_xts, FUN = mean)
nmonths(employment_monthly_reg_xts)

employment_monthly_reg_ts <- employment_monthly_reg_xts %>%
  tk_ts(start = c(2011, 1), end = c(2017, 12), frequency = 12)
employment_monthly_reg_ts

# Join employment data with the bike trips and weather data

bike_weather_empl_reg_ts <- bike_weather_reg_ts %>% 
  merge.xts(employment_monthly_reg_xts['2011-1/2017-12']) %>%
  tk_ts(start = c(2011, 1), frequency = 12)

frequency(bike_weather_empl_reg_ts)
nmonths(bike_weather_empl_reg_ts)
bike_weather_empl_reg_ts

# Is employment variable normally distributed?

hist(bike_weather_empl_reg_ts[, "employment"]) # it looks skeweed to the left
qqnorm(bike_weather_empl_reg_ts[, "employment"]) # Q-Q plots confirm a deviation from normality
qqline(bike_weather_empl_reg_ts[, "employment"], lwd = 2, lty = 2, col = "red")

# Shapiro-Wilk test:
shapiro.test(bike_weather_empl_reg_ts[, "employment"]) # the S-W test is significant, there is evidence the data is not normally distributed

# Convert employment data to logs for normality testing

test_employment <- bike_weather_empl_reg_ts[, 'employment']
shapiro.test(log(test_employment)) # apparently, using logs doesn't make it normaly distributed. For purposes of the work, we will test the variable, still (as it is a time series analysis)

# Prepare the training and testing data sets
data_train <- window(bike_weather_empl_reg_ts, start = c(2011, 1), end = c(2016, 12))
data_test <- window(bike_weather_empl_reg_ts, start = c(2017, 1))

# Replicating the multiplicative seasonal naive Holt-Winters model from Week 9 (best fit)

training_data <- window(trips_monthly_ts, 
                        start = c(2011, 1), end = c(2016, 12)) # 72 months
training_data

testing_data <- window(trips_monthly_ts, 
                       start = c(2017, 1)) # 12 months
testing_data

hw_multi <- hw(training_data, seasonal="multiplicative", h=12)
summary(hw_multi)

# alpha = 0.0734 
# beta = 0.0096 
# gamma = 0.0001 

# a.) model 1:
model1 <- lm(N ~ avg_temp + avg_pressure, 
             data = data_train)
model1
summary(model1) # aR2=0.50; F=37.8; 

## Test:
y_test <- data_test[, "N"] # the actual values of the target variable in the test dataset
y_test
pred_y_model1 <- forecast(model1, newdata = data_test %>% tk_tbl())
pred_y_model1 # predicted y values for the testing dataset

# R-squared: 
cor(y_test, pred_y_model1$mean)^2 # R2=0.86 - fair accuracy accuracy on the testing set

# Adjusted R-squared:
summary(lm(y_test~pred_y_model1$mean))$adj.r.squared # 0.85 

# b.) model 2:

model2 <- lm(N ~ avg_temp + avg_pressure + employment, 
             data = data_train)
model2
summary(model2) # aR2=0.87; F=137; 

## Test:
y_test <- data_test[, "N"] 
y_test
pred_y_model2 <- forecast(model2, newdata = data_test %>% tk_tbl())
pred_y_model2 # predicted y values for the testing dataset

# R-squared: 
cor(y_test, pred_y_model2$mean)^2 # R2=0.92 - Much better accuracy!

# Adjusted R-squared:
summary(lm(y_test~pred_y_model2$mean))$adj.r.squared # 0.92

# c.) model 3:

arima <- auto.arima(training_data)
summary(arima)
# ARIMA(0,1,3)(0,1,0)

pred_arima<-(forecast(arima, h=12))


# Forecast accuracy of all models
forecast::accuracy(hw_multi, testing_data)
forecast::accuracy(pred_y_model1, testing_data)
forecast::accuracy(pred_y_model2, testing_data)
summary(arima)
# It seems that hw_multi is still the best fit of the models!

# Convert prediction variables to TS
pred_y_model1_ts <- ts(as.vector(pred_y_model1$mean), 
                       start = c(2017, 1), frequency = 12)
pred_y_model1_ts


pred_y_model2_ts <- ts(as.vector(pred_y_model2$mean), 
                       start = c(2017, 1), frequency = 12)
pred_y_model2_ts

# Compare the above models by plotting forecasts against test data:
autoplot(data_train[, "N"]) +
  autolayer(data_test[, "N"], 
            series = "Actual test values", lwd = 1.2) +
  autolayer(pred_y_model1_ts, 
            series = "Predicted values - linear model 1", 
            alpha = 0.4, lwd = 0.8) +
  autolayer(pred_y_model2_ts, 
            series = "Predicted values - linear model 2 - reduced", 
            alpha = 0.4, lwd = 0.8) + 
  autolayer(pred_y_model3$mean, 
            series = "Predicted values - linear model 3 \nwith trend and seasonality", 
            alpha = 0.4, lwd = 0.8) + 
  autolayer(pred_y_model4$mean, 
            series = "Predicted values - non-linear model 4", 
            alpha = 0.4, lwd = 0.8) + 
  ggtitle("Forecasts of no. of trips from linear and non-linear regression models.") +
  xlab("Date") + ylab("No. of trips") +
  guides(colour=guide_legend(title = "Series:", nrow = 2)) +
  theme_main

# Create an interactive visualization of predictions:
hchart(training_data, name = "Training set") %>%
  hc_add_series(testing_data,  name = "Testing set") %>%
  hc_add_series(hw_multi, name = "Multiplicative seasonal Holt-Winters") %>%
  hc_add_series(pred_y_model1_ts, name = "Linear model 1") %>%
  hc_add_series(pred_y_model2_ts, name = "Linear model 2") %>%
  hc_add_series(pred_arima, name = "ARIMA (0,1,3)") %>%
  hc_xAxis(title = list(text = "<b>Date</b>")) %>%
  hc_yAxis(title = list(text = "<b>No. of trips</b>")) %>%
  hc_title(text = "Predictions of no. of trips from different models", 
           align = "left") %>% 
  hc_subtitle(text = "<i>Data source: Capital BikeShare from https://www.capitalbikeshare.com/system-data</i>", 
              align = "left") %>%
  hc_credits(enabled = TRUE,
             text = "KOBRA and ROyal Tenenbaums",
             href = "www.KRT.NZ") %>%
  hc_tooltip(crosshairs = TRUE, shared = TRUE, 
             valueDecimals = 2,
             headerFormat = "<b>Date:</b> {point.key}<br>") %>%
  hc_plotOptions(line = list(states = list(hover = list(enabled = TRUE, lineWidth = 5)), 
                             lineWidth = 3), 
                 series = list(marker = list(enabled = FALSE, 
                                             states = list(hover = list(radius = 3))))) %>%
  hc_chart(zoomType = 'xy') %>%
  hc_add_theme(hc_theme_darkunica()) %>%
  hc_exporting(enabled = TRUE)
