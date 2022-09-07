library(tidyverse)
library(lubridate)
library(ggplot2)
library(readr)
library(tidyr)
library(dplyr)
library(skimr)
library(janitor)
library(scales)

#load file
dailyActivity_merged <- read.csv("dailyActivity_merged.csv")
weightLogInfo_merged <- read.csv("weightLogInfo_merged.csv")
sleep_daily <- read.csv("sleepDay_merged.csv")
hour_step <- read.csv("hourlySteps_merged.csv")


#dataset exploration
str(weightLogInfo_merged) #all right type of data
weightLogInfo_merged <- weightLogInfo_merged[!duplicated(weightLogInfo_merged$Id), ] #number of duplicates
sum(weightLogInfo_merged$WeightKg > quantile(weightLogInfo_merged$WeightKg , .95)) #outliarer data points
sum(is.na(weightLogInfo_merged)) #number of na in table
length(weightLogInfo_merged[weightLogInfo_merged==FALSE]) #number of false in column

daily_activity <- dailyActivity_merged %>% ##create new table from Daily Activity
  filter(TotalSteps !=0) ## filter out users without data

weight <- weightLogInfo_merged %>% ##create new table to separate out time
  separate(Date, c("Date", "Time"), " ")

sleep <- sleep_daily %>% ##create new table to separate out time
  separate(SleepDay, c("Date", "Time"), " ")

library(lubridate)
#look for trends between step time
hour_step$ActivityHour <- mdy_hms(hour_step$ActivityHour) #convert 12 to 24 hour
hr_step <- hour_step %>% ##create new table to separate out time
  separate(ActivityHour, c("year", "month", "day", "Hour", "minutes", "seconds")) 
#calculates average steps per hour
new_hr_step <- group_by(hr_step, Hour) %>% summarize(Averaged_Step = mean(StepTotal))
hr_steps <- new_hr_step %>% #making condition for bar graph
  mutate(condition=if_else(Averaged_Step>500, "Noon to 2pm & 5~7pm are the most active time according to users' step average.","More than 500 steps per hour"))

#Graph AverageStep VS Hour
AverageStepVSHour <- ggplot(data=hr_steps, aes(x=Hour, y=Averaged_Step, fill=condition)) + 
  geom_bar(stat="identity", width=0.5) + 
  scale_y_continuous(labels = scales::comma) +
  scale_fill_manual(values=c("grey","#f55e61")) +
  theme(legend.position="bottom", plot.title = element_text(color="#f55e61", size=14, face="bold")) + 
  ggtitle("Average Steps by Hour")
                    
#There is a trend with total steps vs Sedentary time
ggplot(data=daily_activity, aes(x=TotalSteps, y=SedentaryMinutes)) + geom_point()

# combine daily activities and sleep log to find relationship
combined_data <- merge(sleep, daily_activity, by="Id")
n_distinct(combined_data$Id)

#There is no relationship between Sedentary time vs sleep time
ggplot(data=combined_data, aes(x=SedentaryMinutes, y=TotalMinutesAsleep)) + geom_point()

frequency = daily_activity %>% dplyr::count(Id) #number of the users (33 unique users)
frequency %>% dplyr::count(n) #only 20/33 users has activities everyday

weight %>% dplyr::count(Id) #only 8/33 users log in weight & only 2 log in more than 20/31 days

sleep_freq = sleep_daily %>% dplyr::count(Id) #only 24/33 user wear to sleep for log
sleep_freq %>% dplyr::count(n) #only 14/33 wear to sleep more than 20/31 days
##conclusion1: more product to link to the user member create better picture - have a weight scale product linked like waterbottle 

#Finding relationship between sleep and activte time
ggplot(data=combined_data, aes(x=VeryActiveDistance, y=TotalMinutesAsleep)) + geom_point()+geom_smooth(method=lm, se=FALSE) 
#conclusion2: more sleep may result more active time. Encourage users to wear device to sleep
# and promote the importance to see the full picture of the users' life style
