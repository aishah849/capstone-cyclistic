library(tidyverse)
library(lubridate)

#load .csv files, a years worth of data from April 2022 to March 2023
apr_2022 <- read_csv("~/Desktop/Capstone/Dataset/Copy/202204-divvy-tripdata.csv")
may_2022 <- read_csv("~/Desktop/Capstone/Dataset/Copy/202205-divvy-tripdata.csv")
jun_2022 <- read_csv("~/Desktop/Capstone/Dataset/Copy/202206-divvy-tripdata.csv")
jul_2022 <- read_csv("~/Desktop/Capstone/Dataset/Copy/202207-divvy-tripdata.csv")
aug_2022 <- read_csv("~/Desktop/Capstone/Dataset/Copy/202208-divvy-tripdata.csv")
sep_2022 <- read_csv("~/Desktop/Capstone/Dataset/Copy/202209-divvy-publictripdata.csv")
oct_2022 <- read_csv("~/Desktop/Capstone/Dataset/Copy/202210-divvy-tripdata.csv")
nov_2022 <- read_csv("~/Desktop/Capstone/Dataset/Copy/202211-divvy-tripdata.csv")
dec_2022 <- read_csv("~/Desktop/Capstone/Dataset/Copy/202212-divvy-tripdata.csv")
jan_2023 <- read_csv("~/Desktop/Capstone/Dataset/Copy/202301-divvy-tripdata.csv")
feb_2023 <- read_csv("~/Desktop/Capstone/Dataset/Copy/202302-divvy-tripdata.csv")
mar_2023 <- read_csv("~/Desktop/Capstone/Dataset/Copy/202303-divvy-tripdata.csv")

#combine all 12 data in 1 dataframe
all_trips<-bind_rows(apr_2022, may_2022, jun_2022, jul_2022, aug_2022, sep_2022, oct_2022, nov_2022, dec_2022, jan_2023, feb_2023, mar_2023)
remove(apr_2022, may_2022, jun_2022, jul_2022, aug_2022, sep_2022, oct_2022, nov_2022, dec_2022, jan_2023, feb_2023, mar_2023)

#removing columns
all_trips <- all_trips%>%
  select(-c(start_lat, start_lng, end_lat, end_lng))

#renaming columns
all_trips <- rename(all_trips, bike_type=rideable_type, start_time=started_at, end_time=ended_at, from_station_name=start_station_name, from_station_id=start_station_id, to_station_name=end_station_name, to_station_id=end_station_id, user_type=member_casual)

#create column for trip duration (Ride)
all_trips$ride_length <- difftime(all_trips$end_time, all_trips$start_time)

# Convert "ride_length" from Factor to numeric so we can run calculations on the data
all_trips$ride_length <- as.numeric(as.character(all_trips$ride_length))

#data cleaning
all_trips <- na.omit(all_trips) #remove rows with NA values
all_trips <- distinct(all_trips) #remove duplicate rows
all_trips <- all_trips[!(all_trips$ride_length<300),]#remove trip duration <300secs

#create columns for: date, day of week, day, month, year, time, hour
all_trips$date <- as.Date(all_trips$start_time) #created date column
all_trips$day_of_week <- format(as.Date(all_trips$date), "%A") #created day of week column
all_trips$day <- format(as.Date(all_trips$date), "%d") #created day of month column
all_trips$month <- format(as.Date(all_trips$date), "%B") #created month column
all_trips$year <- format(as.Date(all_trips$date), "%Y") #created year column
all_trips$time <- format(as.Date(all_trips$date), "%H:%M:%S") #format time as HH:MM:SS
all_trips$time <- as_hms((all_trips$start_time)) #created time column
all_trips$hour <- hour(all_trips$time) #create hour column

#create column for different time_of_day: Night, Morning, Afternoon, Evening
all_trips <-all_trips %>%
  mutate(time_of_day =
           case_when(hour == "0" ~ "Night", hour == "1" ~ "Night", hour == "2" ~ "Night", hour == "3" ~ "Night", hour == "4" ~ "Night", hour == "5" ~ "Morning", hour == "6" ~ "Morning", hour == "7" ~ "Morning", hour == "8" ~ "Morning", hour == "9" ~ "Morning", hour == "10" ~ "Morning", hour == "11" ~ "Morning", hour == "12" ~ "Afternoon", hour == "13" ~ "Afternoon", hour == "14" ~ "Afternoon", hour == "15" ~ "Afternoon", hour == "16" ~ "Afternoon", hour == "17" ~ "Afternoon", hour == "18" ~ "Evening", hour == "19" ~ "Evening", hour == "20" ~ "Evening", hour == "21" ~ "Night", hour == "22" ~ "Night", hour == "23" ~ "Night"))

#create column for different seasons: Spring, Summer, Fall, Winter
all_trips <-all_trips %>% 
  mutate(season = case_when(month == "March" ~ "Spring", month == "April" ~ "Spring", month == "May" ~ "Spring", month == "June"  ~ "Summer", month == "July"  ~ "Summer", month == "August"  ~ "Summer", month == "September" ~ "Fall", month == "October" ~ "Fall", month == "November" ~ "Fall", month == "December" ~ "Winter", month == "January" ~ "Winter", month == "February" ~ "Winter"))

#view the final data
View(all_trips)

#total number of rides
nrow(all_trips)

#-----------------MEMBER TYPE---------------------
all_trips %>%
  group_by(user_type) %>% 
  count(user_type)
cyclistic_userType<-all_trips %>%
  group_by(user_type) %>% 
  count(user_type)

#----------------TYPE OF BIKE---------------------
#total rides 
all_trips %>%
  group_by(bike_type) %>% 
  count(bike_type)

#total rides by member type 
all_trips %>%
  group_by(user_type, bike_type) %>% 
  count(bike_type)

#-------------------HOUR--------------------------
#total rides by member type 
all_trips %>%
  group_by(user_type) %>% 
  count(hour) %>% 
  print(n = 48)

#total rides
all_trips %>%
  count(hour) %>% 
  print(n = 24)

#----------------------TIME OF DAY-----------------------
#-----morning-------
#total rides
all_trips %>%
  filter(time_of_day == "Morning") %>% 
  count(time_of_day)

#total rides by member type 
all_trips %>%
  group_by(user_type) %>% 
  filter(time_of_day == "Morning") %>% 
  count(time_of_day)

#-----afternoon-------
#total rides 
all_trips %>%
  filter(time_of_day == "Afternoon") %>% 
  count(time_of_day)

#total rides by member type 
all_trips %>%
  group_by(user_type) %>% 
  filter(time_of_day == "Afternoon") %>% 
  count(time_of_day)

#-----evening-------
#total rides
all_trips %>%
  filter(time_of_day == "Evening") %>% 
  count(time_of_day)

#total rides by member type
all_trips %>%
  group_by(user_type) %>% 
  filter(time_of_day == "Evening") %>% 
  count(time_of_day)

#-----night-------
#number of rides 
all_trips %>%
  filter(time_of_day == "Night") %>% 
  count(time_of_day)

#number of rides by member type
all_trips %>%
  group_by(user_type) %>% 
  filter(time_of_day == "Night") %>% 
  count(time_of_day)

#---all times of day----
#number of rides
all_trips %>%
  group_by(time_of_day) %>% 
  count(time_of_day)

#total rides by member type 
all_trips %>%
  group_by(user_type) %>% 
  count(time_of_day)

#----------------DAY OF THE WEEK------------------
#total rides 
all_trips %>%
  count(day_of_week)

#total rides by member type
all_trips %>%
  group_by(user_type) %>% 
  count(day_of_week)


#----------------DAY OF THE MONTH-----------------
#total rides
all_trips %>%
  count(day) %>% 
  print(n = 31)

#total rides by member type
all_trips %>%
  group_by(user_type) %>% 
  count(day) %>% 
  print(n = 62)


#---------------------MONTH-----------------------
#total rides
all_trips %>%
  count(month) 

#total rides by member type 
all_trips %>%
  group_by(user_type) %>% 
  count(month) %>% 
  print(n = 24)

#--------------------SEASON-----------------------
#-----spring-------
#total rides
all_trips %>%
  filter(season == "Spring") %>% 
  count(season)

#total rides by member type 
all_trips %>%
  group_by(user_type) %>% 
  filter(season == "Spring") %>% 
  count(season)

#-----summer-------
#total rides
all_trips %>%
  filter(season == "Summer") %>% 
  count(season)

#total rides by member type
all_trips %>%
  group_by(user_type) %>% 
  filter(season == "Summer") %>% 
  count(season)

#-----fall-------
#total rides
all_trips %>%
  filter(season == "Fall") %>% 
  count(season)

#total rides by member type
all_trips %>%
  group_by(user_type) %>% 
  filter(season == "Fall") %>% 
  count(season)

#-----winter-------
#total rides 
all_trips %>%
  filter(season == "Winter") %>% 
  count(season)

#total rides by member type
all_trips %>%
  group_by(user_type) %>% 
  filter(season == "Winter") %>% 
  count(season)

#-----all seasons-------
#total rides
all_trips %>%
  group_by(season) %>% 
  count(season)

#total rides by member type
all_trips %>%
  group_by(season, user_type) %>% 
  count(season)

#------------------------------------AVERAGE RIDE LENGTH-----------------------------------
#average of ride_length
summary(all_trips$ride_length)
cyclistic_avgRide <- mean(all_trips$ride_length)
print(cyclistic_avgRide)

#------------------MEMBER TYPE--------------------
#average ride_length
all_trips %>% group_by(user_type) %>% 
  summarise_at(vars(ride_length),
               list(time = mean))


# Compare members and casual users
aggregate(all_trips$ride_length ~ all_trips$user_type, FUN = mean)
aggregate(all_trips$ride_length ~ all_trips$user_type, FUN = median)
aggregate(all_trips$ride_length ~ all_trips$user_type, FUN = max)
aggregate(all_trips$ride_length ~ all_trips$user_type, FUN = min)

#----------------TYPE OF BIKE---------------------
#average ride_length
all_trips %>% group_by(bike_type) %>% 
  summarise_at(vars(ride_length),
               list(time = mean))

#total rides by member type 
all_trips %>% group_by(user_type, bike_type) %>% 
  summarise_at(vars(ride_length),
               list(time = mean))

#-----------------------HOUR-------------------------
#average ride_length
all_trips %>% group_by(hour) %>% 
  summarise_at(vars(ride_length),
               list(time = mean)) %>% 
  print(n=24)

#average ride_length by member type
all_trips %>% group_by(hour, user_type) %>% 
  summarise_at(vars(ride_length),
               list(time = mean)) %>% 
  print(n=48)

#--------------------TIME OF DAY---------------------
#----morning----
#average ride length
all_trips %>% 
  filter(time_of_day == "Morning") %>% 
  summarise_at(vars(ride_length),
               list(time = mean))

#average ride length by member type
all_trips %>% 
  group_by(user_type) %>% 
  filter(time_of_day == "Morning") %>% 
  summarise_at(vars(ride_length),
               list(time = mean))

#----afternoon----
#average ride length
all_trips %>% 
  filter(time_of_day == "Afternoon") %>% 
  summarise_at(vars(ride_length),
               list(time = mean))

#average ride length by member type
all_trips %>% 
  group_by(user_type) %>% 
  filter(time_of_day == "Afternoon") %>% 
  summarise_at(vars(ride_length),
               list(time = mean))

#----evening----
#average ride length
all_trips %>% 
  filter(time_of_day == "Evening") %>% 
  summarise_at(vars(ride_length),
               list(time = mean))

#average ride length by member type
all_trips %>% 
  group_by(user_type) %>% 
  filter(time_of_day == "Evening") %>% 
  summarise_at(vars(ride_length),
               list(time = mean))

#----night----
#average ride length
all_trips %>% 
  filter(time_of_day == "Night") %>% 
  summarise_at(vars(ride_length),
               list(time = mean))

#average ride length by member type 
all_trips %>% 
  group_by(user_type) %>% 
  filter(time_of_day == "Night") %>% 
  summarise_at(vars(ride_length),
               list(time = mean))

#---all times of day---
#average ride length
all_trips %>% 
  group_by(time_of_day) %>% 
  summarise_at(vars(ride_length),
               list(time = mean))

#average ride length by member type
all_trips %>% 
  group_by(time_of_day, user_type) %>% 
  summarise_at(vars(ride_length),
               list(time = mean))

#-------------------DAY OF THE WEEK-----------------
#average ride_length 
all_trips %>% group_by(day_of_week) %>% 
  summarise_at(vars(ride_length),
               list(time = mean))

#average ride_length by member type
all_trips %>% group_by(day_of_week, user_type,) %>% 
  summarise_at(vars(ride_length),
               list(time = mean))

#-----------------DAY OF THE MONTH------------------
#average ride_length
all_trips %>% group_by(day) %>% 
  summarise_at(vars(ride_length),
               list(time = mean)) %>% 
  print(n=31)

#average ride_length by member type
all_trips %>% group_by(day, user_type) %>% 
  summarise_at(vars(ride_length),
               list(time = mean)) %>% 
  print(n=62)

#---------------------MONTH--------------------------
#average ride_length
all_trips %>% group_by(month) %>% 
  summarise_at(vars(ride_length),
               list(time = mean))

#average ride_length by member type
all_trips %>% group_by(month, user_type) %>% 
  summarise_at(vars(ride_length),
               list(time = mean)) %>% 
  print(n=24)

#----------------------SEASON-------------------------
#-----spring------
#average ride length
all_trips %>% 
  filter(season == "Spring") %>% 
  summarise_at(vars(ride_length),
               list(time = mean))

#average ride length by member type
all_trips %>% 
  group_by(user_type) %>% 
  filter(season == "Spring") %>% 
  summarise_at(vars(ride_length),
               list(time = mean))

#-----summer------
#average ride length for summer 
all_trips %>% 
  filter(season == "Summer") %>% 
  summarise_at(vars(ride_length),
               list(time = mean))

#average ride length by member type for summer 
all_trips %>% 
  group_by(user_type) %>% 
  filter(season == "Summer") %>% 
  summarise_at(vars(ride_length),
               list(time = mean))

#-----fall------
#average ride length
all_trips %>% 
  filter(season == "Fall") %>% 
  summarise_at(vars(ride_length),
               list(time = mean))

#average ride length by member type
all_trips %>% 
  group_by(user_type) %>% 
  filter(season == "Fall") %>% 
  summarise_at(vars(ride_length),
               list(time = mean))

#-----winter-----
#average ride length
all_trips %>% 
  filter(season == "Winter") %>% 
  summarise_at(vars(ride_length),
               list(time = mean))

#average ride length by member type
all_trips %>% 
  group_by(user_type) %>% 
  filter(season == "Winter") %>% 
  summarise_at(vars(ride_length),
               list(time = mean))

#----all seasons----
#average ride length 
all_trips %>% 
  group_by(season) %>% 
  summarise_at(vars(ride_length),
               list(time = mean))

#average ride length by member type
all_trips %>% 
  group_by(season, user_type) %>% 
  summarise_at(vars(ride_length),
               list(time = mean))
#------------------------------------

#average ride_length by usertype
avgRideLength_userType <-
  all_trips %>% group_by(user_type) %>% 
  summarise_at(vars(ride_length),
               list(time = mean))

#analyze ridership data by type hourly
avgRideLength_hour <-
  all_trips %>%
  group_by(user_type, hour) %>%
  summarise(number_of_rides = n(), average_duration = mean(ride_length)) %>%
  arrange(hour, user_type)

#analyze ridership data by type and time of day
avgRideLength_timeOfDay <-
  all_trips %>%
  group_by(user_type, time_of_day) %>%
  summarise(number_of_rides = n(), average_duration = mean(ride_length)) %>%
  arrange(time_of_day, user_type)

#analyze ridership data by type and weekday
avgRideLength_dayOfWeek <-
  all_trips %>%
  group_by(user_type, day_of_week) %>%
  summarise(number_of_rides = n(), average_duration = mean(ride_length)) %>%
  arrange(day_of_week,user_type)


#analyze ridership data by type monthly
avgRideLength_month <-
  all_trips %>%
  group_by(user_type, month) %>%
  summarise(number_of_rides = n(), average_duration = mean(ride_length)) %>%
  arrange(month, user_type)

#analyze ridership data by type and season
avgRideLength_season <-
  all_trips %>%
  group_by(user_type, season) %>%
  summarise(number_of_rides = n(), average_duration = mean(ride_length)) %>%
  arrange(season, user_type)

#visualize the number of rides by rider type
all_trips %>%
  group_by(user_type, day_of_week) %>%
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>%
  arrange(user_type, day_of_week) %>%
  ggplot(aes(x = day_of_week, y = number_of_rides, fill = user_type)) +
  geom_col(position = "dodge")

#visualization for average duration
all_trips %>%
  group_by(user_type, day_of_week) %>%
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>%
  arrange(user_type, day_of_week) %>%
  ggplot(aes(x = day_of_week, y = average_duration, fill = user_type)) +
  geom_col(position = "dodge")

#export dataframes
write_csv(all_trips,"cyclistic_12months.csv")
write_csv(avgRideLength_userType,"cyclistic_avgRideLength.csv")
write_csv(avgRideLength_dayOfWeek,"cyclistic_dayOfWeek.csv")
write_csv(avgRideLength_hour,"cyclistic_hourly.csv")
write_csv(avgRideLength_timeOfDay,"cyclistic_timeOfDay.csv")
write_csv(avgRideLength_month,"cyclistic_monthly.csv")
write_csv(avgRideLength_season,"cyclistic_seasons.csv")
write_csv(cyclistic_userType,"cyclistic_userType.csv")
