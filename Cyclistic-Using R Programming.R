#Lets begin by first loading our packages for cleaning and analyzing
install.packages("tidyverse")
library(tidyverse)
install.packages("skimr")
library(skimr)
install.packages("geosphere")
library(geosphere)
library(readr)
library(lubridate)
install.packages("geodist")
library(geodist)
install.packages("formattable")
library(formattable)


getwd()

#Time to import the datasets(12)

data1 <- read_csv("C:\\Users\\User\\Documents\\Cyclistic Data\\202104-divvy-tripdata.csv")
data2 <- read_csv("C:\\Users\\User\\Documents\\Cyclistic Data\\202105-divvy-tripdata.csv")
data3 <- read_csv("C:\\Users\\User\\Documents\\Cyclistic Data\\202106-divvy-tripdata.csv")
data4 <- read_csv("C:\\Users\\User\\Documents\\Cyclistic Data\\202107-divvy-tripdata.csv")
data5 <- read_csv("C:\\Users\\User\\Documents\\Cyclistic Data\\202108-divvy-tripdata.csv")
data6 <- read_csv("C:\\Users\\User\\Documents\\Cyclistic Data\\202109-divvy-tripdata.csv")
data7 <- read_csv("C:\\Users\\User\\Documents\\Cyclistic Data\\202110-divvy-tripdata.csv")
data8 <- read_csv("C:\\Users\\User\\Documents\\Cyclistic Data\\202111-divvy-tripdata.csv")
data9 <- read_csv("C:\\Users\\User\\Documents\\Cyclistic Data\\202112-divvy-tripdata.csv")
data10 <- read_csv("C:\\Users\\User\\Documents\\Cyclistic Data\\202201-divvy-tripdata.csv")
data11 <- read_csv("C:\\Users\\User\\Documents\\Cyclistic Data\\202202-divvy-tripdata.csv")
data12 <- read_csv("C:\\Users\\User\\Documents\\Cyclistic Data\\202203-divvy-tripdata.csv")


#Lets merge the datasets for efficient analyzation
Merged_Trips <- bind_rows(data1,data2,data3,data4,data5,data6,data7,data8,data9,data10,data11,data12)

#Ready to view the merged datasets
summary(Merged_Trips)
str(Merged_Trips)

#Rename some columns
Merged_Trips <- rename(Merged_Trips, "user_type" = "member_casual")
Merged_Trips <- rename(Merged_Trips, "bike_type" = "rideable_type")

#Remove NA values
Merged_Trips <- drop_na(Merged_Trips)

#Create a new column,(header = ride_length) extracted from start_at and ended_ at columns in minutes
Merged_Trips$ride_length <- (as.double(difftime(Merged_Trips$ended_at, Merged_Trips$started_at)))/60


#Create a new column,(header = ride_distance) in kilometers
Merged_Trips$ride_distance <- (distGeo(matrix(c(Merged_Trips$start_lng, Merged_Trips$start_lat),ncol = 2),matrix(c(Merged_Trips$end_lng, Merged_Trips$end_lat), ncol = 2))/1000)

#Create a new column,(header = hour_started) 24 hour clock format
Merged_Trips$hour_started <- as.numeric(format(as.POSIXct(Merged_Trips$started_at), "%H")) 

#Create a new column, (header = day_of_week)
Merged_Trips$day_of_week <- weekdays(as.Date(Merged_Trips$started_at))

#Create new column (header = month)
Merged_Trips$months <- months(as.Date(Merged_Trips$started_at))

# Accidently added an 's" to the end of the month so lets correct that
Merged_Trips <- rename(Merged_Trips, "month" = "months")

#Create a new column,(header = year) extracted from start_at column
Merged_Trips$year <- format(as.Date(Merged_Trips$started_at, format = "%Y-%m-%d %h:%m:%s"), "%Y")

#ride_length max is 55944, delete ride_length greater than 1 day(1440 minutes)
Merged_Trips <- Merged_Trips[!Merged_Trips$ride_length > 1440,]

#ride_length min. is -55.90, delete ride_length less than less than 0
Merged_Trips <- Merged_Trips[!Merged_Trips$ride_length < 0,]


head(Merged_Trips)
> str(Merged_Trips)
tibble [4,640,068 × 19] (S3: tbl_df/tbl/data.frame)
 $ ride_id           : chr [1:4640068] "6C992BD37A98A63F" "1E0145613A209000" "1887262AD101C604" "C123548CAB2A32A5" ...
 $ bike_type         : chr [1:4640068] "classic_bike" "docked_bike" "classic_bike" "docked_bike" ...
 $ started_at        : POSIXct[1:4640068], format: "2021-04-12 18:25:36" "2021-04-27 17:27:11" "2021-04-17 09:17:42" ...
 $ ended_at          : POSIXct[1:4640068], format: "2021-04-12 18:56:55" "2021-04-27 18:31:29" "2021-04-17 09:42:48" ...
 $ start_station_name: chr [1:4640068] "State St & Pearson St" "Dorchester Ave & 49th St" "Honore St & Division St" "Loomis Blvd & 84th St" ...
 $ start_station_id  : chr [1:4640068] "TA1307000061" "KA1503000069" "TA1305000034" "20121" ...
 $ end_station_name  : chr [1:4640068] "Southport Ave & Waveland Ave" "Dorchester Ave & 49th St" "Southport Ave & Waveland Ave" "Loomis Blvd & 84th St" ...
 $ end_station_id    : chr [1:4640068] "13235" "KA1503000069" "13235" "20121" ...
 $ start_lat         : num [1:4640068] 41.9 41.8 41.9 41.7 41.9 ...
 $ start_lng         : num [1:4640068] -87.6 -87.6 -87.7 -87.7 -87.6 ...
 $ end_lat           : num [1:4640068] 41.9 41.8 41.9 41.7 41.9 ...
 $ end_lng           : num [1:4640068] -87.7 -87.6 -87.7 -87.7 -87.6 ...
 $ user_type         : chr [1:4640068] "member" "casual" "member" "casual" ...
 $ ride_length       : num [1:4640068] 31.317 64.3 25.1 91.283 0.683 ...
 $ ride_distance     : num [1:4640068] 6.34 0 5.07 0 0 ...
 $ hour_started      : num [1:4640068] 18 17 9 12 18 16 16 15 15 18 ...
 $ day_of_week       : chr [1:4640068] "Monday" "Tuesday" "Saturday" "Saturday" ...
 $ month             : chr [1:4640068] "April" "April" "April" "April" ...
 $ year              : chr [1:4640068] "2021" "2021" "2021" "2021" ...


#Merged datasets have been cleaned, it is time to start analyzing.

#Count per user type
user_type_count <- Merged_Trips %>%
  group_by(user_type) %>%
  summarise(count = n()) %>%
  mutate(percentage = formattable:: percent(count / sum(count))) %>% 
  arrange(desc(percentage))
	
View(user_type_count)


#Bike type count per user type
bike_type_count <- Merged_Trips %>%
	group_by(bike_type, user_type) %>%
	summarise(count = n()) %>%
  mutate(percentage = formattable:: percent(count / sum(count(user_type)))
  

View(bike_type_count)


#Average ride time per user type
avg_ride_time <- Merged_Trips %>%
	group_by(user_type) %>%
	summarise(average_time = round(mean(ride_length), digits = 0)) %>%
	arrange(user_type)

View(avg_ride_time)


#Average ride_distance per user type and bike type
avg_ride_distance <- Merged_Trips %>%
	group_by(user_type, bike_type) %>%
	summarise(average_distance = round(mean(ride_distance),digits = 2)) %>%
	arrange(user_type)

View(avg_ride_distance)

#Average ride time and distance per day of the week by user type
count_day_of_week <- Merged_Trips %>%
	group_by(day_of_week, user_type) %>%
	summarise( count = n(), avg_time = round(mean(ride_length), digits = 0), avg_distance = round(mean(ride_distance),digits = 2)) 

count_day_of_week$day_of_week <- ordered(count_day_of_week$day_of_week, levels=c("Monday", "Tuesday", "Wednesday", "Thursday", 
"Friday", "Saturday", "Sunday"))

count_day_of_week[order(count_day_of_week$day_of_week), ]
	
	
View(count_day_of_week)


#Average ride time and distance per hour by user type
rides_per_hour <- Merged_Trips %>%
	group_by(hour_started, user_type) %>%
	summarise( count = n(), avg_time = round(mean(ride_length), digits = 0), avg_distance = round(mean(ride_distance),digits = 2)) %>%
	arrange(hour_started)


View(rides_per_hour)


#Average ride time and distance per month by user type
rides_per_month <- Merged_Trips %>%
	group_by(month, user_type) %>%
	summarise( count = n(), avg_time = round(mean(ride_length), digits = 0), avg_distance = round(mean(ride_distance),digits = 2)) 

rides_per_month$month <- ordered(rides_per_month$month, levels=c("January", "February", "March", "April","May","June","July","August","September","October","November","December"))

rides_per_month[order(rides_per_month$month), ]


View(rides_per_month)


#Analyzation Phase is complete, it is now time to create visuals to share to the company stakeholders.

library(ggplot2)

#Visual of riders per user type

user_type_count %>%
	group_by(user_type, percentage) %>%
	ggplot(aes(x = user_type, y = count, fill = user_type)) +
	geom_bar(stat = 'identity') +
	scale_fill_manual(values = c("casual" = "red", "member" = "#009933")) +
	geom_text(aes(label = percentage), vjust = 1.5) +
	labs(title = 'Number of Rides per User Type', x = 'Customer User Type', y = 'Number of Rides')


#Visual of the different bike types by user

bike_type_count %>%
	group_by(bike_type, user_type, percentage) %>%
	ggplot(aes(x = bike_type, y = count, fill = user_type, group = user_type)) +
	geom_col(position = "dodge2") + 
	scale_fill_manual(values = c("casual" = "red", "member" = "#009933")) +
	geom_text(aes(label = percentage), vjust = 3, position = position_dodge(.9)) +
	labs(title = 'Bike Type by Customer User Type', x = 'Customer User Type', y = 'Count')


#Visuals of the average ride time and distance per day of the week by user type

count_day_of_week %>%
	group_by(day_of_week, user_type)%>%
	ggplot(aes(x = day_of_week, y = avg_time, colour = user_type, group = user_type, fill = user_type)) +
	geom_line() +
	scale_fill_manual(values = c("casual" = "red", "member" = "#009933")) +
	geom_point(size = 4, shape = 21) +
	labs(title = 'Average Ride Time per Customer User Type and Day of the Week', x = 'Customer User Type', y = 'Average Time') 
	

count_day_of_week %>%
	group_by(day_of_week, user_type)%>%
	ggplot(aes(x = day_of_week, y = avg_distance, colour = user_type, group = user_type, fill = user_type)) +
	geom_line() +
	scale_fill_manual(values = c("casual" = "red", "member" = "#009933")) +
	geom_point(size = 4, shape = 21) +
	labs(title = 'Average Ride Distance per Customer User Type and Day of the Week', x = 'Customer User Type', y = 'Average Distance') 


#Visuals of the average ride time and distance per hour of the day by user type

rides_per_hour %>%
	group_by(hour_started, user_type)%>%
	ggplot(aes(x = hour_started, y = avg_time, colour = user_type, group = user_type, fill = user_type)) +
	geom_line() +
	scale_fill_manual(values = c("casual" = "red", "member" = "#009933")) +
	geom_point(size = 4, shape = 21) +
	plot(rides_per_hour$hour_started, rides_per_hour$avg_time,pch =19, xlim = c(0,23), ylim = c(0,31)) +
	labs(title = 'Average Ride Time per Hour by Customer User Type', x = 'Hour(Military Time)', y = 'Average Time') 
	
	

rides_per_hour %>%
	group_by(hour_started, user_type)%>%
	ggplot(aes(x = hour_started, y = avg_distance, colour = user_type, group = user_type, fill = user_type)) +
	geom_line() +
	scale_fill_manual(values = c("casual" = "red", "member" = "#009933")) +
	geom_point(size = 4, shape = 21) +
	plot(rides_per_hour$hour_started, rides_per_hour$avg_time,pch =19, xlim = c(0,23), ylim = c(0,31)) +
	labs(title = 'Average Ride Distance per Hour by Customer User Type', x = 'Hour(Military Time)', y = 'Average Distance') 


#Visuals of the average ride time and distance per month by user type

rides_per_month %>%
	group_by(month, user_type)%>%
	ggplot(aes(x = month, y = avg_time, colour = user_type, group = user_type, fill = user_type)) +
	geom_line() +
	scale_fill_manual(values = c("casual" = "red", "member" = "#009933")) +
	geom_point(size = 4, shape = 21) +
	labs(title = 'Average Ride Time per Month by Customer User Type', x = 'Month', y = 'Average Time') 
	

rides_per_month %>%
	group_by(month, user_type)%>%
	ggplot(aes(x = month, y = avg_distance, colour = user_type, group = user_type, fill = user_type)) +
	geom_line() +
	scale_fill_manual(values = c("casual" = "red", "member" = "#009933")) +
	geom_point(size = 4, shape = 21) +
	labs(title = 'Average Ride Distance per Month by Customer User Type', x = 'Month', y = 'Average Distance') 
	
	
	





