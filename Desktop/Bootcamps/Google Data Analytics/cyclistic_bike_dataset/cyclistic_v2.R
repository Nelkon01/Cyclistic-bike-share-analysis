###import libraries
#manipulate data
library(tidyverse)

#date library
library(lubridate)

#data viz
library(ggplot2)

setwd("/Users/nelkon/Desktop/cyclistic_bike_dataset/Uncleaned")
#data upload
q1_2018 <-read_csv("Divvy_Trips_2018_Q1.csv")
q2_2018 <-read_csv("Divvy_Trips_2018_Q2.csv")
q3_2018 <-read_csv("Divvy_Trips_2018_Q3.csv")
q4_2018 <-read_csv("Divvy_Trips_2018_Q4.csv")

#2019 Data Upload
q1_2019 <-read_csv("Divvy_Trips_2019_Q1.csv")
q2_2019 <-read_csv("Divvy_Trips_2019_Q2.csv")
q3_2019 <-read_csv("Divvy_Trips_2019_Q3.csv")
q4_2019 <-read_csv("Divvy_Trips_2019_Q4.csv")

#2020 data upload
q1_2020 <- read_csv("Divvy_Trips_2018_Q4.csv")
apr_20 <-read_csv("202004-divvy-tripdata.csv")
may_20 <-read_csv("202005-divvy-tripdata.csv")
jun_20 <- read_csv("202006-divvy-tripdata.csv")
jul_20 <- read_csv("202007-divvy-tripdata.csv")
aug_20 <- read_csv("202008-divvy-tripdata.csv")
sep_20 <-read_csv("202009-divvy-tripdata.csv")
oct_20 <-read_csv("202010-divvy-tripdata.csv")
nov_20 <- read_csv("202011-divvy-tripdata.csv")
dec_20 <- read_csv("202012-divvy-tripdata.csv")

#2021 data upload
jan_21 <-read_csv("202101-divvy-tripdata.csv")
feb_21 <-read_csv("202102-divvy-tripdata.csv")
mar_21 <-read_csv("202103-divvy-tripdata.csv")
apr_21 <-read_csv("202104-divvy-tripdata.csv")
may_21 <- read_csv("202105-divvy-tripdata.csv")
jun_21 <-read_csv("202106-divvy-tripdata.csv")
jul_21 <- read_csv("202107-divvy-tripdata.csv")
aug_21 <- read_csv("202108-divvy-tripdata.csv")
sep_21 <-read_csv("202109-divvy-tripdata.csv")
oct_21 <-read_csv("202110-divvy-tripdata.csv")
nov_21 <- read_csv("202111-divvy-tripdata.csv")
dec_21 <- read_csv("202112-divvy-tripdata.csv")

#2022 data upload
jan_22 <-read_csv("202201-divvy-tripdata.csv")
feb_22 <-read_csv("202202-divvy-tripdata.csv")
mar_22 <-read_csv("202203-divvy-tripdata.csv")
apr_22 <-read_csv("202204-divvy-tripdata.csv")
may_22 <- read_csv("202205-divvy-tripdata.csv")
jun_22 <-read_csv("202206-divvy-tripdata.csv")
jul_22 <- read_csv("202207-divvy-tripdata.csv")
aug_22 <- read_csv("202208-divvy-tripdata.csv")
sep_22 <-read_csv("202209-divvy-publictripdata.csv")
oct_22 <-read_csv("202210-divvy-tripdata.csv")
nov_22 <- read_csv("202211-divvy-tripdata.csv")
dec_22 <- read_csv("202212-divvy-tripdata.csv")

#Rename q1_2018 column names to match the other quarters dataframes
(q1_2018 <- rename(q1_2018,
                   trip_id="01 - Rental Details Rental ID"
                   ,start_time="01 - Rental Details Local Start Time"
                   ,end_time="01 - Rental Details Local End Time"
                   ,bike_id="01 - Rental Details Bike ID"
                   ,tripduration="01 - Rental Details Duration In Seconds Uncapped"
                   ,from_station_id="03 - Rental Start Station ID"
                   ,from_station_name="03 - Rental Start Station Name"
                   ,to_station_id="02 - Rental End Station ID"
                   ,to_station_name="02 - Rental End Station Name"
                   ,usertype="User Type"
                   ,gender="Member Gender"
                   ,birthyear="05 - Member Details Member Birthday Year"))


#rename columns to match q1_2020 colnames as this will be the standard going foward

#2018 q1
(q1_2018 <- rename(q1_2018
                   ,ride_id = trip_id
                   ,rideable_type = bike_id
                   ,started_at = start_time
                   ,ended_at = end_time
                   ,start_station_name = from_station_name 
                   ,start_station_id = from_station_id
                   ,end_station_name = to_station_name 
                   ,end_station_id = to_station_id 
                   ,member_casual = usertype))
#2018 q2
(q2_2018 <- rename(q2_2018
                   ,ride_id = trip_id
                   ,rideable_type = bikeid
                   ,started_at = start_time
                   ,ended_at = end_time
                   ,start_station_name = from_station_name 
                   ,start_station_id = from_station_id
                   ,end_station_name = to_station_name 
                   ,end_station_id = to_station_id 
                   ,member_casual = usertype))
#2018 q3
(q3_2018 <- rename(q3_2018
                   ,ride_id = trip_id
                   ,rideable_type = bikeid
                   ,started_at = start_time
                   ,ended_at = end_time
                   ,start_station_name = from_station_name 
                   ,start_station_id = from_station_id
                   ,end_station_name = to_station_name 
                   ,end_station_id = to_station_id 
                   ,member_casual = usertype))
#2018 q4
(q4_2018 <- rename(q4_2018
                   ,ride_id = trip_id
                   ,rideable_type = bikeid
                   ,started_at = start_time
                   ,ended_at = end_time
                   ,start_station_name = from_station_name 
                   ,start_station_id = from_station_id
                   ,end_station_name = to_station_name 
                   ,end_station_id = to_station_id 
                   ,member_casual = usertype))
#2019 q1
(q1_2019 <- rename(q1_2019
                   ,ride_id = trip_id
                   ,rideable_type = bikeid
                   ,started_at = start_time
                   ,ended_at = end_time
                   ,start_station_name = from_station_name 
                   ,start_station_id = from_station_id
                   ,end_station_name = to_station_name 
                   ,end_station_id = to_station_id 
                   ,member_casual = usertype))
#2019 q2
(q2_2019 <- rename(q2_2019
                   ,ride_id = "01 - Rental Details Rental ID"
                   ,rideable_type = "01 - Rental Details Bike ID" 
                   ,started_at = "01 - Rental Details Local Start Time"  
                   ,ended_at = "01 - Rental Details Local End Time"  
                   ,start_station_name = "03 - Rental Start Station Name" 
                   ,start_station_id = "03 - Rental Start Station ID"
                   ,end_station_name = "02 - Rental End Station Name" 
                   ,end_station_id = "02 - Rental End Station ID"
                   ,member_casual = "User Type"))
#2019 q3
(q3_2019 <- rename(q3_2019
                   ,ride_id = trip_id
                   ,rideable_type = bikeid
                   ,started_at = start_time
                   ,ended_at = end_time
                   ,start_station_name = from_station_name 
                   ,start_station_id = from_station_id
                   ,end_station_name = to_station_name 
                   ,end_station_id = to_station_id 
                   ,member_casual = usertype))
#2019 q4
(q4_2019 <- rename(q4_2019
                   ,ride_id = trip_id
                   ,rideable_type = bikeid
                   ,started_at = start_time
                   ,ended_at = end_time
                   ,start_station_name = from_station_name 
                   ,start_station_id = from_station_id
                   ,end_station_name = to_station_name 
                   ,end_station_id = to_station_id 
                   ,member_casual = usertype))

#2020 q1
(q1_2020 <- rename(q1_2020
                   ,ride_id = trip_id
                   ,rideable_type = bikeid
                   ,started_at = start_time
                   ,ended_at = end_time
                   ,start_station_name = from_station_name 
                   ,start_station_id = from_station_id
                   ,end_station_name = to_station_name 
                   ,end_station_id = to_station_id 
                   ,member_casual = usertype))

### inspect the dataframes for incongruencies
str(q1_2018)
str(q2_2018)
str(q3_2018)
str(q4_2018)

str(q1_2019)
str(q2_2019)
str(q3_2019)
str(q4_2019)

# mutuate ride_id and rideable type to character to as it keeps in line with what the other data in the df 
q1_2018 <-  mutate(q1_2018, ride_id = as.character(ride_id)
                   ,rideable_type = as.character(rideable_type)) 
q2_2018 <-  mutate(q2_2018, ride_id = as.character(ride_id)
                   ,rideable_type = as.character(rideable_type)) 
q3_2018 <-  mutate(q3_2018, ride_id = as.character(ride_id)
                   ,rideable_type = as.character(rideable_type)) 
q4_2018 <-  mutate(q4_2018, ride_id = as.character(ride_id)
                   ,rideable_type = as.character(rideable_type)) 
q1_2019 <-  mutate(q1_2019, ride_id = as.character(ride_id)
                   ,rideable_type = as.character(rideable_type)) 
q2_2019 <-  mutate(q2_2019, ride_id = as.character(ride_id)
                   ,rideable_type = as.character(rideable_type)) 
q3_2019 <-  mutate(q3_2019, ride_id = as.character(ride_id)
                   ,rideable_type = as.character(rideable_type)) 
q4_2019 <-  mutate(q4_2019, ride_id = as.character(ride_id)
                   ,rideable_type = as.character(rideable_type)) 

q1_2020 <-  mutate(q1_2020, ride_id = as.character(ride_id)
                   ,rideable_type = as.character(rideable_type)) 

#stack all individual quarters into one big df and the months into their year data
y18_trips <- bind_rows(q1_2018, q2_2018, q3_2018, q4_2018)
y19_trips <- bind_rows(q1_2019, q2_2019, q3_2019, q4_2019) 

#bind 2020 trips into a df y20_trips
## first mutuate start_station_id and end_sattion_id to character from double of 2020
apr_20 <-  mutate(apr_20, start_station_id = as.character(start_station_id)
                    ,end_station_id = as.character(end_station_id))
may_20 <-  mutate(may_20, start_station_id = as.character(start_station_id)
                  ,end_station_id = as.character(end_station_id))
jun_20 <-  mutate(jun_20, start_station_id = as.character(start_station_id)
                   ,end_station_id = as.character(end_station_id))
jul_20 <-  mutate(jul_20, start_station_id = as.character(start_station_id)
                  ,end_station_id = as.character(end_station_id))
aug_20 <-  mutate(aug_20, start_station_id = as.character(start_station_id)
                  ,end_station_id = as.character(end_station_id))
sep_20 <-  mutate(sep_20, start_station_id = as.character(start_station_id)
                  ,end_station_id = as.character(end_station_id))
oct_20 <-  mutate(oct_20, start_station_id = as.character(start_station_id)
                  ,end_station_id = as.character(end_station_id))
nov_20 <-  mutate(nov_20, start_station_id = as.character(start_station_id)
                  ,end_station_id = as.character(end_station_id))
q1_2020 <-  mutate(q1_2020, start_station_id = as.character(start_station_id)
                  ,end_station_id = as.character(end_station_id))

y20_trips <- bind_rows(q1_2020, apr_20, may_20, jun_20, jul_20, aug_20, sep_20, nov_20, dec_20)

#bind 2021 trips into a df y21_trips
y21_trips <- bind_rows(jan_21, feb_21, mar_21, apr_21,may_21, jun_21, jul_21, aug_21, sep_21, oct_21, nov_21, dec_21)

#bind all 2022 trips into a df y22_trips
y22_trips <- bind_rows(jan_22, feb_22, mar_22, apr_22,may_22, jun_22, jul_22, aug_22, sep_22, oct_22, nov_22, dec_22)

#Merge all the years trips into one df all_trips

# but first mutate columns to ensure they have the same datatypes
y18_trips <-  mutate(y18_trips, start_station_id = as.character(start_station_id)
                     ,end_station_id = as.character(end_station_id))
y19_trips <-  mutate(y19_trips, start_station_id = as.character(start_station_id)
                     ,end_station_id = as.character(end_station_id))
all_trips <-bind_rows(y18_trips, y19_trips, y20_trips, y21_trips, y22_trips)

# Remove lat, long, birthyear, and gender fields as this data was dropped beginning in 2020
all_trips <- all_trips %>%  
  select(-c(start_lat, start_lng, end_lat, end_lng, birthyear, gender, "01 - Rental Details Duration In Seconds Uncapped"
            , "05 - Member Details Member Birthday Year","Member Gender", "tripduration"))



####clean up and add data for analysis 
#¢inspect the new data athat has been created 
colnames(all_trips)  #List of column names
nrow(all_trips)  #How many rows are in data frame?
dim(all_trips)  #Dimensions of the data frame?
head(all_trips)  #See the first 6 rows of data frame.  Also tail(all_trips)
str(all_trips)  #See list of columns and data types (numeric, character, etc)
summary(all_trips)  #Statistical summary of data. Mainly for numerics

# we need to fix a few problems in the data
#1. in the member casual_column, there are two names for members(member/ subscriber), and there are olso two names for casual users(casual/customer)
#2. the data can only be aggregated at the ride level which has a large number of individual pieces of information. it will make more 
# sense to add some additional colunms such as day, month year, that will provide an opportunity to aggregate the data 
#3 We therefore add a new column to measure the length of ride for the trips
#4 There are some rides where tripduration shows up as negative, including several hundred rides where 
#Divvy took bikes out of circulation for Quality Control reasons. We will want to delete these rides.

# In the "member_casual" column, replace "Subscriber" with "member" and "Customer" with "casual"
# Before 2020, Divvy used different labels for these two types of riders ... we will want to make our dataframe consistent with their current nomenclature
# N.B.: "Level" is a special property of a column that is retained even if a subset does not contain any values from a specific level
# Begin by seeing how many observations fall under each usertype
table(all_trips$member_casual)
# now change the values to conform with correct values
all_trips <-  all_trips %>% 
  mutate(member_casual = recode(member_casual
                                ,"Subscriber" = "member"
                                ,"Customer" = "casual"))
#check to make sure the proper ammount of observations were reassigned

# Add columns that list the date, month, day, and year of each ride
# This will allow us to aggregate ride data for each month, day, or year ... before completing these operations we could only aggregate at the ride level
all_trips$date <- as.Date(all_trips$started_at) #The default format is yyyy-mm-dd
all_trips$month <- format(as.Date(all_trips$date), "%m")
all_trips$day <- format(as.Date(all_trips$date), "%d")
all_trips$year <- format(as.Date(all_trips$date), "%Y")
all_trips$day_of_week <- format(as.Date(all_trips$date), "%A")


# Add a "ride_length" calculation to all_trips (in seconds)
all_trips$ride_length <- difftime(all_trips$ended_at,all_trips$started_at)
str(all_trips)

#to convert the ride_length from factor to numeric so we can run calculations on the data
is.factor(all_trips$ride_length)
all_trips$ride_length<- as.numeric(as.character(all_trips$ride_length))
is.numeric(all_trips$ride_length)

#remove "bad" data
#the df includes a few hundred entries when bikes were taken out of docks and quality tested or ride_length was negative
#we therefore create a new version of the df since we're dropping data
all_trips_v2<-all_trips[!(all_trips$start_station_name == "HQ QR"|all_trips$ride_length<0),]

#droping null values
library(tidyr)
all_trips_v3 <- all_trips_v2 %>% drop_na()


### conducting descriptive analysis
#descriptive analysis on ride length(secs)

summary(all_trips_v2$ride_length)
summary(all_trips_v3$ride_length)

#compare members and casual users
aggregate(all_trips_v3$ride_length~all_trips_v3$member_casual, FUN = mean)
aggregate(all_trips_v3$ride_length~all_trips_v3$member_casual, FUN = median)
aggregate(all_trips_v3$ride_length~all_trips_v3$member_casual, FUN = max)
aggregate(all_trips_v3$ride_length~all_trips_v3$member_casual, FUN = min)

#average time by each day for members vs casual users 
aggregate(all_trips_v3$ride_length~all_trips_v3$member_casual+all_trips_v3$day_of_week, FUN = mean)

#since the days of the week are out of order, we fix that 
all_trips_v3$day_of_week<-ordered(all_trips_v3$day_of_week, levels=c("Sunday", "Monday", "Tuesday", "Wednesday", 
                                                                     "Thursday", "Friday", "Saturday"))
#now rerunning the aggregate function
aggregate(all_trips_v3$ride_length~all_trips_v3$member_casual+all_trips_v3$day_of_week, FUN = mean)

#analyse ridership data by type and weekday 
all_trips_v3 %>% mutate(weekday = wday(started_at, label=TRUE)) %>%   #creates weekday field using wday()
  group_by(member_casual, weekday) %>%    #groups by usertype and weekday
  summarise(number_of_rides = n()      #calculates the number of rides and average duration
            , average_duration = mean(ride_length)) %>%     #calculate the average duration
              arrange(member_casual, weekday) # sorts
#export the csv for viz
library(data.table)
fwrite(all_trips_v3, file = "alltrips.csv")

write.csv(all_trips_v3, file = "alltrips.csv")


#visualizing the number of rides by rider type
all_trips_v2 %>% mutate(weekday = wday(started_at, label=TRUE)) %>% 
  group_by(member_casual,weekday) %>%
  summarise(number_of_rides =n()
            , avearage_duration = mean(ride_length)) %>%
  arrange(member_casual, weekday) %>%
  ggplot(aes(x =weekday, y = number_of_rides, fill = member_casual)) + geom_col(position = "dodge")

#creating a viz for average trip duration
all_trips_v2 %>% 
  mutate(weekday =wday(started_at, label=TRUE)) %>%
  group_by(member_casual, weekday) %>%
  summarise(number_of_rides =n(),
            average_duration = mean(ride_length)) %>%
  arrange(member_casual, weekday) %>%
  ggplot(aes(x=weekday, y=average_duration,fill=member_casual)) + geom_col(position = "dodge")




##To compare the ride length between annual members and casual riders, we can use a t-test or ANOVA.
# T-test
t.test(all_trips_v3$ride_length[all_trips_v3$member_casual == "member"], 
       all_trips_v3$ride_length[all_trips_v3$member_casual == "casual"])

# ANOVA
model <- aov(ride_length ~ member_casual, data = all_trips_v3)
summary(model)

##To compare the frequency of bike usage between annual members and casual riders,
#you can use the table() and barplot() functions to create a bar graph.

# Count the number of rides taken by each group
rides_by_member_type <- table(all_trips_v3$member_casual)

# Plot the results in a bar graph
barplot(rides_by_member_type, main = "Rides by Member Type", xlab = "Member Type", ylab = "Number of Rides")



