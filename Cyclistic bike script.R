#### Installing packages
install.packages("tidyverse")
install.packages("lubridate")
install.packages("here")
install.packages("janitor")
install.packages("skimr")
install.packages("ggplot2")
install.packages("tidyr")
install.packages("readr")
install.packages("dplyr")

#### loading library
library(tidyverse)
library(lubridate)
library(janitor)
library(here)
library(skimr)
library(ggplot2)
library(readr)
library(tidyr)
library(dplyr)

####displays your working directory
getwd()
setwd("/Users/SOFIYAT/Documents") 


#### uploading our csv files
apr_2020 <- read_csv("cyclic project/202004-divvy-tripdata.csv")
may_2020 <- read_csv("cyclic project/202005-divvy-tripdata.csv")
jun_2020 <- read_csv("cyclic project/202006-divvy-tripdata.csv")
jul_2020 <- read_csv("cyclic project/202007-divvy-tripdata.csv")
aug_2020 <- read_csv("cyclic project/202008-divvy-tripdata.csv")
sep_2020 <- read_csv("cyclic project/202009-divvy-tripdata.csv")
oct_2020 <- read_csv("cyclic project/202010-divvy-tripdata.csv")
nov_2020 <- read_csv("cyclic project/202011-divvy-tripdata.csv")
dec_2020 <- read_csv("cyclic project/202012-divvy-tripdata.csv")
jan_2021 <- read_csv("cyclic project/202101-divvy-tripdata.csv")
feb_2021 <- read_csv("cyclic project/202102-divvy-tripdata.csv")
mar_2021 <- read_csv("cyclic project/202103-divvy-tripdata.csv")

# STEP 2: WRANGLE DATA AND COMBINE INTO A SINGLE FILE
#### to view all the column names
colnames(apr_2020)
colnames(may_2020)
colnames(jun_2020)
colnames(jul_2020)
colnames(aug_2020)
colnames(sep_2020)
colnames(oct_2020)
colnames(nov_2020)
colnames(dec_2020)
colnames(jan_2021)
colnames(feb_2021)
colnames(mar_2021)

#### Inspect the data frames and look for incongruencies
str(apr_2020)
str(may_2020)
str(jun_2020)
str(jul_2020)
str(aug_2020)
str(sep_2020)
str(oct_2020)
str(nov_2020)
str(dec_2020)
str(jan_2021)
str(feb_2021)
str(mar_2021)

### Converting and unifying the datatype to be in character
apr_2020 <- mutate(apr_2020, start_station_id = as.character(start_station_id),
                   end_station_id = as.character(end_station_id))
may_2020 <- mutate(may_2020, start_station_id = as.character(start_station_id),
                   end_station_id = as.character(end_station_id))
jun_2020 <- mutate(jun_2020, start_station_id = as.character(start_station_id),
                   end_station_id = as.character(end_station_id))
jul_2020 <- mutate(jul_2020, start_station_id = as.character(start_station_id),
                   end_station_id = as.character(end_station_id))
aug_2020 <- mutate(aug_2020, start_station_id = as.character(start_station_id),
                   end_station_id = as.character(end_station_id))
sep_2020 <- mutate(sep_2020, start_station_id = as.character(start_station_id),
                   end_station_id = as.character(end_station_id))
oct_2020 <- mutate(oct_2020, start_station_id = as.character(start_station_id),
                   end_station_id = as.character(end_station_id))
nov_2020 <- mutate(nov_2020, start_station_id = as.character(start_station_id),
                   end_station_id = as.character(end_station_id))
dec_2020 <- mutate(dec_2020, start_station_id = as.character(start_station_id),
                   end_station_id = as.character(end_station_id))
jan_2021 <- mutate(jan_2021, start_station_id = as.character(start_station_id),
                   end_station_id = as.character(end_station_id))
feb_2021 <- mutate(feb_2021, start_station_id = as.character(start_station_id),
                   end_station_id = as.character(end_station_id))
mar_2021 <- mutate(mar_2021, start_station_id = as.character(start_station_id),
                   end_station_id = as.character(end_station_id))
#### merge all the 12 data frames into 1 data frame called bike_ride
bike_trips <- bind_rows(apr_2020,may_2020,jun_2020,jul_2020,aug_2020,sep_2020,
                        oct_2020,nov_2020,dec_2020,jan_2021,feb_2021,mar_2021)

# STEP 3: CLEAN UP AND ADD DATA TO PREPARE FOR ANALYSIS
#### lets check out our merge data
colnames(bike_trips)
str(bike_trips)
head(bike_trips)
nrow(bike_trips)
summary(bike_trips)

### Parse Date times
#### Add date, month, day, year and trip length column
bike_trips$date <- as.Date(bike_trips$started_at)
bike_trips$day <- format(as.Date(bike_trips$date), "%d")
bike_trips$month <- format(as.Date(bike_trips$date), "%m")
bike_trips$year <- format(as.Date(bike_trips$date), "%Y")
bike_trips$day_of_week <- format(as.Date(bike_trips$date), "%A")
bike_trips$trip_length <- difftime(bike_trips$ended_at,bike_trips$started_at)

#### returns a summary of empty colons on each variable
colSums(is.na(bike_trips)) 
bike_trips_clean <- bike_trips [complete.cases(bike_trips), ]

#### Remove all negative and zero trip length as well as NA/null values
bike_trips_clean <- subset(bike_trips_clean, trip_length>0)

#### Convert ride length to integer
bike_trips_clean$trip_length <- as.integer(bike_trips_clean$trip_length)

# STEP 4: CONDUCT DESCRIPTIVE ANALYSIS
#### Descriptive analysis on the ride length
summary(bike_trips_clean$trip_length)

#### compare members and casual users
aggregate(bike_trips_clean$trip_length ~ bike_trips_clean$member_casual, FUN = mean)
aggregate(bike_trips_clean$trip_length ~ bike_trips_clean$member_casual, FUN = median)
aggregate(bike_trips_clean$trip_length ~ bike_trips_clean$member_casual, FUN = max)
aggregate(bike_trips_clean$trip_length ~ bike_trips_clean$member_casual, FUN = min)

#### Aggregate data to determine average trip length of riders for each month of the year
aggregate(trip_length ~ member_casual + month, bike_trips_clean, mean)

#### Aggregate data to determine average trip length of riders on each day of the week
aggregate(trip_length ~ day + member_casual, bike_trips_clean, mean)

#### Aggregate data to determine how riders use the different bike types available
aggregate(trip_length ~ rideable_type + member_casual, bike_trips_clean, mean)

#### See the average ride time by each day for members vs casual users
aggregate(bike_trips_clean$trip_length ~ bike_trips_clean$member_casual + bike_trips_clean$day_of_week, FUN = mean)

# Notice that the days of the week are out of order. Let's fix that.
bike_trips_clean$day_of_week <- ordered(bike_trips_clean$day_of_week, levels=c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))

# Now, let's run the average ride time by each day for members vs casual users
aggregate(bike_trips_clean$trip_length ~ bike_trips_clean$member_casual + bike_trips_clean$day_of_week, FUN = mean)

# STEP 6

#### Group and visualize rider type by day of the week
rider_week <- bike_trips_clean %>% group_by(day, member_casual) %>%
  count()
ggplot(data=rider_week)+
  geom_col(mapping=aes(x=day, y=n, fill=member_casual))+
  facet_grid(~member_casual)+
  labs(y="Trip length", x="Rider type", fill="Member/Casual",
       title="Ride length by day of the week")+
  theme(axis.text.x=element_text(angle=45))

# Visualization

## Group, summarize and visualize data to determine ride count by rider type
ridecount <- bike_trips_clean %>% group_by(member_casual) %>% count()
ridecount
ggplot(data=ridecount)+
  geom_col(mapping=aes(x=member_casual, y=n, fill=member_casual))+
  labs(y="Trip length", x="Rider type", fill="Member/Casual",
       title="Number of trips by different rider types")

## Summarize and visualize data by rider type and average trip length to ascertain length of ride taken by different rider types
ride_length <- bike_trips_clean %>% group_by(member_casual) %>% summarise(mean_trip_length = mean(trip_length, na.rm = TRUE))
ggplot(data=ride_length)+
  geom_col(mapping=aes(x=member_casual, y=mean_trip_length, fill=member_casual))+
  labs(y="Trip length", x="Rider type", fill="Member/Casual",
       title="Lenght of ride taken by different rider types")

## Group and visualize data by member_casual and rideable_type to determine prefer type of bike by riders
ride_type <- bike_trips_clean %>% group_by(member_casual, rideable_type) %>%
  count()
ggplot(data=ride_type)+
  geom_col(mapping=aes(x=rideable_type, y=n, fill=member_casual))+
  facet_wrap(~member_casual)+
  labs(y="Number of rides", x="Type of bike", fill="Member/Casual",
       title="Most prefered type of bike by riders")

### TOP 10 STATIONS

## Create a new data frame for all stations
full_stations <- bind_rows(data.frame("stations" = bike_trips_clean$start_station_name, 
                                     "member_casual" = bike_trips_clean$member_casual),
                          data.frame("stations" = bike_trips_clean$end_station_name,
                                     "member_casual" = bike_trips_clean$member_casual))

#### Exclude entries with no station name
all_stations <- full_stations[!(full_stations$stations == "" | is.na(full_stations$stations)),]

#### Separate the data frame by rider type
full_stations_member <- all_stations[all_stations$member_casual == 'member',]
full_stations_casual <- all_stations[all_stations$member_casual == 'casual',]


#### top 10 popular stations all, members and casual riders
top_10_station <- all_stations %>% 
  group_by(stations) %>% 
  summarise(station_count = n()) %>% 
  arrange(desc(station_count)) %>% 
  slice(1:10)

#### Visualization of top 10 stations
ggplot(data=top_10_station)+
  geom_col(mapping=aes(x=stations,y=station_count, fill=stations))+
  labs(title="Top 10 stations", subtitle = "Most populous bike stations.")+
  theme(axis.text = element_text(angle=45))

#### Top 10 popular stations for members
top_10_stations_member <- full_stations_member %>% 
  group_by(stations) %>% 
  summarise(station_count = n()) %>% 
  arrange(desc(station_count)) %>% 
  head(n=10)

#### Visualization of top 10 popular stations for member
ggplot(data=top_10_stations_member)+
  geom_col(mapping=aes(x=stations,y=station_count, fill=stations))+
  labs(title="Top 10 stations", subtitle = "Most populous bike stations amongst members.")+
  theme(axis.text.x = element_text(angle=45))

#### Get the top 10 popular stations for casual
top_10_stations_casual <- full_stations_casual %>% 
  group_by(stations) %>% 
  summarise(station_count = n()) %>% 
  arrange(desc(station_count)) %>% 
  head(n=10)

#### Visualization of top 10 popular stations for casual
ggplot(data=top_10_stations_casual)+
  geom_col(mapping=aes(x=stations,y=station_count, fill=stations))+
  labs(title="Top 10 stations", subtitle = "Most populous bike stations amongst casuals.")+
  theme(axis.text = element_text(angle=45))


ggplot(data=top_10_stations_casual)+
  geom_col(mapping=aes(x=stations,y=station_count, fill=stations), show.legend = FALSE)+
  labs(title="Top 10 stations", caption = "Most populous bike stations amongst casuals.")
 
### file exported for further analysis
write.csv(bike_trips_clean,file = 'C:/Users/SOFIYAT/Documents/cleanride.csv')
write.csv(all_stations, file = 'C:/Users/SOFIYAT/Documents/all_stations.csv')


