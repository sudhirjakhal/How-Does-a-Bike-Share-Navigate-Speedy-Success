library(tidyverse)  #helps wrangle data
library(lubridate)  #helps wrangle date attributes
library(ggplot2)  #helps visualize data
getwd() #displays your working directory
setwd("E:/Case Study/Case Study 1/Downloads") 

#=====================
# STEP 1: COLLECT DATA
#=====================
# Upload Divvy datasets (csv files) here
data_05_2020 <- read_csv("202005-divvy-tripdata.csv")
data_06_2020 <- read_csv("202006-divvy-tripdata.csv")
data_07_2020 <- read_csv("202007-divvy-tripdata.csv")
data_08_2020 <- read_csv("202008-divvy-tripdata.csv")
data_09_2020 <- read_csv("202009-divvy-tripdata.csv")
data_10_2020 <- read_csv("202010-divvy-tripdata.csv")
data_11_2020 <- read_csv("202011-divvy-tripdata.csv")
data_12_2020 <- read_csv("202012-divvy-tripdata.csv")
data_01_2021 <- read_csv("202101-divvy-tripdata.csv")
data_02_2021 <- read_csv("202102-divvy-tripdata.csv")
data_03_2021 <- read_csv("202103-divvy-tripdata.csv")
data_04_2021 <- read_csv("202104-divvy-tripdata.csv")

#====================================================
# STEP 2: WRANGLE DATA AND COMBINE INTO A SINGLE FILE
#====================================================
# Compare column names each of the files
# While the names don't have to be in the same order, they DO need to match perfectly before we can use a command to join them into one file

colnames(data_05_2020)
colnames(data_06_2020)
colnames(data_07_2020)
colnames(data_08_2020)
colnames(data_09_2020)
colnames(data_10_2020)
colnames(data_11_2020)
colnames(data_12_2020)
colnames(data_01_2021)
colnames(data_02_2021)
colnames(data_03_2021)
colnames(data_04_2021)


# Inspect the dataframes and look for inconguencies
str(data_05_2020)
str(data_06_2020)
str(data_07_2020)
str(data_08_2020)
str(data_09_2020)
str(data_10_2020)
str(data_11_2020)
str(data_12_2020)
str(data_01_2021)
str(data_02_2021)
str(data_03_2021)
str(data_04_2021)

# Convert start_station_id from double to character so that it can stack correctly
data_05_2020 <-  mutate(data_05_2020, start_station_id = as.character(start_station_id), end_station_id = as.character(end_station_id))
data_06_2020 <-  mutate(data_06_2020, start_station_id = as.character(start_station_id), end_station_id = as.character(end_station_id))
data_07_2020 <-  mutate(data_07_2020, start_station_id = as.character(start_station_id), end_station_id = as.character(end_station_id))
data_08_2020 <-  mutate(data_08_2020, start_station_id = as.character(start_station_id), end_station_id = as.character(end_station_id))
data_09_2020 <-  mutate(data_09_2020, start_station_id = as.character(start_station_id), end_station_id = as.character(end_station_id))
data_10_2020 <-  mutate(data_10_2020, start_station_id = as.character(start_station_id), end_station_id = as.character(end_station_id))
data_11_2020 <-  mutate(data_11_2020, start_station_id = as.character(start_station_id), end_station_id = as.character(end_station_id))


# Stack individual quarter's data frames into one big data frame
all_trips <- bind_rows(data_05_2020, data_06_2020, data_07_2020, data_08_2020, data_09_2020, data_10_2020,
                       data_11_2020, data_12_2020, data_01_2021, data_02_2021, data_03_2021, data_04_2021)


#======================================================
# STEP 3: CLEAN UP AND ADD DATA TO PREPARE FOR ANALYSIS
#======================================================
# Inspect the new table that has been created
colnames(all_trips)  #List of column names
nrow(all_trips)  #How many rows are in data frame?
dim(all_trips)  #Dimensions of the data frame?
head(all_trips)  #See the first 6 rows of data frame.  Also tail(qs_raw)
str(all_trips)  #See list of columns and data types (numeric, character, etc)
summary(all_trips)  #Statistical summary of data. Mainly for numerics

# There are a few problems we will need to fix:
# (1) The dataset may contain NA values so we have to remove NA values from our dataset
# (2) The data can only be aggregated at the ride-level, which is too granular. We will want to add some additional columns of data -- such as day, month, year -- that provide additional opportunities to aggregate the data.
# (3) We will want to add a calculated field for length of ride i.e. ride_length.
# (4) There are some rides where ride_length shows up as negative, including several hundred rides where Divvy took bikes out of circulation for Quality Control reasons. We will want to delete these rides.

# (1) Remove NA values from the dataset
all_trips <- drop_na(all_trips)

# (2)Add columns that list the date, month, day, and year of each ride
# This will allow us to aggregate ride data for each month, day, or year ... before completing these operations we could only aggregate at the ride level
all_trips$date <- as.Date(all_trips$started_at) #The default format is yyyy-mm-dd
all_trips$month <- format(as.Date(all_trips$date), "%m")
all_trips$day <- format(as.Date(all_trips$date), "%d")
all_trips$year <- format(as.Date(all_trips$date), "%Y")
all_trips$day_of_week <- format(as.Date(all_trips$date), "%A")

#(3) Add a "ride_length" calculation to all_trips (in seconds)
all_trips$ride_length <- difftime(all_trips$ended_at,all_trips$started_at)

# Inspect the structure of the columns
str(all_trips)

# Convert "ride_length" from Factor to numeric so we can run calculations on the data
is.factor(all_trips$ride_length)
all_trips$ride_length <- as.numeric(as.character(all_trips$ride_length))
is.numeric(all_trips$ride_length)

# (4)Remove "bad" data
# The dataframe includes a few hundred entries when bikes were taken out of docks and checked for quality by Divvy or ride_length was negative
# We will create a new version of the dataframe (v2) since data is being removed
all_trips_v2 <- all_trips[!(all_trips$start_station_name == "HQ QR" | all_trips$ride_length<0),]

#=====================================

# STEP 4: CONDUCT DESCRIPTIVE ANALYSIS
#=====================================
# Descriptive analysis on ride_length (all figures in seconds)
mean(all_trips_v2$ride_length) #straight average (total ride length / rides)
median(all_trips_v2$ride_length) #midpoint number in the ascending array of ride lengths
max(all_trips_v2$ride_length) #longest ride
min(all_trips_v2$ride_length) #shortest ride

# You can condense the four lines above to one line using summary() on the specific attribute
summary(all_trips_v2$ride_length)

# Compare members and casual users
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = mean)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = median)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = max)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = min)

# See the average ride time by each day for members vs casual users
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual + all_trips_v2$day_of_week, FUN = mean)

# Notice that the days of the week are out of order. Let's fix that.
all_trips_v2$day_of_week <- ordered(all_trips_v2$day_of_week, levels=c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))

# Now, let's run the average ride time by each day for members vs casual users
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual + all_trips_v2$day_of_week, FUN = mean)

# analyze ridership data by type and weekday
all_trips_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>%  #creates weekday field using wday()
  group_by(member_casual, weekday) %>%  #groups by member_casual and weekday
  summarise(number_of_rides = n()                            #calculates the number of rides and average duration 
            ,average_duration = mean(ride_length)) %>%         # calculates the average duration
  arrange(member_casual, weekday)                                # sorts

# Let's visualize the number of rides by rider type
all_trips_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>% 
  arrange(member_casual, weekday)  %>% 
  ggplot(aes(x = weekday, y = number_of_rides, fill = member_casual)) +
  geom_col(position = "dodge")

# Let's create a visualization for average duration
all_trips_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>% 
  arrange(member_casual, weekday)  %>% 
  ggplot(aes(x = weekday, y = average_duration, fill = member_casual)) +
  geom_col(position = "dodge")

#Create a new data frame with only the rows with info in the "bike type" column:

with_bike_type <- all_trips_v2 %>% filter(rideable_type=="classic_bike" | rideable_type=="electric_bike")

#Then lets check the bike type usage by user type:

with_bike_type %>%
  group_by(member_casual,rideable_type) %>%
  summarise(totals=n(), .groups="drop")  %>%
  
  ggplot()+
  geom_col(aes(x=member_casual,y=totals,fill=rideable_type), position = "dodge") + 
  labs(title = "Bike type usage by user type",x="User type",y=NULL, fill="Bike type") +
  scale_fill_manual(values = c("classic_bike" = "#746F72","electric_bike" = "#FFB100")) +
  theme_minimal() +
  theme(legend.position="top")

#And their usage by both user types during a week:

with_bike_type %>%
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual,rideable_type,weekday) %>%
  summarise(totals=n(), .groups="drop") %>%
  
  ggplot(aes(x=weekday,y=totals, fill=rideable_type)) +
  geom_col(, position = "dodge") + 
  facet_wrap(~member_casual) +
  labs(title = "Bike type usage by user type during a weekday",x="User type",y=NULL,caption = "Data by Motivate International Inc") +
  scale_fill_manual(values = c("classic_bike" = "#746F72","electric_bike" = "#FFB100")) +
  theme_minimal() +
  theme(legend.position="none")

