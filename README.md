# Case Study 1: How Does a Bike-Share Navigate Speedy Success?

 
**The Mission Statement**

The bike sharing company wants to analyze their user data to find the main differences in behaviour between their two types of users, the “casual” who pays for each ride and the annual member who pays a yearly subscription to the service.

**PHASE 1 : ASK**

**Key objectives:**

**1.Identify the business task:**

The company wants to improve their earnings reaching out to their “casual” riders, and for that they have to analyze in what aspects the “casual” and the annual customers differ, to be able to create a focused and successful marketing message to the “casual” customers that makes them change to the annual subscription.

**2.Consider key stakeholders:**

The main stakeholders here are the director of marketing and my manager Lily Moreno, the rest of the marketing analytics team, and the Cyclistic executive team.

**3.The business task:**

How do annual members and casual riders use Cyclistic bikes differently?

**PHASE 2 : Prepare**

**Key objectives:**

**1.Determine the credibility of the data:**

The data is public data from a bike sharing company. It starts from the year 2013 until 2021 (three months), there isn't much of a naming convention as the files are sometimes organized by quarter, or month, or the whole year and their names vary a lot. The naming of the columns also changes and there are some columns added and deleted over the years. Nevertheless the data seems to be in good condition and its first hand data collected by the company itself with lots of entries and with lots of useful data.

**2.Sort and filter the data:**

For this analysis I'm going to focus on the 2020-2021 period as it's the more relevant period to the business task and it has the more complete data with geo-location coordinates, and types of bike used.

**PHASE 3 : Process**

**Key objectives:**

**1.Clean the data, and prepare the data for analysis:**

Now that we have all the data in one place so we can start to clean the data of possible errors like NA. Also we will make some changes to the data adding useful new columns based on calculations of already existing columns in order to facilitate our analysis and arrive at more insightful conclusions.


**PHASE 4 : Analyze**

**Key objectives:**

**1.Identify trends and relationships:**

Now we have a complete dataframe with all the information we need to identify the difference between the behaviour of the casual and the member users.


**Analysis:**

![number of rides per weekday](https://github.com/sudhirjakhal/How-Does-a-Bike-Share-Navigate-Speedy-Success/blob/main/number_of_rides_per_weekday.png)![average duration per weekday](https://github.com/sudhirjakhal/How-Does-a-Bike-Share-Navigate-Speedy-Success/blob/main/average_duration_per_weekday.png)

* It seems that the casual users travel the same average distance than the member users, but they have much longer rides, that would indicate a more leisure oriented usage vs a more "public transport" or pragmatic use of the bikes by the annual members.

* This idea is reinforced by the fact that annual users have a very stable use of the service during the week, but the casual users are more of a weekend user.

![bike type usage by user type](https://github.com/sudhirjakhal/How-Does-a-Bike-Share-Navigate-Speedy-Success/blob/main/bike_type_usgae_by_user_type.png)![bike type usage by user type per wekday](https://github.com/sudhirjakhal/How-Does-a-Bike-Share-Navigate-Speedy-Success/blob/main/bike_type_usage_by_user_per_weekday.png)

* Here we can see that the annual members use both types of bikes for their rides, but the casual users show a clear preference for the electric bikes, which makes sense given the long duration of their rides.

* On a weekly basis we can see that for the annual members there is a small difference of usage between the start of the week, where they prefer the classic bike and the end of the week, where they use more electric bikes.

* For the casual users we see in general the same pattern of usage from the previous weekly charts, preferring the electric vs the classic bikes and having a weekend usage of the service.

**PHASE 5 : Share**

**Key objectives:**

**1.Share my conclusions.:

Taking in consideration both the business task: How do annual members and casual riders use Cyclistic bikes differently? and the insights we've learned from the available data.

We can make some conclusions:

1)The Casual users have leisure, and tourism rides mostly on weekends and using electric bikes.

2)The Annual users have commute or pragmatic rides, during all week using both electric & classic bikes
