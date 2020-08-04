# Jumpman23_NewMarketAnalysis
new market analysis for a delivery service company - data challenge for postmates
click HTML file for output
click rmd file for R Code


---
title: "New Market Analysis"
author: "Elisa Cui"
date: "7/28/2020"
output:
  prettydoc::html_pretty:
    theme: cayman
    highlight: github
    self_contained: true
    toc: true
vignette: >
  %\VignetteIndexEntry{Vignette Title}
  %\VignetteEncoding{UTF-8}
  %\VignetteEngine{knitr::rmarkdown}
---

### Challenge
How are things in New York for Jumpman23? Come up with a plan to grow the market by 20% in two months. Dive into the reports on data integrity issues and if they exist, outline where and how they may impact the analysis.

### Market Analysis
Jumpman23 is an on-demand delivery platform connecting "Jumpmen" to customers purchasing a variety of goods. This platform is a three-sided market place that benefits customers, restaurants and drivers. The core value of this product is to connect customers with what they need in the most efficient way possible. The key metrics for growing this market are new user acquisition, activation, retention and revenue. Now that this product has identified market fit within New York City, I have identified the growth model to be:  

**Growth = (Topline Traffic) x (Account Sign-Up) x (1st Time User Purchase) x (Repeated User Behavior)**
  
I will be diving into how we can grow these metrics to result in a 20% increase in growth in two months. I will also be noting the data integrity issues and how they may impact the analysis. I have also offered suggestions on how to fix the data integrity issues which also correlates to overall efficiency.

```{r setup, include=FALSE, echo=FALSE}
library(lubridate)
library(ggmap)
library(Rssa)
library(ggplot2)
library(readr)
library(tidyverse)
library(dplyr)
library(chron)
library(geosphere)
library(leaflet)
library(gridExtra)
library(tibble)
library(reshape2)
library(anytime)
require(devtools)
devtools::install_github("dkahle/ggmap", ref = "tidyup")
library(ggrepel)
library(data.table)
library(forecast)
library(fpp2)
library(TTR)
library(prettydoc)
library(knitr)
```
### Understanding The Dataset
I have imported the dataset and transformed the variables for further analysis. Due to the amount of faulty data (missing values and incorrect timestamps), I will clean up the dataset for growth evaluation in order to give the most accurate analysis possible.
```{r results = "hide"}
data <- read.csv(file = "../analyze_me.csv", header = TRUE, na.strings=c("","NA"))
glimpse(data)
#18 columns & 5983 obs
#3 columns used for identification of order: delivery_id, customer_id, jumpman_id
#transforming data and using haversine formula to determine distance of pickup and dropoff
data <- data %>% rowwise() %>% mutate(distance = distHaversine(c(pickup_lon, pickup_lat), c(dropoff_lon, dropoff_lat))) 
#creating variables and transforming time data
#start to pickup: delivery started minus jumpman arrived at pickup
#start to end: total time of delivery
data$start_to_pickup <- difftime(as.POSIXct(data$when_the_Jumpman_arrived_at_pickup, format = "%Y-%m-%d %H:%M:%S"), as.POSIXct(data$when_the_delivery_started, format = "%Y-%m-%d %H:%M:%S"))
data$start_to_end <- difftime(as.POSIXct(data$when_the_Jumpman_arrived_at_dropoff, format = "%Y-%m-%d %H:%M:%S"), as.POSIXct(data$when_the_delivery_started, format ="%Y-%m-%d %H:%M:%S"))
#distance_miles: distance for each pickup and dropoff
data$distance_miles <- (data$distance / 1609)
#which_day: for determining out which day of the week people order from the most
#which_hour: for determining which hour of the day people order the most
data$which_day <- weekdays(as.Date(data$when_the_delivery_started))
data$which_hour <- format(as.POSIXct(data$when_the_delivery_started, format ="%Y-%m-%d %H:%M:%S"), "%H")
```
I have removed the rows in which item_quantity is null and pickup time occurs before the delivery started. The dataset I will be working with is now called *cleaned.data*.
```{r warning=FALSE, message=FALSE}
#removing rows without item quantity and rows where time to pick up is negative 
cleaned.data <- data[which(!is.na(data$item_quantity) & data$start_to_pickup > 0),]
```
### New Customer Acquisition 
In order to obtain new customers, these are the key factors that impact this metric:  
  
* Streamlined experience for the sign-up process
* Restaurants partnerships
* Customer acquisition campaigns (PR marketing, virality)

#### User Experience
There are two perspectives on user experience, one from a customer's side and one from a business' side (restaurants). By enhancing the sign-up process for a new user, it will allow them to seamlessly go through the application and reach their end goal of purchasing goods efficiently. As from a business' standpoint, if we also have seamless integration for a restaurant to work with Jumpman23, this could also benefit the company because a driving factor of customers wanting to purchase a good is based off of what is offered. **If we prioritize supply by expanding our restaurant partnerships, we will see a growth on our demand due to having predictable options for suburban consumers.**
```{r warning=FALSE, message=FALSE}
register_google(key = "AIzaSyCDnYvyOg7yNC_znzzIZC64cXHdRKP6ujI")
ny_pickup <- ggmap(get_map(location=c(lon = -73.972026, lat = 40.745362), zoom=13, scale=4)) + stat_density2d(data = cleaned.data, aes(x =pickup_lon, y = pickup_lat, fill = ..level.., alpha = ..level..), geom = "polygon") 
ny_pickup + ggtitle("Map of Pickup Locations") 
ny_dropoff <- ggmap(get_map(location=c(lon = -73.972026, lat = 40.745362), zoom=13, scale=4)) + stat_density2d(data = cleaned.data, aes(x =dropoff_lon, y = dropoff_lat, fill = ..level.., alpha = ..level..), geom = "polygon") 
ny_dropoff + ggtitle("Map of Drop Off Locations") 
```
  
Based off the geographical heatmaps, majority of the pickups happen within lower Manhattan, East Village and NoHo and then dropped off at homes that are a few miles outside of the concentrated areas. In order to expand this, we would want to onboard popular businesses within the Upper East Side, which in return, will bring new customers on board as well. High drop off location density implies that customers in that area could have a higher income, leading to more frequent use of delivery services. 
```{r warning=FALSE, message=FALSE}
cleaned.data$only_date <- format(as.POSIXct(cleaned.data$when_the_delivery_started, format="%Y-%m-%d %H:%M:%S"), "%Y-%m-%d")
customer <- cleaned.data %>% select(customer_id, only_date) 
customer_by_day <- customer %>% group_by(only_date) %>% dplyr::summarise(count = n())
daily_order <- cleaned.data %>% group_by(only_date, delivery_id) %>% dplyr::summarise(count = n())
ggplot(customer_by_day, aes(x = as.Date(only_date), y = count)) + geom_point() + labs(x="Date", y="Number of Customers", title="Number of Unique Customers Ordering per Day") + geom_smooth(colour="#e642f5", size=1.5, method="loess", se=FALSE) + geom_smooth(method="lm", se=FALSE)
```
  
Unique customers are growing at a steady rate (blue line) while the number of new customers (pink line) have decreased, which means we need to pour more resources into increasing obtaining new customers. These acquisition campaigns could be incentivised referral programs or influencer advertisement to cater to a large audience.

### Existing Customer Retention and Engagement
Divide the customers into different classes based off their ordering history and create incentives on a level by level basis to gain customer retention. **By using a customer's own data to determine the best time to send out an incentive to increase existing customer engagement and predict customer's future actions.**
```{r warning=FALSE, message=FALSE}
#which hour is the most popular
order_by_hour <- cleaned.data %>% group_by(which_hour, delivery_id) %>% dplyr::summarise(count = n())
ggplot(order_by_hour, aes(x = which_hour)) + geom_bar(fill = "#A15FE8") + geom_text(stat = 'count', aes(label=..count..), vjust = -1) + labs(title = "Orders by Hour", x = "Hour of the Day", y = "Orders") + ylim(0,600)
```
```{r warning=FALSE, message=FALSE}
#which day is the most popular
order_by_day <- cleaned.data %>% group_by(which_day, delivery_id) %>% dplyr::summarise(count = n())
ggplot(order_by_day, aes(x = which_day)) + geom_bar(fill = "#0073C2FF") + geom_text(stat = 'count', aes(label=..count..), vjust = -1) + labs(title = "Orders by Day", x = "Day of the Week", y = "Orders") + ylim(0,700)
```
  
By classifying the customers into different types of buyers based on their ordering history - daily, weekly, bi-weekly or monthly customers, we can use this to determine data-driven incentives for each level of buyer. By predicting the most popular time/day a user is on the application, we can turn these promotions into purchases, therefore increasing repeated user behavior.
```{r warning=FALSE, message=FALSE}
order_by_day_hour <- cleaned.data %>% group_by(which_day, which_hour) %>% dplyr::summarise(count = n())
#delivery by time and day
ggplot(order_by_day_hour, aes(x = which_hour, y = which_day, fill = count)) + geom_tile() + labs(x = "Hour of Day", y = "Day of Week", title = "Number of Deliveries, by Day and Hour")
```
  
This heatmap shows that peak number of deliveries occur on the weekend and between the hours of 5PM - 8PM. By predicting user behavior, we can increase customer retention and grow dollar amount per purchase by using the customer's track record to market the correct incentive.

### Optimizing Delivery Time
Incentives work on both the customer side and the delivery side in the micromobility industry. One of the data integrity issues I will mention here is the missing arrival and departure time of the Jumpman from the pickup location. This information correlates with determining the estimated time of arrival of the Jumpman to the customer.
  
With this field missing, it can complicate the Jumpman's rating and efficiency while impacting the location's actual estimated delivery time. A customer's decision will be based off of what they see as most efficient, therefore, if the delivery will take longer based on past deliverers not tracking down their time, it could potentially affect the business. 
```{r}
#count missing jumpman arrival/pickup (correlated data so i will only be using the dataset of arrival time for pickup)
pickup_time <- data[!is.na(data$when_the_Jumpman_arrived_at_pickup),]
empty_pickup_time <- data[is.na(data$when_the_Jumpman_arrived_at_pickup),]
#creating variable for the time it takes for a delivery starting to when the jumpman brings it to the customer
empty_pickup_time$start_to_pickup <- difftime(as.POSIXct(empty_pickup_time$when_the_Jumpman_arrived_at_pickup, format = "%Y-%m-%d %H:%M:%S"), as.POSIXct(empty_pickup_time$when_the_delivery_started, format = "%Y-%m-%d %H:%M:%S"))
#creating variable for the time it takes for a delivery starting to when the jumpman brings it to the customer
empty_pickup_time$start_to_end <- difftime(as.POSIXct(empty_pickup_time$when_the_Jumpman_arrived_at_dropoff, format = "%Y-%m-%d %H:%M:%S"), as.POSIXct(empty_pickup_time$when_the_delivery_started, format ="%Y-%m-%d %H:%M:%S"))
median(pickup_time$distance_miles)
median(empty_pickup_time$distance_miles)
median(pickup_time$start_to_end)
median(empty_pickup_time$start_to_end)
```
By splitting the dataset into recorded pickup times and non-recorded pickup times, it is important to note that when a Jumpman does not report their time of arrival/departure from the pickup, it is not only shorter distance but also longer delivery time. This means there is some ambiguity between the Jumpman forgetting to note their time and the actual distance of the delivery.
```{r warning=FALSE, message=FALSE}
#creating variable for the time it takes for a delivery starting to when the jumpman picks it up
pickup_time$start_to_pickup <- difftime(as.POSIXct(pickup_time$when_the_Jumpman_arrived_at_pickup, format = "%Y-%m-%d %H:%M:%S"), as.POSIXct(pickup_time$when_the_delivery_started, format = "%Y-%m-%d %H:%M:%S"))
#creating variable for the time it takes for a delivery starting to when the jumpman brings it to the customer
pickup_time$start_to_end <- difftime(as.POSIXct(pickup_time$when_the_Jumpman_arrived_at_dropoff, format = "%Y-%m-%d %H:%M:%S"), as.POSIXct(pickup_time$when_the_delivery_started, format ="%Y-%m-%d %H:%M:%S"))
sum(pickup_time$start_to_pickup < 0)
```
Another notable data integrity issue is the arrival time being earlier than when the delivery started. This resulted in negative time for calculating the total amount of time it takes for the jumpman to go to the pickup location. These false values account for 497 observations within the dataset.  
  
#### Incentive for Deliverers
If we can create an incentive in which the Jumpman is encouraged to record their arrival/departure time at the pickup location in order to complete a "perfect delivery". This can be a streak or tally mark of how many perfect deliveries this Jumpman has completed, which in return, can ensure trust within the customer that their Jumpman is reliable and efficent. Something to note would be that this is not an incentive based on the quickness of the delivery; it is based on the Jumpman solely being able to note these times so that data and software engineers have reliable data to determine the estimated time of a delivery.
  
By using existing data of a "perfect delivery," it can track the reasonable amount of time it takes for a Jumpman to get from point A to point B based off of other perfect deliveries via that location. The Jumpman can aim to deliver in a certain amount of time since a "perfect delivery" is not based on how quick, but how cautious the Jumpman is with the protocol. This will also make the estimation of the delivery more realistic, therefore giving customers a more accurate time for when they decide to order.

### Data Integrity Issues  
There are a few columns have missing values in the dataset, which are:  
  
* place_category: 883  
* item_name: 1230  
* item_quantity: 1230  
* item_category_name: 1230  
* how_long_it_took_to_order: 2945  
* when_the_jumpman_arrived_at_pickup: 550  
* when_the_jumpman_left_pickup: 550  
  
In most cases, we would be able to drop the missing values if the number of cases is less than 5% of the sample. If I were to omit all the rows with missing values, it would deduct the dataset by 61.9% which is a dramatic decrease in the number of observations given. Therefore, the best method is to judge the missing information by their local category. For example, looking at the item's name, quantity and category, there is a correlation in which if one variable is null, it will leave the next variable null and so on.

#### Null Item Name, Quantity, Category
About 20.5% of the dataset has missing information about the items ordered. Since these fields are related to one another, I will be splitting the dataset based on item_quantity. If this field is left blank, there is no clear KPI that the item is doing well which directly affects the business working with Jumpman23.
```{r include = FALSE, warning=FALSE, message=FALSE}
#creating df for item_quantity in which the column has no missing values
data$item_quantity <- as.numeric(data$item_quantity)
item_quantity <- data[!is.na(data$item_quantity),]
#creating df for empty_quantity in which the column has all missing values
empty_quantity <- data[is.na(data$item_quantity),]
count(empty_quantity)
```
```{r}
ggplot() + geom_density(data = item_quantity, aes(x = distance_miles, fill = "Item Noted"), adjust = 1, alpha = .3) + geom_density(data = empty_quantity, aes(x = distance_miles, fill = "N/A"), adjust = 1, alpha = .3) + scale_fill_manual(name = "Item Quantity", values = c("Item Noted" = "#1A97DB", "N/A" = "#E68877")) + labs(x = "Miles Traveled", y = "Density", title = "Delivery Distance vs. Null Item") 
```
  
There is a correlation between shorter delivery distanace and missing items in the dataset. From this graph, it is possible that customers could have changed their minds about ordering something due to the proximity of where the customer is and the place they are ordering from.

I want to see if this null field is related to other attributes within the dataset. If we can separating this issue from other attributes, we can further investigate the root of the problem in regard's to a customer's behavior when it comes to ordering from Jumpman.
```{r warning=FALSE, message=FALSE}
#querying for results of any correlation within the two datasets of item_quantity & empty_quantity
nrow(item_quantity[which(item_quantity$jumpman_id %in% unique(empty_quantity$jumpman_id)), ])
nrow(item_quantity[which(item_quantity$customer_id %in% unique(empty_quantity$customer_id)), ])
nrow(item_quantity[which(item_quantity$pickup_place %in% unique(empty_quantity$pickup_place)), ])
nrow(item_quantity[which(item_quantity$vehicle_type %in% unique(empty_quantity$vehicle_type)), ])
nrow(item_quantity[which(item_quantity$place_category %in% unique(empty_quantity$place_category)), ])
```
Since these results came out positive, that means there is no direct correlation of the identification number, pickup place, vehicle type or place category to the item fields being missing. 

If the transaction is completed, meaning the Jumpman successfully delivered the items to the customer from the pickup location, and the item's name, category, quantity are missing; under both these conditions, Jumpman23 can create application that checks for missing data by comparing the empty field to the receipt of the customer and filling out the data accordingly due to their receipt. This way Jumpman23's database will not be negatively impacted by orders with missing items.
  
#### Empty Place Category
This is presumably due to when a business is being onboarded by Jumpman23, the business owner or the customer success agent could have forgotten to include the place category. As mentioned previously, it is important to streamline the business onboarding process to ensure a successful partnership. If the location has a name, the category should be a required field to be entered and stored into the API. This is important because some businesses could be lost in the directory when a customer is looking to purchase by category. As someone who frequently uses delivery services, I can strongly say that when I am in an indecisive mood, I will be looking through the categories to figure out what I'm in the mood for. 

#### Empty Values for Order Time
This has missing values potentially due to the fact that customers could sometimes log in and out of the application due to decision fatigue. If a customer was distracted or another event came up while they were trying to order in the application, it is difficult to track exactly how much time it took for them to order. A quick solution to this could be to only track the time of a customer when they have selected an item for their shopping cart, if that shopping cart is inactive for 30 minutes (this is just an estimation, but this time could be predicted by behavioral data scientists), the timer that is counting time will reset due to inactivity. The timer will finish only when the purchase is completed and therefore that column will not be populated with a value.  

### Conclusion
With the proposed solutions to grow the company and combat data integrity issues, Jumpman23 should see an increase in productivity, new customer acquisition and existing customer retention for an overall growth of 20% or more. 

Thank you for your time. :-)
