#Exploratory Analysis for Project_1


library(ggplot2)
library(reshape2)
library(gridExtra)
library(dplyr)
library(data.table)

  #READ DATASETS
citibike_July <-  fread("201707-citibike-tripdata.csv", sep = ",", header = TRUE, stringsAsFactors = FALSE)
citibike_Jan <-  fread("201701-citibike-tripdata.csv", sep = ",", header = TRUE, stringsAsFactors = FALSE)
weather <-  fread("Weather_data.csv", sep = ",", header = TRUE, stringsAsFactors = FALSE)
weather <- weather %>% select(., DATE, AWND, PRCP, SNOW, SNWD, TMAX, TMIN, WT01, WT02, WT04, WT06, WT08)



#rbind citibike datasets for Jan and July. Results in 2,462,275 rows

colnames(citibike_July) <- c("trip_duration", "start_time", "stop_time", "start_station_id", "start_station_name", "start_station_lat",
                             "start_station_long", "end_station_id", "end_station_name", "end_station_lat", "end_station_long",
                             "bikeid", "usertype", "birth_year", "gender")
colnames(citibike_Jan) <- colnames(citibike_July)

l = list(citibike_Jan, citibike_July)
citibike <- rbindlist(l, use.names = TRUE, fill = FALSE, idcol = FALSE)

colnames(weather) <- tolower(colnames(weather))

#CHECK MISSING VALUES
#1. Check number of missing values
sum( is.na(citibike)) # 29,076
which( colSums( is.na(citibike)) != 0) #14: birth_year

#228,596!!! is NULL due to the birth year column
sum(citibike$birth_year =="NULL", na.rm = TRUE)

#convertTO NA character type
citibike$birth_year <- ifelse(citibike$birth_year == "NA", NA_character_, citibike$birth_year)


#FORMAT COLUMNS
# 1. convert times to date-time
citibike$start_time = as.POSIXct(citibike$start_time, format = "%Y-%m-%d %H:%M:%S")
citibike$stop_time = as.POSIXct(citibike$stop_time, format = "%Y-%m-%d %H:%M:%S")
                                          
#2. convert trip duration to minutes and round to 3 decimals
citibike[,trip_duration := trip_duration/60]
citibike$trip_duration <- round(citibike$trip_duration, 3)

#3. convert birth_year to numeric
citibike[, birth_year := as.numeric(birth_year)]

#4. convert gender to "M", "F" or "U"
citibike$gender <-  ifelse(citibike$gender == 0, "U", ifelse(citibike$gender == 1, 'M', "F"))
       

#CLEAN DATA
# Delete rows where station start name = "*8D OPS 01"
# Delete rows where station start name = "Kiosk in a box Motivate"
# Delete rows with station id = 3446
citibike <- citibike[citibike$start_station_name != "Kiosk in a box Motivate"]
citibike <- citibike[citibike$start_station_name != "8D OPS 01"]
citibike <- citibike%>% filter(., start_station_id != 3446)



#CREATE DATA TABLE OF STATION IDS WITH THEIR SPATIAL COORDINATES (WILL BE OVERLAID WITH NEIGHBORHOOD AND BOROUGH INFO)

start_geo <- citibike %>% group_by(., start_station_id, start_station_name, start_station_lat, start_station_long) %>% 
  summarise(., n()) %>% arrange(., start_station_id)

end_geo <- citibike %>% group_by(., end_station_id, end_station_name, end_station_lat, end_station_long) %>% 
  summarise(., n()) %>% arrange(., end_station_id)


start_geo %>% group_by(., start_station_id) %>% filter(., n() > 1) #some start station ids have different geospatial or names. see below
end_geo %>% group_by(., end_station_id) %>% filter(., n() > 1) #some start station ids have different geospatial or names. see below

# A tibble: 8 x 5
# Groups:   start_station_id [4]
# start_station_id          start_station_name start_station_lat start_station_long `n()`
# <int>                       <chr>             <dbl>              <dbl> <int>
# 1              468          Broadway & W 55 St          40.76527          -73.98192  2807
# 2              468          Broadway & W 56 St          40.76527          -73.98192  4406
# 3             3078      Broadway & Roebling St          40.70925          -73.96063  1659
# 4             3078      Broadway & Roebling St          40.70926          -73.95982   730
# 5             3111     Norman Ave & Leonard St          40.72585          -73.95065   434
# 6             3111 Norman Ave & Leonard St - 2          40.72585          -73.95065   468
# 7             3143             5 Ave & E 78 St          40.77632          -73.96427  4092
# 8             3143             5 Ave & E 78 St          40.77683          -73.96389  1142


#ADD BOROUGH AND NEIGHBORHOOD COLUMNS
citibike2 <- inner_join(citibike, temp_geo_df, by = "start_station_id") #join the start hoods and boroughs
citibike2 <- citibike2 %>% rename(., start_neighborhood = neighborhood, start_borough = borough) #rename added columns to start ....

citibike2 <- inner_join(citibike2, temp_geo_df, by = c("end_station_id" = "start_station_id"))#join end station hoods and boroughs
citibike2 <- citibike2 %>% rename(., end_neighborhood = neighborhood, end_borough = borough) #rename added columns to end ....

# split start/end asPOSIXct objects into day and time
citibike2$start_day_temp <- format(citibike2$start_time, format = "%m-%d")
citibike2$start_day <- as.Date.character(citibike2$start_day_temp, format = "%m-%d")

citibike2$stop_day_temp <- format(citibike2$stop_time, format = "%m-%d")
citibike2$stop_day <- as.Date.character(citibike2$stop_day_temp, format = "%m-%d")

citibike2 <- citibike2[, -c(25,26)] #delete the temporary columns created above

citibike2$start_time2 <- format(citibike2$start_time, format = "%H:%M:%S") 
citibike2$stop_time2 <- format(citibike2$stop_time, format = "%H:%M:%S")

#added a month column for easy grouping for graphs
citibike2$month <- ifelse(citibike2$start_time<= "2017-02-01", "Jan.", "July")

# RESEARCH QUESTIONS:

# 1. Is location of trip start/end affected by season? If so, how?
#   a. What is the daily count of trips in the Jan and July time periods? 
#   b. Which neighorhoods experience the biggest change (positive and negative) in terms of number of trips?
#   c. Any commonalities in these neighborhoods? Eg: 1) distance to waterfront; 2) distance to subway station, 3) distance to bike paths

#daily count of trips 
summary1 <- citibike2 %>% filter(., (usertype !='' & trip_duration <= 60)) %>% 
  group_by (.,month, start_day) %>% summarise(., num_trips = n())
day_of_week <- weekdays.Date(summary1$start_day) #create vector of weekdays
weekday_cat <- ifelse(day_of_week %in% c("Saturday", "Sunday"), "Weekend", "Weekday") #column of "weekday" or "weekend"
summary1$day_of_week <- day_of_week
summary1$weekday_cat <- weekday_cat

g <- ggplot(summary1, aes(x = start_day, y = num_trips, group = month))
g2 <- g + geom_line() + theme_bw() + facet_wrap( ~ month, scales = "free_x")
g2 <- g2 + theme(plot.title = element_text(hjust = 0.5)) + labs(x = "Date", y = "Number of Trips") + 
  ggtitle("Number of Daily Trips Comparison")

#CHANGE IN RIDERSHIP LEVEL: WEEKDAY VERSUS WEEKEND
week_vec2 = c(rep(0,2), rep(1,7), rep(2,7), rep(3,7), rep(4,7), rep(5,1))
week_vec1 = c(rep(0,1), rep(1,7), rep(2,7), rep(3,7), rep(4,7), rep(5,2))
week_vec = c(week_vec1, week_vec2) #first create column with week# and add to summary1
summary1$week_num <- week_vec

  #Comparsion by average daily trips
summary2 <- summary1 %>% group_by(., month, week_num, weekday_cat) %>% summarise(., avg_trips = round(mean(num_trips),0)) %>% 
  filter(., !(week_num %in% c(0,5)))

g <- ggplot(summary2, aes(x=week_num, y = avg_trips, fill = as.factor(weekday_cat)))
g <- g + geom_col(aes(fill = weekday_cat),position = "dodge") + facet_wrap(~ month)
g <- g + scale_fill_manual(values=c("red", "blue"), name = "Legend")
g <- g + labs(x = "Week Number", y ="Avg. Daily Number of Trips", 
               title ="Avg. Daily Trips: Weekday vs. Weekend") + 
  theme(plot.title = element_text(hjust = 0.5), strip.text.x = element_text(size = 14))


  #Comparsion by median daily trips
summary2b <- summary1 %>% group_by(., month, week_num, weekday_cat) %>% summarise(., median_trips = round(median(num_trips),0)) %>% 
  filter(., !(week_num %in% c(0,5)))
g <- ggplot(summary2b, aes(x=week_num, y = median_trips, fill = as.factor(weekday_cat)))
g <- g + geom_col(aes(fill = weekday_cat),position = "dodge") + facet_wrap(~ month)
g <- g + scale_fill_manual(values=c("red", "blue"), name = "Legend")
g <- g + labs(x = "Week Number", y ="Median Daily Number of Trips", 
              title ="Median Daily Trips: Weekday vs. Weekend") + theme(plot.title = element_text(hjust = 0.5))


  #Comparison of Jan. vs. July based on median percent change from weekday to weekend
filtered1b <- summary2b %>% filter(., weekday_cat == "Weekday")
filtered2b <- summary2b %>% filter(., weekday_cat == "Weekend")
filtered1b$weekend_trips <- filtered2b$median_trips
colnames(filtered1)[4] = "weekday_trips"
filtered1b <- filtered1b %>% mutate(., percent_change = round(100*(weekend_trips/weekday_trips-1),0))

g <- ggplot(filtered1b, aes(x=week_num, y = percent_change, fill = as.factor(month)))
g <- g + geom_col(aes(fill = month),position = "dodge")
g <- g + scale_fill_manual(values=c("red", "blue"), name = "Legend")
g <- g + labs(x = "Week Number", y ="Percent Change", 
              title ="Percent Change of Median Dailly Trips from Weekday to Weekend") + theme(plot.title = element_text(hjust = 0.5))

  #Comparison of Jan. vs. July based on average percent change from weekday to weekend
filtered1 <- summary2 %>% filter(., weekday_cat == "Weekday")
filtered2 <- summary2 %>% filter(., weekday_cat == "Weekend")
filtered1$weekend_trips <- filtered2$avg_trips
colnames(filtered1)[4] = "weekday_trips"
filtered1 <- filtered1 %>% mutate(., percent_change = round(100*(weekend_trips/weekday_trips-1),0))

g <- ggplot(filtered1, aes(x=week_num, y = percent_change, fill = as.factor(month)))
g <- g + geom_col(aes(fill = month),position = "dodge")
g <- g + scale_fill_manual(values=c("red", "blue"), name = "Legend")
g <- g + labs(x = "Week Number", y ="Percent Change", 
              title ="Percent Change of Avg. Dailly Trips from Weekday to Weekend") + theme(plot.title = element_text(hjust = 0.5))

  #Comparison of Avg. Ridership by Day of the Week
summary3 <- summary1 %>% group_by(., month, day_of_week) %>% summarise(., avg_trips = round(mean(num_trips),0)) 
summary3$day_of_week <- factor(summary3$day_of_week, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday",
                                                                "Saturday", "Sunday"))
g <- ggplot(summary3, aes(x = day_of_week, y = avg_trips))
g <- g + geom_col(aes(fill = month)) + facet_wrap(~ month) 
g <- g + labs(x = "Day of the Week", y ="Avg. Daily Number of Trips", 
              title ="Avg. Daily Trips by Day of the Week") + theme(plot.title = element_text(hjust = 0.5))


  #Comparison of Median Ridership by Day of the Week
summary3b <- summary1 %>% group_by(., month, day_of_week) %>% summarise(., median_trips = round(median(num_trips),0)) 
summary3b$day_of_week <- factor(summary3b$day_of_week, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday",
                                                                "Saturday", "Sunday"))
g <- ggplot(summary3b, aes(x = day_of_week, y = median_trips))
g <- g + geom_col(aes(fill = month)) + facet_wrap(~ month) 
g <- g + labs(x = "Day of the Week", y ="Median Daily Number of Trips", 
              title ="Median Daily Trips by Day of the Week") + theme(plot.title = element_text(hjust = 0.5))

  #Weekday versus Weekend by User Type (THERE ARE 3,193 OBSERVATIONS WITH MISSING INFORMATION IN USER TYPE)
user_summary <- citibike2 %>% filter(., (usertype !='' & trip_duration <= 60)) %>% 
  group_by (.,month, start_day, usertype) %>% summarise(., num_trips = n()) 
  
user_summary$day_of_week <- weekdays(user_summary$start_day) #add day of week column
user_summary <- user_summary %>% mutate(., weekday_cat = 
                                          ifelse(day_of_week %in% c("Saturday", "Sunday"), "Weekend", "Weekday")) #add weekend/weekday
temp_vec <- user_summary %>% group_by(., start_day) %>% summarise(., num_trips = sum(num_trips)) #vector of total trips by day
user_summary <- inner_join(user_summary, temp_vec, by = "start_day") %>% rename(., num_trips = num_trips.x, daily_total = num_trips.y)

week_vec2 = c(rep(0,4), rep(1,14), rep(2,14), rep(3,14), rep(4,14), rep(5,2))
week_vec1 = c(rep(0,2), rep(1,14), rep(2,14), rep(3,14), rep(4,14), rep(5,4))
week_vec = c(week_vec1, week_vec2)
user_summary$week_num <- week_vec

user_summary <- user_summary %>% group_by(., month, week_num, weekday_cat, usertype) %>% 
  summarise(., num_trips = sum(num_trips), daily_total = sum(daily_total)) %>% filter(., !(week_num %in% c(0,5)))
user_summary <- user_summary %>%mutate(., Percent_of_Trips = round(100*(num_trips/daily_total),0))

filtered_user <- user_summary %>% filter(., usertype == "Customer")

g <- ggplot(filtered_user, aes(x=week_num, y = Percent_of_Trips, fill = as.factor(weekday_cat)))
g <- g + geom_col(aes(fill = weekday_cat),position = "dodge") + facet_wrap(~ month)
g <- g + scale_fill_manual(values=c("red", "blue"), name = "Legend")
g <- g + labs(x = "Week Number", y ="Percent of TotalTrips", 
              title ="Share of Total Daily Trips by Non-Subscribers") + 
  theme(plot.title = element_text(hjust = 0.5), strip.text.x = element_text(size = 14))


g <- ggplot(data = filtered_user, aes(x = week_num, y = Percent_of_Trips, color = weekday_cat))
g1 <- g + geom_line() + facet_wrap( ~ month)

#WHICH STATIONS ARE THE MOST POPULAR: START
popular_start <- citibike2 %>% group_by(., month, start_station_id, start_station_name, start_neighborhood, start_borough) %>% 
  summarise(., count = n()) %>% arrange(., desc(count))
popular_start_Jan <- popular_start %>% filter(., month == "Jan.")
popular_start_July <- popular_start %>% filter(., month == "July")

grid.table(head(popular_start_Jan, 20))
grid.table(head(popular_start_July, 20))

#WHICH STATIONS ARE THE MOST POPULAR: END
popular_end <- citibike2 %>% group_by(., month, end_station_id, end_station_name, end_neighborhood, end_borough) %>% 
  summarise(., count = n()) %>% arrange(., desc(count))
popular_end_Jan <- popular_end %>% filter(., month == "Jan.")
popular_end_July <- popular_end %>% filter(., month == "July")

grid.table(head(popular_end_Jan, 20))
grid.table(head(popular_end_July, 20))

#WHICH STATIONS ARE THE MOST POPULAR: COMBINED
popular_combined_Jan <- inner_join(popular_start_Jan, popular_end_Jan, by = c("start_station_id" = "end_station_id"))
popular_combined_Jan <- popular_combined_Jan %>% select(., month.x, start_station_id, start_station_name,
                          start_neighborhood, count.x, count.y)
popular_combined_Jan$total_trips <- popular_combined_Jan$count.x + popular_combined_Jan$count.y
popular_combined_Jan <- popular_combined_Jan %>% arrange(., desc(total_trips))

popular_combined_July <- inner_join(popular_start_July, popular_end_July, by = c("start_station_id" = "end_station_id"))
popular_combined_July <- popular_combined_July %>% select(., month.x, start_station_id, start_station_name,
                                                        start_neighborhood, count.x, count.y)
popular_combined_July$total_trips <- popular_combined_July$count.x + popular_combined_July$count.y
popular_combined_July <- popular_combined_July %>% arrange(., desc(total_trips))

grid.table(head(popular_combined_Jan, 20))
grid.table(head(popular_combined_July, 20))

#WHICH STATIONS HAD THE BIGGEST INCREASE FROM JAN TO JULY
popular_combined = inner_join(popular_combined_July, popular_combined_Jan, by = "start_station_id")
popular_combined <- popular_combined %>% select(., -c(8, 9, 10))
colnames(popular_combined) = c("IGNORE", "station_id", "station_name", "neighborhood", "July_start_trips", "July_end_trips", "July_combined",
                               "Jan_start_trips", "Jan_end_trips", "Jan_combined")
popular_combined <- popular_combined %>% mutate(., percent_change = round(100*((July_combined/Jan_combined)-1),1)) 

popular_combined_sorted <- popular_combined[order(-popular_combined$percent_change),]
popular_combined_sorted_printing <- popular_combined_sorted %>% select(., c(2,3,4,7,10,11))
grid.table(head(popular_combined_sorted_printing, 30))
grid.table(tail(popular_combined_sorted_printing, 30))

######################### ANALYSIS FOR PANEL 3 of Shiny App ###################################################
#subset the data frame to only include July trips <= 60 minutes and usertype is known
citibike2_July <-  citibike2 %>% filter(., month == "July", trip_duration <= 60, usertype != 'U') 
customer_df <- citibike2_July %>% filter(., usertype == "Customer")
subscriber_df <- citibike2_July%>% filter(., usertype == "Subscriber")


############################ANALYSIS FOR SUBSCRIBERS IN PANEL THREE ####################################
wkend_vec = as.Date(c("2017-07-08", "2017-07-09", "2017-07-15", "2017-07-16", "2017-07-22", "2017-07-23", "2017-07-29", 
                      "2017-07-30"))
x_subscriber <- subscriber_df %>% filter(., start_day %in% wkend_vec) #only 


subscriber_df_wkend <- x_subscriber
x <- subscriber_df %>% filter(., start_day %in% wkend_vec) 
subscriber_df_wkend <- x #filter to get only wkend subscribers
w <- strptime(subscriber_df_wkend$start_time2, format = "%H:%M:%S")
subscriber_df_wkend$start_time2_w <- w # convert start_time2 to POSIXlt
subscriber_df_wkend$start_hour <- subscriber_df_wkend$start_time2_w$hour #get the hour component of time

w = integer(length(subscriber_df_wkend$start_hour)) #for loop to add time period
i = 1
while (i < length(subscriber_df_wkend$start_hour)) {
  if (subscriber_df_wkend$start_hour[i] >=0 & subscriber_df_wkend$start_hour[i]<6) {
    w[i] = 1 } else if (subscriber_df_wkend$start_hour[i]>=6 & subscriber_df_wkend$start_hour[i]< 9) {
      w[i] = 2 } else if (subscriber_df_wkend$start_hour[i]>=9 & subscriber_df_wkend$start_hour[i]< 12) {
        w[i] = 3} else if(subscriber_df_wkend$start_hour[i]>=12 & subscriber_df_wkend$start_hour[i]< 15) {
          w[i] = 4} else if(subscriber_df_wkend$start_hour[i]>=15 & subscriber_df_wkend$start_hour[i]< 18) {
            w[i] = 5}  else if(subscriber_df_wkend$start_hour[i]>=18 & subscriber_df_wkend$start_hour[i]<=21) {
              w[i] = 6} else {
                w[i] = 7
              }
  i = i +1
}

subscriber_df_wkend <- subscriber_df_wkend %>% select(., -c(start_time2_w)) #delete start_time2_w column
subscriber_df_wkend$start_period <- w #append start period
subscriber_df_wkend$start_period[322838] <- 7 #change value of last row
################### ANALYSIS OF NON-SUBSCRIBERS FOR PANEL 3 ###############################################

wkend_vec = as.Date(c("2017-07-08", "2017-07-09", "2017-07-15", "2017-07-16", "2017-07-22", "2017-07-23", "2017-07-29", 
              "2017-07-30"))
x_customer <- customer_df %>% filter(., start_day %in% wkend_vec) #only 

customer_df_wkend <- x_customer

w <- strptime(customer_df_wkend$start_time2, format = "%H:%M:%S")
customer_df_wkend$start_time2_w <- w

customer_df_wkend$start_hour <- customer_df_wkend$start_time2_w$hour

w = integer(101340)
i = 1
while (i < length(customer_df_wkend$start_hour)) {
   if (customer_df_wkend$start_hour[i] >=0 & customer_df_wkend$start_hour[i]<6) {
     w[i] = 1 } else if (customer_df_wkend$start_hour[i]>=6 & customer_df_wkend$start_hour[i]< 9) {
     w[i] = 2 } else if (customer_df_wkend$start_hour[i]>=9 & customer_df_wkend$start_hour[i]< 12) {
       w[i] = 3} else if(customer_df_wkend$start_hour[i]>=12 & customer_df_wkend$start_hour[i]< 15) {
         w[i] = 4} else if(customer_df_wkend$start_hour[i]>=15 & customer_df_wkend$start_hour[i]< 18) {
           w[i] = 5}  else if(customer_df_wkend$start_hour[i]>=18 & customer_df_wkend$start_hour[i]<=21) {
             w[i] = 6} else {
               w[i] = 7
             }
  i = i +1
}
customer_df_wkend$start_period <- w

###### DELETE column start_time_time2_w ###############
customer_df_wkend <- customer_df_wkend %>% select(., -c(start_time2_w))

customer_df_wkend$start_period <- w #append start period
customer_df_wkend$start_period[101340] <- 7 #change value of last row
###################### ANALYSIS FOR PANEL 4 of Shiny App ########################################
  ##Interborough crossings 
subscriber_wkend_dt <- as.data.table(subscriber_df_wkend, keep.rownames = TRUE)

subscriber_wkend_dt[(start_borough != end_borough), .(trips = .N), 
                    by = .(start_period)][order(start_period)][, sum(trips)] #14,826

subscriber_wkend_dt[(start_borough != end_borough), .(trips = .N), 
                    by = .(start_borough, end_borough)][order(-trips)]

customer_wkend_dt <- as.data.table(customer_df_wkend, keep.rownames = TRUE)
customer_wkend_dt[(start_borough != end_borough), .(trips = .N), 
                    by = .(start_period)][order(start_period)][, sum(trips)] #6,852

customer_wkend_dt[(start_borough != end_borough), .(trips = .N), 
                    by = .(start_borough, end_borough)][order(-trips)]

citibike2_July_dt <- as.data.table(citibike2_July, keep.rownames = FALSE)
citibike2_July_dt$day <- lubridate::wday(citibike2_July_dt$start_day, label = TRUE, abbr = FALSE)

setkey(citibike2_July_dt, day)
key(citibike2_July_dt)
citibike2_July_dt[!(day %in% c("Saturday", "Sunday"))][usertype == "Customer"] [(start_borough != end_borough), 
              .(trips = .N), by = .(start_borough, end_borough)][order(-trips)]

citibike2_July_dt[!(day %in% c("Saturday", "Sunday"))][usertype == "Subscriber"] [(start_borough != end_borough), 
               .(trips = .N), by = .(start_borough, end_borough)][order(-trips)]

########################### ANALYSIS FOR PANEL 5 of Shiny App #################################

########Create a data table of interborough crossings
#table of subscriber inter-borough (wkend)
table_1 <- subscriber_wkend_dt[(start_borough != end_borough), .(trips = .N),
                               by = .(start_borough, end_borough)][order(start_borough)]

#table of non-subscriber inter-borough (wkend)
table_2 <- customer_wkend_dt[(start_borough != end_borough), .(trips = .N),
                               by = .(start_borough, end_borough)][order(start_borough)]

#table of non-subscriber inter-borough (wkend)
table_3 <-  citibike2_July_dt[!(day %in% c("Saturday", "Sunday"))][usertype == 
              "Customer"] [(start_borough != end_borough), .(trips = .N), 
              by = .(start_borough, end_borough)][order(start_borough)]

table_4 <-  citibike2_July_dt[!(day %in% c("Saturday", "Sunday"))][usertype == 
              "Subscriber"] [(start_borough != end_borough), .(trips = .N), 
                  by = .(start_borough, end_borough)][order(start_borough)]

interboro_dt <- data.table(table_3, table_4, table_2, table_1)

interboro_dt[[4]] = NULL
interboro_dt[[4]] = NULL
interboro_dt[[6]] = NULL

names(interboro_dt)[3] = "Trips_Non_Subscriber"
names(interboro_dt)[6] = "Trips_Subscriber"

interboro_wkend_dt <- interboro_dt

interboro_dt[[5]] = NULL
interboro_wkend_dt[[3]] = NULL
interboro_dt[, Percent_Non_Subscriber := 
               round(100*(Trips_Non_Subscriber/(Trips_Subscriber+Trips_Non_Subscriber)),1)]
interboro_dt[, Percent_Subscriber := 
               round(100*(Trips_Subscriber/(Trips_Subscriber+Trips_Non_Subscriber)),1)]

x <- data.table(start_borough = "Total", end_borough = '', 
                Trips_Non_Subscriber = '', 
                Trips_Subscriber = '', Percent_Non_Subscriber = '',
                Percent_Subscriber = '')
interboro_dt <- rbind(interboro_dt, x)
interboro_dt[7,3] =sum(as.integer(interboro_dt[[3]][1:6]))
interboro_dt[7,4] =sum(as.integer(interboro_dt[[4]][1:6]))
interboro_dt[7,5] = round(100*(8065/(8065+52722)),1)
interboro_dt[7,6] = round(100*(52722/(8065+52722)),1)

interboro_wkend_dt[, Percent_Non_Subscriber := 
               round(100*(Trips_Non_Subscriber/(Trips_Subscriber+Trips_Non_Subscriber)),1)]
interboro_wkend_dt[, Percent_Subscriber := 
               round(100*(Trips_Subscriber/(Trips_Subscriber+Trips_Non_Subscriber)),1)]

interboro_wkend_dt <- rbind(interboro_wkend_dt, x)
interboro_wkend_dt[7,3] =sum(as.integer(interboro_wkend_dt[[3]][1:6]))
interboro_wkend_dt[7,4] =sum(as.integer(interboro_wkend_dt[[4]][1:6]))
interboro_wkend_dt[7,5] = round(100*(6852/(6852+14826)),1)
interboro_wkend_dt[7,6] = round(100*(14826/(6852+14826)),1)

### CREATE DATA TABLES FOR GGPLOT2
x = rbind(interboro_dt, interboro_dt)
df = as.data.frame(x)
x = x[-c(7,14),]
x = x[, -c(3,4)]
x[7:12, 3] = x[7:12,4]
x[1:6,4] = "Non-Subscriber"
x[7:12,4] = "Subscriber"
colnames(x)[3] = "Percent"
colnames(x)[4] = "User_Type"
vec = as.numeric(x$Percent)
x[,3] = vec

interboro_gg_dt <- x
interboro_gg_dt[1:6,3] = interboro_dt[1:6, 3]
interboro_gg_dt[7:12,3] = interboro_dt[1:6, 4]
colnames(interboro_gg_dt)[3] = "Num_Trips"

x = rbind(interboro_wkend_dt, interboro_wkend_dt)
x = x[-c(7,14),]
x = x[, -c(3,4)]
x[7:12, 3] = x[7:12,4]
x[1:6,4] = "Non-Subscriber"
x[7:12,4] = "Subscriber"
colnames(x)[3] = "Percent"
colnames(x)[4] = "User_Type"
vec = as.numeric(x$Percent)
x[,3] = vec
interboro_wkend_gg_dt <- x  
interboro_wkend_gg_dt[1:6,3] = interboro_wkend_dt[1:6, 3]
interboro_wkend_gg_dt[7:12,3] = interboro_wkend_dt[1:6, 4]
colnames(interboro_wkend_gg_dt)[3] = "Num_Trips"
##########   ADD TOTAL ROW IN DATA TABLE FOR GG PLOT #########
temp_dt <- data.table(start_borough = c('Total','Total'), end_borough = c('',''), 
                  Percent = c(13.3, 86.7), User_Type = c("Non-Subscriber", "Subscriber"))
                

temp_dt <- rbind(interboro_gg_dt, temp_dt)
interboro_gg_dt <- temp_dt
interboro_gg_dt = interboro_gg_dt[-c(13,14),]
interboro_gg_dt[13,4] = "Non-Subscriber"
interboro_gg_dt[14,4] = "Subscriber"

temp_dt <- data.table(start_borough = c('Total','Total'), end_borough = c('',''), 
                      Percent = c((31.6), 68.4), User_Type = c("Non-Subscriber", "Subscriber"))
temp_dt <- rbind(interboro_wkend_gg_dt, temp_dt)
interboro_wkend_gg_dt <- temp_dt

########################### ANALYSIS FOR PANEL 6 of Shiny App ##############################

table_2 <- customer_wkend_dt[(start_borough != end_borough), .(trips = .N),
                             by = .(start_station_id, start_station_name)][order(start_station_id)]

table_3 <- customer_wkend_dt[(start_borough != end_borough), .(trips = .N),
                             by = .(end_station_id, end_station_name)][order(end_station_id)]

table_4 <- left_join(table_2, table_3, by = c("start_station_id" = "end_station_id"))
table_4[is.na(table_4) == 1] = 0
colnames(table_4)[c(3,5)] = c("Outflow", "Inflow")
top_stations <- table_4 %>% mutate(.,  Total = Outflow + Inflow) %>% arrange(., desc(Total))

x <- rbind(customer_df_wkend, subscriber_df_wkend)
x_dt <- as.data.table(x)

out_temp <- x_dt[start_station_id %in% c( 387, 532, 232,217, 392, 321, 3129, 412, 322, 2000)]  

out_temp <- out_temp[, .(Outflow = .N),by = .(start_period, start_station_id, start_station_name,
        start_station_lat, start_station_long)][order(start_period)][order(start_station_id)]

in_temp <- x_dt[end_station_id %in% c( 387, 532, 232,217, 392, 321, 3129, 412, 322, 2000)]  

in_temp <- in_temp[, .(Inflow = .N),by = .(start_period, end_station_name, 
        end_station_id)][order(start_period)][order(end_station_id)]

x_temp <- cbind(out_temp, in_temp)
colnames(x_temp)
x_temp <-  x_temp[,-c(7:8)]
x_temp <- x_temp[,-c(7)]
x_temp[,c("Difference") := (Inflow - Outflow)]

interboro_df <- x_temp

colnames(interboro_df)[c(2,3)] = c("station_id", "station_name")
#######################   ANALYSIS FOR PANEL 4 of Shiny App ####################################
x <- rbind(customer_df_wkend, subscriber_df_wkend)
x_dt <- as.data.table(x)

#outflows from Central Park
out_temp <- x_dt[start_neighborhood == 'Central Park' & start_period> 2 & start_period < 7,
    .(outflow = .N), by = .(start_period, start_station_id)][order(start_period)][order(start_station_id)]

#inflows into Central Park
in_temp <- x_dt[end_neighborhood == 'Central Park' & start_period> 2 & start_period < 7,
     .(inflow = .N), by = .(start_period, end_station_id)][order(start_period)][order(end_station_id)]

same_temp  <- x_dt[start_neighborhood == 'Central Park' & end_neighborhood == 'Central Park' & 
       start_period> 2 & start_period < 7, .(same = .N), by = 
       .(start_period, end_station_id)][order(start_period)][order(end_station_id)]

x <- in_temp[,2:3]
out_temp <- out_temp[,-c(4)]
x2 <- cbind(out_temp, x[,1:2])
x3 <- cbind(x2, same_temp[,2:3])
x3 <- x3[, c("Difference") := (inflow - outflow)]
x3 <- inner_join(x3, start_geo2, by = "start_station_id")
x3 <- x3[, -c(9:11)]

cpark_df <- x3
cpark_df <- cpark_df[-c(14,16,18, 20),]
cpark_dt <- as.data.table(cpark_df)
colnames(cpark_df)[c(2,6)] = c("station_id", "station_name")
colnames(cpark_df)[c(3,4)] = c("Outflow", "Inflow")
################ EXAMINE WEATHER'S IMPACT ON RIDERSHIP LEVELS #######################
citibike2_Jan <-  citibike2 %>% filter(., month == "Jan.", trip_duration <= 60, usertype !='') 

###combined citibike2_Jan with weather data in console
citibike2_Jan <-  inner_join(citibike2_Jan, weather, by = c("start_day" = "date"))
citibike2_Jan <- citibike2_Jan %>% select(., -c(31:35))

x <- citibike2_Jan %>% group_by(., start_day, mid, usertype) %>% summarise(., trips = n()) #used in 2nd scatterplot below

#density function of trip duration
plot(density(citibike2_Jan$trip_duration))
summary(citibike2_Jan$trip_duration)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 1.017   5.433   8.567  10.842  13.783  59.967 

#Histogram of trips by day
temp_df <- citibike2_Jan %>% group_by(., start_day, tmax, tmin, mid) %>% summarise(., trips = n()) 
citibike2_Jan_sum <- temp_df
#summary table of trips by day with weather data

g <- ggplot(data = citibike2_Jan_sum)
g2 <-  g + geom_line(aes(x = start_day, y = mid)) + ylab("Temperature (Fahrenheit)") + xlab("Date") +
      ggtitle("Daily Temperature") + theme(plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),
          axis.title = element_text(size = 14, face = "bold"), axis.text = element_text(size = 12))

#Daily temperature graph
g3 <-  g2 + geom_text(x=citibike2_Jan_sum$start_day[12], y=citibike2_Jan_sum$mid[12], label="January 12: 66°F", vjust = -0.5) + 
         geom_text(x=citibike2_Jan_sum$start_day[26], y=citibike2_Jan_sum$mid[26], label="January 26: 56°F", vjust = -0.5) + 
          geom_text(x=citibike2_Jan_sum$start_day[9], y=citibike2_Jan_sum$mid[9], label="January 9: 23°F", vjust = +2) +
          geom_text(x=citibike2_Jan_sum$start_day[24], y=citibike2_Jan_sum$mid[24], label="January 24: 38°F", vjust = +2)  
  
#Scatterplot of temperature versus number of trips
p <- ggplot(citibike2_Jan_sum, aes(x=mid, y= trips)) + geom_point(shape=19) +  geom_smooth(method=lm)  + 
  xlab("Temperature (Fahrenheit)") + ylab("Number of Trips") + ggtitle("Temperature vs. Number of Trips") + 
  theme(plot.title = element_text(hjust = 0.5, size = 18, face = "bold"), 
        axis.title = element_text(size = 14, face = "bold"), axis.text =  element_text(size = 12)) +
  geom_text(x=citibike2_Jan_sum$mid[12], y=citibike2_Jan_sum$trips[12],  label="January 12: 66°F", hjust = +1.2, vjust = 0.2) +
  geom_jitter()

#Split scatterplot into subscribers versus non-subscribers 
g <- ggplot(x, aes(x=mid, y= trips, group = usertype, fill = usertype)) + geom_point(shape=19) +  geom_smooth(method=lm)  + 
  xlab("Temperature (Fahrenheit)") + ylab("Number of Trips") + ggtitle("Daily Temperature") + theme(plot.title = element_text(hjust = 0.5)) +
  geom_text(x=x$mid[24], y=x$trips[24],  label="January 12: 66°F", hjust = +1.2, vjust = 0.2)

############################FURTHER EXPLORATORY ANALYSIS OF JANUARY 2017#################################
weekday_vec = wday(citibike2_Jan$start_day, label = TRUE, abbr = FALSE)
temp_vec <- citibike2_Jan %>% filter(., !(start_day %in% c(2017-01-01, 2017-01-30, 2017-01-31)))
temp_vec$day <- weekday_vec
temp_vec %>% group_by(., day) %>% summarise(., Average = n()/4)
temp_vec_dt <-  as.data.table(temp_vec, keep.rownames = TRUE)
temp_vec_dt[, .( average = .N/4), by = .(day, usertype)]
temp_vec_dt[, .(trips = .N, time = mean(trip_duration)), by = .(start_day, usertype)]
temp <- temp_vec_dt[, .(trips = .N), by = .(start_day, usertype, start_neighborhood)]
setkey(temp, start_day)
key(temp)
x <- temp[, by = .(start_day, usertype), .SD[order(-trips)][c(1:5)]]
