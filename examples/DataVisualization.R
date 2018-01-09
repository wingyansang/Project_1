#file to create data frames/static charts used in some of the Shiny App panels


library(leaflet)
library(dplyr)
library(geojsonio)
library(sp)
library(scales)


#################STATIC MAP FOR PANEL 1 #########################################################

# Code to create popular_lat_long
x <-  inner_join(popular_combined_sorted, start_geo2, by = c("station_id" = "start_station_id"))
 
x <- x %>% select(., -c(5,6,8,9,12,15,16))
 
popular_lat_long <- x
colnames(popular_lat_long)[4] = "neighborhood"
colnames(popular_lat_long[8:9]) = c("latitude", "longitude")
popular_lat_long <- popular_lat_long %>% arrange(., desc(percent_change))

leaflet(data = popular_lat_long[1:100,]) %>% addTiles() %>%
  addMarkers(~start_station_long, ~start_station_lat, popup = ~as.character(percent_change), 
             label = ~as.character(percent_change))

####### CREATE DATA FRAME FOR EACH USER TYPE FOR PANEL 3 #########################################
      ###################### SUBSCRIBERS ############################
#convert number of trips by time period into columns
#  number of trips in each neighborhood by time period
x2 <- subscriber_df_wkend %>% group_by(., start_period, start_neighborhood) %>% summarise(., trips = n()) 
col1 = x2 %>% filter(., start_period == 1) 
col2 = x2 %>% filter(., start_period == 2) 
col3 = x2 %>% filter(., start_period == 3) 
col4 = x2 %>% filter(., start_period == 4) 
col5 = x2 %>% filter(., start_period == 5) 
col6 = x2 %>% filter(., start_period == 6)
col7 = x2 %>% filter(., start_period == 7) 

neighborhood_names = data.frame(unique(neighborhood_df$neighborhoods), stringsAsFactors = FALSE)
colnames(neighborhood_names) = "start_neighborhood"
x3 = left_join(neighborhood_names, col1, by = "start_neighborhood")
x4 = left_join(x3, col2, by = "start_neighborhood")
x5 = left_join(x4, col3, by = "start_neighborhood")
x6 = left_join(x5, col4, by = "start_neighborhood")
x7 = left_join(x6, col5, by = "start_neighborhood")
x8 = left_join(x7, col6, by = "start_neighborhood")
x9 = left_join(x8, col7, by = "start_neighborhood")

x10 = x9 %>% select(., -c(2, 4, 6, 8, 10, 12, 14))

colnames(x10) = c("neighborhoods", "P1", "P2", "P3", "P4", "P5", "P6", "P7")

x10[is.na(x10) == 1] = 0 # set NAs to O
all_names <-  neighborhoods$neighborhood #turn neighborhood names from SP data frame into characters
x_df <- data.frame(neighborhoods = all_names)
char_vec <- as.character(x_df$neighborhoods)
x_df$neighborhoods = char_vec
x12 = left_join(x_df, x10, by = "neighborhoods") #add all the missing neighborhoods into the summary table

x12[is.na(x12) == 1] = 0 # set NAs to O. THIS DATA FRAME WILL BE USED IN SHINY PANEL 3


###################### NON-SUBSCRIBERS ############################
#convert number of trips by time period into columns
#  number of trips in each neighborhood by time period
x2 <- customer_df_wkend %>% group_by(., start_period, start_neighborhood) %>% summarise(., trips = n()) 
col1 = x2 %>% filter(., start_period == 1) 
col2 = x2 %>% filter(., start_period == 2) 
col3 = x2 %>% filter(., start_period == 3) 
col4 = x2 %>% filter(., start_period == 4) 
col5 = x2 %>% filter(., start_period == 5) 
col6 = x2 %>% filter(., start_period == 6)
col7 = x2 %>% filter(., start_period == 7) 

neighborhood_names = data.frame(unique(neighborhood_df$neighborhoods), stringsAsFactors = FALSE)
colnames(neighborhood_names) = "start_neighborhood"
x3 = left_join(neighborhood_names, col1, by = "start_neighborhood")
x4 = left_join(x3, col2, by = "start_neighborhood")
x5 = left_join(x4, col3, by = "start_neighborhood")
x6 = left_join(x5, col4, by = "start_neighborhood")
x7 = left_join(x6, col5, by = "start_neighborhood")
x8 = left_join(x7, col6, by = "start_neighborhood")
x9 = left_join(x8, col7, by = "start_neighborhood")

x10 = x9 %>% select(., -c(2, 4, 6, 8, 10, 12, 14))

colnames(x10) = c("neighborhoods", "P1", "P2", "P3", "P4", "P5", "P6", "P7")

x10[is.na(x10) == 1] = 0 # set NAs to O
all_names <-  neighborhoods$neighborhood #turn neighborhood names from SP data frame into characters
x_df <- data.frame(neighborhoods = all_names)
char_vec <- as.character(x_df$neighborhoods)
x_df$neighborhoods = char_vec
x11 = left_join(x_df, x10, by = "neighborhoods") #add all the missing neighborhoods into the summary table

x11[is.na(x11) == 1] = 0 # set NAs to O. THIS DATA FRAME WILL BE USED IN SHINY PANEL 3

########################## STATIC MAP FOR PANEL 3 ########################################################
neighborhoods <- geojson_read("geojson_data.geojson", what = "sp") ##Moved to Shiny Project Folder


neighborhoods$neighborhood = x
pal <- colorNumeric("Blues", domain = NULL)

map <- leaflet(neighborhoods) %>% addTiles %>% addPolygons(stroke = FALSE, smoothFactor = .3, fillOpacity = 1,
color = ~pal(x11$P1), label = neighborhoods$neighborhood)

########################## STATIC CHARTS FOR PANEL 4 ########################################################
dt = interboro_gg_dt
# dt <- dt %>% mutate(pos=cumsum(Percent)-0.5*Percent)
g<- ggplot(dt, aes(x =start_borough, y = Percent, fill = User_Type))
g <-  g + geom_col(aes(fill = User_Type), width = 1)
pie <- g + coord_polar(theta = "y")

pie + theme_void() + ggtitle("Share of Inter-borough Crossings (Weekday)") + 
  geom_text(aes(x = 1, 
      label = percent(Percent/sum(Percent))), position=position_stack(vjust=0.5), size=5) +
          theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"))



pie + scale_fill_brewer("Blues") + blank_theme +
  theme(axis.text.x=element_blank())+
  geom_text(aes(y = Num_Trips/2 + c(0, cumsum(Num_Trips)[-length(Num_Trips)]), 
                label = percent(Num_Trips/100)), size=5)
