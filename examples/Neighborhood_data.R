#Create data frame of neighborhoods and boroughs for each station based on geojson file

library(geojsonio)
library(sp)
library(dplyr)

# grab the data
json_data <- 
  geojson_read("./geojson_data.geojson", method = "local", parse = TRUE)

# create vector of neigbhorhoods
neighborhoods <- json_data

# create list of coordinates
coordinates <- json_data$features$geometry$coordinates

# create vector of boroughs
boroughs <- json_data$features$properties$borough

# combine vectors/lists to form data frame
neighborhood_df <- data.frame(neighborhoods = neighborhoods, boroughs = boroughs)
neighborhood_df$coordinates <- coordinates
neighborhood_df$neighborhoods <- as.character(neighborhood_df$neighborhoods)

#use point.in.polygon function to find whether station coordinate falls within polygon
#Test 1: Upper East Side with station 3152 (71st and 3rd Ave). Output: 1 = Yes!
Vertex <- matrix(c(-73.96120, 40.76874),1, 2)
query <- neighborhood_df$coordinates[[290]][1,,]
point.in.polygon(Vertex[1], Vertex[2], query[,1], query[,2])

#Test 2: Station 3143 (78th St and 5th Ave) in Central Park. Output: 1 = Yes!
Vertex2 =  matrix(c(-73.96389, 40.77683 ),1, 2)
query2 <- neighborhood_df$coordinates[[50]][1,,]
point.in.polygon(Vertex2[1], Vertex2[2], query2[,1], query2[,2])

#Test 3: Station 2004 (6th ave and Broome) in Soho? Output: 1 = Yes!
Vertex3 =  matrix(c(-74.00470, 40.72440),1, 2)
query3 <- neighborhood_df$coordinates[[266]][1,,]
point.in.polygon(Vertex3[1], Vertex3[2], query3[,1], query3[,2])

#Test 4: Station 258 (DeKalb and Vanderbilt) in Clinton Hill?Output: 1 = Yes!
Vertex4 = matrix(c(-73.96885 , 40.68941),1, 2)
query4 <- neighborhood_df$coordinates[[61]][1,,]
point.in.polygon(Vertex4[1], Vertex4[2], query4[,1], query4[,2])

#remove Staten Island and Bronx from neighborhoods data frame
neighborhood_df <- neighborhood_df %>% filter(., !(boroughs %in% c("Bronx", "Staten Island")))

#recode boroughs as characters
neighborhood_df$boroughs <- as.character(neighborhood_df$boroughs)

#arrange start_geo by start station id in ascending order and only select lat and long columns
start_geo <- start_geo %>% arrange(., start_station_id)
start_reduced <- data.frame(station_id = start_geo$start_station_id, longitude = start_geo$start_station_long, 
                            latitude = start_geo$start_station_lat)

#define a function that takes lat/long info from 1 data frame and attaches the neigbhorhood and borough data from 
#corresponding row in 2nd data frame; x is the xth row in df1

Attach_hood = function (df1, x, df2) {
  vec = c(NULL)
  j = 1
    while (is.null(vec)) {
    if (point.in.polygon(df1[x,2], df1[x,3], df2[[3]][[j]][,,1], df2[[3]][[j]][,,2]) != 0) {
      vec[1] = df2[[1]][[j]]
      vec[2] = df2[[2]][[j]]
              
    } else{
      j = j + 1
    }
    }
  return (vec) 
}

#filter out station ids 3446 because it gave an error when for loop was run below
start_geo <- start_geo %>% filter(., start_station_id != 3446)
start_reduced <- start_reduced %>% filter(.,station_id != 3446)

# for loop to create data frame of neighborhoods and boroughs
#length(start_reduced$station_id)

n = length(start_reduced$station_id)
temp_geo_df <- data.frame(station_id = start_reduced$station_id, neighborhood = character(n), borough = character(n),
                          stringsAsFactors = FALSE)

for (i in 1:n) {
  temp_geo_df[i,2] = Attach_hood(start_reduced, i, neighborhood_df)[1]
  temp_geo_df[i,3] = Attach_hood(start_reduced, i, neighborhood_df)[2]
  i = i + 1
}

#filter out duplicate station ids
temp_geo_df %>% group_by(., station_id) %>% filter(., n()>1) #rows
temp_geo_df <- temp_geo_df[-c(228,351,378,409),]

#join temp_geo_df with start_geo to start_geo df to review the accuracy of new hood and borough columns.
temp_geo_df <- temp_geo_df %>% rename(., start_station_id = station_id) #first rename join by column in temp_geo_df
start_geo2 <- inner_join(start_geo, temp_geo_df, by = "start_station_id")
