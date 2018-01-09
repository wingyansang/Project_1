library(geojsonio)
library(dplyr)
library(data.table)
#assumes current working directory is inside the app folder.

load("../data/popular_lat_long.rda") # for tab 2
load("../data/x11.rda") #for tab 3
load("../data/x12.rda") #for tab 3
load("../data/interboro_dt.rda") # fortab 4
load("../data/interboro_wkend_dt.rda") #for tab 4
load("../data/interboro_gg_dt.rda") #for tab 4
load("../data/interboro_wkend_gg_dt.rda") #for tab 4
load("../data/cpark_df.rda") #for tab 5
load("../data/interboro_df.rda") #for tab 6

neighborhoods <- geojson_read("../data/geojson_data.geojson", what = "sp")
options(DT.options = list(pageLength = 10, lengthMenu = 10))







