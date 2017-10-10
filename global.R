library(geojsonio)
library(dplyr)
library(data.table)

load("./popular_lat_long.rda") # for tab 2
load("./x11.rda") #for tab 3
load("./x12.rda") #for tab 3
load("./interboro_dt.rda") # fortab 4
load("./interboro_wkend_dt.rda") #for tab 4
load("./interboro_gg_dt.rda") #for tab 4
load("./interboro_wkend_gg_dt.rda") #for tab 4
load("./cpark_df.rda") #for tab 5
load("./interboro_df.rda") #for tab 6

neighborhoods <- geojson_read("./geojson_data.geojson", what = "sp")
options(DT.options = list(pageLength = 10, lengthMenu = 10))







