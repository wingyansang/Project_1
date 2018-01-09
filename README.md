Project_1

This is a Citi Bike visual exploration project focused on the ridership behavior of non-subscribers (1-day and 3-day members). Data source 
files are the January and July Citibike Trip History data found here:

https://s3.amazonaws.com/tripdata/index.html

The following is a description of the Github repository:

'app' folder:
Contains the Shiny app.   The 'www' folder contains the image files called in the server.R file.

'data' folder:
Contains data source files from  national weather service, and the rda (R data frame/data table files) files with the data used in the app. They are loaded in the global.R file within the 'app' folder. 

'examples' folder:
Contains 3 files.
- Exploratory_Analysis.R contains the exploratory analyses used to create many of the rda files in the 'data' folder.
- Neighborhood_data.R contains the code used to mark each station with the neighborhood and borough based on coordinates in geojson file.
- DataVisualization.R contains the code to create some of the data frames and static charts/maps used in the Shiny app.


The Shiny app can be found here:
 https://wing.shinyapps.io/Project1/

Blog post:
 https://nycdatascience.com/blog/student-works/citi-bike-visual-exploration/
