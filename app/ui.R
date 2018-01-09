library(shiny)
library(leaflet)
library(data.table)

#choice of drop-downs for tab3
timePeriod <- c(
  "12:00am - 6:00am",
  " 6:00am - 9:00am",
  " 9:00am - 12:00pm",
  "12:00pm -  3:00pm",
  " 3:00pm -  6:00pm",
  " 6:00pm -  9:00pm",
  " 9:00pm - 12:00am")
  
  
  # "12:00am - 6:00am" = "P1",
  # " 6:00am - 9:00am" = "P2",
  # " 9:00am - 12:00pm" = "p3",
  # "12:00pm -  3:00pm" = "P4",
  # " 3:00pm -  6:00pm" = "P5",
  # " 6:00pm -  9:00pm" = "P6",
  # " 9:00pm - 12:00am" = "P7")
 
shinyUI(
  navbarPage(theme = "cerulean", "Citi Bike Visual Exploration", fluid = TRUE,
  
#**************PANEL ONE******************************************** 
tabPanel("Seasonal Comparison",
         titlePanel("Ridership Analysis: Weekday versus Weekend"),
         fluidRow(
           tabsetPanel(
             tabPanel("All Citi Bike Riders",
                      fluidRow(
                        column(8, offset = 2,
                               img(src='AvgTrips_WkdayVsEnd.jpeg', height = 600, width = 900, align = "right")))),
             tabPanel("Non-Subscribers",
                      fluidRow(
                        column(8, offset = 2,
                               img(src='Non-SubscriberShare_WkdayVsEnd.jpeg', height = 600, width = 900, align = "right"))))
           )
         )
),

#**************PANEL TWO********************************************  

tabPanel("Ridership Level Analysis",
         fluidPage(
           fluidRow(
             column(4, fluidRow(
               div(style = "height: 100px;",
                   sliderInput("range",
                               "From Highest to Lowest Rank Selected",
                               min = 0,
                               max = 100, step = 10,
                               value = c(0,20)))),
               div(style = "height: 100px;",        
                   fluidRow(
                     DT::dataTableOutput("table")))),
             
             # Show a map of the popular stations
             
             h3("Location of Largest Increase in Activity: Jan-17 to July-17"),
             column(8,
                    leafletOutput("map", width = 800, height = 650)
             ))
         )),



#**************PANEL THREE********************************************  
tabPanel("Subscriber vs. Non-Subscriber",
      fluidPage(
           fluidRow(
              column(6,
                h3("Subscriber Weekend Bike Trips in July 2017"),
                selectInput("var2",label = "Choose a Time Period to Display:",
                        choices = c("12am - 6am", "6am - 9am", "9am - 12pm", "12pm -  3pm",
                                    "3pm -  6pm", "6pm -  9pm", "9pm - 12am"),
                        selected = "12am - 6am"),
              leafletOutput("map3", width = 550, height = 600)),
              column(6,
                h3("Non-Subscriber Weekend Bike Trips in July 2017"),
                selectInput("var",label = "Choose a Time Period to Display:",
                         choices = c("12am - 6am", "6am - 9am", "9am - 12pm", "12pm -  3pm",
                                     "3pm -  6pm", "6pm -  9pm", "9pm - 12am"),
                         selected = "12am - 6am"),

           # Show a map of the popular stations
                    leafletOutput("map2", width = 550, height = 600))
                    ))
  ),
#**************PANEL FOUR ********************************************  
tabPanel("Central Park Stations",
         fluidPage(
           fluidRow(
             column(4, fluidRow(
               div(style = "height: 250px;",
                   h3("Citi Bike Stations Analysis"),
                   selectInput("time_CP",label = "Choose a Time Period to Display:",
                               choices = c("9am - 12pm", "12pm -  3pm",
                                           "3pm -  6pm", "6pm -  9pm"),
                               selected = "9am - 12pm"))),
               div(style = "height: 200px;",        
                   fluidRow(
                     DT::dataTableOutput("table_CP" 
                                         )))),
             
             h3("Central Park Stations in July (Weekend)", align = "center"),
             column(8,
                    leafletOutput("map_CP", width = 800, height = 650)
             )))
),

#**************PANEL FIVE ********************************************  

tabPanel("Inter-borough Analysis",
         h2("Inter-borough Trips"),
         tabsetPanel(
           tabPanel("Pie Chart - Weekday",
                    sidebarLayout(
                      sidebarPanel(
                        radioButtons("weekday", 
                                     label = h4("Choose Route (Weekday):"),
                                     choices = c("All",
                                                 "Brooklyn to Manhattan", 
                                                 "Brooklyn to Queens", 
                                                 "Manhattan to Brooklyn",
                                                 "Manhattan to Queens",
                                                 "Queens to Brooklyn",
                                                 "Queens to Manhattan"),
                                     selected = "All")
                      ),
                      mainPanel(
                        h3("Share of Inter-borough Trips (Weekday)", aligne = 'center'),
                        plotOutput("plot1", height = 600, width = 700)
                      ))),
           tabPanel("Pie Chart - Weekend",
                    sidebarLayout(
                      sidebarPanel(
                        radioButtons("weekend", 
                                     label = h4("Choose Route (Weekend):"),
                                     choices = c("All",
                                                 "Brooklyn to Manhattan", 
                                                 "Brooklyn to Queens", 
                                                 "Manhattan to Brooklyn",
                                                 "Manhattan to Queens",
                                                 "Queens to Brooklyn",
                                                 "Queens to Manhattan"),
                                     selected = "All")
                      ),
                      mainPanel(
                        h3("Share of Inter-borough Trips (Weekend)", aligne = 'center'),
                        plotOutput("plot2", height = 600, width = 700)
                      ))),
           
           tabPanel("Data Table - Weekday",
                    fluidRow(
                      h3("Weekday Inter-borough Trips", align = "center"),
                      column(8, offset = 2,
                             fluidRow(
                               DT::dataTableOutput("table1")
                             )
                      ))),
           
           tabPanel("Data Table - Weekend",
                    fluidRow(
                      h3("Weekend Inter-borough Trips", align = "center"),
                      column(8, offset = 2,
                             fluidRow(
                               DT::dataTableOutput("table2")
                             )
                      )))
         )),



#**************PANEL SIX ********************************************  
tabPanel("Inter-borough Stations",
         fluidPage(
           fluidRow(
             column(4, fluidRow(
               div(style = "height: 250px;",
                   h3("Citi Bike Stations Analysis"),
                   selectInput("time_InterB",label = "Choose a Time Period to Display:",
                               choices = c("9am - 12pm", "12pm -  3pm",
                                           "3pm -  6pm", "6pm -  9pm"),
                               selected = "9am - 12pm"))),
               div(style = "height: 200px;",        
                   fluidRow(
                     DT::dataTableOutput("table_InterB" 
                     )))),
             
             h3("Top Inter-Borough Start/End Stations", align = "center"),
             column(8,
                    leafletOutput("map_InterB", width = 800, height = 650)
             )))
)

))
