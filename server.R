library(shiny)
library(leaflet)
library(dplyr)
library(scales)
library(ggplot2)
library(data.table)


shinyServer(function(input, output, session) {
  
  initial_lat = 40.737694
  initial_lng = -73.9888917
  initial_zoom = 12
  
#************CODE FOR TAB 2 ***********************************************
  filteredData <- reactive({
    min_r = input$range[1]
    max_r= input$range[2]
    popular_lat_long[min_r:max_r,]
  })
  
  dt <- reactive({
    min_r = input$range[1]
    max_r= input$range[2]
    data <-  popular_lat_long %>% select(., c(3,4,7)) 
    data[min_r:max_r,]
})
  
  output$map <- renderLeaflet({
    leaflet() %>% addTiles() %>% 
      addMarkers(data = filteredData(), ~start_station_long, ~start_station_lat, 
        label = ~as.character(percent_change)) %>% 
        setView(initial_lng, initial_lat, zoom = 12)
      
    })
  
    output$table <- DT::renderDataTable(DT::datatable({
      dt()
      }))
    

#************CODE FOR TAB 3 ***********************************************
#CODE FOR NON-SUBSCRIBER MAP
    
    tripsPlot <- reactive({
      switch(input$var, 
             "12am - 6am" = x11$P1,
             "6am - 9am" = x11$P2,
             "9am - 12pm" = x11$P3,
             "12pm -  3pm" = x11$P4,
             "3pm -  6pm" = x11$P5,
             "6pm -  9pm" = x11$P6,
             "9pm - 12am" = x11$P7)
    })
    
    pal <- colorNumeric("Blues", domain = NULL)
    
    output$map2 <- renderLeaflet({
      leaflet(neighborhoods) %>% addTiles()  %>% 
        addPolygons(stroke = FALSE, smoothFactor = .3, fillOpacity = 1, color = ~pal(tripsPlot()), 
                    label = paste0(neighborhoods$neighborhood, ": ", tripsPlot()), 
                    labelOptions= labelOptions(noHide = T, opacity = 1, textsize = "12px")) %>%
        setView(lat = initial_lat, lng = initial_lng, zoom = initial_zoom) %>%
        addLegend(pal = pal,
                  values  = c(tripsPlot()),
                  position = "bottomleft",
                  title = "Number of Trips")
    })
      
  #CODE FOR SUBSCRIBER MAP
      tripsPlot2 <- reactive({
        switch(input$var2,
               "12am - 6am" = x12$P1,
               "6am - 9am" = x12$P2,
               "9am - 12pm" = x12$P3,
               "12pm -  3pm" = x12$P4,
               "3pm -  6pm" = x12$P5,
               "6pm -  9pm" = x12$P6,
               "9pm - 12am" = x12$P7)
      })

      pal2 <- colorNumeric("Blues", domain = NULL)

      output$map3 <- renderLeaflet({
        leaflet(neighborhoods) %>% addTiles()  %>%
          addPolygons(stroke = FALSE, smoothFactor = .3, fillOpacity = 1, color = ~pal2(tripsPlot2()),
                      label = paste0(neighborhoods$neighborhood, ": ", tripsPlot2()),
                      labelOptions= labelOptions(noHide = T, opacity = 1, textsize = "12px")) %>%
          setView(lat = initial_lat, lng = initial_lng, zoom = initial_zoom) %>%
          addLegend(pal = pal2,
                    values  = c(tripsPlot2()),
                    position = "bottomleft",
                    title = "Number of Trips")


      #labelOptions(noHide = FALSE, opacity = 0.2)
    })

  #************CODE FOR TAB 4 ***********************************************
      dt_CP <- reactive({
        dt <- switch(input$time_CP,
                     "9am - 12pm" = cpark_df %>% filter(., start_period == 3),
                     "12pm -  3pm" =cpark_df %>% filter(., start_period == 4),
                     "3pm -  6pm" = cpark_df %>% filter(., start_period == 5),
                     "6pm -  9pm" = cpark_df %>% filter(., start_period == 6))
        dt %>% select(., station_name, Inflow, Outflow, Difference) %>% arrange(., Difference)
      })
      
      output$table_CP <- DT::renderDataTable({DT::datatable(
        dt_CP(), options = list(pageLength = 5, lengthMenu = 5))
        })
        
      
      filteredData_CP <- reactive({
        switch(input$time_CP,
               "9am - 12pm" = cpark_df %>% filter(., start_period == 3),
               "12pm -  3pm" =cpark_df %>% filter(., start_period == 4),
               "3pm -  6pm" = cpark_df %>% filter(., start_period == 5),
               "6pm -  9pm" = cpark_df %>% filter(., start_period == 6))
      })
      
      output$map_CP <- renderLeaflet({
        dt <- filteredData_CP()
        radius <- ifelse(abs(dt$Difference)>150, 25, ifelse(abs(dt$Difference)>100, 20, 
                                                            ifelse(abs(dt$Difference)>50, 15, 10)))
        
        color_vec = ifelse(dt$Difference >0, "green", "red")
        leaflet() %>% addTiles() %>% clearShapes() %>%
          addCircleMarkers(data = dt, ~start_station_long, ~start_station_lat, 
                           radius=radius, stroke=FALSE, fillOpacity=.6, fillColor=color_vec,
                           label = ~as.character(Difference)) %>%
          addLegend("bottomright", colors=c("green", "red"), labels = c("net inflow", "net outflow")) %>%
          setView(lat = 40.7794302, lng = -73.9712564, zoom = 14)})
   
        #************CODE FOR TAB 5 ***********************************************       
        boroPlot1 <- reactive({
          switch(input$weekday, 
                 "All" = interboro_gg_dt[start_borough == "Total"],
                 "Brooklyn to Manhattan" = interboro_gg_dt[start_borough == "Brooklyn" & end_borough == "Manhattan"],
                 "Brooklyn to Queens" = interboro_gg_dt[start_borough == "Brooklyn" & end_borough == "Queens"],
                 "Manhattan to Brooklyn" = interboro_gg_dt[start_borough == "Manhattan" & end_borough == "Brooklyn"],
                 "Manhattan to Queens" = interboro_gg_dt[start_borough == "Manhattan" & end_borough == "Queens"],
                 "Queens to Brooklyn" = interboro_gg_dt[start_borough == "Queens" & end_borough == "Brooklyn"],
                 "Queens to Manhattan" = interboro_gg_dt[start_borough == "Queens" & end_borough == "Manhattan"]
          )
          
        })
        output$plot1 <- renderPlot({
          dt = boroPlot1()
          g<- ggplot(dt, aes(x =start_borough, y = Percent, fill = User_Type))
          g <-  g + geom_col(aes(fill = User_Type), width = 1)
          pie <- g + coord_polar(theta = "y")
          
          pie + theme_void() +  geom_text(aes(x = 1, label = percent(Percent/sum(Percent)),
                                              size = 32, fontface = "bold"), position = position_stack(vjust=0.5), size=5) + 
            theme( legend.title=element_text(size=24, face = "bold"), 
                   legend.text=element_text(size=14, face = "bold"))
          
        })
        
        boroPlot2 <- reactive({
          switch(input$weekend, 
                 "All" = interboro_wkend_gg_dt[start_borough == "Total"],
                 "Brooklyn to Manhattan" = interboro_wkend_gg_dt[start_borough == "Brooklyn" & end_borough == "Manhattan"],
                 "Brooklyn to Queens" = interboro_wkend_gg_dt[start_borough == "Brooklyn" & end_borough == "Queens"],
                 "Manhattan to Brooklyn" = interboro_wkend_gg_dt[start_borough == "Manhattan" & end_borough == "Brooklyn"],
                 "Manhattan to Queens" = interboro_wkend_gg_dt[start_borough == "Manhattan" & end_borough == "Queens"],
                 "Queens to Brooklyn" = interboro_wkend_gg_dt[start_borough == "Queens" & end_borough == "Brooklyn"],
                 "Queens to Manhattan" = interboro_wkend_gg_dt[start_borough == "Queens" & end_borough == "Manhattan"]
          )
          
        })
        output$plot2 <- renderPlot({
          dt = boroPlot2()
          g<- ggplot(dt, aes(x =start_borough, y = Percent, fill = User_Type))
          g <-  g + geom_col(aes(fill = User_Type), width = 1)
          pie <- g + coord_polar(theta = "y")
          
          pie + theme_void() +  geom_text(aes(x = 1, label = percent(Percent/sum(Percent)), size = 32,
                                              fontface= "bold"), position=position_stack(vjust=0.5), size=5) + 
            theme( legend.title=element_text(size=18, face = "bold") , 
                   legend.text=element_text(size=14, face = "bold"))
          
        })
        
        output$table1 <- DT::renderDataTable(DT::datatable({
          data <- interboro_dt}))
        
        output$table2 <- DT::renderDataTable(DT::datatable({
          data <- interboro_wkend_dt}))
        


#************CODE FOR TAB 6 ***********************************************     
dt_InterB <- reactive({
  dt <- switch(input$time_InterB, 
               "9am - 12pm" = interboro_df %>% filter(., start_period == 3),
               "12pm -  3pm"= interboro_df %>% filter(., start_period == 4),
               "3pm -  6pm" = interboro_df %>% filter(., start_period == 5),
               "6pm -  9pm" = interboro_df %>% filter(., start_period == 6))
  dt %>% select(., station_name, Inflow, Outflow, Difference) %>% arrange(., Difference)
})

output$table_InterB <- DT::renderDataTable({DT::datatable(
  dt_InterB(), options = list(pageLength = 5, lengthMenu = 5))
})


filteredData_InterB <- reactive({
  switch(input$time_InterB,
         "9am - 12pm" = interboro_df %>% filter(., start_period == 3),
         "12pm -  3pm" =interboro_df %>% filter(., start_period == 4),
         "3pm -  6pm" = interboro_df %>% filter(., start_period == 5),
         "6pm -  9pm" = interboro_df %>% filter(., start_period == 6))
})

output$map_InterB <- renderLeaflet({
  dt <- filteredData_InterB()
  radius <- ifelse(abs(dt$Difference)>100, 25, ifelse(abs(dt$Difference)>75, 20, 
                                                      ifelse(abs(dt$Difference)>50, 15, 10)))
  
  color_vec = ifelse(dt$Difference >0, "green", "red")
  leaflet() %>% addTiles() %>% clearShapes() %>%
    addCircleMarkers(data = dt, ~start_station_long, ~start_station_lat, 
                     radius=radius, stroke=FALSE, fillOpacity=.8, fillColor=color_vec,
                     label = ~as.character(Difference)) %>%
    addLegend("bottomright", colors=c("green", "red"), labels = c("net inflow", "net outflow")) %>%
    setView(lat = 40.7181311, lng = -73.9649334, zoom = 13)
  })
})
