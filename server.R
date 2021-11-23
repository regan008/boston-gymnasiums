library("shiny")
library("leaflet")
require("dplyr")
library("highcharter")

shinyServer(function(input, output, session) {
  
  data.selected <- reactive({
    #Filter to map year
    map.data <- yearlyattendance %>% filter(year == input$map.year)
    map.data
  })

  output$yearlyattendancechart <- renderHighchart({
      attdata <- yearlyattendance %>% group_by(year, type) %>% summarize(yattendance = sum(attendance))
      attcount <- yearlyattendance %>% group_by(year) %>% count(type)
      attdata <- merge(attdata, attcount)
      hchart(attdata, "line", hcaes(x=year, y=yattendance/n, group="type")) %>% hc_title(text="Average Attendance, 1909-192?") %>% hc_yAxis(title = list(text ="Total Attendance at Municipal Gymnasia", minorGridLineDashStyle = "LongDashDotDot")) %>% hc_xAxis(plotBands = list(
      list(
        from = 1918,
        to = 1920,
        color = "rgba(100, 0, 0, 0.1)",
        label = list(text = "1918 Pandemic")
      )))
    
  })
  
  output$spaces_map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles("CartoDB.Positron") %>%
      setView(-71.061229, 42.357379, zoom = 12) %>%
      addCircleMarkers(data=data.selected(), label=~name,
                       weight = 3, 
                       radius= ~attendance/10000, 
                       color="#E4D00A")
  })  
 # pal <- colorFactor(
 #   palette = c('green', 'purple', 'orange'),
 #   domain = data$type
 # )
  observe({
    
    df <- data.selected()
    
    leafletProxy("spaces_map", session) %>%
      clearShapes() %>% clearMarkers() %>% clearPopups()
    
    leafletProxy("spaces_map", data = df) %>%
      addPolygons(data=bostonwards,
                  col = "#ababab",
                  label = ~Ward_Num,
                  stroke = TRUE,
                  weight= 1,
                  fillOpacity = .3, 
                  smoothFactor = 0.5) %>% 
      addCircleMarkers(data=df, label=~name,
                       weight = 2, 
                       radius=5,
                       color="#D2042D") %>%
      addCircleMarkers(data=privategyms, label=~name,
                     weight = 2, 
                     radius=5,
                     color="#702963")
   
      
  
  }) # End Observe
  observeEvent(input$reset_button, {
    updateSliderTextInput(session, "map.year", selected=1965)
  })
}) # end ShinyServer function