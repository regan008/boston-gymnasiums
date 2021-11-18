library("shiny")
library("leaflet")
library("dplyr")
library("highcharter")

shinyServer(function(input, output, session) {
  
  data.selected <- reactive({
    #Filter to map year
    map.data <- yearly.attendance.bygym %>% filter(Year == input$map.year)
    map.data
  })
  output$yearlyattendancechart <- renderHighchart({
    attdata <- yearly.attendance.bygym %>% group_by(Year) %>% summarize(yearly.total = sum(total))
    hchart(attdata, "line", hcaes(x=Year, y=yearly.total)) %>% hc_title(text="Gymnasia Attendance, 1909-192?") %>% hc_yAxis(title = list(text ="Total Attendance at Municipal Gymnasia", minorGridLineDashStyle = "LongDashDotDot")) %>% hc_xAxis(plotBands = list(
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
     addPolygons(data=bostonwards,
                col = 'dodgerblue',
                label = ~Ward_Num,
                stroke = TRUE,
                fillOpacity = .2, 
                smoothFactor = 0.5)
  })  
  pal <- colorFactor(
    palette = c('green', 'purple', 'orange'),
    domain = data$type
  )
  observe({
    
    df <- data.selected()
    
    leafletProxy("spaces_map", session) %>%
      clearShapes() %>% clearMarkers() %>% clearPopups()
    
    leafletProxy("spaces_map", data = df) %>%
      addCircleMarkers(data=df, label=~name,
                       weight = 1, 
                       radius=3,
                       color="#0000FF") %>%
      addCircleMarkers(data=privategyms, label=~name,
                     weight = 1, 
                     radius=3,
                     color="#FF0000")
  
  }) # End Observe
  observeEvent(input$reset_button, {
    updateSliderTextInput(session, "map.year", selected=1965)
  })
}) # end ShinyServer function