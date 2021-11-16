library("shiny")
library("leaflet")
library("DT")
library("dplyr")
library(htmlwidgets)
library(htmltools)

shinyServer(function(input, output, session) {
  
  data.selected <- reactive({
    #Filter to map year
    map.data <- data %>% filter(Year == input$map.year)
    
    map.data
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
      clearMarkerClusters() %>%
      clearPopups()
    
    leafletProxy("spaces_map", data = df) %>%
      addCircleMarkers(data=data, label=~gym.name,
                       weight = 1, 
                       radius=5,
                       color=~pal(type.x))
    
  }) # End Observe
  observeEvent(input$reset_button, {
    updateSliderTextInput(session, "map.year", selected=1965)
  })
}) # end ShinyServer function