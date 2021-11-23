library(tidyverse)
library(leaflet)
library(geojson)
library(htmlwidgets)
library(htmltools)
library(rgdal)
library(artyfarty)

boston <- read.csv("gyms.csv", stringsAsFactors=F)
bostoneight <- readOGR("Ward-Shapfiles/boston_wards_1915_1925.shp")
bostoneight <- spTransform(bostoneight, CRS("+proj=longlat +ellps=GRS80"))

pal <- colorFactor(
  palette = c('red', 'blue', 'green', 'purple', 'orange'),
  domain = parks$type
)
boston1895to1912<- readOGR("Ward-Shapfiles/boston_wards_1895_1912.shp")
boston1895to1912 <- spTransform(bostoneight, CRS("+proj=longlat +ellps=GRS80"))
bostonmap1912 <- leaflet(boston) %>% 
  addProviderTiles("CartoDB.Positron") %>%
  setView(-71.061229, 42.357379, zoom = 13) %>% 
  addPolygons(data=bostoneight,
              col = 'dodgerblue',
              label = ~Ward_Num,
              stroke = TRUE,
              fillOpacity = .2, 
              smoothFactor = 0.5) %>%
  addCircleMarkers(label=~gym.name,
                   weight = 3, 
                   radius=5, 
                   color=~pal(type))
bostonmap1912

boston1913to1914<- readOGR("Ward-Shapfiles/boston_wards_1913_1914.shp")
boston1913to1914 <- spTransform(bostoneight, CRS("+proj=longlat +ellps=GRS80"))
bostonmap1914 <- leaflet(boston) %>% 
  addProviderTiles("CartoDB.Positron") %>%
  setView(-71.061229, 42.357379, zoom = 13) %>% 
  addPolygons(data=bostoneight,
              col = 'dodgerblue',
              label = ~Ward_Num,
              stroke = TRUE,
              fillOpacity = .2, 
              smoothFactor = 0.5) %>%
  addCircleMarkers(label=~gym.name,
                   weight = 3, 
                   color=~pal(type))
bostonmap1914
bostonmap1915 <- leaflet(boston) %>% 
  addProviderTiles("CartoDB.Positron") %>%
  setView(-71.061229, 42.357379, zoom = 13) %>% 
  addPolygons(data=bostoneight,
              col = 'dodgerblue',
              label = ~Ward_Num,
              stroke = TRUE,
              fillOpacity = .2, 
              smoothFactor = 0.5) %>%
  addCircleMarkers(label=~gym.name,
                   weight = 3, 
                   radius=5, 
                   color=~pal(type))
bostonmap1915
library(ggplot2)
gymattendance <- read.csv("data-attendance.csv", stringsAsFactors=F)

gymattendance <- gymattendance %>% group_by(Year, sex) %>% summarize(total = sum(attendance)) 

gymattendance
ggplot(gymattendance, aes(x=Year, y=total, colour=sex)) +
  geom_line()


total.yearly.attendance <- gymattendance %>% group_by(Year) %>% summarize(total = sum(total))
ggplot(total.yearly.attendance, aes(x=Year, y=total)) +
  geom_line()

attendance <- read.csv("data-attendance.csv", stringsAsFactors=F)
attendance <- attendance %>% group_by(Year, gymid) %>% filter(Year == 1915) %>% summarize(total = sum(attendance)) 
gyms <- read.csv("data-gymnasiums.csv", stringsAsFactors = FALSE)
bostongymnasiums <- left_join(gyms, attendance)

bostonmap1915 <- leaflet(bostongymnasiums) %>% 
  addProviderTiles("CartoDB.Positron") %>%
  setView(-71.061229, 42.357379, zoom = 13) %>% 
  addPolygons(data=bostoneight,
              col = 'dodgerblue',
              label = ~Ward_Num,
              stroke = TRUE,
              fillOpacity = .2, 
              smoothFactor = 0.5) %>%
  addCircleMarkers(label=~name,
                   weight = 3, 
                   radius= ~total/1000, 
                   color=~pal(type))
bostonmap1915


parks <- read.csv("playground-attendance.csv", stringsAsFactors = FALSE)
parks <- parks %>% group_by(year) %>% summarize(total = sum(attendance))

hchart(parks, "line", hcaes(x=year, y=total ))
library(highcharter)

total.yearly.attendance.single <- total.yearly.attendance %>% group_by(Year) %>% summarize(yearly.total = sum(total))
hchart(total.yearly.attendance.single, "line", hcaes(x=Year, y=yearly.total ))


parks <- read.csv("playgrounds.csv", stringsAsFactors = FALSE)
parkattendance <- read.csv("playground-attendance.csv", stringsAsFactors = FALSE)
parks <- left_join(parks, parkattendance, by="name")
parks <- parks %>% filter(year == 1919)
bostonparks <- leaflet(parks) %>% 
  addProviderTiles("CartoDB.Positron") %>%
  setView(-71.061229, 42.357379, zoom = 13) %>% 
  addPolygons(data=bostoneight,
              col = 'dodgerblue',
              label = ~Ward_Num,
              stroke = TRUE,
              fillOpacity = .2, 
              smoothFactor = 0.5) %>%
  addCircleMarkers(label=~name,
                   weight = 3, 
                   radius= ~attendance/20000, 
                   color=~pal(type))
bostonparks


yearlyattendance %>% filter(year==1918) %>% count(type)



