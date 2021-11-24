require("rgdal")
require("magrittr")
require("dplyr")
attendance <- read.csv("data-attendance.csv", stringsAsFactors=F)
yearly.attendance.bygym <- attendance %>% group_by(Year, gymid) %>% summarize(total = sum(attendance)) 
yearly.attendance.bygym <- left_join(yearly.attendance.bygym, gyms, by="gymid")

gyms <- read.csv("data-gymnasiums.csv", stringsAsFactors = FALSE)
data <- left_join(attendance, gyms, by="gymid")
bostonwards <- readOGR("Ward-Shapfiles/boston_wards_1915_1925.shp")
bostonwards <- spTransform(bostonwards, CRS("+proj=longlat +ellps=GRS80"))
privategyms <-  gyms %>% filter(type == "private")


parks <- read.csv("playgrounds.csv", stringsAsFactors = FALSE)
parkattendance <- read.csv("playground-attendance.csv", stringsAsFactors = FALSE)
parks <- left_join(parks, parkattendance, by="name")

pyearlyattendance <- parks %>% select(year, name, long, lat, attendance)
pyearlyattendance <- pyearlyattendance %>% mutate(type = "recspace")

gyearlyattendance <- yearly.attendance.bygym %>% select(Year, total, name, lat, long) 
gyearlyattendance <- gyearlyattendance %>% rename(year = Year, attendance = total)
gyearlyattendance <- gyearlyattendance %>% mutate(type = "gymnasium")

yearlyattendance <- merge(pyearlyattendance, gyearlyattendance, all=TRUE)
