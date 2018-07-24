# load packages 
library(ggplot2)
library(ggmap)
library(maps)
library(mapdata)


setwd("~/GHRP")

# Load storm data. 
strmEvents <- read.csv("StormEvents_locations-ftp_v1.0_d2014_c20180718.csv")

# NCEI DSI 3910_03
# gov.noaa.ncdc:C00510


# load us map and plot it. 
usa <- map_data("usa")
gg1 <- ggplot() + geom_polygon(data = usa, aes(x=long, y = lat, group = group)) + 
  coord_fixed(1.3)

# lets make a smaller dataframe with all the strm events.
# lets exclude all those with a longitude ofgreater than 130 so I dont get hawaii.

onlyUS <- subset(strmEvents, (LONGITUDE > -140 & LONGITUDE < -50))

corStrmEvents <- data.frame(lat = onlyUS$LATITUDE,
                            long = onlyUS$LONGITUDE,
                            names = onlyUS$EVENT_ID,
                            stringsAsFactors = FALSE)

# lets add it to the plot
gg1 + 
  geom_point(data = corStrmEvents, 
             aes(x = long, y = lat), color = "red", size = exp(-100))


# Lets look at an income dataset. 
incomeKag <- read.csv("kaggle_income.csv")



poorPeople <- subset(incomeKag, Lon > -140 & Mean < 25000)

corPoorPeople <- data.frame(lat = poorPeople$Lat,
                              long = poorPeople$Lon,
                              names= poorPeople$Place,
                              stringsAsFactors = FALSE)


richPeople <- subset(incomeKag, Lon > -140 & Mean > 100000)

corRichPeople <- data.frame(lat = richPeople$Lat,
                            long = richPeople$Lon,
                            names = richPeople$Place,
                            stringsAsFactors = FALSE)

gg1 + geom_point(data = corPoorPeople, 
           aes(x = long, y = lat), color = "blue", size = 1) +
      geom_point(data = corStrmEvents,
           aes(x = long, y = lat), color = "red", size = 0.0001) +
      geom_point(data = corRichPeople,
           aes(x = long, y = lat), color = "green", size = 1)
             


# Calculate the distance for each poor person and rich person 
# to the nearest extreme weather

calcDistance <- function(person,weather) {
  xE = weather$long
  yE = weather$lat 
  xS = person$long 
  yS = person$lat
  
  return(distance = (xE - xS)^2 + (yE - yS)^2)
}

poorDistances <- c()
for (i in 1:length(corPoorPeople[,1])){
  poorDistances <- append(poorDistances,min(calcDistance(corPoorPeople[i,],corStrmEvents)))
}

richDistances <- c()
for (i in 1:length(corRichPeople[,1])){
  richDistances <- append(richDistances,min(calcDistance(corRichPeople[i,],corStrmEvents)))
}

summary(poorDistances)
summary(richDistances)

# Now to calculate difference between each zip code, 
# and the coordinate of the extreme event.

