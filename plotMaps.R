# load packages 
library(ggplot2)
library(ggmap)
library(maps)
library(mapdata)


setwd("~/GHRP")

# Load storm data. 
strmEvents <- read.csv("StormEvents_locations-ftp_v1.0_d2018_c20180718.csv")


# load us map and plot it. 
usa <- map_data("usa")
gg1 <- ggplot() + geom_polygon(data = usa, aes(x=long, y = lat, group = group)) + 
  coord_fixed(1.3)

# lets make a smaller dataframe with all the strm events.
# lets exclude all those with a longitude ofgreater than 130 so I dont get hawaii.

onlyUS <- strmEvents[strmEvents$LONGITUDE > -140,]
corStrmEvents <- data.frame(lat = onlyUS$LATITUDE,
                            long = onlyUS$LONGITUDE,
                            names = onlyUS$EVENT_ID,
                            stringsAsFactors = FALSE)

# lets add it to the plot
gg1 + 
  geom_point(data = corStrmEvents, 
             aes(x = long, y = lat), color = "red", size = 0.000001)


# Lets look at an income dataset. 

incomeKag <- read.csv("kaggle_income.csv")

incomeUS <- subset(incomeKag, Lon > -140 & Mean < 25000)

corIncomeEvents <- data.frame(lat = incomeUS$Lat,
                              long = incomeUS$Lon,
                              names= incomeUS$Place,
                              stringsAsFactors = FALSE)


richPeople <- subset(incomeKag, Lon > -140 & Mean > 100000)

corRichPeople <- data.frame(lat = richPeople$Lat,
                            long = richPeople$Lon,
                            names = richPeople$Place,
                            stringsAsFactors = FALSE)

gg1 + geom_point(data = corIncomeEvents, 
           aes(x = long, y = lat), color = "blue", size = 0.000001) +
      geom_point(data = corStrmEvents,
           aes(x = long, y = lat), color = "red", size = 0.000001) +
      geom_point(data = corRichPeople,
           aes(x = long, y = lat), color = "green", size = 0.000001)
                          

# Now to calculate difference between each zip code, 
# and the coordinate of the extreme event.

