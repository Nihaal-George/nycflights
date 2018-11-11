library(plotly)
library(dplyr)
library(nycflights13)

flights <- flights
airlines <- airlines
airports <- airports
planes <- planes
weather <- weather

usefulData <- flights[flights$month == 1,]
usefulData <- inner_join(usefulData, airlines, by = "carrier")

usefulData <- usefulData[,c("day", "dep_delay", "origin", "name")]

usefulData <- as.data.frame(na.omit(usefulData))

for(i in (1:nrow(usefulData))){
  if(usefulData$dep_delay[i] >= 15){
    usefulData$delayed[i] = 1
  } else{
    usefulData$delayed[i] = 0
  }
}

usefulData$total <- 1

plotData <- aggregate(. ~day + origin + name, data = usefulData, sum)
plotData$delayProp <- plotData$delayed/plotData$total

plotData$origin[plotData$origin == "EWR"] <- "Newark"
plotData$origin[plotData$origin == "LGA"] <- "LaGuardia"

interactivePlot <- plotData %>%
  plot_ly(x = ~total, y = ~delayProp, color = ~origin, type = 'scatter', mode = 'markers',
          frame = ~day, text = ~name, hoverinfo = "text")

layout(interactivePlot, xaxis = list(title = "Total Number of Flights for Each Airport"),
       yaxis = list(title = "Proportion of Flights Delayed by More than 15 Minutes"))
