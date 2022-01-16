library(dplyr)
library(runner)
library(ggplot2)
library(tidyverse)
library(plotly)
library(gapminder)
library(gganimate)
library(gifski)
library(png)
library(av)
library(countrycode)
library(hrbrthemes)

riderHist <- read.csv('riderHistory.csv')
stageHist <- read.csv('stageHistory.csv')
stages <- read.csv('stages.csv')

stageHist <- subset(stageHist, select = -1)
riderHist <- subset(riderHist, select = -1)


# RIDERS PER YEAR | linear trend + confidence interval 

ridersPerYear <- riderHist %>% count(Year, name = 'Riders')

p3 <- ggplot(ridersPerYear, aes(x=Year, y=Riders)) +
  geom_point(color='#457b9d') +
  geom_smooth(method=loess , color="#e63946", fill="#a8dadc", se=TRUE) +
  theme(panel.background = element_rect(fill = '#f1faee', colour = '#1d3557'))
p3


stages$Year <- as.POSIXct(stages$Date, format = "%Y-%m-%d")
stages$Year <- format(stages$Year, format="%Y")

stageTypeCountPerYear <- stages %>% count(Type,Year)
stageTypeCountPerYear$Year <- format(stageTypeCountPerYear$Year, format="%Y")

ggplot(stageTypeCountPerYear, aes(x=Year, y=n, z=Type)) +
  geom_point(color='#457b9d')

stcpy <-plot_ly(
  stageTypeCountPerYear, x = ~n, y = ~Year, z = ~Type, 
  color = ~Type, colors = c('#e63946', '#1d3557'),size = 1
) %>%
  add_markers() %>%
  layout(
    
    scene = list(yaxis = list(title = 'Year'),
                 xaxis = list(title = 'Number of stages'),
                 zaxis = list(title = ''))
  )

stcpy

#Most Stage Wins by Rider



#Most Stage Wins By Country Per Year------------------------------------------------------------------------------------
stageWinsByCountryPerYear <- stages %>% count(Winner,Year,name = 'Stage_Wins')
stageWinsByCountryPerYear <- stageWinsByCountryPerYear %>% na_if("") %>% na.omit


#Cumulative Wins Over Time By Country-----------------------------------------------------------------------------------
test <- stageWinsByCountryPerYear %>% group_by(Year) %>% arrange(Stage_Wins)
test$Winner_Country <- countrycode(test$Winner_Country,'ioc','country.name')
test <- test %>% arrange(Year)
test$Year <- as.integer(test$Year)
test <-test %>% group_by(Winner_Country) %>% mutate(Total_Wins = cumsum(Stage_Wins))
test <- test %>% complete(Winner_Country,Year = 1903:2017, fill = list(Stage_Wins = 0))
test <- test %>% fill(Total_Wins, .direction = "down")
ggp <- ggplot(test, aes(x = Total_Wins, y = Winner)) +
  geom_bar(stat = "identity", aes(fill = Winner)) +
  transition_states(Year, transition_length = 20, state_length = 2, wrap = FALSE)  +   # Optional, I used to see settled states clearer
  labs(title = "Year {closest_state}", y = "Country", x = "Cumulative Stage Wins") +
  theme(plot.title = element_text(size = 20, face = "bold"))
animate(ggp, width = 500, height = 600, fps = 30, duration = 20, nframes = 1000, end_pause = 100)


#Rider Average Speed
riderHist$TotalMinutes <- riderHist$TotalSeconds/60
riderHist$TotalHours <- riderHist$TotalMinutes/60
riderHist$Average_Speed <- riderHist$Distance..km. / riderHist$TotalHours

#Most Stage Wins By Rider Per Year
stageWinsByWinnerPerYear <- stages %>% count(Winner,Year,name = 'Stage_Wins')
stageWinsByWinnerPerYear <- stageWinsByWinnerPerYear %>% na_if("") %>% na.omit




winners <- stageWinsByWinnerPerYear %>% group_by(Year) %>% arrange(Stage_Wins)
winners <- winners %>% arrange(Year)
winners$Year <- as.integer(winners$Year)
winners <-winners %>% group_by(Winner) %>% mutate(Total_Wins = cumsum(Stage_Wins))

testing <- winners %>%                                      # Top N highest values by group
  arrange(Total_Wins) %>% 
  group_by(Total_Wins,Year) %>%
  slice(1)
testing <- testing %>% arrange(desc(Total_Wins))
testing <- testing[!duplicated(testing$Winner),]
testing <- testing[-c(7),]
tplot1 <- testing[1:10,]

tplot1 <- tplot %>% arrange(desc(Total_Wins))


tplot2 <-ggplot(tplot1, aes(x = Total_Wins, y = reorder(Winner,Total_Wins))) +
  geom_bar(aes(fill = Winner), stat="identity") +
  geom_text(aes(label = Total_Wins, x = Total_Wins/2), size = 5) +
  geom_text(aes(label = Year, x = Total_Wins + 2), size = 3) +
  labs(y= "Cyclists", x = "Total Stage Wins") 
tplot2




animate(p1, nframes = 750, fps = 25, end_pause = 50, width = 1200, height = 900)

#Total Race Distance Per Year
distanceByYear <- stageHist %>% group_by(Year) %>% summarise(mean(TotalTDFDistance))
distanceByYear <- rename(distanceByYear, "Distance" = "mean(TotalTDFDistance)" )

p4 <- ggplot(distanceByYear, aes(x=Year, y=Distance)) +
  geom_point(color='#212121') +
  geom_smooth(method=loess , color="#e63946", fill="#FFFFF7", se=TRUE) 
  
p4


