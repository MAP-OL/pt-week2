## Load the necessary libraries ################################################

library(readr)        # to import tabular data (e.g. csv)
library(dplyr)        # to manipulate (tabular) data
library(ggplot2)      # to visualize data
library(sf)           # to handle spatial vector data
library(terra)        # To handle raster data
library(lubridate)    # To handle dates and times

## Import the downloaded csv ##################################################

wildschwein_BE <- read_delim("C:\\Users\\melin\\OneDrive\\Dokumente\\ZHAW\\Patterns&Trends\\R_GIT\\Week 2\\week2-rexercise\\wildschwein_BE_2056.csv",",") 

wildschwein_BE <- st_as_sf(wildschwein_BE, coords = c("E", "N"), crs = 2056, remove = FALSE)
View(wildschwein_BE)

## Task 1
# Calculate the time difference between subsequent rows as described in the demo.

Wildschwein<-wildschwein_BE%>%
  group_by(TierID)%>%
  mutate(timelag = as.integer(difftime(lead(DatetimeUTC),DatetimeUTC, units="sec")))
View(Wildschwein)

# How many individuals were tracked?

table(Wildschwein$TierID)
ggplot(data=Wildschwein, aes(x=DatetimeUTC,y=TierID))+
  geom_line()
# Three individuals were tracked.

#For how long were the individual tracked? Are there gaps?

Begin <-min(Wildschwein$DatetimeUTC)
End <-max(Wildschwein$DatetimeUTC)

difftime(Begin, End, units="days")
#The individuals were tracked for around 338 days.

#Were all individuals tracked concurrently or sequentially?

ggplot(data=Wildschwein, aes(x=DatetimeUTC,y=timelag,colour=TierID))+
  geom_point()+
  geom_line()
  
#The graph shows that in the beginning of the sampling period only individual 002A was tracked and then in the beginning of November the individuals were tracked concurrently. 

#What is the temporal sampling interval between the locations?

Wildschwein%>%
  group_by(TierID)%>%
  summarise(mean(timelag,na.rm=TRUE))

ggplot(data=Wildschwein,mapping=aes(x=timelag))+
  geom_histogram(binwidth =50)+
  scale_y_log10()+
  xlim(0,15000)+
  theme_classic()

#the mean for all individuals is around 1400.

