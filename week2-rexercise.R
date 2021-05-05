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

##Task 2
Wildschwein <- Wildschwein %>%
  group_by(TierID) %>%
  mutate(steplength = sqrt((E- lead(E,1))^2 + (N -lead(N,1))^2))

Wildschwein <- Wildschwein %>%
  mutate(speed=steplength/timelag)

View(Wildschwein)
#What speed unit do you get?
#Answer: m/s

##Task 3 Cross-scale movement analysis
caro <- read_delim("caro60.csv",",")
caro<-st_as_sf(caro, coords = c("E", "N"), crs = 2056, remove = FALSE)

View(caro)

caro3<- seq(1,200, by=3) #why doesn't this easier variant work as well? Tried it with length.out also, but always got an error message. :S  
caro3 <- caro[seq(1,nrow(caro),by=3),]
caro6 <- caro[seq(1,nrow(caro),by=6),]
caro9<- caro[seq(1,nrow(caro),by=9),]

nrow(caro)
nrow(caro3)
nrow(caro6)
nrow(caro9)

#caro
#timelag
caro<- caro%>%
  mutate(timelag = as.integer(difftime(lead(DatetimeUTC),DatetimeUTC, units="sec")))
#steplength
caro<-caro%>%
  mutate(steplength = sqrt((E- lead(E,1))^2 + (N -lead(N,1))^2))
#speed
caro<-caro%>%
  mutate(speed=steplength/timelag)

#caro3
#timelag
caro3<- caro3%>%
  mutate(timelag = as.integer(difftime(lead(DatetimeUTC),DatetimeUTC, units="sec")))
#steplength
caro3<-caro3%>%
  mutate(steplength = sqrt((E- lead(E,1))^2 + (N -lead(N,1))^2))
#speed
caro3<-caro3%>%
  mutate(speed=steplength/timelag)

#caro6
#timelag
caro6<- caro6%>%
  mutate(timelag = as.integer(difftime(lead(DatetimeUTC),DatetimeUTC, units="sec")))
#steplength
caro6<-caro6%>%
  mutate(steplength = sqrt((E- lead(E,1))^2 + (N -lead(N,1))^2))
#speed
caro6<-caro6%>%
  mutate(speed=steplength/timelag)

#caro9
#timelag
caro9<- caro9%>%
  mutate(timelag = as.integer(difftime(lead(DatetimeUTC),DatetimeUTC, units="sec")))
#steplength
caro9<-caro9%>%
  mutate(steplength = sqrt((E- lead(E,1))^2 + (N -lead(N,1))^2))
#speed
caro9<-caro9%>%
  mutate(speed=steplength/timelag)

#Vizualised line plots

#Original with 3 minutes
ggplot(data=caro,mapping=aes(x=E, y=N))+
  geom_path(data=caro,mapping=aes(color="1 minute"))+
  geom_path(data = caro3, mapping=aes(color="3 minutes"))+
  geom_point(data=caro,mapping=aes(color="1 minute"))+
  geom_point(data = caro3, mapping=aes(color="3 minutes"))+
  theme_light()+
  labs(color="Trajectory", title = "Comparing original- with 3 minutes-resampled data")+
  theme(title=element_text(size=10))

#Original with 6 minutes
ggplot(data = caro,mapping=aes(x=E, y=N))+
  geom_path(data= caro,mapping=aes(color="1 minute"))+
  geom_path(data = caro6, mapping=aes(color="6 minutes"))+
  geom_point(data=caro,mapping=aes(color="1 minute"))+
  geom_point(data = caro6, mapping=aes(color="6 minutes"))+
  theme_light()+
  labs(color="Trajectory", title = "Comparing original- with 6 minutes-resampled data")+
  theme(title=element_text(size=10))

#Original with 9 minutes
ggplot(data = caro,mapping=aes(x=E, y=N))+
  geom_path(data= caro,mapping=aes(color="1 minute"))+
  geom_path(data = caro9, mapping=aes(color="9 minutes"))+
  geom_point(data=caro,mapping=aes(color="1 minute"))+
  geom_point(data = caro9, mapping=aes(color="9 minutes"))+
  theme_light()+
  labs(color="Trajectory", title = "Comparing original- with 9 minutes-resampled data")+
  theme(title=element_text(size=10))

#Speed Comparison
ggplot(data = caro,mapping=aes(x=DatetimeUTC, y=speed))+
  geom_line(data=caro, mapping=aes(colour="1 minute"))+
  geom_line(data=caro3, mapping=aes(colour="3 minutes"))+
  geom_line(data=caro6, mapping=aes(colour="6 minutes"))+
  geom_line(data=caro9, mapping=aes(colour="9 minutes"))+
  theme_light()+
  labs(color="Trajectory", title = "Comparing derived speed at different sampling intervals")+
  theme(title=element_text(size=10))+
  xlab("Time")+
  ylab("Speed (m/s)")

#The different lines for the different temporal granularities show a different amount of data, whereas the 1 minute trajectory covers a lot of Points and Speeding times, the amount of data decreases and 'tracking gaps' occur.

