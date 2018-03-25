#required library
library(dplyr)
library(stringi)
library(tidyr)

setwd("~/Data Science/Uber Assignment")
#Import the data
Uber <- read.csv("Uber Request Data.csv",stringsAsFactors = FALSE,na.strings = c("","NA"))

#Check the data
str(Uber)

#Convert the Request time in standard Date Time format in a new column Request Time
Uber$requestTime<- as.POSIXlt(Uber$Request.timestamp,format = "%d/%m/%Y %H:%M")

#Convert few fields which are in Charecter to standard date time formatin last added column requestTime
Uber[which(is.na(Uber$requestTime)),]$requestTime <- as.POSIXlt(Uber[which(is.na(Uber$requestTime)),]$Request.timestamp,format = "%d-%m-%Y %H:%M:%S")

#Fixing the format of droptime column and add it to new column
Uber$dropTime <- as.POSIXlt(Uber$Drop.timestamp,format = "%d/%m/%Y %H:%M")
Uber[which(is.na(Uber$dropTime)),]$dropTime <- as.POSIXlt(Uber[which(is.na(Uber$dropTime)),]$Drop.timestamp,format = "%d-%m-%Y %H:%M:%S")

#Check the data
str(Uber)

#Check for Duplicate Request Ids
sum(duplicated(Uber$Request.id)) #No Duplicate Id

#Make few column as factors
Uber$Pickup.point <- as.factor(Uber$Pickup.point)
Uber$Status <- as.factor(Uber$Status)
Uber$Driver.id <- as.factor(Uber$Driver.id)

summary(Uber)

#Deriving new fields from Cancelled and not available
Uber$Trip <- ifelse(Uber$Status=="Trip Completed",1,0)
Uber$Trip <- as.factor(Uber$Trip)

summary(Uber)

#Deriving fields for Weekdays
Uber$requestedDayOfWeek <- weekdays(Uber$requestTime)
Uber$requestedDayOfWeek <- as.factor(Uber$requestedDayOfWeek)

Uber$dropDayOfWeek <- weekdays(Uber$dropTime)
Uber$dropDayOfWeek <- as.factor(Uber$dropDayOfWeek)

summary(Uber)

#requestedDayOfWeek   dropDayOfWeek 
##Friday   :1381     Friday   : 546  
##Monday   :1367     Monday   : 582  
##Thursday :1353     Saturday :  30  
##Tuesday  :1307     Thursday : 536  
##Wednesday:1337     Tuesday  : 568  
##                  Wednesday: 569  

#Friday is tops the request days and Monday tops the drop days
#Lets check these data for the cancelled and Not available cars
summary(Uber[which(Uber$Trip==0),])
#Thrusday tops the list but it does not differ from other days of the week. This concludeds that the drivers treat all days same and there is nothing much to do with weekdays and weekend.

## Time Based analysis
Uber$requestedHour <- format(Uber$requestTime,"%H")
Uber$requestedHour <- as.factor(Uber$requestedHour)
Uber$dropHour <- format(Uber$dropTime, "%H")
Uber$dropHour <- as.factor(Uber$dropHour)

#Adding a new column to capture the trip time in minutes
Uber$tripDuration <- as.numeric(format(round(as.numeric(Uber$dropTime - Uber$requestTime)),nsmall = 0),na.rm = T)

summary(Uber$tripDuration)
##Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
# 21.00   41.00   52.00   52.41   64.00   83.00    3914

#The mean and the median are almost same which means the average travel time within the city is 52 mins.

#Lets check the pick up point 

Uber$Airportpickup <- ifelse(Uber$Pickup.point == "Airport",1,0)
Uber$Citypickup <- ifelse(Uber$Pickup.point== "City",1,0)

#Checking for the drivers id who cancelled the trip
Uber_Cancelled <- Uber[which(Uber$Status == 'Cancelled'),]
Uber_Cancelled$Driver.id <- as.factor(Uber_Cancelled$Driver.id)

Aggregate_trip <- aggregate(Citypickup~Driver.id,Uber_Cancelled,sum)
View(Aggregate_trip)
#There are drivers who have cancelled the trip many times but those cannot be categorised in outliers as there is not a big difference from others.

#Grouping the hours of the day into Morning Afternoon and night
Uber$requestedHour <- as.numeric(Uber$requestedHour)

#Defining new columns to dstinguish the time of the day
Uber$RequestedTimeOfDay <- NA
Uber$RequestedTimeOfDay[Uber$requestedHour>= 00 & Uber$requestedHour<= 03] <- 'Early Morning'
Uber$RequestedTimeOfDay[Uber$requestedHour>= 4 & Uber$requestedHour<= 10] <- 'Morning'
Uber$RequestedTimeOfDay[Uber$requestedHour>= 11 & Uber$requestedHour<= 13] <- 'Afternoon'
Uber$RequestedTimeOfDay[Uber$requestedHour>= 14 & Uber$requestedHour<= 16] <- 'Evening'
Uber$RequestedTimeOfDay[Uber$requestedHour>= 17 & Uber$requestedHour<= 24] <- 'Night'

#Converting the numeric values to factor
Uber$requestedHour <- as.factor(Uber$requestedHour)
Uber$RequestedTimeOfDay <- as.factor(Uber$RequestedTimeOfDay)

#Repeating the above classification of time for drop hour
Uber$dropHour <- as.numeric(Uber$dropHour)
Uber$DropTimeOfDay <- NA
Uber$DropTimeOfDay[Uber$dropHour>= 00 & Uber$dropHour<= 03] <- 'Early Morning'
Uber$DropTimeOfDay[Uber$dropHour>= 4 & Uber$dropHour<= 10] <- 'Morning'
Uber$DropTimeOfDay[Uber$dropHour>= 11 & Uber$dropHour<= 13] <- 'Afternoon'
Uber$DropTimeOfDay[Uber$dropHour>= 14 & Uber$dropHour<= 16] <- 'Evening'
Uber$DropTimeOfDay[Uber$dropHour>= 17 & Uber$dropHour<= 24] <- 'Night'

Uber$DropTimeOfDay <- as.factor(Uber$DropTimeOfDay)
Uber$dropHour <- as.factor(Uber$dropHour)

Uber$RequestedTimeOfDay <- factor(Uber$RequestedTimeOfDay, levels = c("Early Morning", "Morning", "Afternoon", "Evening", "Night" ))

## Plotting the charts

library(ggplot2) #Got error for this command- package ‘ggplot2’ was built under R version 3.4.3 
require(ggplot2)

#Supply vs Demand hours         
#To get the insight of the supply and demand hours, will create a new DF to check the ratio of the Supply and Demand

Uber$Trip <- as.numeric(Uber$Trip)
Uber$Trip <- Uber$Trip-1

Uber_SupplyDemand <- Uber[,c("requestedHour","Trip","RequestedTimeOfDay")]
Uber_SupplyDemand_group <- group_by(Uber_SupplyDemand,requestedHour,RequestedTimeOfDay)
Uber_SupplyDemand_group$Trip <- as.numeric(Uber_SupplyDemand_group$Trip)
 
#write.csv(Uber, "Uber_formated_tableau.csv")

Uber_SupplyDemand <- summarise(Uber_SupplyDemand_group, Total_Supply = sum(Trip==1),Total_Demand= sum(!is.na(Trip)))
Uber_SupplyDemand$SupplyDemand_Ratio <- (Uber_SupplyDemand$Total_Demand- Uber_SupplyDemand$Total_Supply)/Uber_SupplyDemand$Total_Demand

#Plotting supply demand per hour
Requested_hour_plot <- ggplot(Uber_SupplyDemand,aes(x= requestedHour))
Requested_hour_plot +geom_point(data = Uber_SupplyDemand,aes(x=requestedHour,y = Total_Demand), color = 'blue',group = 1,size= 2,shape = 20,stroke = 2.5)+
  geom_line(data = Uber_SupplyDemand,aes(x=requestedHour,y = Total_Demand), color = 'blue',group = 1) +labs(x= "Hours of the Day", y = "Total Request")+
  geom_point(data = Uber_SupplyDemand,aes(x=requestedHour,y = Total_Supply), color = 'red',group = 1,size= 2, shape = 20,stroke = 2.5)+
  geom_line(data = Uber_SupplyDemand,aes(x=requestedHour,y = Total_Supply), color = 'red',group = 1)+
  ggtitle("Supply vs Demand per hour")

#plotting Supply demand ratio per hour
Requested_hour_plot+geom_point(data= Uber_SupplyDemand,aes(x= requestedHour, y = SupplyDemand_Ratio), color = 'blue',group = 1,size= 2,shape = 20,stroke = 2.5)+
labs(x= "Hours of the Day", y = "Total Request")+geom_line(data = Uber_SupplyDemand,aes(x=requestedHour,y = SupplyDemand_Ratio),group = 1)+
  ggtitle("Supply vs Demand Ratio per hour")

#From the above 2 plots it can be observed it is poorest at 2 am and at 12 noon

#Plotting total supply vs total demand to get better insight
Requested_hour_plot + geom_bar(alpha = 0.8, width = 0.1,fill= 'blue')+
  labs(x= "Hours of the Day", y = "Total Request")+
  geom_point(data = Uber_SupplyDemand,aes(x=requestedHour, y = Total_Supply), color = 'red',group = 1, size = 2, shape = 21,stroke = 2.5)+
  geom_point(data = Uber_SupplyDemand,aes(x=requestedHour, y = Total_Demand), color = 'blue',group = 1, size = 2, shape = 21,stroke = 2.5)+
  geom_line(data = Uber_SupplyDemand,aes(x=requestedHour, y = Total_Supply), color = 'red',group = 1, size = 1)+
  geom_line(data = Uber_SupplyDemand,aes(x=requestedHour, y = Total_Demand), color = 'blue',group = 1, size = 1)+
  geom_bar(alpha= 0.4,width = 0.4,fill = "green", data = Uber[which(Uber$Trip==1),],aes(x= requestedHour))+
  geom_text(data = Uber_SupplyDemand,aes(x=requestedHour, y = Total_Supply, label = Total_Supply), hjust = 0,vjust=0)+
  geom_text(data = Uber_SupplyDemand,aes(x=requestedHour, y = Total_Demand, label = Total_Demand), hjust = 0, vjust = 0)+
  ggtitle("Total Supply vs Total Demand per hour")

#Categorising the plots based on the day time
Uber_SupplyDemand <- Uber[, c("Trip","RequestedTimeOfDay")]
Uber_SupplyDemand_group <- group_by(Uber_SupplyDemand,RequestedTimeOfDay)              
Uber_SupplyDemand <- summarise(Uber_SupplyDemand_group,Total_Supply = sum(Trip==1), Total_Demand = sum(!is.na(Trip)))

Uber_SupplyDemand$SupplyDemand_Ratio <- (Uber_SupplyDemand$Total_Demand - Uber_SupplyDemand$Total_Supply)/Uber_SupplyDemand$Total_Demand 

Requested_Day_time_plot <- ggplot(Uber_SupplyDemand, aes(x= RequestedTimeOfDay))

Requested_Day_time_plot+geom_point(data= Uber_SupplyDemand,aes(x= RequestedTimeOfDay, y = SupplyDemand_Ratio), color = 'blue',group = 1,size= 2,shape = 20,stroke = 2.5)+
  labs(x= "Requested Time Of Day", y = "Total Request")+geom_line(data = Uber_SupplyDemand,aes(x=RequestedTimeOfDay,y = SupplyDemand_Ratio), group = 1)


Requested_Day_time_plot + geom_bar(alpha = 0.8, width = 0.1,fill= 'blue')+
  labs(x= "Requested Time Of Day", y = "Total Request")+
  geom_point(data = Uber_SupplyDemand,aes(x=RequestedTimeOfDay, y = Total_Supply), color = 'red',group = 1, size = 2, shape = 21,stroke = 2.5)+
  geom_point(data = Uber_SupplyDemand,aes(x=RequestedTimeOfDay, y = Total_Demand), color = 'blue',group = 1, size = 2, shape = 21,stroke = 2.5)+
  geom_line(data = Uber_SupplyDemand,aes(x=RequestedTimeOfDay, y = Total_Supply), color = 'red',group = 1, size = 1)+
  geom_line(data = Uber_SupplyDemand,aes(x=RequestedTimeOfDay, y = Total_Demand), color = 'blue',group = 1, size = 1)+
  geom_bar(alpha= 0.4,width = 0.4,fill = "green", data = Uber[which(Uber$Trip==1),],aes(x= RequestedTimeOfDay))+
  geom_text(data = Uber_SupplyDemand,aes(x=RequestedTimeOfDay, y = Total_Supply, label = Total_Supply), hjust = 0,vjust=0)+
  geom_text(data = Uber_SupplyDemand,aes(x=RequestedTimeOfDay, y = Total_Demand, label = Total_Demand), hjust = 0, vjust = 0)

#Plotting Pick up supply vs demand

#creating a DF for supply demand pick up
Uber_Pickup_SD  <- Uber[,c("requestedHour","Trip", "Pickup.point","RequestedTimeOfDay")]
Uber_Pickup_SD_group <- group_by(Uber_Pickup_SD,Pickup.point,RequestedTimeOfDay)
Uber_Pickup_SD <- summarise(Uber_Pickup_SD_group, Total_Supply= sum(Trip==1), Total_Demand = sum(!is.na(Trip)))

Uber_Pickup_SD$SupplyDemand_Ratio <- (Uber_Pickup_SD$Total_Demand- Uber_Pickup_SD$Total_Supply)/Uber_Pickup_SD$Total_Demand


ggplot(Uber_Pickup_SD,aes(x= RequestedTimeOfDay))+facet_wrap(~Pickup.point)+
  labs(x= "Diffrent day time", y = "Total Request")+
  geom_point(data = Uber_Pickup_SD,aes(x=RequestedTimeOfDay,y=SupplyDemand_Ratio), color ='blue',group = 1, size = 2 , shape= 21,stroke= 2.5)+
  geom_line(data= Uber_Pickup_SD, aes(RequestedTimeOfDay,y = SupplyDemand_Ratio), color = 'blue', group = 1)+
  ggtitle("Supply vs Demand Ratio based on pickup")

ggplot(Uber, aes(x= RequestedTimeOfDay))+
  geom_bar(alpha= 0.8, width = 0.1, fill= 'green')+facet_wrap(~Pickup.point)+
  labs(x= "Diffrent day time", y = "Total Request")+
  ggtitle("Supply vs Demand based on pickup")+
  geom_point(data = Uber_Pickup_SD,aes(x=RequestedTimeOfDay, y = Total_Supply), color = 'red',group = 1, size = 2, shape = 21,stroke = 2.5)+
  geom_point(data = Uber_Pickup_SD,aes(x=RequestedTimeOfDay, y = Total_Demand), color = 'blue',group = 1, size = 2, shape = 21,stroke = 2.5)+
  geom_line(data = Uber_Pickup_SD,aes(x=RequestedTimeOfDay, y = Total_Supply), color = 'red',group = 1, size = 1)+
  geom_line(data = Uber_Pickup_SD,aes(x=RequestedTimeOfDay, y = Total_Demand), color = 'blue',group = 1, size = 1)+
  geom_bar(alpha= 0.4,width = 0.4,fill = "#07843B", data = Uber[which(Uber$Trip==1),],aes(x= RequestedTimeOfDay))
  
  

# Plotting supply vs demand based on the daytime
#now with the help of time variable let us see the problematic area i.e cancelled or no car available.
Uber$RequestedTimeOfDay<- as.factor(Uber$RequestedTimeOfDay)
Uber$DropTimeOfDay <- as.factor(Uber$DropTimeOfDay)
Uber$Trip <- as.factor(Uber$Trip)
Uber$Pickup.point <- as.factor(Uber$Pickup.point)

ggplot(Uber, aes(x= RequestedTimeOfDay))+ geom_bar(alpha= 0.8,width = 0.3, fill = 'blue')+
  geom_bar(alpha = 0.4, fill = 'red', data = Uber[which(Uber$Trip==1),], aes(x=RequestedTimeOfDay))+
  ggtitle("Supply vs Demand during different Daytime")

ggplot(Uber,aes(x= RequestedTimeOfDay))+
  geom_bar(alpha=0.8,width = 0.3,fill = 'blue')+facet_wrap(~Pickup.point)+
  geom_bar(alpha=0.4,fill = 'red',data = Uber[which(Uber$Trip==1),],aes(x=RequestedTimeOfDay))+
  ggtitle("Daytime Supply vs Demand of Airport and City")

#lets see the plot again
ggplot(Uber,aes(x=RequestedTimeOfDay,fill = Status))+geom_histogram(stat = "count") +
  ggtitle("Status of Trip during different Daytime")
# we can see the results as:
#During Night hours there is huge rise in unavailable cars.
#During Morning hours most of the cab got cancelled by drivers.

ggplot(Uber,aes(x= RequestedTimeOfDay, fill = Status))+geom_bar()+facet_wrap(~Pickup.point)+
  ggtitle("Status of Trip during different Daytime of Airport and City")

#Plot to see the in-out cars from city during different hours.

ggplot()+
  geom_bar(alpha = 0.8, width = 0.3,fill = 'blue',data = Uber[which(Uber$Status== 'Cancelled' & Uber$Pickup.point=='City'),], aes(x= RequestedTimeOfDay))+
  geom_bar(alpha = 0.4,fill = "red", data = Uber[which(Uber$Trip==1 & Uber$Pickup.point=='City'),], aes(x= RequestedTimeOfDay))+
  geom_bar(alpha = 0.6, width = 0.6,fill = "green", data = Uber[which(Uber$Pickup.point=='Airport'),], aes(x= DropTimeOfDay))+
  ggtitle("Requests for City to Airport in different hours")

#Plot to see the in-out cars from Airport during different hours.
ggplot()+
  geom_bar(alpha = 0.8, width = 0.3,fill = 'blue',data = Uber[which(Uber$Status== 'No Cars Available' & Uber$Pickup.point=='Airport'),], aes(x= RequestedTimeOfDay), stat = "count")+
  geom_bar(alpha = 0.4, fill = "red", data = Uber[which(Uber$Trip == 1 & Uber$Pickup.point == 'Airport'),],aes(x = RequestedTimeOfDay))+
  geom_bar(alpha = 0.6, width = 0.6, fill = "green", data = Uber[which(Uber$Pickup.point=='City'),], aes(x= DropTimeOfDay)) +
  ggtitle("Requests for Airport to City in different hours")
  

#plot to see if the travel time is an issue during the problematic secens

ggplot(Uber, aes(x= RequestedTimeOfDay,y = tripDuration))+geom_boxplot()+facet_wrap(~Pickup.point)+
  ggtitle("Travel Time during Problematic secens")

median(Uber[which(Uber$RequestedTimeOfDay== "Night" & Uber$Pickup.point == "Airport"),]$tripDuration,na.rm = T)  
median(Uber[which(Uber$RequestedTimeOfDay== "Morning" & Uber$Pickup.point == "City"),]$tripDuration,na.rm = T)  
