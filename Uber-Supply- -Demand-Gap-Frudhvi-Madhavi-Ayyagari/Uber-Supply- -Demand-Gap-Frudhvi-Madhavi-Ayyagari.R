rm(list = ls()) 
#-----------------------------------------------------------------------------
# Course 2 | Module 4 - Uber Supply-Demand Gap
# Individual Assignment 2 - Uber Supply-Demand Gap Analysis
# Name: Frudhvi Madhavi Ayyagari | E- madhavi.a07@gmail.com 
#-----------------------------------------------------------------------------

#-----------------------------------------------------------------------------
# STEP 1: DATA CLEANING
#-----------------------------------------------------------------------------

  # Time stamp format is changed to yyyy-MM-dd HH:mm:ss
  # Column headers are changed to Request_id, Pickup_point, Driver_id	Status, Request_timestamp & Drop_timestamp

#-----------------------------------------------------------------------------
# STEP 2: Import the Uber Request Data set in Rstudio
#-----------------------------------------------------------------------------

  library(readr)
  Uber_Request_Data <- read_csv("Uber Request Data.csv")
  str(Uber_Request_Data)
  
#-----------------------------------------------------------------------------
# STEP 3: Deriving data from columns
#-----------------------------------------------------------------------------

  # Extracting the hour of cab request from the first two characters Request_time stamp and saving in a seperate column of the dataframe uber_masterdata$request_hour <- sub(":.*","",uber_masterdata$Request.time)
  Uber_Request_Data$Request_Hour <- format(strptime(Uber_Request_Data$Request_timestamp,format = '%d-%m-%Y %H:%M'), "%H")
  Uber_Request_Data$Request_Hour <- as.numeric(Uber_Request_Data$Request_Hour)
  
#-----------------------------------------------------------------------------
# STEP 4: Plotting hourwise demand graph from the deriver Request_Hour
#-----------------------------------------------------------------------------
  
  # Load required packages for plotting graphs
  require(dplyr)
  require(ggplot2)
  require(scales)
  
  # Plot the number of cabs requested in a particular hour for all 05 days
    Hourwise_Request_Count <- ggplot(Uber_Request_Data,aes(x=factor(Request_Hour),fill=factor(Pickup_point)))
    Plot1 <- Hourwise_Request_Count+geom_bar(stat='count',position = "dodge")+
    ggtitle("HOURLY DEMAND FOR UBER CABS")+
    labs(x="Time in Hours", y="Number of Cabs Requested")+
    labs(fill="PICKUP POINT")
  
  # View the Plot 1
  Plot1

#-----------------------------------------------------------------------------
# STEP 5: Deriving necessary data frames for analysis
#-----------------------------------------------------------------------------
  
  # Create Time slot data frame
  Request_Hour <- c(0:23)
  Time_Slot <- c("Early_Morning (Upto 6am)","Morning(6am to 11am)","Day_Time(10am to 3pm)","Evening (3pm to 10pm)","Late_Night (10pm to Midnight)")
  Time_Slot_Seperator <- c(6,5,4,7,2)
  Time_Slot <- rep(Time_Slot,Time_Slot_Seperator)
  Time_Slot <- data.frame(Time_Slot,Request_Hour)
  # View the Time slot data frame
  Time_Slot
  
  # Merge the new data frame with the master data frame
  Uber_Request_Data <- merge(Uber_Request_Data,Time_Slot,by="Request_Hour",all.x=TRUE)
  
  #Subset Trips completed as a seperate data frame
  Trips_Completed <- subset(Uber_Request_Data,Uber_Request_Data$Status=="Trip Completed")
  
  #Subset Cancelled as a seperate data frame
  Cancelled_Trips <- subset(Uber_Request_Data,Uber_Request_Data$Status=="Cancelled")
  
  #Subset No Cars Available as a seperate data frame
  No_Cars_Available <- subset(Uber_Request_Data,Uber_Request_Data$Status=="No Cars Available")
  
#-----------------------------------------------------------------------------
# STEP 6: Plotting graphs for derived data
#-----------------------------------------------------------------------------
  
  #Plot a time slot wise graph for trips completed 
  Time_Slot_Bar_Completed <- ggplot(Trips_Completed,aes(x=Time_Slot))
  Plot2 <- Time_Slot_Bar_Completed+geom_bar(stat="count",col="gray",fill="gray")+
    ggtitle("TRIPS COMPLETED DURING DIFFERENT TIME SLOTS")+
    labs(x="Time Slots",y="Trips Completed")+
    geom_text(stat='count',aes(label=..count..),vjust=-1)+
    guides(fill=FALSE)+
    scale_x_discrete(limits=c("Early_Morning (Upto 6am)","Morning(6am to 11am)","Day_Time(11am to 3pm)","Evening (3pm to 10pm)","Late_Night (10pm to Midnight)"))
  
  # View the Plot 2
  Plot2
  
  #Plot a time slot wise graph for cancelled trips 
  Time_Slot_Bar_Cancelled <- ggplot(Cancelled_Trips,aes(x=Time_Slot))
  Plot3 <- Time_Slot_Bar_Cancelled+geom_bar(stat="count",col="gray",fill="gray")+
    ggtitle("TRIPS CANCELLED DURING DIFFERENT TIME SLOTS")+
    labs(x="Time Slots",y="Cancelled Trips")+
    geom_text(stat='count',aes(label=..count..),vjust=-1)+
    guides(fill=FALSE)+
    scale_x_discrete(limits=c("Early_Morning (Upto 6am)","Morning(6am to 11am)","Day_Time(11am to 3pm)","Evening (3pm to 10pm)","Late_Night (10pm to Midnight)"))
  
  # View the Plot 3
  Plot3
  
  #Plot a time slot wise graph for cancelled trips 
  Time_Slot_Bar_No_Cars_Available <- ggplot(No_Cars_Available,aes(x=Time_Slot))
  Plot4 <- Time_Slot_Bar_No_Cars_Available+geom_bar(stat="count",col="gray",fill="gray")+
    ggtitle("CARS NOT AVAILABLE DURING DIFFERENT TIME SLOTS")+
    labs(x="Time Slots",y="No Cars Available")+
    geom_text(stat='count',aes(label=..count..),vjust=-1)+
    guides(fill=FALSE)+
    scale_x_discrete(limits=c("Early_Morning (Upto 6am)","Morning(6am to 11am)","Day_Time(11am to 3pm)","Evening (3pm to 10pm)","Late_Night (10pm to Midnight)"))
  
  # View the Plot 4
  Plot4
  
  
  # Plot to show the consolidated graph for time zone
  Time_Slot_Request_Count <- ggplot(Uber_Request_Data,aes(x=factor(Time_Slot),fill=factor(Status)))
  Plot5 <- Time_Slot_Request_Count+geom_bar(stat="count",position = "stack",col="gray")+
    ggtitle("TRIPS DURING DIFFERENT TIME SLOTS")+
    scale_x_discrete(limits=c("Early_Morning (Upto 6am)","Morning(6am to 11am)","Day_Time(11am to 3pm)","Evening (3pm to 10pm)","Late_Night (10pm to Midnight)"))+
    labs(x="Time Slots",y="Number of Requests")+labs(fill="TRIP STATUS")+
    scale_fill_discrete(limits=c("Trip Completed","No Cars Available","Cancelled"))
  
  # View the Plot 5
  Plot5
  
#-----------------------------------------------------------------------------
# STEP 7: Identifying the problem areas
#-----------------------------------------------------------------------------

  Hypo_1_df <- subset(Uber_Request_Data,Uber_Request_Data$Time_Slot=="Early_Morning (Upto 6am)")
  Hypo_1_count <- ggplot(Hypo_1_df,aes(x=factor(Status),fill=factor(Pickup_point)))
  Plot6 <- Hypo_1_count+geom_bar(stat="count",position = "stack")+
    ggtitle("EARLY MORNING SLOT")+
    labs(x="Status",y="Total count")+
    labs(fill="PICKUP POINT")+scale_x_discrete(limits=c("Trip Completed","Cancelled","No Cars Available"))+
    annotate("text", x=-Inf,y=Inf,label="CONSOLIDATED", hjust=-.1,vjust=1)
  #view the plot
  Plot6
  
  
  Hypo_2_df <- subset(Uber_Request_Data,Uber_Request_Data$Time_Slot=="Evening (3pm to 10pm)")
  Hypo_2_count <- ggplot(Hypo_2_df,aes(x=factor(Status),fill=factor(Pickup_point)))
  Plot7 <- Hypo_2_count+geom_bar(stat="count",position = "stack")+
    ggtitle("EVENING SLOT")+
    labs(x="Status",y="Total count")+
    labs(fill="PICKUP POINT")+scale_x_discrete(limits=c("Trip Completed","Cancelled","No Cars Available"))+
    annotate("text", x=-Inf,y=Inf,label="CONSOLIDATED", hjust=-.1,vjust=1)
  #view the plot
  Plot7
  

  
#--------------------------------END OF ASSIGNMENT------------------------------------	

   
    


    
    
    