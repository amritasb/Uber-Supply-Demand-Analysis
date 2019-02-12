#
# Uber Supply Demand Analysis
# Author          - Amrita Bhadrannavar
#
# Assumptions
# Data Set File - "Uber Request Data.csv" is available in the working directory
#

#--------------------------------------------------------------------------------------------------#

#
# Load required libraries
#
library(lubridate)
library(ggplot2)
library(scales)


#
# Load & view the Data Set
#
DataSetFile <- "Uber Request Data.csv"

if (file.exists(DataSetFile)) {
  uber <- read.csv(DataSetFile, stringsAsFactors = FALSE)
  cat("Info  : Data Set file \"",DataSetFile,"\" successfully loaded\n")
  View(uber)
} else {
  cat("Error : Unable to access Data Set csv file -",DataSetFile,"\n")
  cat("Info  : Please verify the working directory and set as per Data Set file location\n")
}

#--------------------------------------------------------------------------------------------------#

#
# Data Cleaning and Manipulation
#
cat("Info  : Total number of records available for analysis = ",nrow(uber),"\n")


#
# Column - Request.id
cat("Info  : Verifying Column 'Request.id'\n")

# Check if NA values are present in Request.id column
if (sum(is.na(uber$Request.id)) == 0) {
  cat("Info  : Column 'Request.id' has no NA values. No Data manipulation required\n")
}


#
# Column - Pickup.point
cat("Info  : Verifying Column 'Pickup.point'\n")

# Check if NA values are present in Pickup.point column
if (sum(is.na(uber$Pickup.point)) == 0) {
  cat("Info  : Column 'Pickup.point' has no NA values. No Data manipulation required\n")
}


#
# Column - Driver.id
cat("Info  : Verifying Column 'Driver.id'\n")

# Check if NA values are present in Driver.id column
MissingDriverCount <- sum(is.na(uber$Driver.id))
if ( MissingDriverCount == 0) {
  cat("Info  : Column 'Driver.id' has no NA values. No Data manipulation required\n")
} else {
  cat("Info  : Column 'Driver.id' has NA values. Data manipulation required\n")
  cat("Info  : Total no of records with missing Driver ID =", MissingDriverCount,"\n")
}

# Check the corresponding Status for all missing driver ids
cat("Info  : Checking Status of Trip for records with missing Driver ID\n")
cat("Info  : Status is ",unique(uber$Status[is.na(uber$Driver.id)==T]),"\n")
# Output is "No Cars Available" which means Driver ID is NA only when no cars are available and
# Driver ID is available for all other status
cat("Info  : Driver ID is missing only when no cars available. Hence missing data is expected\n")

# Replace NA values with 0 to indicate no driver
cat("Info  : Convert NA values in Driver ID to 0 to indicate no driver\n")
uber$Driver.id[is.na(uber$Driver.id)==T] <- 0


#
# Column - Status
cat("Info  : Verifying Column 'Status'\n")

# Check if NA values are present in Status column
if (sum(is.na(uber$Status)) == 0) {
  cat("Info  : Column 'Status' has no NA values. No Data manipulation required\n")
}


#
# Column - Request.timestamp
cat("Info  : Verifying Column 'Request.timestamp'\n")

# Timestamp is two different formats which has to converted to one form
# %d-%m-%Y %H:%M:%S
# %d/%m,%Y %H:%M
cat("Info  : Request Timestamp is in different formats. Data manipulation required\n")

# Convert to single format
cat("Info  : Convert timestamp to single format and store in ReqTimestamp Column\n")
uber$ReqTimestamp <- parse_date_time(x = uber$Request.timestamp, 
                                     orders = c("%d-%m-%Y %H:%M:%S","%d/%m,%Y %H:%M"))

# Retrieve date from the timestamp
cat("Info  : Retrieve date from timestamp and save as ReqDate\n")
uber$ReqDate <- as.Date(uber$ReqTimestamp)

# Retrieve time (only hour) from the timestamp
cat("Info  : Retrieve hour from timestamp and save as ReqHour\n")
uber$ReqHour <- as.numeric(format(uber$ReqTimestamp, "%H"))


#
# Column - Drop.timestamp
cat("Info  : Verifying Column 'Drop.timestamp'\n")

# Timestamp is two different formats which has to converted to one form
# %d-%m-%Y %H:%M:%S
# %d/%m,%Y %H:%M
cat("Info  : Drop Timestamp is in different formats. Data manipulation required\n")

# Convert to single format
cat("Info  : Convert timestamp to single format and store in DropTimestamp Column\n")
uber$DropTimestamp <- parse_date_time(x = uber$Drop.timestamp, 
                                      orders = c("%d-%m-%Y %H:%M:%S","%d/%m,%Y %H:%M"))

# Retrieve date from the timestamp
cat("Info  : Retrieve date from timestamp and save as DropDate\n")
uber$DropDate <- as.Date(uber$DropTimestamp)

# Retrieve time (only hour) from the timestamp
cat("Info  : Retrieve hour from timestamp and save as DropHour\n")
uber$DropHour <- as.numeric(format(uber$DropTimestamp, "%H"))

# Drop.timestamp has NA values corresponding to Trip Status "No cars available"
# No change will be done as Drop.timestamp would not be required for analysis in 
# "No cars available" cases

cat("Info  : Data Set post cleaning and manipulation\n")
head(uber)
View(uber)

#--------------------------------------------------------------------------------------------------#

#
# Uber Demand Supply Analysis
#


#
# Demand will be total count of requests made. 
# Supply will be total count of successful trips completed.
# Demand and Supply can be analysed at different pickup point for better understanding

# Bar plots can be used to plot and retrieve demand and supply patterns of the Uber cabs
# Across X axis, time in hours can be plotted with discrete values for each hour. 
# Time can also be grouped into time slots for better understanding. Status wise plots are also 
# considered for analysis.
# Across Y axis, no of requests can be plotted based on aspect being analysed. 
# Using stacked bars, filled bars we can further introduce additional dimensions like Pickup point,
# Request Date, Trip Status
# Plots are labelled as needed using ggtitle and axis using labs. Using geom_text labels are added 
# as needed to the plots


#
# Demand pattern day wise grouped hourly

# Plot hourly Uber requests for each day to verify if the request pattern is similar for all days
ReqCount <- ggplot(uber, aes(x = factor(ReqHour), fill = factor(ReqDate)))
ReqCount_Plot <- ReqCount + geom_bar(stat='count', position = "dodge") + 
                        ggtitle("Uber Cabs Demand per hour between 11-15 July 2017") +
                        labs(x="Time in Hrs", y="No of Requests", fill="Request Date")
cat("Plot  : Uber Cabs Demand per hour between 11-15 July 2017\n")
ReqCount_Plot


#
# Demand pattern across 24hours aggregated for all days

# As per ReqCount_Plot, the hourly request pattern is similar in all days.
# So we can aggregate the data for all 5 days on the same axis of 24 hours for further analysis

# Plot hourly Uber requests to identify rush hours
ReqCount_PerHr <- ggplot(uber, aes(x = factor(ReqHour), fill = factor(Pickup.point)))
ReqCount_PerHr_Plot <- ReqCount_PerHr + geom_bar(stat='count', position = "dodge") +
                        ggtitle("Uber Cabs Demand per hour") +
                        labs(x="Time in Hrs", y="No of Requests",fill="Pickup Point")
cat("Plot  : Uber Cabs Hourly Demand\n")
ReqCount_PerHr_Plot


# 
# Time Slot creation

# As per ReqCount_PerHr_Plot, 24hours can be divided into 5 slots based on demand
# EarlyMorning  - 12AM - 04AM
# Morning       - 04AM - 10AM
# Day           - 10AM - 05PM
# Evening       - 05PM - 09PM
# Night         - 09PM - 12AM
cat("Info  : Based on demand, following time slots can be identified :
        EarlyMorning  - 12AM - 04AM
        Morning       - 04AM - 10AM
        Day           - 10AM - 05PM
        Evening       - 05PM - 09PM
        Night         - 09PM - 12AM\n")

# Create TimeSlot Column mapping Request Hour to timeslots as per above categories
cat("Info  : Create TimeSlot Column mapping time slots as per Request Time\n")
uber$TimeSlot <- ifelse(uber$ReqHour <= 4, "EarlyMorning", 
                    ifelse(uber$ReqHour <= 10,"Morning",
                      ifelse(uber$ReqHour <= 17,"Day",
                        ifelse(uber$ReqHour <= 21,"Evening","Night"))))
head(uber)


#
# Demand-Supply pattern in Time Slots

# Plot demands per timeslot identified
ReqCount_PerSlot <- ggplot(uber, aes(x = factor(TimeSlot)))
ReqCount_PerSlot_Plot <- ReqCount_PerSlot + geom_bar(stat='count',fill="skyblue") +
                          ggtitle("Uber Cabs Demand per time slot") +
                          labs(x="Time Slot", y="No of Requests") +
                          geom_text(stat='count',aes(label=..count..),vjust=1)
cat("Plot  : Uber Cabs Demand per Time Slot\n")
ReqCount_PerSlot_Plot

# Plot supply per timeslot identified
TripsDone <- subset(uber, uber$Status=="Trip Completed")
TripsDone_PerSlot <- ggplot(TripsDone, aes(x = factor(TimeSlot)))
TripsDone_PerSlot_Plot <- TripsDone_PerSlot + geom_bar(stat='count',fill="lightgreen") +
          ggtitle("Uber Cabs Supply per time slot") +
          labs(x="Time Slot", y="No of Requests") +
          geom_text(stat='count',aes(label=..count..),vjust=1)
cat("Plot  : Uber Cabs Supply per Time Slot\n")
TripsDone_PerSlot_Plot



#
# Demand pattern based on Trip Status across Time Slots

# ReqCount_PerSlot_Plot can be further analysed based on Trip Status

# Plot requests per timeslot grouped by Trip Status
ReqCount_PerSlot_Status <- ggplot(uber,aes(x=factor(TimeSlot),fill=factor(Status)))
ReqCount_PerSlot_Status_Plot <- ReqCount_PerSlot_Status + 
                    geom_bar(stat="count", position = "stack") +
                    ggtitle("Trip Status per Time Slot") +
                    scale_x_discrete(limits=c("EarlyMorning","Morning","Day","Evening","Night")) +
                    labs(x="Time Slots",y="Number of Requests", fill="Trip Status")+
                    scale_fill_discrete(limits=c("No Cars Available","Trip Completed","Cancelled")) +
                    geom_text(stat='count',aes(label=..count..),position = position_stack(vjust = 0.5))
cat("Plot  : Uber Cabs Request Count per Time Slot grouped by Status\n")
ReqCount_PerSlot_Status_Plot


#
# Identification of issues based on Demand-Supply Patterns plotted

# Based on the Demand Supply plots, following are the demand and supply count:
#
# TIME SLOT       DEMAND  SUPPLY  
# Early Morning     578     214   
# Morning          2346     970
# Day              1399     757
# Evening          1924     633
# Night             498     257
#

# From ReqCount_PerSlot_Status_Plot, following problems can be identified:
# 1. High % of requests are cancelled during the Morning Time slot
# 2. High % of requests are not accepted in Evening Time Slot due to unavailability of cars

# Due to above issues we can see a high demand-supply gap in Morning and Evening Time Slots

#--------------------------------------------------------------------------------------------------#

#
# Problem 1 Analysis
# High % of requests are cancelled during the Morning Time slot
#
cat("Info  : Morning Time Slot Cancellation Issue Analysis\n")

# Subset Morning Time Slot requests for analysis
MorningReq <- subset(uber, uber$TimeSlot=="Morning")
cat("Info  : Total no of requests in Morning Time Slot =", nrow(MorningReq),"\n")


# Plot request count in Morning Slot grouped by Status for all pickup points
ReqCount_Morning <- ggplot(MorningReq, aes(x=factor(Status), fill=factor(Pickup.point)))
ReqCount_Morning_Plot <- ReqCount_Morning + geom_bar(stat="count",position = "stack") +
                ggtitle("Uber Requests in Morning Time Slot") +
                labs(x="Status",y="Total count", fill="Pickup Point") + 
                geom_text(stat='count',aes(label=..count..),position = position_stack(vjust = 0.5))
cat("Plot  : Uber Cabs Request Count in Morning Time Slot grouped by Status\n")
ReqCount_Morning_Plot


# Calculate no of Cancelled trips
Morning_CancelReqCount <- length(which(MorningReq$Status=="Cancelled"))
Morning_CancelReqPer <- percent(Morning_CancelReqCount/nrow(MorningReq))
cat("Info  : Total no of cancelled requests in Morning Time Slot =", Morning_CancelReqCount,"\n")
cat("Info  : Percentage of Cancelled Requests in Morning Time Slot =", Morning_CancelReqPer,"\n")

# Calculate cancelled trips based on pickup point
Morning_CancelReqCount_Airport <- length(which((MorningReq$Status=="Cancelled") 
                                              & (MorningReq$Pickup.point=="Airport")))
Morning_CancelReqPer_Airport <- percent(Morning_CancelReqCount_Airport/Morning_CancelReqCount)
cat("Info  : Total no of Airport pickup point cancellations =",Morning_CancelReqCount_Airport,"\n")
cat("Info  : Percentage of Airport pickup point cancellations =",Morning_CancelReqPer_Airport,"\n")

Morning_CancelReqCount_City <- length(which((MorningReq$Status=="Cancelled") 
                                              & (MorningReq$Pickup.point=="City")))
Morning_CancelReqPer_City <- percent(Morning_CancelReqCount_City/Morning_CancelReqCount)
cat("Info  : Total no of City pickup point cancellations =",Morning_CancelReqCount_City,"\n")
cat("Info  : Percentage of City pickup point cancellations =",Morning_CancelReqPer_City,"\n")

# Cancellations are high when the pickup point is City and trip is to Airport
cat("Info  : Cancellations are high when the pickup point is City and trip is to Airport\n")


# Calculate Demand of City to Airport trips
Morning_ReqCount_City <- length(which(MorningReq$Pickup.point=="City"))
cat("Info  : Demand of Uber cars in Morning Time Slot at City =",Morning_ReqCount_City,"\n")

# Caluclate Supply of cars for City to Airport trips
Morning_TripComplete_City <- length(which((MorningReq$Pickup.point=="City") 
                                          & (MorningReq$Status=="Trip Completed")))
cat("Info  : Supply of Uber cars in Morning Time Slot at City=",Morning_TripComplete_City,"\n")


# Calculate % of demand fulfilled
Demand_Per_Fulfilled <- percent(Morning_TripComplete_City/Morning_ReqCount_City)
cat("Info  : In Morning Time, % of Demand of trip to Airport fulfilled =",Demand_Per_Fulfilled,"\n")


#
# Based on Demand and Supply values, there is definitely a gap which needs to be addressed
#
# In Morning Time Slot, issue of High Cancellation has been identified. 
# On futhur analysis, it was found that high % of cancellations, 96.5%, is from pickup point City.
#
# Based on flight patterns, high no of flights depart in Morning time slot of 4AM - 10AM. But the
# flight arrivals are less when compared to departures. 
# A driver who successfully completes a trip from City to Airport in Morning Time Slot would have
# to wait in Airport to pick a customer back to the city. Return to the City would be loss to the
# driver in terms of toll charges, fuel charges etc. Hence driver will wait in Airport thus having
# idle time in a day when additional trips could have been made if driver was in City.
#
# Hence a large number of cancellation can be seen for City to Airport trips in Morning Time Slots.
#
# Demand of cars to Airport in Morning Time Slot is 1845
# Supply of cars to Airport in Morning Time Slot is 535
# Only 29% of Demand to Airport is met in Morning Time Slot
# 
# Recommendations:
# > Drivers can be provided incentives for City to Airport trip
# > Customers can be charged toll charges, parking fee charges etc
# > Co-ordinate with Airport authority to understand flight patterns and increase the supply
# > Airport Car Pool to combine multiple requests to meet the demands
# > Penalty to driver for cancellations
# > Provide base amount to drivers who return to City with no customer
#

#--------------------------------------------------------------------------------------------------#

#
# Problem 2 Analysis
# High % of requests are not accepted in Evening Time Slot due to unavailability of cars
#
cat("Info  : Evening Time Slot Cars Availability Issue Analysis\n")

# Subset Evening Time Slot requests for analysis
EveningReq <- subset(uber, uber$TimeSlot=="Evening")
cat("Info  : Total no of requests in Evening Time Slot =", nrow(EveningReq),"\n")


# Plot request count in Evening Slot grouped by Status for all pickup points
ReqCount_Evening <- ggplot(EveningReq, aes(x=factor(Status), fill=factor(Pickup.point)))
ReqCount_Evening_Plot <- ReqCount_Evening + geom_bar(stat="count",position = "stack") +
        ggtitle("Uber Requests in Evening Time Slot") +
        labs(x="Status",y="Total count", fill="Pickup Point") + 
        geom_text(stat='count',aes(label=..count..),position = position_stack(vjust = 0.5))
cat("Plot  : Uber Cabs Request Count in Evening Time Slot grouped by Status\n")
ReqCount_Evening_Plot


# Calculate no of unavailable cars
Evening_NoCarReqCount <- length(which(EveningReq$Status=="No Cars Available"))
Evening_NoCarReqPer <- percent(Evening_NoCarReqCount/nrow(EveningReq))
cat("Info  : Total no unavailable cars in Evening Time Slot =", Evening_NoCarReqCount,"\n")
cat("Info  : Percentage of unavailable cars in Evening Time Slot =", Evening_NoCarReqPer,"\n")

# Calculate no of unavailable cars based on pickup point
Evening_NoCarReqCount_Airport <- length(which((EveningReq$Status=="No Cars Available") 
                                              & (EveningReq$Pickup.point=="Airport")))
Evening_NoCarReqPer_Airport <- percent(Evening_NoCarReqCount_Airport/Evening_NoCarReqCount)
cat("Info  : Total no of unavailable cars at Airport pickup point =",
                                  Evening_NoCarReqCount_Airport,"\n")
cat("Info  : Percentage of unavailable cars at Airport pickup point =",
                                  Evening_NoCarReqPer_Airport,"\n")

Evening_NoCarReqCount_City <- length(which((EveningReq$Status=="No Cars Available") 
                                              & (EveningReq$Pickup.point=="City")))
Evening_NoCarReqPer_City <- percent(Evening_NoCarReqCount_City/Evening_NoCarReqCount)
cat("Info  : Total no of unavailable cars at City pickup point =",Evening_NoCarReqCount_City,"\n")
cat("Info  : Percentage of unavailable cars at City pickup point =",Evening_NoCarReqPer_City,"\n")

# Cars availability is low in Evening Time Slot when the pickup point is Airport 
cat("Info  : High % of cars unavailability is seen when pickup is Airport in Evening\n")


# Calculate Demand of Airport to City trips
Evening_ReqCount_Airport <- length(which(EveningReq$Pickup.point=="Airport"))
cat("Info  : Demand of Uber cars in Evening Time Slot at Airport=",Evening_ReqCount_Airport,"\n")

# Caluclate Supply of cars for Airport to City trips
Evening_TripComplete_Airport <- length(which((EveningReq$Pickup.point=="Airport") 
                                              & (EveningReq$Status=="Trip Completed")))
cat("Info  : Supply of Uber cars in Evening Time Slot at Airport=",
                                  Evening_TripComplete_Airport,"\n")

# Calculate % of demand fulfilled
Demand_Per_Fulfilled <- percent(Evening_TripComplete_Airport/Evening_ReqCount_Airport)
cat("Info  : In Evening Time, % of Demand of trip to City fulfilled =",Demand_Per_Fulfilled,"\n")


#
# Based on Demand and Supply values, there is definitely a gap which needs to be addressed
#
# In Evening Time Slot, issue of cars availablity has been identified. 
# On futhur analysis, it was found that high % of unavailability of cars, 95.3%, is from 
# pickup point Airport.
#
# Based on flight patterns, high no of flights arrive in Evening time slot of 5PM - 9PM. But the
# flight departures are less when compared to arrivals. 
# As departures are less, less no of requests to Airport will be made from City to Airport. So less
# no of cars will be available in Airport to fulfill the demand from flight arrivals.
#
# Hence high unavailability of cars will be seen for Airport to City trips in Evening Time Slots.
#
# Demand of cars to Airport in Morning Time Slot is 1492
# Supply of cars to Airport in Morning Time Slot is 299
# Only 20% of Demand to Airport is met in Morning Time Slot
# 
# Recommendations:
# > Drivers can be provided incentives for Airport to City trips
# > Customers can be charged toll charges, parking fee charges etc
# > Co-ordinate with Airport authority to understand flight patterns and increase the supply
# > Airport Car Pool to combine multiple requests to same drop locations to meet the demands
# > Penalty to driver for cancellations
#

#--------------------------------------------------------------------------------------------------#

