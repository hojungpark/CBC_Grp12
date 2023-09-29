#Setup
setwd("C:/Users/shawn/OneDrive - Simon Fraser University (1sfu)/Documents/CMPT 318/CBC_Grp12/Assignment1")
dataset <- read.table("Group_Assignment_1_Dataset.txt", header=TRUE, sep=",")

dataset$Time <- as.POSIXlt(dataset$Time, format="%H:%M:%S")
dataset$Date <- as.POSIXlt(dataset$Date, format = "%d/%m/%Y")
dataset_week12 <- subset(dataset, dataset$Date >= as.POSIXlt("19/3/2007", format="%d/%m/%Y") & dataset$Date <= as.POSIXlt("25/3/2007", format="%d/%m/%Y"))

# Time Windows: 
#   Day Hours = 7:30 AM - 5:30 PM
#   Night Hours = 5:31 PM - 7:29 AM
#
#   Extract each 'Global_intensity' field from each WEEKDAY at a certain time in the DAY (ex. @ 8:22 AM) and calculate the average
#     This becomes one data point in our new data set
#   Extract each 'Global_intensity' field from each WEEKDAY at a certain time in the NIGHT (ex. @ 7:20 PM) and calculate the average
#     This becomes one data point in our new data set
#   Extract each 'Global_intensity' field from each WEEKEND at a certain time in the DAY (ex. @ 8:22 AM) and calculate the average
#     This becomes one data point in our new data set
#   Extract each 'Global_intensity' field from each WEEKEND at a certain time in the NIGHT (ex. @ 7:20 PM) and calculate the average
#     This becomes one data point in our new data set

# dataset_test <- na.omit(subset(dataset_week12, dataset$Time == as.POSIXlt("00:01:00", format="%H:%M:%S"))) # all rows with time = 12:01 AM
# GI_week1_mean <- mean(dataset_test$Global_intensity, na.rm = TRUE)

# Extracting all the rows for each day
data_for_day1 <- dataset_week12[dataset_week12$Date == as.POSIXlt("19/3/2007", format="%d/%m/%Y"),]
data_for_day2 <- dataset_week12[dataset_week12$Date == as.POSIXlt("20/3/2007", format="%d/%m/%Y"),]
data_for_day3 <- dataset_week12[dataset_week12$Date == as.POSIXlt("21/3/2007", format="%d/%m/%Y"),]
data_for_day4 <- dataset_week12[dataset_week12$Date == as.POSIXlt("22/3/2007", format="%d/%m/%Y"),]
data_for_day5 <- dataset_week12[dataset_week12$Date == as.POSIXlt("23/3/2007", format="%d/%m/%Y"),]
data_for_day6 <- dataset_week12[dataset_week12$Date == as.POSIXlt("24/3/2007", format="%d/%m/%Y"),]
data_for_day7 <- dataset_week12[dataset_week12$Date == as.POSIXlt("25/3/2007", format="%d/%m/%Y"),]

day1_day_hours <- data_for_day1[data_for_day1$Time >= as.POSIXlt("07:30:00", format="%H:%M:%S") & 
                                  data_for_day1$Time <= as.POSIXlt("17:30:00", format="%H:%M:%S"),]
# day1_night_hours <- data_for_day1[data_for_day1$Time <= as.POSIXlt("07:29:00", format="%H:%M:%S") & 
#                                   data_for_day1$Time >= as.POSIXlt("17:31:00", format="%H:%M:%S"),]  # This is returning an empty table for some reason, need to fix




