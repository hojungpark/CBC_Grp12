#Setup
# This setwd() should be changed to the appropriate directory on the marker's computer
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

# Extracting all the rows for each day
data_for_day1 <- dataset_week12[dataset_week12$Date == as.POSIXlt("19/3/2007", format="%d/%m/%Y"),]
data_for_day2 <- dataset_week12[dataset_week12$Date == as.POSIXlt("20/3/2007", format="%d/%m/%Y"),]
data_for_day3 <- dataset_week12[dataset_week12$Date == as.POSIXlt("21/3/2007", format="%d/%m/%Y"),]
data_for_day4 <- dataset_week12[dataset_week12$Date == as.POSIXlt("22/3/2007", format="%d/%m/%Y"),]
data_for_day5 <- dataset_week12[dataset_week12$Date == as.POSIXlt("23/3/2007", format="%d/%m/%Y"),]
data_for_day6 <- dataset_week12[dataset_week12$Date == as.POSIXlt("24/3/2007", format="%d/%m/%Y"),]
data_for_day7 <- dataset_week12[dataset_week12$Date == as.POSIXlt("25/3/2007", format="%d/%m/%Y"),]

# Day 1 Global_intensity values
day1_day_GI <- subset(data_for_day1[data_for_day1$Time >= as.POSIXlt("07:30:00", format="%H:%M:%S") & 
                                  data_for_day1$Time <= as.POSIXlt("17:30:00", format="%H:%M:%S"),], select = "Global_intensity")

day1_night_GI <- subset(data_for_day1[data_for_day1$Time >= as.POSIXlt("17:31:00", format="%H:%M:%S") |
                                    data_for_day1$Time <= as.POSIXlt("07:29:00", format="%H:%M:%S"),], select = "Global_intensity")
# Day 2 Global_intensity values
day2_day_GI <- subset(data_for_day2[data_for_day2$Time >= as.POSIXlt("07:30:00", format="%H:%M:%S") & 
                                      data_for_day2$Time <= as.POSIXlt("17:30:00", format="%H:%M:%S"),], select = "Global_intensity")

day2_night_GI <- subset(data_for_day2[data_for_day2$Time >= as.POSIXlt("17:31:00", format="%H:%M:%S") |
                                        data_for_day2$Time <= as.POSIXlt("07:29:00", format="%H:%M:%S"),], select = "Global_intensity")

# Day 3 Global_intensity values
day3_day_GI <- subset(data_for_day3[data_for_day3$Time >= as.POSIXlt("07:30:00", format="%H:%M:%S") & 
                                      data_for_day3$Time <= as.POSIXlt("17:30:00", format="%H:%M:%S"),], select = "Global_intensity")

day3_night_GI <- subset(data_for_day3[data_for_day3$Time >= as.POSIXlt("17:31:00", format="%H:%M:%S") |
                                        data_for_day3$Time <= as.POSIXlt("07:29:00", format="%H:%M:%S"),], select = "Global_intensity")
# Day 4 Global_intensity values
day4_day_GI <- subset(data_for_day4[data_for_day4$Time >= as.POSIXlt("07:30:00", format="%H:%M:%S") & 
                                      data_for_day4$Time <= as.POSIXlt("17:30:00", format="%H:%M:%S"),], select = "Global_intensity")

day4_night_GI <- subset(data_for_day4[data_for_day4$Time >= as.POSIXlt("17:31:00", format="%H:%M:%S") |
                                        data_for_day4$Time <= as.POSIXlt("07:29:00", format="%H:%M:%S"),], select = "Global_intensity")
# Day 5 Global_intensity values
day5_day_GI <- subset(data_for_day5[data_for_day5$Time >= as.POSIXlt("07:30:00", format="%H:%M:%S") & 
                                      data_for_day5$Time <= as.POSIXlt("17:30:00", format="%H:%M:%S"),], select = "Global_intensity")

day5_night_GI <- subset(data_for_day5[data_for_day5$Time >= as.POSIXlt("17:31:00", format="%H:%M:%S") |
                                        data_for_day5$Time <= as.POSIXlt("07:29:00", format="%H:%M:%S"),], select = "Global_intensity")
# Day 6 Global_intensity values
day6_day_GI <- subset(data_for_day6[data_for_day6$Time >= as.POSIXlt("07:30:00", format="%H:%M:%S") & 
                                      data_for_day6$Time <= as.POSIXlt("17:30:00", format="%H:%M:%S"),], select = "Global_intensity")

day6_night_GI <- subset(data_for_day6[data_for_day6$Time >= as.POSIXlt("17:31:00", format="%H:%M:%S") |
                                        data_for_day6$Time <= as.POSIXlt("07:29:00", format="%H:%M:%S"),], select = "Global_intensity")
# Day 7 Global_intensity values
day7_day_GI <- subset(data_for_day7[data_for_day7$Time >= as.POSIXlt("07:30:00", format="%H:%M:%S") & 
                                      data_for_day7$Time <= as.POSIXlt("17:30:00", format="%H:%M:%S"),], select = "Global_intensity")

day7_night_GI <- subset(data_for_day7[data_for_day7$Time >= as.POSIXlt("17:31:00", format="%H:%M:%S") |
                                        data_for_day7$Time <= as.POSIXlt("07:29:00", format="%H:%M:%S"),], select = "Global_intensity")

day_len <- nrow(day1_day_GI)
night_len <- nrow(day1_night_GI)

# Means of GI on WEEKDAYS during DAYTIME hours
means_wkday_day <- double(day_len)
combined_wkday_d <- cbind(day1_day_GI, day2_day_GI, day3_day_GI, day4_day_GI, day5_day_GI)
means_wkday_day <- data.frame(Mean = rowMeans(combined_wkday_d))

# Means of GI on WEEKDAYS during NIGHT TIME hours
means_wkday_night <- double(night_len)
combined_wkday_n <- cbind(day1_night_GI, day2_night_GI, day3_night_GI, day4_night_GI, day5_night_GI)
means_wkday_night <- data.frame(Mean = rowMeans(combined_wkday_n))

# Means of GI on WEEKENDS during DAYTIME hours
means_wkend_day <- double(day_len)
combined_wkend_d <- cbind(day6_day_GI, day7_day_GI)
means_wkend_day <- data.frame(Mean = rowMeans(combined_wkend_d))

# Means of GI on WEEKENDS during NIGHT hours
means_wkend_night <- double(night_len)
combined_wkend_n <- cbind(day6_night_GI, day7_night_GI)
means_wkend_night <- data.frame(Mean = rowMeans(combined_wkend_n))

# y is the mean Global_intensity values
# x is the time
day_time_vector <- 1:601
night_time_vector <- 1:839
wkday_means_day <- vector(, 601)
wkend_means_day <- vector(, 601)

wkday_means_night <- vector(, 839)
wkend_means_night <- vector(, 839)

# Creating vectors from frames so that we can plot it
for (i in 1:601) {
  wkday_means_day[i] <- unlist(means_wkday_day[i,])
  wkend_means_day[i] <- unlist(means_wkend_day[i,])

}
for (j in 1:839) {
  wkday_means_night[j] <- unlist(means_wkday_night[j,])
  wkend_means_night[j] <- unlist(means_wkend_night[j,])
}
################################################################################################################################
plot(day_time_vector, wkday_means_day, main = "Linear Regression of Day-time Global Intensity Values", type = 'n',
     ylab = "Mean Global Intensity Values", xlab = "Time (in minutes)")

lin_fit_wkday_day <- lm(wkday_means_day ~ day_time_vector)
abline(lin_fit_wkday_day, col="red")

lin_fit_wkend_day <- lm(wkend_means_day ~ day_time_vector)
abline(lin_fit_wkend_day, col="blue")

legend(x="right" , legend = c("Weekend", "Weekday"), lty = c(1,1), col = c(1,2))

################################################################################################################################
plot(night_time_vector, wkday_means_night, main = "Linear Regression of Night-time Global Intensity Values", type = 'n',
     ylab = "Mean Global Intensity Values", xlab = "Time (in minutes)")

lin_fit_wkday_night <- lm(wkday_means_night ~ night_time_vector)
abline(lin_fit_wkday_night, col="red")

lin_fit_wkend_night <- lm(wkend_means_night ~ night_time_vector)
abline(lin_fit_wkend_night, col="blue")

legend(x="topleft" , legend = c("Weekend", "Weekday"), lty = c(1,1), col = c(1,2))

################################################################################################################################
plot(day_time_vector, wkday_means_day, main = "Polynomial Regression of Day-time Global Intensity Values", type = 'n',
     ylab = "Mean Global Intensity Values", xlab = "Time (in minutes)", ylim = c(0,21))

fit_poly <- lm(wkday_means_day ~ poly(day_time_vector, 2, raw = TRUE))
lines(smooth.spline(day_time_vector, wkday_means_day), col="red")

fit_poly <- lm(wkend_means_day ~ poly(day_time_vector, 2, raw = TRUE))
lines(smooth.spline(day_time_vector, wkend_means_day), col="blue")

legend(x="topleft" , legend = c("Weekend", "Weekday"), lty = c(1,1), col = c(1,2))

################################################################################################################################
plot(day_time_vector, wkday_means_day, main = "Polynomial Regression of Night-time Global Intensity Values", type = 'n',
     ylab = "Mean Global Intensity Values", xlab = "Time (in minutes)", ylim = c(0,28))

fit_poly <- lm(wkday_means_night ~ poly(night_time_vector, 2, raw = TRUE))
lines(night_time_vector, wkday_means_night, col="red")

fit_poly <- lm(wkend_means_night ~ poly(night_time_vector, 2, raw = TRUE))
lines(night_time_vector, wkend_means_night, col="blue")

legend(x="topleft" , legend = c("Weekend", "Weekday"), lty = c(1,1), col = c(1,2))






