## Set-up
library(ggplot2)
library(ggcorrplot)
setwd("~/Desktop/sfu/CMPT-318/Assignment1")

dataset <- read.table("Group_Assignment_1_Dataset.txt", header=TRUE, sep=",")
dataset$Time <- as.POSIXlt(dataset$Time, format="%H:%M:%S")
dataset$Date <- as.POSIXlt(dataset$Date, format = "%d/%m/%Y")
dataset_week12 <- subset(dataset, dataset$Date >=  as.POSIXlt("19/3/2007", format="%d/%m/%Y") & dataset$Date <= as.POSIXlt("25/3/2007", format="%d/%m/%Y"))

mode <- function(x, na.rm = FALSE) {
  if(na.rm){ 
    x = x[!is.na(x)]
  }
  
  val <- unique(x)
  return(val[which.max(tabulate(match(x, val)))])
}
########################################################################################################################################
### Question 1
## Compute the arithmetic and the geometric mean, the median, the mode and the standard deviation for features A, B and C respectively.

arithmetic_mean_A <- mean(dataset_week12$Global_active_power, na.rm = TRUE)
geometric_mean_A <- exp(mean(log(dataset_week12$Global_active_power), na.rm = TRUE))
median_A <- mean(dataset_week12$Global_active_power, na.rm = TRUE)
mode_A <- mode(dataset_week12$Global_active_power, na.rm=TRUE)
standard_deviation_A <- sd(dataset_week12$Global_active_power, na.rm = TRUE)

arithmetic_mean_B <- mean(dataset_week12$Global_reactive_power, na.rm = TRUE)
geometric_mean_B <- exp(mean(log(dataset_week12$Global_reactive_power), na.rm = TRUE))
median_B <- mean(dataset_week12$Global_reactive_power, na.rm = TRUE)
mode_B <- mode(dataset_week12$Global_reactive_power, na.rm=TRUE)
standard_deviation_B <- sd(dataset_week12$Global_reactive_power, na.rm = TRUE)

arithmetic_mean_C <- mean(dataset_week12$Voltage, na.rm = TRUE)
geometric_mean_C <- exp(mean(log(dataset_week12$Voltage), na.rm = TRUE))
median_C <- mean(dataset_week12$Voltage, na.rm = TRUE)
mode_C <- mode(dataset_week12$Voltage, na.rm=TRUE)
standard_deviation_C <- sd(dataset_week12$Voltage, na.rm = TRUE)

cat("Arithmetic mean of global active power", arithmetic_mean_A,
    "\nGeometric mean of global active power", geometric_mean_A,
    "\nMedian of global active power", median_A,
    "\nMode of global active power", mode_A,
    "\nStandard Deviation of global active power", standard_deviation_A, "\n" , 
    
    "\nArithmetic mean of global reactive power", arithmetic_mean_B,
    "\nGeometric mean of global reactive power", geometric_mean_B,
    "\nMedian of global reactive power", median_B,
    "\nMode of global reactive power", mode_B,
    "\nStandard Deviation of global reactive power", standard_deviation_B, "\n",
    
    "\nArithmetic mean of voltage", arithmetic_mean_C,
    "\nGeometric mean of voltage", geometric_mean_C,
    "\nMedian of voltage", median_C,
    "\nMode of voltage", mode_C,
    "\nStandard Deviation of voltage", standard_deviation_C, "\n")

## For features A and B compute the min and max values on weekdays and weekend days during day hours and night hours respectively
days_of_the_week <- as.POSIXlt(dataset_week12$Date, format = "%d/%m/%Y")$wday #wday is the numeric weekday (0-6 starting from Sunday)
start_of_daytime <- as.POSIXlt("06:00:00", format = "%H:%M:%S")
end_of_daytime <- as.POSIXlt("18:00:00", format = "%H:%M:%S")

weekdays_day <- subset(dataset_week12, days_of_the_week != 0 & days_of_the_week != 6 & dataset_week12$Time >= as.POSIXlt("06:00:00", format = "%H:%M:%S") & dataset_week12$Time <= as.POSIXlt("18:00:00", format = "%H:%M:%S"))
weekdays_night <- subset(dataset_week12, days_of_the_week != 0 & days_of_the_week != 6 & (dataset_week12$Time < as.POSIXlt("06:00:00", format = "%H:%M:%S") | dataset_week12$Time > as.POSIXlt("18:00:00", format = "%H:%M:%S")))
weekends_day <- subset(dataset_week12, (days_of_the_week == 0 | days_of_the_week == 6) & dataset_week12$Time >= as.POSIXlt("06:00:00", format = "%H:%M:%S") & dataset_week12$Time <= as.POSIXlt("18:00:00", format = "%H:%M:%S"))
weekends_night <- subset(dataset_week12, (days_of_the_week == 0 | days_of_the_week == 6) & (dataset_week12$Time < as.POSIXlt("06:00:00", format = "%H:%M:%S")| dataset_week12$Time > as.POSIXlt("18:00:00", format = "%H:%M:%S")))

# min & max for weekdays day & night (Global active power)
weekdays_day_min_A <- min(weekdays_day$Global_active_power, na.rm =TRUE)
weekdays_day_max_A <- max(weekdays_day$Global_active_power, na.rm =TRUE)
weekdays_night_min_A <- min(weekdays_night$Global_active_power, na.rm =TRUE)
weekdays_night_max_A <- max(weekdays_night$Global_active_power, na.rm =TRUE)

# min & max for weekends day & night (Global active power)
weekends_day_min_A <- min(weekends_day$Global_active_power, na.rm = TRUE)
weekends_day_max_A <- max(weekends_day$Global_active_power, na.rm = TRUE)
weekends_night_min_A <- min(weekends_night$Global_active_power, na.rm = TRUE)
weekends_night_max_A <- max(weekends_night$Global_active_power, na.rm = TRUE)

# min & max for weekdays day & night (Global reactive power)
weekdays_day_min_B <- min(weekdays_day$Global_reactive_power, na.rm =TRUE)
weekdays_day_max_B <- max(weekdays_day$Global_reactive_power, na.rm =TRUE)
weekdays_night_min_B <- min(weekdays_night$Global_reactive_power, na.rm =TRUE)
weekdays_night_max_B <- max(weekdays_night$Global_reactive_power, na.rm =TRUE)

# min & max for weekends day & night (Global reactive power)
weekends_day_min_B <- min(weekends_day$Global_reactive_power, na.rm = TRUE)
weekends_day_max_B <- max(weekends_day$Global_reactive_power, na.rm = TRUE)
weekends_night_min_B <- min(weekends_night$Global_reactive_power, na.rm = TRUE)
weekends_night_max_B <- max(weekends_night$Global_reactive_power, na.rm = TRUE)

cat("Minimum global active power on day time of weekdays", weekdays_day_min_A,
    "\nMaximum global active power on day time of weekdays", weekdays_day_max_A,
    "\nMinimum global active power on night time of weekdays", weekdays_night_max_A,
    "\nMaximum global active power on night time of weekdays", weekdays_night_max_A,
    
    "\nMinimum global active power on day time of weekends", weekends_day_min_A,
    "\nMaximum global active power on day time of weekends", weekends_day_max_A,
    "\nMinimum global active power on night time of weekends", weekends_night_max_A,
    "\nMaximum global active power on night time of weekends", weekends_night_max_A,
    
    "\nMinimum global reactive power on day time of weekdays", weekdays_day_min_B,
    "\nMaximum global reactive power on day time of weekdays", weekdays_day_max_B,
    "\nMinimum global reactive power on night time of weekdays", weekdays_night_max_B,
    "\nMaximum global reactive power on night time of weekdays", weekdays_night_max_B,
    
    "\nMinimum global reactive power on day time of weekends", weekends_day_min_B,
    "\nMaximum global reactive power on day time of weekends", weekends_day_max_B,
    "\nMinimum global reactive power on night time of weekends", weekends_night_max_B,
    "\nMaximum global reactive power on night time of weekends", weekends_night_max_B)

#########################################################################################################################
### Question 2
# Compute the correlation for each disjoint pair of the responses, A, B, C, D, E, F and G, using Pearson’s sample correlation coefficient
AB <- cor(dataset_week12$Global_active_power, dataset_week12$Global_reactive_power, method = "pearson", use="na.or.complete")
AC <- cor(dataset_week12$Global_active_power, dataset_week12$Voltage, method = "pearson", use="na.or.complete")
AD <- cor(dataset_week12$Global_active_power, dataset_week12$Global_intensity, method = "pearson", use="na.or.complete")
AE <- cor(dataset_week12$Global_active_power, dataset_week12$Sub_metering_1, method = "pearson", use="na.or.complete")
AF <- cor(dataset_week12$Global_active_power, dataset_week12$Sub_metering_2, method = "pearson", use="na.or.complete")
AG <- cor(dataset_week12$Global_active_power, dataset_week12$Sub_metering_3, method = "pearson", use="na.or.complete")

BC <- cor(dataset_week12$Global_reactive_power, dataset_week12$Voltage, method = "pearson", use="na.or.complete")
BD <- cor(dataset_week12$Global_reactive_power, dataset_week12$Global_intensity, method = "pearson",use="na.or.complete")
BE <- cor(dataset_week12$Global_reactive_power, dataset_week12$Sub_metering_1, method = "pearson", use="na.or.complete")
BF <- cor(dataset_week12$Global_reactive_power, dataset_week12$Sub_metering_2, method = "pearson", use="na.or.complete")
BG <- cor(dataset_week12$Global_reactive_power, dataset_week12$Sub_metering_3, method = "pearson", use="na.or.complete")

CD <- cor(dataset_week12$Voltage, dataset_week12$Global_intensity, method = "pearson", use="na.or.complete")
CE <- cor(dataset_week12$Voltage, dataset_week12$Sub_metering_1, method = "pearson", use="na.or.complete")
CF <- cor(dataset_week12$Voltage, dataset_week12$Sub_metering_2, method = "pearson", use="na.or.complete")
CG <- cor(dataset_week12$Voltage, dataset_week12$Sub_metering_3, method = "pearson", use="na.or.complete")

DE <- cor(dataset_week12$Global_intensity, dataset_week12$Sub_metering_1, method = "pearson", use="na.or.complete")
DF <- cor(dataset_week12$Global_intensity, dataset_week12$Sub_metering_2, method = "pearson", use="na.or.complete")
DG <- cor(dataset_week12$Global_intensity, dataset_week12$Sub_metering_3, method = "pearson", use="na.or.complete")

EF <- cor(dataset_week12$Sub_metering_1, dataset_week12$Sub_metering_2, method = "pearson", use="na.or.complete")
EG <- cor(dataset_week12$Sub_metering_1, dataset_week12$Sub_metering_3, method = "pearson", use="na.or.complete")

FG <- cor(dataset_week12$Sub_metering_2, dataset_week12$Sub_metering_3, method = "pearson", use="na.or.complete")

cat("Pearson correlation between:",
    "\nGlobal active power & Global reactive power", AB,
    "\nGlobal active power & Voltage", AC,
    "\nGlobal active power & Global intensity", AD,
    "\nGlobal active power & Submetering 1", AE,
    "\nGlobal active power & Submetering 2", AF,
    "\nGlobal active power & Submetering 3", AG,
    
    "\nGlobal reactive power & Voltage", BC,
    "\nGlobal reactive power & Global intensity", BD,
    "\nGlobal reactive power & Submetering 1", BE,
    "\nGlobal reactive power & Submetering 2", BF,
    "\nGlobal reactive power & Submetering 3", BG, 
    
    "\nVoltage & Global intensity", CD,
    "\nVoltage & Submetering 1", CE,
    "\nVoltage & Submetering 2", CF,
    "\nVoltage & Submetering 3", CG,
    
    "\nGlobal intensity & Submetering 1", DE,
    "\nGlobal intensity & Submetering 2", DF,
    "\nGlobal intensity & Submetering 3", DG,
    
    "\nSubmetering 1 & Submetering 2", EF,
    "\nSubmetering 1 & Submetering 3", EG,
    "\nSubmetering 2 & Submetering 3", FG)
# correlation analysis in terms of correlation matrix, reference: https://www.youtube.com/watch?v=E3De2A73ako
A_to_G <- dataset_week12[,-c(1,2)]
correlation_matrix<- round(cor(A_to_G, use="na.or.complete"), 4)
ggcorrplot::ggcorrplot(correlation_matrix, lab = TRUE, digits = 4)

###########################################################################################################
# Question 3

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