## Set-up
getwd()
setwd("~/Desktop/sfu/CMPT-318/Assignment1")
dataset <- read.table("Group_Assignment_1_Dataset.txt", header=TRUE, sep=",")

dataset$Time <- as.POSIXlt(dataset$Time, format="%H:%M:%S")
dataset$Date <- as.POSIXlt(dataset$Date, format = "%d/%m/%Y")
start_of_week12 <- as.POSIXlt("19/3/2007", format="%d/%m/%Y")
end_of_week12 <- as.POSIXlt("25/3/2007", format="%d/%m/%Y")

dataset_week12 <- subset(dataset, dataset$Date >= start_of_week12 & dataset$Date <= end_of_week12)

mode <- function(x, na.rm = FALSE) {
  if(na.rm){ 
    x = x[!is.na(x)]
  }
  
  val <- unique(x)
  return(val[which.max(tabulate(match(x, val)))])
}

### 1
  ## Compute the arithmetic and the geometric mean, the median, the mode and the standard deviation for features A, B and C respectively.
  
  arithmetic_mean_A <- mean(dataset_week12$Global_active_power, na.rm = TRUE)
  geometric_mean_A <- exp(mean(log(dataset_week12$Global_active_power), na.rm = TRUE))
  median_A <- mean(dataset_week12$Global_active_power, na.rm = TRUE)
  mode_A <- mode(dataset_week12$Global_active_power, na.rm=TRUE)
  standard_deviation_A <- sd(ourData$Global_active_power, na.rm = TRUE)
  
  arithmetic_mean_B <- mean(dataset_week12$Global_reactive_power, na.rm = TRUE)
  geometric_mean_B <- exp(mean(log(dataset_week12$Global_reactive_power), na.rm = TRUE))
  median_B <- mean(dataset_week12$Global_reactive_power, na.rm = TRUE)
  mode_B <- mode(dataset_week12$Global_reactive_power, na.rm=TRUE)
  standard_deviation_B <- sd(ourData$Global_reactive_power, na.rm = TRUE)
  
  arithmetic_mean_C <- mean(dataset_week12$Voltage, na.rm = TRUE)
  geometric_mean_C <- exp(mean(log(dataset_week12$Voltage), na.rm = TRUE))
  median_C <- mean(dataset_week12$Voltage, na.rm = TRUE)
  mode_C <- mode(dataset_week12$Voltage, na.rm=TRUE)
  standard_deviation_C <- sd(ourData$Voltage, na.rm = TRUE)
  
  cat("Arithmetic mean of global active power", arithmetic_mean_A,
      "\nGeometric mean of global active power", geometric_mean_A,
      "\nMedian of global active power", median_A,
      "\nMode of global active power", mode_A,
      "\nStandard Deviation of global active power", standard_deviation_A, "\n")
  
  cat("Arithmetic mean of global reactive power", arithmetic_mean_B,
            "\nGeometric mean of global reactive power", geometric_mean_B,
            "\nMedian of global reactive power", median_B,
            "\nMode of global reactive power", mode_B,
            "\nStandard Deviation of global reactive power", standard_deviation_B, "\n")
  
  cat("Arithmetic mean of voltage", arithmetic_mean_C,
            "\nGeometric mean of voltage", geometric_mean_C,
            "\nMedian of voltage", median_C,
            "\nMode of voltage", mode_C,
            "\nStandard Deviation of voltage", standard_deviation_C, "\n")
  
  ## For features A and B compute the min and max values on weekdays and weekend days during day hours and night hours respectively
  days_of_the_week <- as.POSIXlt(dataset_week12$Date, format = "%d/%m/%Y")$wday #wday is the numeric weekday (0-6 starting from Sunday)
  start_of_daytime <- as.POSIXlt("06:00:00", format = "%H:%M:%S")
  end_of_daytime <- as.POSIXlt("18:00:00", format = "%H:%M:%S")
  
  weekdays_day <- subset(dataset_week12, days_of_the_week != 0 & days_of_the_week != 6 & dataset_week12$Time >= start_of_daytime & dataset_week12$Time <= end_of_daytime)
  weekdays_night <- subset(dataset_week12, days_of_the_week != 0 & days_of_the_week != 6 & (dataset_week12$Time < start_of_daytime | dataset_week12$Time > end_of_daytime))
  weekends_day <- subset(dataset_week12, (days_of_the_week == 0 | days_of_the_week == 6) & dataset_week12$Time >= start_of_daytime & dataset_week12$Time <= end_of_daytime)
  weekends_night <- subset(dataset_week12, (days_of_the_week == 0 | days_of_the_week == 6) & (dataset_week12$Time < start_of_daytime| dataset_week12$Time > end_of_daytime))
  
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
  
